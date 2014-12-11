# options(java.parameters = "-Xmx1000m")
# require(xlsx)
# require(gdata)
# myDf <- read.xls ("~/Desktop/SBA_individual_loan_data/7a_504_DataFile_1990_201402.xlsx", sheet = 1, header = TRUE)
# 
# t1 <- read.xlsx2("~/Desktop/SBA_individual_loan_data/7a_504_DataFile_1990_201402.xlsx", 1 ) #, stringsAsFactors=F
# SecondTable <- read.xlsx2("MyExcelFile.xlsx", 2 , stringsAsFactors=F)
# 
# read.xlsx2("myfile.xlsx", sheetName = "Sheet1")
library(data.table)
library(sqldf)
library(ggplot2)
library(plyr)
library(stringr)
library(stringdist)

options("scipen"=20, "digits"=4) #this stops R from turning everything into exponents/exponentials/scientific notation 
sampler <- function (x,y,replacer=F) x[sample(1:nrow(x), size = (ceiling(nrow(x)*y)), replace= replacer, prob=NULL ),]##sampling function, takes dataset and proportion as inputs, optional replacement (default false)
bucketize <- function (x,y) floor((y*rank(x)- 1)/length(x)) ##floor function, takes variable and number of buckets as inputs
top15 <- function(dataset, olddataname, newdataname,topnum) { #dataset must be a data frame
  countries <-count(dataset, vars = olddataname) 
  countrylist = head(countries[order(-countries$freq),],topnum)#get top 15 countries, dump everyone else in same bucket
  colnames(countrylist)[colnames(countrylist)==olddataname] <- newdataname
  dataset_new <- merge(dataset,countrylist,by.x=c(olddataname), by.y =c(newdataname), all.x=T ) 
  #join in top n countries to dataset
  dataset_new[!is.na(dataset_new$freq),newdataname] <- as.vector(dataset_new[!is.na(dataset_new$freq),olddataname]) #fix levels of the top n countries (go from factor to vector)
  dataset_new[is.na(dataset_new$freq),newdataname] <- "Other" #rename things outside top n to "other"
  dataset_new= subset(dataset_new,select = -c(freq)) #remove (now useless) frequency column
  dataset_new[,newdataname] <- factor(dataset_new[,newdataname]) #reconverting back into a factor
  return(dataset_new)
} #olddataname and newdataname are passed as strings



dat <- read.csv(file="~/Desktop/SBA_individual_loan_data/RocDocs_individual_files/sheet1.csv", header=TRUE, sep=",")
dat2 <- read.csv(file="~/Desktop/SBA_individual_loan_data/RocDocs_individual_files/sheet2.csv", header=TRUE, sep=",")
# dat3 <- read.csv(file="~/Desktop/SBA_individual_loan_data/RocDocs_individual_files/sheet3.csv", header=TRUE, sep=",")

bigdat = rbind(dat,dat2)
rm(dat,dat2)

####do some entity resolution on business names

bigdat$cleanname = gsub("[^[:alnum:][:blank:]+&/\\-]", "", tolower(bigdat$BorrName)) #remove all punctuation except +&/- 
bigdat$cleanname=gsub("(^[[:space:]]+|[[:space:]]+$)", "", bigdat$cleanname) #remove leading and trailing whitespace
bigdat$cleanname=gsub("\\s+", " ", bigdat$cleanname) # replace multiple spaces with single space

throw_away_ends = c("llc", " co", "inc", " corp", " ltd", "company")
for (i in throw_away_ends) { #remove various endings
bigdat$cleanname=gsub(paste("(",i,"+$)",sep=""), "", bigdat$cleanname) 
}
bigdat$cleanname=gsub("(^[[:space:]]+|[[:space:]]+$)", "", bigdat$cleanname) #remove leading and trailing whitespace again because of endings removal

bigdat$streetnums = gsub("\\D","",bigdat$BorrStreet) ##keeps only digits of address
bigdat$concatname = paste(bigdat$BorrState, bigdat$cleanname, bigdat$streetnums,sep ="||") # use this as main identity/key for business

##this function finds close misspellings of businesses and maps back to one entity  
close_spelling_mapping <- function(business_name_vector) {
business_name_list=unique(business_name_vector[order(business_name_vector)]) ##grab unique names
business_name_comparisons= data.frame(cbind(business_name_list,c(business_name_list[2:length(business_name_list)],"the end"))) #find adjacent name examples
colnames(business_name_comparisons) = c("name1","name2")
business_name_comparisons$name1 = as.character(business_name_comparisons$name1)
business_name_comparisons$name2 = as.character(business_name_comparisons$name2)
business_name_comparisons$lengther =  nchar(business_name_comparisons$name1)
business_name_comparisons$stringdister = stringdist(business_name_comparisons$name1,business_name_comparisons$name2) #find string difference between alphabetically similar business names 
name_fixes= subset(business_name_comparisons, stringdister <3 & lengther > 6, select = c("name1","name2")) ##grabs names that are very close, in the same state (essentially guaranteed because it's alphabetized first by state), and longer than a few characters
return(name_fixes)
}

####run it to catch loan entries with 2 close names (around ~25k businesses)
business_maps = close_spelling_mapping(bigdat$concatname)
bigdat = merge(bigdat,business_maps, all.x=T, by.x = c("concatname"),by.y=c("name2"))
bigdat$concatname = ifelse(is.na(bigdat$name1),bigdat$concatname,bigdat$name1) #replace names if 
bigdat$name1 <- NULL
####run it again to catch loan entries with 3 close names (around 1100 businesses)
business_maps = close_spelling_mapping(bigdat$concatname)
bigdat = merge(bigdat,business_maps, all.x=T, by.x = c("concatname"),by.y=c("name2"))
bigdat$concatname = ifelse(is.na(bigdat$name1),bigdat$concatname,bigdat$name1)
bigdat$name1 <- NULL

###get things in the right format for the random forest

bigdat$dater= as.Date(bigdat$ApprovalDate, "%m-%d-%y")
bigdat$actual_year = year(bigdat$dater)
bigdat$chargeoff_year = year(as.Date(bigdat$ChargeOffDate, "%m-%d-%y"))
bigdat$Naics = substr(bigdat$NaicsCode,1,4)

bigdat = top15(data.frame(bigdat), "BorrState", "BorrState_label",50) # make buckets for top 50 BorrState
bigdat = top15(data.frame(bigdat), "BankName", "BankName_label",50) # make buckets for top 50 BankName
bigdat = top15(data.frame(bigdat), "Naics", "Naics_label",50) # make buckets for top 50 Naics 

##this data.table operation grabs the first element (essentially random) for  Naics_label, BankName_label, BusinessType, BorrState_label, takes min (non-null) chargeoff_year, takes mean (non-null) TermInMonths and InitialInterestRate, takes sum of GrossApproval, SBAGuaranteedApproval, GrossChargeOffAmount
dt= as.data.table(bigdat)
smalldat = dt[, list(Naics_label = Naics_label[1], BankName_label = BankName_label[1], BusinessType = BusinessType[1], BorrState_label = BorrState_label[1], subpgmdesc = subpgmdesc[1], chargeoff_year = min(as.numeric(as.character(chargeoff_year)), na.rm = TRUE), TermInMonths = mean(TermInMonths, na.rm = TRUE), GrossApproval = sum(GrossApproval, na.rm = TRUE), SBAGuaranteedApproval = sum(SBAGuaranteedApproval, na.rm = TRUE), InitialInterestRate = mean(InitialInterestRate, na.rm = TRUE), GrossChargeOffAmount = sum(GrossChargeOffAmount, na.rm = TRUE)), by=list(concatname,actual_year)]

#generate cross-join for year-business
years = data.table(2000:2013)
res<-setkey(smalldat[,c(k=1,.SD)],k)[years[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL] ##k is a dummy key that's removed at the end--this code does a cartesian join
res = data.frame(res)
names(res)[names(res)=="V1"] = "year_in_question"
res = subset(res, year_in_question >actual_year) #filter only for years_in_question that occur after the first loan
bigdat2 = as.data.frame(smalldat)

dur = sqldf("select a.*, coalesce(b.actual_year,0) as had_loan, coalesce(b.GrossApproval,0) as size_of_new_loan from res a left join bigdat2 b on a.concatname = b.concatname and a.year_in_question = b.actual_year")

dur$had_loan = ceiling(dur$had_loan/1000000) #transform had_loan into indicator variable
dur$priorchargeoff = ceiling((dur$year_in_question - dur$chargeoff_year )/100000) * dur$GrossChargeOffAmount
DT <- data.table(dur, key=c('concatname','year_in_question'))
DT = DT[order(DT$concatname,DT$year_in_question,-DT$actual_year),]
DT = DT[, num_previous_loans := cumsum(had_loan), by=list(concatname)] #this is broken, fix this at some point--right now all it does is identify duplicated rows

#split data into groups that have doublecounting problems, ones that don't, remove doubled-up businesses from doublecounters, rbind back together
singlecounters = subset(DT, num_previous_loans <= 0 )
doublecounters = subset(DT, num_previous_loans > 0 )
doublecounters$temp_keyid = paste(doublecounters$concatname,doublecounters$year_in_question)
setkey(doublecounters, temp_keyid)
doublecounters_fixed = doublecounters[J(unique(temp_keyid)),mult="first"] ###works super-fast with one key, breaks with anything else
doublecounters_fixed$temp_keyid = NULL

DT = rbind(singlecounters,doublecounters_fixed, use.names=TRUE)
DT <- data.table(DT, key=c('concatname','year_in_question'))
DT = DT[order(DT$concatname,DT$year_in_question,-DT$actual_year),]
DT = DT[, num_previous_loans := cumsum(had_loan), by=list(concatname)] #this should work now, post-duplication
DT$num_previous_loans = DT$num_previous_loans - as.numeric(as.character(DT$had_loan)) #this removes the counting during the year of the loan (duh)

invisible(lapply(names(DT),function(.name) set(DT, which(is.infinite(DT[[.name]])), j = .name,value =NA))) ###replaces infinities with NAs in DT
DT[is.na(DT)] <- -1

factors = subset(DT, select = c(had_loan, BusinessType , BorrState_label , BankName_label , subpgmdesc , Naics_label , year_in_question , actual_year )) #grab factor variables
nonfactors = subset(DT, select = -c(had_loan,BusinessType , BorrState_label , BankName_label , subpgmdesc , Naics_label , year_in_question , actual_year )) #grab non-factor variables
factories = data.frame(lapply(factors,  function (x) as.factor(x)))
final_prepped = cbind(factories, nonfactors)
rm(factors,nonfactors,factories,res,DT,dur,dt,singlecounters,doublecounters_fixed,doublecounters)

final_prepped$years_left_on_loan= as.numeric(as.character(final_prepped$actual_year)) + as.numeric(as.character(final_prepped$TermInMonths))/12 - as.numeric(as.character(final_prepped$year_in_question)) 

 write.table(final_prepped,"~/Desktop/SBA_data_parsed.csv", row.names=FALSE , col.names=T ,quote=F, append = FALSE, sep="," , eol="\r\n")


