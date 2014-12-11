SCRAP

options("scipen"=20, "digits"=4) #this stops R from turning everything into exponents/exponentials/scientific notation 


fff = subset(dat, is.na(GrossApproval))

#group by
ddply(bigdat,~round(bigdat$InitialInterestRate/1000,0) + ApprovalFiscalYear,summarise,meaner=mean(GrossApproval),summer=sum(abs(as.numeric(GrossApproval))), counter= length(GrossApproval))

unique(dat3$DeliveryMethod)

fff= subset(sampler(bigdat, .1), is.na(GrossApproval)==F)
fff=subset (bigdat, GrossApproval > 500000)

###histogram
dev.off()
x11()
par(bg = 'white') 
ggplot(fff, aes(x = GrossApproval)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_x_continuous(limits = c(0, 2000000))

ddply(dat3,~round(InitialInterestRate/1000,0) + ApprovalFiscalYear,summarise,meaner=mean(GrossApproval),summer=sum(abs(as.numeric(GrossApproval))), counter= length(GrossApproval))

fff = cumsum(as.numeric(bigdat$GrossApproval[order(bigdat$GrossApproval)]))

plot(fff)



text_to_find <- c("dmd","dds","dent")
head(subset(bigdat, grepl(paste(text_to_find,collapse="|"), bigdat$concatname) ),10)

fff = ddply(bigdat,~cleanname,summarise,meaner=mean(GrossApproval),summer=sum(abs(as.numeric(GrossApproval))), counter= length(GrossApproval))

lol = subset(fff, counter > 1)

fff2 = unique(paste(substr(bigdat$cleanname,1,10) , bigdat$BorrZip))

fff2 = unique(paste(substr(bigdat$cleanname,1,20) ))



fff=unique(bigdat$BankName[order(bigdat$BankName)])
fff[5500:5520]

fff = sqldf("select * from (select substr(cleanname,1,30) || BorrState , count(distinct actual_year) as counter from bigdat where cleanname is not null group by 1) a where counter > 1")
fff = sqldf("select * from (select cleanname || BorrState || streetnums as namer, count(distinct actual_year) as counter, min(ApprovalFiscalYear)  as miner from bigdat where cleanname is not null group by 1) a where counter > 1")
fff = sqldf("select * from (select substr(cleanname,1,10)  || BorrState || streetnums as namer, count(distinct actual_year) as counter from bigdat where cleanname is not null group by 1) a where counter > 1")

sqldf("select sum(case when indicator > 0 then 1 else 0 end) numberer from ggg")

ggg = sqldf("select a.*,  case when miner = actual_year then 0 else coalesce(counter, 0) end as repeater from (select *, a.cleanname || a.BorrState namer from bigdat a ) a left join fff b on a.namer = b.namer  ")

sum(fff$counter)
head(fff,10)

sqldf("select count ( distinct nerr ) from (select cleanname || BorrState || streetnums || actual_year as nerr from bigdat) a ")










options(java.parameters = "-Xmx1000m")

require(xlsx)
myDf <- read.xls ("~/Desktop/SBA_individual_loan_data/7a_504_DataFile_1990_201402.xlsx", sheet = 1, header = TRUE)

t1 <- read.xlsx2("~/Desktop/SBA_individual_loan_data/7a_504_DataFile_1990_201402.xlsx", 1 ) #, stringsAsFactors=F
SecondTable <- read.xlsx2("MyExcelFile.xlsx", 2 , stringsAsFactors=F)

read.xlsx2("myfile.xlsx", sheetName = "Sheet1")

sampler(subset(final_prepped, num_previous_loans >3),.000)

text_to_find <- c("construction")
head(subset(bigdat, grepl(paste(text_to_find,collapse="|"), bigdat$concatname)) ,10)


bigdat2 = sqldf("select concatname, actual_year, Naics_label, BankName_label, BusinessType, BorrState_label, subpgmdesc, chargeoff_year, avg(TermInMonths) as TermInMonths,  sum(GrossApproval) as GrossApproval, sum(SBAGuaranteedApproval) as SBAGuaranteedApproval, avg(InitialInterestRate) as InitialInterestRate, sum (GrossChargeOffAmount) as GrossChargeOffAmount, count(*) counter from bigdat group by 1,2,3,4,5,6,7,8")

####some experiments with entity resolution
business_name_vector=bigdat$concatname
business_name_list=unique(business_name_vector[order(business_name_vector)]) ##grab unique names
business_name_comparisons= data.frame(cbind(business_name_list,c(business_name_list[2:length(business_name_list)],"the end")))
colnames(business_name_comparisons) = c("name1","name2")
business_name_comparisons$name1 = as.character(business_name_comparisons$name1)
business_name_comparisons$name2 = as.character(business_name_comparisons$name2)
business_name_comparisons$lengther =  nchar(business_name_comparisons$name1)
business_name_comparisons$stringdister = stringdist(business_name_comparisons$name1,business_name_comparisons$name2) #find string difference between alphabetically similar business names 
name_fixes= subset(business_name_comparisons, stringdister <3 & lengther > 6 & ( substr(name1, nchar(name1)-1,nchar(name1))==substr(name2, nchar(name2)-1,nchar(name2)) ), select = c("name1","name2")) ##grabs names that are very close, in the same state, and longer than a few characters

sampler(subset(fff, is.na(name1)==F),.0001)

# qqq = subset(ff, substr(ff[,1],1,floor(lengther/2))==substr(ff[,2],1,floor(lengther/2)))
# lol = subset(ff, substr(fff,lengther - 2,lengther) =="inc")

cbind(fff )
fff[order(fff$BorrStreet),]


lol = fff$BorrStreet
gsub("\\D","",lol) ##keeps only digits

qq = subset(ff, substr(ff[,2], )


###this is from http://www.r-bloggers.com/down-sampling-using-random-forests/
####functions don't work
training = sampler(DT,.08)
testing = sampler(DT,.18)

nmin <- sum(training$had_loan == 0)

ctrl <- trainControl(method = "cv",
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary)
 
 set.seed(2)
 rfDownsampled <- train(Class ~ ., data = training,
                        method = "rf",
                        ntree = 150,
                        tuneLength = 5,
                        metric = "ROC",
                        trControl = ctrl,
                        ## Tell randomForest to sample by strata. Here, 
                        ## that means within each class
                        strata = training$Class,
                        ## Now specify that the number of samples selected
                        ## within each class should be the same
                        sampsize = rep(nmin, 2))

#################################

iris = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", sep = ",", header = FALSE)
names(iris) = c("sepal.length", "sepal.width", "petal.length", "petal.width", "iris.type")

iris$indicator = ifelse(iris$iris.type == 'Iris-virginica',1,0)


model = randomForest(as.factor(iris$indicator) ~ iris$sepal.length , data = iris, sampsize=c(10,10), strata=iris$indicator, ntree = 70)



sqldf(" select count(distinct BusinessType) BusinessType, count(distinct BorrState) BorrState, count(distinct BankName) BankName, count(distinct GrossApproval) GrossApproval, count(distinct SBAGuaranteedApproval) SBAGuaranteedApproval, count(distinct subpgmdesc) subpgmdesc, count(distinct InitialInterestRate) InitialInterestRate, count(distinct TermInMonths) TermInMonths,  count(distinct Naics) Naics, count(distinct year_in_question) year_in_question, count(distinct actual_year) actual_year from DT ")




training = top15(data.frame(training), "BorrState", "BorrState_label",50) # make buckets for top 50 BorrState
training = top15(data.frame(training), "BankName", "BankName_label",50) # make buckets for top 50 BankName
training = top15(data.frame(training), "Naics", "Naics_label",50) # make buckets for top 50 Naics 


#histogram of votes for random forest
lol = sampler(grrr,.08)
dev.off()
x11()
par(bg = 'white') 
ggplot(lol, aes(votes_yes)) + geom_histogram() 


eng2 = cbind( nonperced,do.call(cbind,lapply(perced,  function (x) percentilize(x))))


#group by
tester_bins = ddply(tester,~gross_bins,summarise,meaner=mean(as.numeric(as.character(had_loan))), counter= length(had_loan))


dev.off()
x11()
par(bg = 'white') 

plot(tester_bins)

durr  = final_prepped
durr[,1]=NULL
durr = subset(durr, as.numeric(as.character(actual_year)) >2010) # InitialInterestRate>0
durr = subset(durr, as.numeric(as.character(year_in_question)) ==2013) # InitialInterestRate>0
durr = subset(durr, num_previous_loans>0) # BorrState_label =='CO'

# durr = subset(durr, InitialInterestRate>0) 
# durr$relevant_bins = as.numeric(as.character(durr$year_in_question)) - as.numeric(as.character(durr$actual_year))
durr$relevant_bins = bucketize(durr$num_previous_loans, 100)
durr$relevant_bins = durr$num_previous_loans

fff = ddply(durr,~relevant_bins,summarise,meaner=mean(as.numeric(as.character(had_loan))), counter= length(had_loan))
plot(fff$relevant_bins, fff$meaner)
fff

length(unique(bigdat$BankName))
nrow(subset(bigdat, toupper(BankName) == 'BILBAO'))

sqldf("select sum(GrossApproval) from bigdat where upper(BankName) == 'SQUARE 1 BANK'")

bestest = tester[1:60000,]
bestest= subset(bestest, as.numeric(as.character(actual_year ))> 2007 & as.numeric(as.character(year_in_question )) ==2013 & as.numeric(as.character(had_loan)) ==0)

fff = subset(bigdat, select = c("concatname", "BorrName","BorrStreet","BorrState", "BorrCity", "BorrZip","GrossApproval" ), as.numeric(as.character(actual_year ))> 2007)

lol = sqldf("select a.* from fff a inner join bestest b  on a.concatname = b.concatname")
business_names = unique(lol)

# write.table(business_names,"~/Desktop/good_candidate_business_names.csv", row.names=FALSE , col.names=T ,quote=F, append = FALSE, sep="\t" , eol="\r\n")

text_to_find <- c("ensoft")
fff = subset(bigdat, grepl(paste(text_to_find,collapse="|"), bigdat$concatname) )

fff$
ff = subset(fff, as.character(BorrState) =='CO')


###histogram
dev.off()
x11()
par(bg = 'white') 
ggplot(subset(tester, as.numeric(as.character(had_loan))==1), aes(x = num_previous_loans)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
scale_x_continuous(limits = c(0, 50))



tester = tester[, how_many_pos := cumsum(as.numeric(as.character(had_loan)))]
tester$hit_rate = 1:nrow(tester)
tester$hit_rate = tester$how_many_pos/tester$hit_rate




fff = sampler(training, .01)
model = randomForest(x=fff[,2:5], y=as.factor(fff[,1]), ntree = num_trees, replace=TRUE) ## currently doesn't work, only works with variable specification

print(model)

fff= top60[order(top60$votes_no),]
# fff = fff[10000:10010,] 


text_to_find <- c("Roofing")
fff = subset(lol, grepl(paste(text_to_find,collapse="|"), lol$NaicsDescription) )
# fff = subset(fff, select = c("NaicsDescription", "NaicsCode"))


ff = as.character(bigdat$NaicsDescription)
naics_list=unique(ff[order(ff)]) ##grab unique names


text_to_find <- c("daycare")
fff = subset(bigdat, grepl(paste(text_to_find,collapse="|"), bigdat$concatname))
nerr = unique(fff$NaicsDescription)
