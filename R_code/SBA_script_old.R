library(plyr)
library(ggplot2)
dat <- read.csv(file="~/Desktop/oregon_ucc_test.csv", header=TRUE, sep=",")
dat <- read.csv(file="~/Desktop/SBALOANS_ASSISTANCE_2013.10.22_ACCEPTED_TRANSACTIONS.txt", header=TRUE, sep="\t")
dat <- read.csv(file="~/Desktop/SBA_individual_loan_data/usaspending_individual_files/2013_SBA_Loans_Full_20140915.csv", header=TRUE, sep=",")
samp = read.csv(file="~/Desktop/sba7a13_sample.txt", header=TRUE, sep="\t")

sum(abs(dat$face_loan_guran))

names(dat)
head(dat)

rm(dat)
f= subset(dat, is.na(duns_no)==F)
length(unique(f$duns_no))


fff = tolower(unique(dat$recipient_name))
fff = tolower(dat$recipient_name)
tester = sample(dat$cleannname, 100)
dat$cleanname = tolower(dat$recipient_name)
# if I wanted to remove ALL punctuation
#dat$cleannname = gsub("[[:punct:]]", "", dat$cleanname)
#remove all punctuation except +&/-
dat$cleannname = gsub("[^[:alnum:][:blank:]+&/\\-]", "", dat$cleanname)

text_to_find <- c("dmd", "dent", "dds")
dat2 = subset(dat, grepl(paste(text_to_find,collapse="|"), dat$cleanname) )
head(dat2)

co_dentists = subset(dat2,principal_place_state_code =="CO")

 bob <- data.frame(lapply(bob, as.character), stringsAsFactors=FALSE)

data.frame(lapply(data.frame(co_dentists$cleanname, co_dentists$principal_place_cc), as.character), stringsAsFactors=FALSE)


#this finds all elements that match this string
fff[grep('machine tool, inc', fff, fixed="TRUE")]

all_CA = subset(dat,principal_place_state_code =="CA" & abs(face_loan_guran) > 500000 & abs( face_loan_guran) < 2000000)


#group by
ddply(all_CA,~principal_place_cc,summarise,meaner=mean(face_loan_guran),summer=sum(abs(face_loan_guran)), counter= length(face_loan_guran))



dev.off()
x11()
par(bg = 'white') 
ggplot(dat, aes(face_loan_guran)) + geom_histogram(binwidth=4) 
+stat_bin(aes(y=cumsum(..density..)),geom="line",color="green",binwidth=binwidther) + geom_density()


dat2 = subset(dat2, grepl('umpqua', tolower(dat2$SECURED.PARTY), fixed="TRUE"))
