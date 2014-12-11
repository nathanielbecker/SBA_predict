# library(rpart)
# library(AppliedPredictiveModeling)
library(randomForest)
library(ROCR)
# library(Hmisc)
# library(caret)
# library(e1071)
# library(klaR)
# source("http://bioconductor.org/biocLite.R")
# biocLite("IRanges")
# require(IRanges)
# library(zoo)

minimum_biz_year_of_last_loan = 2005
threshold_year_start_training = 2006
year_to_test = 2013
num_trees = 151
pos_neg_ratio= 1
features_to_try=
countery = 1
outputs_for_comparison=NULL

fff = colnames(final_prepped) 
vector_of_features = fff [! fff %in% c('had_loan','concatname','size_of_new_loan')]

loopy_list = list(vector_of_features[c(4,5,7,9,12,14)], vector_of_features[c(4,5,7,9,12,14,15)],  vector_of_features[c(4,5,7,9,12,14,16)])

for (a in loopy_list) { #,2010
#   minimum_biz_year_of_last_loan = a
#   threshold_year_start_training = a + 1
#     num_trees = a
# specification = make_features_into_formula(a)
# a = vector_of_features[c(12,9,5,4,10,15)]
  features_to_try = a
    tryCatch({

training = subset(final_prepped, as.numeric(as.character(year_in_question)) >= threshold_year_start_training & as.numeric(as.character(year_in_question)) < year_to_test & as.numeric(as.character(actual_year))  >= minimum_biz_year_of_last_loan )
testing = subset(final_prepped, as.numeric(as.character(actual_year))  >= minimum_biz_year_of_last_loan & as.numeric(as.character(year_in_question)) == year_to_test)

tmp = as.vector(table(training$had_loan))
num_classes = length(tmp)
min_size = tmp[order(tmp,decreasing=FALSE)[1]]
# sampsizes = rep(min_size,num_classes)
sampsizes = c(min_size*pos_neg_ratio,min_size)

#this specification works FAR worse than one fewer features
# specification = had_loan ~ BusinessType + BorrState_label + BankName_label + GrossApproval + SBAGuaranteedApproval + subpgmdesc + InitialInterestRate + TermInMonths + Naics_label + year_in_question + actual_year + num_previous_loans
# specification = had_loan ~ InitialInterestRate + TermInMonths + Naics_label + priorchargeoff + num_previous_loans + actual_year

model = randomForest(x=data.frame(training[,features_to_try]), y=as.factor(training[,1]), sampsize=sampsizes, strata=as.factor(training$had_loan), ntree = num_trees, replace=TRUE) 

print(model)
print(importance(model))

# testing = training
fff <- data.frame(predict(model,testing,"prob"))
names(fff) = c("votes_no", "votes_yes")
grrr <- cbind(testing,fff)

tester = data.table(grrr)
# tester$gross_bins = bucketize(tester$votes_yes, 100)
# tester$percentages = percentilize(tester$votes_yes)

tester = tester[order(-tester$votes_yes),]
# tester = tester[40000:nrow(tester),]
# tester = tester[5500:nrow(tester),]

tester = tester[, how_many_pos := cumsum(as.numeric(as.character(had_loan)))]
#this gives the rolling window to apply
rolling_window = 6000
tester$how_many_pos_back = c(tester$how_many_pos[rolling_window:nrow(tester)],rep(0,rolling_window - 1))
tester$hit_rate = (tester$how_many_pos_back - tester$how_many_pos) / rolling_window

#  tester_just_2013 = subset(tester, as.numeric(as.character(year_in_question = 2013)))
#  dev.off(); 
# x11(); par(bg = 'white')
plot(tester$hit_rate,col="royalblue", lwd=2, pch=20, xlim= c(1, 100000), ylim=c(0,.1))

outputs_for_comparison = rbind(outputs_for_comparison,cbind(max(tester$hit_rate),tester$hit_rate[20000],mean(as.numeric(as.character(tester$had_loan))), nrow(training), toString(a), min_size, minimum_biz_year_of_last_loan , threshold_year_start_training , year_to_test, num_trees, countery ) )
colnames(outputs_for_comparison)[1:5] = c("hit_rate_max","hit_rate_20k","sample_avg_hit_rate","training_size", "specification")
print(outputs_for_comparison)

dev.copy(jpeg,filename=paste("~/Desktop/model_plots/",countery, ".jpg", sep=""))
dev.off(); 
# dev.off(); 

countery = countery +1
  print(countery)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}  
  
 write.table(outputs_for_comparison,"~/Desktop/model_plots/outputs_for_comparison.csv", row.names=FALSE , col.names=T ,quote=F, append = FALSE, sep="\t" , eol="\r\n")

  i

####this builds an ROC curve
dev.off(); x11(); par(bg = 'white')
predictions=as.vector(model$votes[,2])
pred=prediction(predictions,training$had_loan)
#First calculate the AUC value
perf_AUC=performance(pred,"auc")
AUC=perf_AUC@y.values[[1]]
#Then, plot the actual ROC curve
perf_ROC=performance(pred,"tpr","fpr")
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
abline(0,1)
dev.off()

#join in data on top 60k people
top60 = head(tester,60000)
lol = data.table(bigdat)
setkey(lol, concatname)
lol<-lol[J(unique(concatname)),mult="first"]
top60 = merge(top60, lol, by=c("concatname"))
rm(lol)


ff = fff[, .N, by = substr(BorrZip,1,3)]

ff[,order]
##get most popular zipcodes






#####crap below here

### test the model on a small sample
fit <- rpart(had_loan ~ BusinessType + BorrState + BankName + GrossApproval + SBAGuaranteedApproval + subpgmdesc + InitialInterestRate + TermInMonths  + Naics + year_in_question + actual_year,  method="class", data=tester)

fit <- rpart(had_loan ~  GrossApproval ,  method="class", data=tester)

printcp(fit) # display the results 
dev.off()
x11()
par(bg = 'white') 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])


fit <- randomForest(had_loan ~  GrossApproval,   data=tester)
print(fit) # view results 
importance(fit) # importance of each predictor

##########caret naive bayes

pos = final_prepped[final_prepped$had_loan == '1',]

tester = rbind(sampler(final_prepped,.01), sampler(pos, 7,T))
nrow(tester[tester$had_loan == '1',])
tester2 = sampler(tester, .1)

cv = sampler(final_prepped,.1)

# define training control
train_control <- trainControl(method="cv", number=4)
# train the model 
# model <- train(had_loan ~ GrossApproval + Naics_label, data=tester2, trControl=train_control, method="nb")

features = tester2[,c("GrossApproval","Naics_label", "BorrState_label","BankName_label", "subpgmdesc","InitialInterestRate", "TermInMonths", "year_in_question", " actual_year")]
yvar = tester2[,"had_loan"]
model <- train(y= yvar, x = features , data=tester2, trControl=train_control, method="nb")


 + BorrState_label + BankName_label + GrossApproval + SBAGuaranteedApproval + subpgmdesc + InitialInterestRate + TermInMonths + Naics_label + year_in_question + actual_year

model <- train(iris.type~sepal.length +  sepal.width + petal.length + petal.width, data=iris, trControl=train_control, method="nb")

table(predict(model, features), yvar)
# make predictions
predictions <- predict(model, iris[,1:4])
# summarize results
confusionMatrix(predictions, iris$Species)
