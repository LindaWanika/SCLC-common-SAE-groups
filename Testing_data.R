library(readr)
library(dplyr)
library(caret)
library(DMwR)
library(xgboost)
library(mlr)
library(pRoc)
library(tidyr)

testildlabels<-testdata$GRADE
testildlabels<-as.numeric(testildlabels)
testdata<-testdata[-1]
#Use the same imputation and scaling models as the training data 
testdata<-predict(trainmissingimput, newdata = testdata)
testdata<-predict(modelscaling,testdata)

testdata$muscle.relaxant[which(testdata$muscle.relaxant<0.5)]<-0
testdata$muscle.relaxant[which(testdata$muscle.relaxant>0.5)]<-1
testdata$Cardiac.Therapy[which(testdata$Cardiac.Therapy<0.5)]<-0
testdata$Cardiac.Therapy[which(testdata$Cardiac.Therapy>0.5)]<-1
testdata$Infections[which(testdata$Infections<0.5)]<-0
testdata$Infections[which(testdata$Infections>0.5)]<-1
testdata$Gout[which(testdata$Gout<0.5)]<-0
testdata$Gout[which(testdata$Gout>0.5)]<-1
testdata$Brain.and.Mind[which(testdata$Brain.and.Mind<0.5)]<-0
testdata$Brain.and.Mind[which(testdata$Brain.and.Mind>0.5)]<-1
testdata$Osteoporosis[which(testdata$Osteoporosis<0.5)]<-0
testdata$Osteoporosis[which(testdata$Osteoporosis>0.5)]<-1
testdata$Vaso.acting.drugs[which(testdata$Vaso.acting.drugs<0.5)]<-0
testdata$Vaso.acting.drugs[which(testdata$Vaso.acting.drugs>0.5)]<-1
testdata$Diabetes[which(testdata$Diabetes<0.5)]<-0
testdata$Diabetes[which(testdata$Diabetes>0.5)]<-1
testdata$Nitrate[which(testdata$Nitrate<0.5)]<-0
testdata$Nitrate[which(testdata$Nitrate>0.5)]<-1
testdata$Respiratory[which(testdata$Respiratory<0.5)]<-0
testdata$Respiratory[which(testdata$Respiratory>0.5)]<-1
testdata$Hypertension.Medications[which(testdata$Hypertension.Medications<0.5)]<-0
testdata$Hypertension.Medications[which(testdata$Hypertension.Medications>0.5)]<-1
testdata$GI.Tract[which(testdata$GI.Tract<0.5)]<-0
testdata$GI.Tract[which(testdata$GI.Tract>0.5)]<-1
testdata$Anti.Inflammatory[which(testdata$Anti.Inflammatory<0.5)]<-0
testdata$Anti.Inflammatory[which(testdata$Anti.Inflammatory>0.5)]<-1
testdata$Blood.Agents[which(testdata$Blood.Agents<0.5)]<-0
testdata$Blood.Agents[which(testdata$Blood.Agents>0.5)]<-1
testdata$Analgesic[which(testdata$Analgesic<0.5)]<-0
testdata$Analgesic[which(testdata$Analgesic>0.5)]<-1
testdata$Statin[which(testdata$Statin<0.5)]<-0
testdata$Statin[which(testdata$Statin>0.5)]<-1
testdata$Thyroid[which(testdata$Thyroid<0.5)]<-0
testdata$Thyroid[which(testdata$Thyroid>0.5)]<-1
testdata$Cancer[which(testdata$Cancer<0.5)]<-0
testdata$Cancer[which(testdata$Cancer>0.5)]<-1
testdata$White[which(testdata$White<0.5)]<-0
testdata$White[which(testdata$White>0.5)]<-1
testdata$Black[which(testdata$Black<0.5)]<-0
testdata$Black[which(testdata$Black>0.5)]<-1
testdata$Asian[which(testdata$Asian<0.5)]<-0
testdata$Asian[which(testdata$Asian>0.5)]<-1
testdata$Other[which(testdata$Other<0.5)]<-0
testdata$Other[which(testdata$Other>0.5)]<-1

testmatrix<-as.matrix(testdata)


dtest<-xgb.DMatrix(testmatrix,label=testildlabels)


#Predictions 

normaltestresults<-predict(model1,dtest,reshape = T)

testroc<-roc(testildlabels,normaltestresults)
testroc$auc
