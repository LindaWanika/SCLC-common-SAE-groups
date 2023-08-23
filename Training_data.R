library(readr)
library(dplyr)
library(caret)
library(DMwR)
library(xgboost)
library(mlr)
library(pRoc)
library(tidyr)

#As mentioned the "modeldata" for each of the common SAE groups can be requested from the author after 
#users have obtained approvial and successfully downloaded the clinical trial data 
trainRowNumbers <- createDataPartition(modeldata$GRADE, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
traindata <- modeldata[trainRowNumbers,]

traindata$GRADE<-as.factor(traindata$GRADE)

traindata<-SMOTE(GRADE~.,data=traindata)

traingradelabels<-traindata$GRADE
traindata<-traindata[-1]
# Step 3: Create the test dataset
testdata <- modeldata[-trainRowNumbers,]


trainmissingimput <- preProcess(traindata, method='knnImpute')

traindata <- predict(trainmissingimput, newdata = traindata)

#Scaling the transformed data 
#Scaling the transformed data 

modelscaling<-preProcess(traindata,method='range')


traindata<-predict(modelscaling,traindata)

#fix some of the columns 

traindata$muscle.relaxant[which(traindata$muscle.relaxant<0.5)]<-0
traindata$muscle.relaxant[which(traindata$muscle.relaxant>0.5)]<-1
traindata$Cardiac.Therapy[which(traindata$Cardiac.Therapy<0.5)]<-0
traindata$Cardiac.Therapy[which(traindata$Cardiac.Therapy>0.5)]<-1
traindata$Infections[which(traindata$Infections<0.5)]<-0
traindata$Infections[which(traindata$Infections>0.5)]<-1
traindata$Gout[which(traindata$Gout<0.5)]<-0
traindata$Gout[which(traindata$Gout>0.5)]<-1
traindata$Brain.and.Mind[which(traindata$Brain.and.Mind<0.5)]<-0
traindata$Brain.and.Mind[which(traindata$Brain.and.Mind>0.5)]<-1
traindata$Osteoporosis[which(traindata$Osteoporosis<0.5)]<-0
traindata$Osteoporosis[which(traindata$Osteoporosis>0.5)]<-1
traindata$Vaso.acting.drugs[which(traindata$Vaso.acting.drugs<0.5)]<-0
traindata$Vaso.acting.drugs[which(traindata$Vaso.acting.drugs>0.5)]<-1
traindata$Diabetes[which(traindata$Diabetes<0.5)]<-0
traindata$Diabetes[which(traindata$Diabetes>0.5)]<-1
traindata$Nitrate[which(traindata$Nitrate<0.5)]<-0
traindata$Nitrate[which(traindata$Nitrate>0.5)]<-1
traindata$Respiratory[which(traindata$Respiratory<0.5)]<-0
traindata$Respiratory[which(traindata$Respiratory>0.5)]<-1
traindata$Hypertension.Medications[which(traindata$Hypertension.Medications<0.5)]<-0
traindata$Hypertension.Medications[which(traindata$Hypertension.Medications>0.5)]<-1
traindata$GI.Tract[which(traindata$GI.Tract<0.5)]<-0
traindata$GI.Tract[which(traindata$GI.Tract>0.5)]<-1
traindata$Anti.Inflammatory[which(traindata$Anti.Inflammatory<0.5)]<-0
traindata$Anti.Inflammatory[which(traindata$Anti.Inflammatory>0.5)]<-1
traindata$Blood.Agents[which(traindata$Blood.Agents<0.5)]<-0
traindata$Blood.Agents[which(traindata$Blood.Agents>0.5)]<-1
traindata$Analgesic[which(traindata$Analgesic<0.5)]<-0
traindata$Analgesic[which(traindata$Analgesic>0.5)]<-1
traindata$Statin[which(traindata$Statin<0.5)]<-0
traindata$Statin[which(traindata$Statin>0.5)]<-1
traindata$Thyroid[which(traindata$Thyroid<0.5)]<-0
traindata$Thyroid[which(traindata$Thyroid>0.5)]<-1
traindata$Cancer[which(traindata$Cancer<0.5)]<-0
traindata$Cancer[which(traindata$Cancer>0.5)]<-1
traindata$White[which(traindata$White<0.5)]<-0
traindata$White[which(traindata$White>0.5)]<-1
traindata$Black[which(traindata$Black<0.5)]<-0
traindata$Black[which(traindata$Black>0.5)]<-1
traindata$Asian[which(traindata$Asian<0.5)]<-0
traindata$Asian[which(traindata$Asian>0.5)]<-1
traindata$Other[which(traindata$Other<0.5)]<-0
traindata$Other[which(traindata$Other>0.5)]<-1




#for the hyper tuning the labels need to be attached to the training data data
traindatahyp<-cbind(traindata,traingradelabels)
traindatahyp$traingradelabels<-as.character(traindatahyp$traingradelabels)
traindatahyp$traingradelabels<-make.names(traindatahyp$traingradelabels)



lrn<-makeLearner("classif.xgboost",predict.type="response")
lrn$par.vals<-list(objective="binary:logistic",eval_metric="logloss",nrounds=100L,eta=0.1)
#lower eta in order to make the output more sensitive to the predicted probabilities 

#Most of the time XGBOOST will use gbtree as its optimal booster, however XGBOOST can also produce optimal results based on a generalised linear model.
params<-makeParamSet(makeDiscreteParam("booster",values=c("gbtree","gblinear")),
                     makeIntegerParam("max_depth",lower=3L, upper = 10L),
                     makeNumericParam("min_child_weight",lower=1L, upper=10L), 
                     makeNumericParam("subsample",lower=0.5, upper=1), 
                     makeNumericParam("colsample_bytree",lower=0.5, upper=1))
#These are the parameters we are interested in optimising. 

rdesc<-makeResampleDesc("CV",stratify = T, iters=5L)


traindatahyp$traingradelabels<-factor(traindatahyp$traingradelabels)
traintask<-makeClassifTask(data=traindatahyp,target="traingradelabels" )


ctrl<-makeTuneMultiCritControlRandom(maxit= 100L) #Uses random search for hyper parameter tuning







normaltune<-tuneParamsMultiCrit(learner = lrn, task = traintask, resampling = rdesc, 
                                measures = list(tpr,tnr), par.set = params, control = ctrl, show.info=T)



trainmatrix<-as.matrix(traindata)

traingradelabels<-as.numeric(as.character(traingradelabels))

dtrain<-xgb.DMatrix(trainmatrix, label=traingradelabels)
#cross validation
crossmodel<-xgb.cv(data=dtrain,params=normaltune$x[[1]],prediction = T, nrounds=1000, metrics="logloss",
                   nfold=5, objective="binary:logistic", early_stopping_rounds = 10)
#best parameter in terms of performance


model1<-xgboost(data=dtrain, objective= "binary:logistic", eval_metric="logloss",nrounds = 21,
                params = normaltune$x[[1]])
