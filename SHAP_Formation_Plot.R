library(dplyr)
library(readr)
library(pROC)
library(caret)
library(xgboost)
library(tidyr)
library(DMwR)
library(ggplot2)
library(ggpubr)
library(cowplot)

#model version 47 was the best in terms of performance

shapv1<-xgb.plot.shap(model = modelv47,data = testmatrixv47,plot = FALSE,top_n = 5)
testdatap1 <- modeldata[-trainRowNumbers,]
rownames(testdatap1)==rownames(shapv1$data)


shapval<-shapv1$shap_contrib
shapval<-as.data.frame(shapval)
origdat<-testdatap1[colnames(shapval)]

#example plot formation

ccode1<-rep(NA,22)
#values are based on the quartile ranges of the original data values
ccode1[which(origdat$Alkaline.Phosphatase..U.L.<86.75)]<-"vl"
ccode1[which(origdat$Alkaline.Phosphatase..U.L.>=86.75  & 132.56  >=origdat$Alkaline.Phosphatase..U.L.)]<-"l"
ccode1[which(origdat$Alkaline.Phosphatase..U.L.>132.56   & 125.25>=origdat$Alkaline.Phosphatase..U.L.)]<-"h"
ccode1[which(origdat$Alkaline.Phosphatase..U.L.>125.25)]<-"vh"


ccode2<-rep(NA,22)
ccode2[which(origdat$Haemoglobin..g.L.< 13.62)]<-"vl"
ccode2[which(origdat$Haemoglobin..g.L.>= 13.62  & 82.44  >=origdat$Haemoglobin..g.L.)]<-"l"
ccode2[which(origdat$Haemoglobin..g.L.>82.44   & 132.25 >=origdat$Haemoglobin..g.L.)]<-"h"
ccode2[which(origdat$Haemoglobin..g.L.>132.25)]<-"vh"


ccode3<-rep(NA,22)
ccode3[which(origdat$Leukocytes..10.9.L.<7.04)]<-"vl"
ccode3[which(origdat$Leukocytes..10.9.L.>=7.04  & 11.00  >=origdat$Leukocytes..10.9.L.)]<-"l"
ccode3[which(origdat$Leukocytes..10.9.L.>11.00   & 12.18>=origdat$Leukocytes..10.9.L.)]<-"h"
ccode3[which(origdat$Leukocytes..10.9.L.>12.18)]<-"vh"


ccode4<-rep(NA,22)
ccode4[which(origdat$Total.Bilirubin..umol.L.<5.001 )]<-"vl"
ccode4[which(origdat$Total.Bilirubin..umol.L.>=5.001  & 7.776 >=origdat$Total.Bilirubin..umol.L.)]<-"l"
ccode4[which(origdat$Total.Bilirubin..umol.L.>7.776  & 8.662>=origdat$Total.Bilirubin..umol.L.)]<-"h"
ccode4[which(origdat$Total.Bilirubin..umol.L.>8.662)]<-"vh"


ccode5<-rep(NA,22)
ccode5[which(origdat$Female==1)]<-"vl"
ccode5[which(origdat$Female==2)]<-"vh"



Features<-c(rep("Alkaline Phosphatase (U/L)",22),rep("Haemoglobin (G/L)",22),
            rep("Leukocytes (10^9/L)",22),rep("Total Bilirubin (µMol/L)",22),rep("Female",22))

shappyval<-c(shapval$Alkaline.Phosphatase..U.L.,shapval$Haemoglobin..g.L.,shapval$Leukocytes..10.9.L.,
             shapval$Total.Bilirubin..umol.L.,shapval$Female)

plotdata<-cbind(Features,shappyval)

rangefill<-c(ccode1,ccode2,ccode3,ccode4,ccode5)

plotdata<-cbind(plotdata,rangefill)
plotdata<-as.data.frame(plotdata)

#remove any features that were orginally na 

plotdata<-plotdata[!(is.na(plotdata$rangefill)),]

plotdata$shappyval<-as.numeric(plotdata$shappyval)
plotdata$Features<-as.factor(plotdata$Features)

shapplot<-ggplot(data = plotdata,aes())+geom_point(aes(x=shappyval,
                                                       y=reorder(Features,
                                                                 sort(as.numeric(Features),decreasing = TRUE)),
                                                       colour=rangefill))+scale_x_continuous(limits = c(-2,2),breaks=c(-2,-1.5,-1,-0.5,0,
                                                                                                                       0.5,1,1.5,2))+
  xlab("SHAP")+scale_colour_manual(values = c("orange","skyblue1","red","grey"))+
  geom_vline(xintercept = 0,size=1,linetype=2)+
  theme_pubr()+theme(axis.title.y = element_blank(),legend.position = "none",panel.grid.major = element_line(linetype = 2))


