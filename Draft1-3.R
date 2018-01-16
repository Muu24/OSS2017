library(gdata)
library(readr)
library(ggplot2)
library(tidyverse)
##read the data
df_ag = read.xls ("Actigraphy Data.xlsx",header=TRUE)
#head(df_ag)
df_sleeplog1 = read.xls ("Sleep Logs Data (Rested, sleep quality, latency, highness, tolerance).xlsx",header=TRUE)
head(df_sleeplog1)
df_sleeplog2 = read.xls ("Sleep Logs Data (Sleep Duration).xlsx",header=TRUE)
head(df_sleeplog2)
#df_EQ5D =read.xls ("EQ-5D Data.xlsx",header=TRUE)
df_ISI=read.xls ("ISI Data.xlsx",header=TRUE)
#head(df_ISI)
##
#head(df)
cISI_B<-df_ag$ISI_B
cgender<-as.factor(df_ag$gender_m)
cage<-df_ag$Age
cst1<-as.factor(df_ag$strain_1)
cst2<-as.factor(df_ag$strain_2)
cst3<-as.factor(df_ag$strain_3)
length(cISI_B)
##
cdat<-data_frame(cISI_B,cgender,cage,cst1,cst2,cst3)
cdatf<-cdat[complete.cases(cdat),]
#head(as.data.frame(cdat))
#cdat <-mutate(cdat,cage=cut_number(age,3))
gg1<-ggplot(cdatf,aes(x=cage,y=cISI_B,colour=cgender))+geom_point((aes(shape=cgender)),size=4)
gg1
print(gg1)

summary(df_ag)
#gender 1 for female and 2 for male
mean(cdatf$cISI_B[cdatf[,'cgender']=='0'])
mean(cdatf$cISI_B[cdatf[,'cgender']=='1'])
#length female
dim(cdatf[cdatf[,'cgender']=='0',] )
#length female and different strains
dim(cdatf[cdatf[,'cgender']=='0' & cdatf[,'cst1']==1,] )
dim(cdatf[cdatf[,'cgender']=='0' & cdatf[,'cst1']==2,] )
dim(cdatf[cdatf[,'cgender']=='0' & cdatf[,'cst2']==3,] )
dim(cdatf[cdatf[,'cgender']=='0' & cdatf[,'cst2']==4,] )
dim(cdatf[cdatf[,'cgender']=='0' & cdatf[,'cst2']==5,] )
dim(cdatf[cdatf[,'cgender']=='0' & cdatf[,'cst3']==6,] )
dim(cdatf[cdatf[,'cgender']=='0' & cdatf[,'cst3']==7,] )
dim(cdatf[cdatf[,'cgender']=='0' & cdatf[,'cst3']==8,] )
dim(cdatf[cdatf[,'cgender']=='0' & cdatf[,'cst3']==9,] )
#length male and different strains
dim(cdatf[cdatf[,'cgender']=='1' & cdatf[,'cst1']==1,] )
dim(cdatf[cdatf[,'cgender']=='1' & cdatf[,'cst1']==2,] )
dim(cdatf[cdatf[,'cgender']=='1' & cdatf[,'cst2']==3,] )
dim(cdatf[cdatf[,'cgender']=='1' & cdatf[,'cst2']==4,] )
dim(cdatf[cdatf[,'cgender']=='1' & cdatf[,'cst2']==5,] )
dim(cdatf[cdatf[,'cgender']=='1' & cdatf[,'cst3']==6,] )
dim(cdatf[cdatf[,'cgender']=='1' & cdatf[,'cst3']==7,] )
dim(cdatf[cdatf[,'cgender']=='1' & cdatf[,'cst3']==8,] )
dim(cdatf[cdatf[,'cgender']=='1' & cdatf[,'cst3']==9,] )
#male mean age
mean(cdatf[cdatf[,'cgender']=='1',]$cage)
sd(cdatf[cdatf[,'cgender']=='1',]$cage)
#female mean age
mean(cdatf[cdatf[,'cgender']=='0',]$cage)
sd(cdatf[cdatf[,'cgender']=='0',]$cage)
#gg1+geom_smooth(method="lm")+facet_wrap(~f_age)
cdat1 <-mutate(cdatf,cage=cut_number(cage,5))
gg2<-ggplot(cdat1,aes(cage,cISI_B))+geom_boxplot(aes(col=cage))
gg2+geom_point(alpha=0.3)

#cage
cdatf[,'cage']<-cut(cdatf$cage,seq(20,80,length.out=5))
#cdat2<-split(cdat,cut(cdat[,'cage'],seq(20,80,length.out=5)))

gg2<-ggplot(cdatf,aes(cage,cISI_B))+geom_boxplot(aes(col=cage))
gg2
gg2+geom_point(alpha=0.3)
##
#library(mlmRev)
#gg0<-ggplot((Contraception),aes(age,use))+geom_point()
#gg0
#gg0+geom_jitter()
#gg0+stat_sum()
ggplot(cdatf,aes(cage,cISI_B,fill=cgender))+geom_boxplot()+geom_point()
ggplot(cdatf,aes(cage,cISI_B,fill=cst1))+geom_boxplot()+geom_point()
###
library(Rmisc)
CI(cdatf$cISI_B)
summary(df_ag)


### A new dataframe for comparing ISI 
df_ISI=read.xls ("ISI Data.xlsx",header=TRUE)
df_ISI[,'strain_1']<-as.factor(df_ISI[,'strain_1'])
df_ISI[,'strain_2']<-as.factor(df_ISI[,'strain_2'])
df_ISI[,'strain_3']<-as.factor(df_ISI[,'strain_3'])
cISI<-as_data_frame(df_ISI[,1:11],cage,cgender)
cISI<-cISI[complete.cases(cISI),]
colnames(cISI)<-c('ID','St1','St2','St3','ISI_B','ISI_w1','ISI_w2','ISI_w3','ISI_w4','ISI_w5','ISI_w6')
###Divide into different strains
#strain 1
cISI1_1<-cISI[cISI[,'St1']==1,]
cISI1_2<-cISI[cISI[,'St1']==2,]
#Confidence interval and paired t test 
#Among strain dose by comparing escalated, (old weeks 1,3,5)
st11_ISI_b1<-t.test(cISI1_1$ISI_B,cISI1_1$ISI_w1,paired=TRUE)
st11_ISI_b3<-t.test(cISI1_1$ISI_B,cISI1_1$ISI_w3,paired=TRUE)
st11_ISI_b5<-t.test(cISI1_1$ISI_B,cISI1_1$ISI_w5,paired=TRUE)
st11_ISI_13<-t.test(cISI1_1$ISI_w1,cISI1_1$ISI_w3,paired=TRUE)
st11_ISI_35<-t.test(cISI1_1$ISI_w3,cISI1_1$ISI_w5,paired=TRUE)
st11_ISI_15<-t.test(cISI1_1$ISI_w1,cISI1_1$ISI_w5,paired=TRUE)
st12_ISI_b1<-t.test(cISI1_2$ISI_B,cISI1_2$ISI_w1,paired=TRUE)
st12_ISI_b3<-t.test(cISI1_2$ISI_B,cISI1_2$ISI_w3,paired=TRUE)
st12_ISI_b5<-t.test(cISI1_2$ISI_B,cISI1_2$ISI_w5,paired=TRUE)
st12_ISI_13<-t.test(cISI1_2$ISI_w1,cISI1_2$ISI_w3,paired=TRUE)
st12_ISI_35<-t.test(cISI1_2$ISI_w3,cISI1_2$ISI_w5,paired=TRUE)
st12_ISI_15<-t.test(cISI1_2$ISI_w1,cISI1_2$ISI_w5,paired=TRUE)
a<-c(st11_ISI_b1$estimate,st11_ISI_b3$estimate,st11_ISI_b5$estimate,
st11_ISI_13$estimate,st11_ISI_35$estimate,st11_ISI_15$estimate,
st11_ISI_b1$estimate,st12_ISI_b3$estimate,st12_ISI_b5$estimate,
st12_ISI_13$estimate,st12_ISI_35$estimate,st12_ISI_15$estimate)
b<-rbind(st11_ISI_b1$conf.int,st11_ISI_b3$conf.int,st11_ISI_b5$conf.int,
     st11_ISI_13$conf.int,st11_ISI_35$conf.int,st11_ISI_15$conf.int,
     st11_ISI_b1$conf.int,st12_ISI_b3$conf.int,st12_ISI_b5$conf.int,
     st12_ISI_13$conf.int,st12_ISI_35$conf.int,st12_ISI_15$conf.int)
st1_ISI<-cbind(a,b)
colnames(st1_ISI)<-c('Mean Difference','Lower Bound','Upper Bound')
rownames(st1_ISI)<-c('St11_ISI_b1','St11_ISI_b3','St11_ISI_b5','St11_ISI_13','St11_ISI_15','St11_ISI_35',
                     'St12_ISI_b1','St12_ISI_b3','St12_ISI_b5','St12_ISI_13','St12_ISI_15','St12_ISI_35')
#Confidence interval and paired t test 
#Among Maintenance dose by comparing escalated, (even weeks 2,4,6)
t.test(cISI1_1$ISI_B,cISI1_1$ISI_w1,paired=TRUE)
t.test(cISI1_1$ISI_w1,cISI1_1$ISI_w3,paired=TRUE)
t.test(cISI1_1$ISI_w3,cISI1_1$ISI_w5,paired=TRUE)
t.test(cISI1_1$ISI_w1,cISI1_1$ISI_w5,paired=TRUE)

