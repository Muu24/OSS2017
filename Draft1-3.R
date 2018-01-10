library(gdata)
##read the data
df_ag = read.xls ("Actigraphy Data.xlsx",header=TRUE)
#head(df_ag)
df_sleeplog1 = read.xls ("Sleep Logs Data (Rested, sleep quality, latency, highness, tolerance).xlsx",header=TRUE)
head(df_sleeplog1)
df_sleeplog2 = read.xls ("Sleep Logs Data (Sleep Duration).xlsx",header=TRUE)
head(df_sleeplog2)
#df_EQ5D =read.xls ("EQ-5D Data.xlsx",header=TRUE)
df_ISI=read.xls ("ISI Data.xlsx",header=TRUE)
head(df_ISI)
##
head(df)
cISI_B<-df_ag$ISI_B
cgender<-as.factor(df_ag$gender_m)
cage<-df_ag$Age
length(cISI_B)
##
cdat<-na.omit(data_frame(cISI_B,cgender,cage))
length(cdat[,1])
cdat[,'cgender']<-as.factor(cdat[,'cgender'])
library(ggplot2)
#library(readr)
#library(dplyr)
library(tidyverse)
head(as.data.frame(cdat))
#cdat <-mutate(cdat,cage=cut_number(age,3))
gg1<-ggplot(cdat,aes(x=cage,y=cISI_B,colour=cgender))+geom_point((aes(shape=cgender)),size=4)
gg1
print(gg1)

summary(df_ag)
#gender 1 for female and 2 for male
mean(cdat$cISI_B[cdat[,'cgender']==1])
mean(cdat$cISI_B[cdat[,'cgender']==2])
#gg1+geom_smooth(method="lm")+facet_wrap(~f_age)
cdat1 <-mutate(cdat,cage=cut_number(cage,5))
gg2<-ggplot(cdat1,aes(cage,cISI_B))+geom_boxplot(aes(col=cage))
gg2
gg2+geom_point(alpha=0.3)

cage
cdat[,'cage']<-cut(cdat$cage,seq(20,80,length.out=10))
cdat2<-split(cdat,cut(cdat[,'cage'],seq(20,80,length.out=5)))

gg2<-ggplot(cdat,aes(cage,cISI_B))+geom_boxplot(aes(col=cage))
gg2
gg2+geom_point(alpha=0.3)
##
#library(mlmRev)
#gg0<-ggplot((Contraception),aes(age,use))+geom_point()
#gg0
#gg0+geom_jitter()
#gg0+stat_sum()

##

ggplot(cdat,aes(cage,cISI_B,fill=cgender))+geom_boxplot()+geom_point(alpha=0.3)
