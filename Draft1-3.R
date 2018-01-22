library(gdata)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
#data(container.df)
#fit = hotelling.test(.~gp, data = container.df)
#fit
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
gg1<-ggplot(cdatf,aes(x=cage,y=cage,colour=cgender))+geom_point((aes(shape=cgender)),size=4)
gg1
gg<-ggplot(cdatf,aes(x=cgender,y=cage,colour=cgender,group=cgender))+geom_boxplot()+geom_jitter()
gg
print(gg1)
##

(as_data_frame(Contraception)
  %>% group_by(age,urban)
  %>% summarise(prop=mean(as.numeric(use)-1),
                n=n(),
                se=sqrt(prop*(1-prop)/n))
)->contr_sum
contr_sum %>% group_vars()
contr_sum %>% group_by(n,add=FALSE) %>%group_vars

(ggplot(contr_sum,aes(age,prop,colour=urban))+geom_point(aes(size=n))
  +geom_linerange(aes(ymin=prop-se,ymax=prop+se))
  +scale_color_brewer(palette = "Dark2"))
###

(as_data_frame(cdat)
  %>% group_by(cage,cgender)
  %>% summarise(prop=mean(as.numeric(cgender)-1),
                n=n(),
                se=sqrt(prop*(1-prop)/n))
)->contr_sum
contr_sum %>% group_vars()
contr_sum %>% group_by(n,add=FALSE) %>%group_vars

(ggplot(contr_sum,aes(age,prop,colour=urban))+geom_point(aes(size=n))
  +geom_linerange(aes(ymin=prop-se,ymax=prop+se))
  +scale_color_brewer(palette = "Dark2"))

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
cISI<-as_data_frame(df_ISI[,1:11])
cISI<-cISI[complete.cases(cISI),]
colnames(cISI)<-c('ID','St1','St2','St3','ISI_B','ISI_w1','ISI_w2','ISI_w3','ISI_w4','ISI_w5','ISI_w6')
###Divide into different strains
#strain 1

###Insomnian Severity Index: 
#(a) Betweem Strains,
#Baseline
t.test(cISI$ISI_B,cISI$ISI_w1,paired = TRUE)
t.test(cISI$ISI_B,cISI$ISI_w2,paired = TRUE)
t.test(cISI$ISI_B,cISI$ISI_w3,paired = TRUE)
t.test(cISI$ISI_B,cISI$ISI_w4,paired = TRUE)
t.test(cISI$ISI_B,cISI$ISI_w5,paired = TRUE)
t.test(cISI$ISI_B,cISI$ISI_w6,paired = TRUE)
#St1 and St2
t.test(cISI$ISI_w1,cISI$ISI_w3,paired = TRUE)
t.test(cISI$ISI_w2,cISI$ISI_w4,paired = TRUE)
#St1 and St3
t.test(cISI$ISI_w3,cISI$ISI_w5,paired = TRUE)
t.test(cISI$ISI_w4,cISI$ISI_w6,paired = TRUE)
#St2 and St3
t.test(cISI$ISI_w1,cISI$ISI_w5,paired = TRUE)
t.test(cISI$ISI_w2,cISI$ISI_w6,paired = TRUE)
##
library(dplyr)
Cat_cISI  <-( cISI
              %>% mutate(Cat_ISI_w1=cut(ISI_w1, breaks=c(0, 7, 14, 21,28),
                labels=c("Absence","Sub","Moderate","Severe")))
              %>% mutate(Cat_ISI_w2=cut(ISI_w2, breaks=c(0, 7, 14, 21,28),
                                        labels=c("Absence","Sub","Moderate","Severe")))
              %>% mutate(Cat_ISI_w3=cut(ISI_w3, breaks=c(0, 7, 14, 21,28),
                                        labels=c("Absence","Sub","Moderate","Severe")))
              %>% mutate(Cat_ISI_w4=cut(ISI_w4, breaks=c(0, 7, 14, 21,28),
                                        labels=c("Absence","Sub","Moderate","Severe")))
              %>% mutate(Cat_ISI_w5=cut(ISI_w5, breaks=c(0, 7, 14, 21,28),
                                        labels=c("Absence","Sub","Moderate","Severe")))
              %>% mutate(Cat_ISI_w6=cut(ISI_w6, breaks=c(0, 7, 14, 21,28),
                                        labels=c("Absence","Sub","Moderate","Severe")))
              )





###



####





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
par(mfrow=c(3,3))
cISIp<-as.data.frame(cISI)
for (i in 5:11){  
  qqnorm(cISIp[, i], main = names(cISI[i]))
  qqline(cISIp[, i],col=1)
}
str(cISI)






###between strains for ISB:
#1.For St1=1 named 'Lot 3' and St2=3 named 'Argyle':
St11_23<-cISI %>% filter(St1 %in% 1,St2 %in% 3)
t.test(St11_23$ISI_w1,St11_23$ISI_w3,paired=TRUE)
t.test(St11_23$ISI_w2,St11_23$ISI_w4,paired=TRUE)
#t.test(St11_23$ISI_w1,St11_23$ISI_w2,paired=TRUE)
#t.test(St11_23$ISI_w3,St11_23$ISI_w4,paired=TRUE)

#2.For St1=1 named 'Lot 3' and St2=4 named 'Bediol':
St11_24<-cISI %>% filter(St1 %in% 1,St2 %in% 4)
t.test(St11_24$ISI_w1,St11_24$ISI_w3,paired=TRUE)
t.test(St11_24$ISI_w2,St11_24$ISI_w4,paired=TRUE)
#t.test(St11_24$ISI_w1,St11_24$ISI_w2,paired=TRUE)
#t.test(St11_24$ISI_w3,St11_24$ISI_w4,paired=TRUE)

#3.For St1=1 named 'Lot 3' and St2=5 named 'Blue':
St11_25<-cISI %>% filter(St1 %in% 1,St2 %in% 5)
t.test(St11_25$ISI_w1,St11_25$ISI_w3,paired=TRUE)
t.test(St11_25$ISI_w2,St11_25$ISI_w4,paired=TRUE)
#t.test(St11_25$ISI_w1,St12_25$ISI_w2,paired=TRUE)
#t.test(St11_25$ISI_w3,St12_25$ISI_w4,paired=TRUE)

#4.For St1=1 named 'Lot 3' and St3=6 named 'Red Sativa':
St11_36<-cISI %>% filter(St1 %in% 1,St3%in% 6)
dim(St11_36)[1]
t.test(St11_36$ISI_w1,St11_36$ISI_w5,paired=TRUE)
t.test(St11_36$ISI_w2,St11_36$ISI_w6,paired=TRUE)
#t.test(St11_36$ISI_w1,St11_36$ISI_w2,paired=TRUE)
#t.test(St11_36$ISI_w5,St11_36$ISI_w6,paired=TRUE)

#5.For St1=1 named 'Lot 3' and St3=7 named 'Red Indica':
St11_37<-cISI %>% filter(St1 %in% 1,St3 %in% 7)
t.test(St11_37$ISI_w1,St11_37$ISI_w5,paired=TRUE)
t.test(St11_37$ISI_w2,St11_37$ISI_w6,paired=TRUE)
#t.test(St11_37$ISI_w1,St11_37$ISI_w2,paired=TRUE)
#t.test(St11_37$ISI_w5,St11_37$ISI_w6,paired=TRUE)

#6.For St1=1 named 'Lot 3' and St3=8 named 'Bedica':
St11_38<-cISI %>% filter(St1 %in% 1,St2 %in% 8)
t.test(St11_38$ISI_w1,St11_38$ISI_w5,paired=TRUE)
t.test(St11_38$ISI_w2,St11_38$ISI_w6,paired=TRUE)
#t.test(St11_38$ISI_w1,St11_38$ISI_w2,paired=TRUE)
#t.test(St11_38$ISI_w5,St11_38$ISI_w6,paired=TRUE)

#7.For St1=1 named 'Lot 3' and St3=9 named 'Bakerstreet':
St11_39<-cISI %>% filter(St1 %in% 2,St2 %in% 9)
t.test(St11_39$ISI_w1,St11_39$ISI_w5,paired=TRUE)
t.test(St11_39$ISI_w2,St11_39$ISI_w6,paired=TRUE)
#t.test(St11_39$ISI_w1,St11_39$ISI_w2,paired=TRUE)
#t.test(St11_39$ISI_w5,St11_39$ISI_w6,paired=TRUE)

################################################################################
#For St1=2 named 'Yellow ' and St2=3 named 'Argyle':
St12_23<-cISI %>% filter(St1 %in% 2,St2 %in% 3)
t.test(St12_23$ISI_w1,St12_23$ISI_w3,paired=TRUE)
t.test(St12_23$ISI_w2,St12_23$ISI_w4,paired=TRUE)
#t.test(St12_23$ISI_w1,St12_23$ISI_w2,paired=TRUE)
#t.test(St12_23$ISI_w3,St12_23$ISI_w4,paired=TRUE)

#For St1=2 named 'Yellow ' and St2=4 named 'Bediol':
St12_24<-cISI %>% filter(St1 %in% 2,St2 %in% 4)
t.test(St12_24$ISI_w1,St12_24$ISI_w3,paired=TRUE)
t.test(St12_24$ISI_w2,St12_24$ISI_w4,paired=TRUE)
#t.test(St12_24$ISI_w1,St12_24$ISI_w2,paired=TRUE)
#t.test(St12_24$ISI_w3,St12_24$ISI_w4,paired=TRUE)

#For St1=2 named 'Yellow ' and St2=5 named 'Blue':
St12_25<-cISI %>% filter(St1 %in% 2,St2 %in% 5)
t.test(St12_25$ISI_w1,St12_25$ISI_w3,paired=TRUE)
t.test(St12_25$ISI_w2,St12_25$ISI_w4,paired=TRUE)
#t.test(St12_25$ISI_w1,St12_25$ISI_w2,paired=TRUE)
#t.test(St12_25$ISI_w3,St12_25$ISI_w4,paired=TRUE)

################################################################
#For St1=2 named 'Yellow ' and St3=6 named 'Red Sativa':
St12_36<-cISI %>% filter(St1 %in% 2,St3 %in% 6)
t.test(St12_36$ISI_w1,St12_36$ISI_w5,paired=TRUE)
t.test(St12_36$ISI_w2,St12_36$ISI_w6,paired=TRUE)
#t.test(St12_36$ISI_w1,St12_36$ISI_w2,paired=TRUE)
#t.test(St12_36$ISI_w5,St12_36$ISI_w6,paired=TRUE)

#For St1=2 named 'Yellow ' and St3=7 named 'Red Indica':
St12_37<-cISI %>% filter(St1 %in% 2,St3 %in% 7)
t.test(St12_37$ISI_w1,St12_37$ISI_w5,paired=TRUE)
t.test(St12_37$ISI_w2,St12_37$ISI_w6,paired=TRUE)
#t.test(St12_37$ISI_w1,St12_37$ISI_w2,paired=TRUE)
#t.test(St12_37$ISI_w5,St12_37$ISI_w6,paired=TRUE)

#For St1=2 named 'Yellow' and St3=8 named 'Bedica':
St12_38<-cISI %>% filter(St1 %in% 2,St3 %in% 8)
t.test(St12_38$ISI_w1,St12_38$ISI_w5,paired=TRUE)
t.test(St12_38$ISI_w2,St12_38$ISI_w6,paired=TRUE)
#t.test(St12_38$ISI_w1,St12_38$ISI_w2,paired=TRUE)
#t.test(St12_38$ISI_w5,St12_38$ISI_w6,paired=TRUE)

#For St1=2 named 'Yellow ' and St3=9 named 'Bakerstreet':
St12_39<-cISI %>% filter(St1 %in% 2,St3 %in% 9)
t.test(St12_39$ISI_w1,St12_39$ISI_w5,paired=TRUE)
t.test(St12_39$ISI_w2,St12_39$ISI_w6,paired=TRUE)
#t.test(St12_39$ISI_w1,St12_39$ISI_w2,paired=TRUE)
#t.test(St12_39$ISI_w5,St12_39$ISI_w6,paired=TRUE)

#####################################################
#1. For St2=3 named 'Argyle' and St3=6 named 'Red Sativa':
St23_36<-cISI %>% filter(St2 %in% 3,St3 %in% 6)
t.test(St23_36$ISI_w3,St23_36$ISI_w5,paired=TRUE)
t.test(St23_36$ISI_w4,St23_36$ISI_w6,paired=TRUE)

#2. For St2=3 named 'Argyle' and St3=7 named 'Red Indica':
St23_37<-cISI %>% filter(St2 %in% 3,St3 %in% 7)
t.test(St23_37$ISI_w3,St23_37$ISI_w5,paired=TRUE)
t.test(St23_37$ISI_w4,St23_37$ISI_w6,paired=TRUE)

#3. For St2=3 named 'Argyle' and St3=8 named 'Bedica':
St23_38<-cISI %>% filter(St2 %in% 3,St3 %in% 8)
t.test(St23_38$ISI_w3,St23_38$ISI_w5,paired=TRUE)
t.test(St23_38$ISI_w4,St23_38$ISI_w6,paired=TRUE)

#4. For St2=3 named 'Argyle' and St3=9 named 'Bakerstreet':
St23_39<-cISI %>% filter(St2 %in% 3,St3 %in% 9)
t.test(St23_39$ISI_w3,St23_39$ISI_w5,paired=TRUE)
t.test(St23_39$ISI_w4,St23_39$ISI_w6,paired=TRUE)

###################################################
#1. For St2=4 named 'Bediol' and St3=6 named 'Red Sativa':
St24_36<-cISI %>% filter(St2 %in% 4,St3 %in% 6)
t.test(St24_36$ISI_w3,St24_36$ISI_w5,paired=TRUE)
t.test(St24_36$ISI_w4,St24_36$ISI_w6,paired=TRUE)

#2. For St2=4 named 'Bediol' and St3=7 named 'Red Indica':
St24_37<-cISI %>% filter(St2 %in% 4,St3 %in% 7)
t.test(St24_37$ISI_w3,St24_37$ISI_w5,paired=TRUE)
t.test(St24_37$ISI_w4,St24_37$ISI_w6,paired=TRUE)

#3. For St2=4 named 'Bediol' and St3=8 named 'Bedica':
St24_38<-cISI %>% filter(St2 %in% 4,St3 %in% 8)
t.test(St24_38$ISI_w3,St24_38$ISI_w5,paired=TRUE)
t.test(St24_38$ISI_w4,St24_38$ISI_w6,paired=TRUE)

#4. For St2=4 named 'Bediol' and St3=9 named 'Bakerstreet':
St23_39<-cISI %>% filter(St2 %in% 4,St3 %in% 9)
t.test(St23_39$ISI_w3,St23_39$ISI_w5,paired=TRUE)
t.test(St23_39$ISI_w4,St23_39$ISI_w6,paired=TRUE)

###############################################
#1. For St2=5 named 'Blue' and St3=6 named 'Red Sativa':
St25_36<-cISI %>% filter(St2 %in% 5,St3 %in% 6)
t.test(St25_36$ISI_w3,St25_36$ISI_w5,paired=TRUE)
t.test(St25_36$ISI_w4,St25_36$ISI_w6,paired=TRUE)

#2. For St2=5 named 'Blue' and St3=7 named 'Red Indica':
St25_37<-cISI %>% filter(St2 %in% 5,St3 %in% 7)
t.test(St25_37$ISI_w3,St25_37$ISI_w5,paired=TRUE)
t.test(St25_37$ISI_w4,St25_37$ISI_w6,paired=TRUE)

#3. For St2=5 named 'Blue' and St3=8 named 'Bedica':
St25_38<-cISI %>% filter(St2 %in% 5,St3 %in% 8)
t.test(St25_38$ISI_w3,St25_38$ISI_w5,paired=TRUE)
t.test(St25_38$ISI_w4,St25_38$ISI_w6,paired=TRUE)

#4. For St2=5 named 'Blue' and St3=9 named 'Bakerstreet':
St25_39<-cISI %>% filter(St2 %in% 5,St3 %in% 9)
t.test(St25_39$ISI_w3,St25_39$ISI_w5,paired=TRUE)
t.test(St25_39$ISI_w4,St25_39$ISI_w6,paired=TRUE)



#Two way anova Test, 
#Two-way ANOVA test hypotheses: 
  
#  1. There is no difference in the means of factor A.

#2. There is no difference in means of factor B.

#3. There is no interaction between factors A and B.

#High CBD Value against Strains



df_ISI=read.xls ("Actigraphy Data.xlsx",header=TRUE)
df_ISI[,'strain_1']<-as.factor(df_ISI[,'strain_1'])
df_ISI[,'strain_2']<-as.factor(df_ISI[,'strain_2'])
df_ISI[,'strain_3']<-as.factor(df_ISI[,'strain_3'])
cISI<-as_data_frame(df_ISI[,c('X.Subject.ID','strain_1','strain_2','strain_3','smin_B'
                              ,'smin_W1','smin_W2','smin_W3','smin_W4','smin_W5','smin_W6')])
cISI<-cISI[complete.cases(cISI),]
colnames(cISI)<-c('ID','St1','St2','St3','ISI_B','ISI_w1','ISI_w2','ISI_w3','ISI_w4','ISI_w5','ISI_w6')
cISI
