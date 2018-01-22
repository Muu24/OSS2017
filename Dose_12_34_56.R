library(gdata)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
df_ISI=read.xls ("ISI Data.xlsx",header=TRUE)
df_ISI[,'strain_1']<-as.factor(df_ISI[,'strain_1'])
df_ISI[,'strain_2']<-as.factor(df_ISI[,'strain_2'])
df_ISI[,'strain_3']<-as.factor(df_ISI[,'strain_3'])
cISI<-as_data_frame(df_ISI[,1:11])
cISI<-cISI[complete.cases(cISI),]
colnames(cISI)<-c('ID','St1','St2','St3','ISI_B','ISI_w1','ISI_w2','ISI_w3','ISI_w4','ISI_w5','ISI_w6')


StB1<-cISI %>% filter(St1 %in% 1)
t.test(StB1$ISI_B,StB1$ISI_w1,paired=TRUE)
t.test(StB1$ISI_B,StB1$ISI_w2,paired=TRUE)
t.test(StB1$ISI_w1,StB1$ISI_w2,paired=TRUE)

StB2<-cISI %>% filter(St1 %in% 2)
t.test(StB2$ISI_B,StB2$ISI_w1,paired=TRUE)
t.test(StB2$ISI_B,StB2$ISI_w2,paired=TRUE)

StB3<-cISI %>% filter(St2 %in% 3)
t.test(StB3$ISI_B,StB3$ISI_w3,paired=TRUE)
t.test(StB3$ISI_B,StB3$ISI_w4,paired=TRUE)

#two sample t-test
t.test(StB1$ISI_w1,StB2$ISI_w1)
t.test(StB1$ISI_w2,StB2$ISI_w2)
t.test(StB2$ISI_w1,StB3$ISI_w1)
