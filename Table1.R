library(ggplot2)
library(tidyverse)
library(gdata)
df_ISI=read.xls ("ISI Data.xlsx",header=TRUE)
df_ISI[,'strain_1']<-as.factor(df_ISI[,'strain_1'])
df_ISI[,'strain_2']<-as.factor(df_ISI[,'strain_2'])
df_ISI[,'strain_3']<-as.factor(df_ISI[,'strain_3'])
cISI<-as_data_frame(df_ISI[,1:11])
cISI<-cISI[complete.cases(cISI),]
colnames(cISI)<-c('ID','St1','St2','St3','ISI_B','ISI_w1','ISI_w2','ISI_w3','ISI_w4','ISI_w5','ISI_w6')
cISI

#
library(ggplot2)
library(tidyverse)
library(gdata)
df_ISI=read.xls ("Actigraphy Data.xlsx",header=TRUE)
df_ISI[,'strain_1']<-as.factor(df_ISI[,'strain_1'])
df_ISI[,'strain_2']<-as.factor(df_ISI[,'strain_2'])
df_ISI[,'strain_3']<-as.factor(df_ISI[,'strain_3'])
cISI<-as_data_frame(df_ISI[,c('X.Subject.ID','strain_1','strain_2','strain_3','smin_B'
                              ,'smin_W1','smin_W2','smin_W3','smin_W4','smin_W5','smin_W6')])
cISI<-cISI[complete.cases(cISI),]
colnames(cISI)<-c('ID','St1','St2','St3','ISI_B','ISI_w1','ISI_w2','ISI_w3','ISI_w4','ISI_w5','ISI_w6')
cISI

df_ISI=read.xls ("Actigraphy Data.xlsx",header=TRUE)
df_ISI[,'strain_1']<-as.factor(df_ISI[,'strain_1'])
df_ISI[,'strain_2']<-as.factor(df_ISI[,'strain_2'])
df_ISI[,'strain_3']<-as.factor(df_ISI[,'strain_3'])
cISI<-as_data_frame(df_ISI[,c('X.Subject.ID','strain_1','strain_2','strain_3','lgwep_B'
                              ,'lgwep_W1','lgwep_W2','lgwep_W3','lgwep_W4','lgwep_W5','lgwep_W6')])
cISI<-cISI[complete.cases(cISI),]
colnames(cISI)<-c('ID','St1','St2','St3','ISI_B','ISI_w1','ISI_w2','ISI_w3','ISI_w4','ISI_w5','ISI_w6')
cISI

df_ISI=read.xls ("Actigraphy Data.xlsx",header=TRUE)
df_ISI[,'strain_1']<-as.factor(df_ISI[,'strain_1'])
df_ISI[,'strain_2']<-as.factor(df_ISI[,'strain_2'])
df_ISI[,'strain_3']<-as.factor(df_ISI[,'strain_3'])
cISI<-as_data_frame(df_ISI[,c('X.Subject.ID','strain_1','strain_2','strain_3','lgwep_B'
                              ,'lgwep_W1','lgwep_W2','lgwep_W3','lgwep_W4','lgwep_W5','lgwep_W6')])
cISI<-cISI[complete.cases(cISI),]
colnames(cISI)<-c('ID','St1','St2','St3','ISI_B','ISI_w1','ISI_w2','ISI_w3','ISI_w4','ISI_w5','ISI_w6')
cISI

df_ISI=read.xls ("Actigraphy Data.xlsx",header=TRUE)
df_ISI[,'strain_1']<-as.factor(df_ISI[,'strain_1'])
df_ISI[,'strain_2']<-as.factor(df_ISI[,'strain_2'])
df_ISI[,'strain_3']<-as.factor(df_ISI[,'strain_3'])
cISI<-as_data_frame(df_ISI[,c('X.Subject.ID','strain_1','strain_2','strain_3','wmin_B'
                              ,'wmin_W1','wmin_W2','wmin_W3','wmin_W4','wmin_W5','wmin_W6')])
cISI<-cISI[complete.cases(cISI),]
colnames(cISI)<-c('ID','St1','St2','St3','ISI_B','ISI_w1','ISI_w2','ISI_w3','ISI_w4','ISI_w5','ISI_w6')
cISI


df_ISI=read.xls ("Actigraphy Data.xlsx",header=TRUE)
df_ISI[,'strain_1']<-as.factor(df_ISI[,'strain_1'])
df_ISI[,'strain_2']<-as.factor(df_ISI[,'strain_2'])
df_ISI[,'strain_3']<-as.factor(df_ISI[,'strain_3'])
cISI<-as_data_frame(df_ISI[,c('X.Subject.ID','strain_1','strain_2','strain_3','seff_B'
                              ,'seff_W1','seff_W2','seff_W3','seff_W4','seff_W5','seff_W6')])
cISI<-cISI[complete.cases(cISI),]
colnames(cISI)<-c('ID','St1','St2','St3','ISI_B','ISI_w1','ISI_w2','ISI_w3','ISI_w4','ISI_w5','ISI_w6')
cISI
#First, we check the normaility to satisfy the normal population assumption:
#  ```{r}



###


library(ggplot2)
library(tidyverse)
library(gdata)
df_ISI=read.xls ("EQ-5D Data.xlsx",header=TRUE)
df_ISI[,'Strain_1']<-as.factor(df_ISI[,'Strain_1'])
df_ISI[,'Strain_2']<-as.factor(df_ISI[,'Strain_2'])
df_ISI[,'Strain_3']<-as.factor(df_ISI[,'Strain_3'])
cISI<-as_data_frame(df_ISI[,c('Subject.ID','Strain_1','Strain_2','Strain_3','EQ_Index_b'
                              ,'EQ_Index_wk1','EQ_Index_wk2','EQ_Index_wk3','EQ_Index_wk4','EQ_Index_wk5','EQ_Index_wk6')])
cISI<-cISI[complete.cases(cISI),]
colnames(cISI)<-c('ID','St1','St2','St3','ISI_B','ISI_w1','ISI_w2','ISI_w3','ISI_w4','ISI_w5','ISI_w6')
cISI
###
par(mfrow=c(3,3))
cISIp<-as.data.frame(cISI)
for (i in 5:11){  
  qqnorm(cISIp[, i], main = names(cISI[i]))
  qqline(cISIp[, i],col=1)
}
#```
#From the Plot, we can tell the assumption is reasonable.

#Insomnia Severity Index: mean difference ($\pm$ 95\% CI) and note any clinically significant change in score.
#\subsubsection{Strains(including difference to baseline)}
#In this section, we group the individuals by different nine Strains, which are (Lot 3, Yellow), (Argyle, Bediol, Blue), (Red Sativa, Red Indica, Bedica, Bakerstreet). As for different weeks, each individuals have different Strains, so the data are dependent with each other, such the statistic tool we need to apply is the paired t test.

#Tables 1: Table for ISI Mean Difference by Strains against Baseline
#
#**Statistic**: paried t statistic, mean difference with Baseline 's 95\% Confidence Interval

#**Methoodology \& Variables**: Paired t-test and Strains#

#**Hypothesis**: 

#$H_0$: The selected Stain has no differences with Baseline

#$H_a$: The selected Stain has differences with Baseline

#**Subtext**: 1-9 listed below in this section.

#**Discussion of Results**: Further details of results are all listed below with explanations. In total, we have $9$ pairs to be listed, where we compare different Strains with baseline, therefore we have total 9 pairs. (But we have 18 results due to the two levels of dose value for each stage).
#1. Stain 1= 1 Lot 3 with Baseline
#```{r}
StB1<-cISI %>% filter(St1 %in% 1)
StB1
ct1<-t.test(StB1$ISI_B,StB1$ISI_w1,paired=TRUE)
ct1
ct2<-t.test(StB1$ISI_B,StB1$ISI_w2,paired=TRUE)
ct2
#```
#2. Stain 1= 2 Yellow with Baseline
#```{r}
StB2<-cISI %>% filter(St1 %in% 2)
StB2
ct3<-t.test(StB2$ISI_B,StB2$ISI_w1,paired=TRUE)
ct4<-t.test(StB2$ISI_B,StB2$ISI_w2,paired=TRUE)
#```
#3. Stain 2= 3 Argyle with Baseline
#```{r}
StB3<-cISI %>% filter(St2 %in% 3)
StB3
ct5<-t.test(StB3$ISI_B,StB3$ISI_w3,paired=TRUE)
ct6<-t.test(StB3$ISI_B,StB3$ISI_w4,paired=TRUE)
#```
#4. Stain 2= 4 Bediol with Baseline
#```{r}
StB4<-cISI %>% filter(St2 %in% 4)
StB4
ct7<-t.test(StB4$ISI_B,StB4$ISI_w3,paired=TRUE)
ct8<-t.test(StB4$ISI_B,StB4$ISI_w4,paired=TRUE)
#```
#5. Stain 2= 5 Blue with Baseline
#```{r}
StB5<-cISI %>% filter(St2 %in% 5)
StB5
ct9<-t.test(StB5$ISI_B,StB5$ISI_w3,paired=TRUE)
ct10<-t.test(StB5$ISI_B,StB5$ISI_w4,paired=TRUE)
#```

#6. Stain 2= 6 Bedica with Baseline
#```{r}
StB6<-cISI %>% filter(St3 %in% 6)
StB6
ct11<-t.test(StB6$ISI_B,StB6$ISI_w5,paired=TRUE)
ct12<-t.test(StB6$ISI_B,StB6$ISI_w6,paired=TRUE)
#```

#7. Stain 2= 7 Bakerstreet with Baseline
#```{r}
StB7<-cISI %>% filter(St3 %in% 7)
StB7
ct13<-t.test(StB7$ISI_B,StB7$ISI_w5,paired=TRUE)
ct14<-t.test(StB7$ISI_B,StB7$ISI_w6,paired=TRUE)
#```
#8. Stain 2= 6 Red Indica with Baseline
#```{r}
StB8<-cISI %>% filter(St3 %in% 8)
StB8
ct15<-t.test(StB8$ISI_B,StB8$ISI_w5,paired=TRUE)
ct16<-t.test(StB8$ISI_B,StB8$ISI_w6,paired=TRUE)
#```
#9. Stain 2= 6 Red Sativa with Baseline
#```{r}
StB9<-cISI %>% filter(St3 %in% 9)
StB9
ct17<-t.test(StB9$ISI_B,StB9$ISI_w5,paired=TRUE)
ct18<-t.test(StB9$ISI_B,StB9$ISI_w6,paired=TRUE)
#```

#```{r}
a<-c(ct1$estimate,ct2$estimate,ct3$estimate,
ct4$estimate,ct5$estimate,ct6$estimate,
ct7$estimate,ct8$estimate,ct9$estimate,
ct10$estimate,ct11$estimate,ct12$estimate,
ct13$estimate,ct14$estimate,ct15$estimate,
ct16$estimate,ct17$estimate,ct18$estimate)
b<-rbind(ct1$conf.int,ct2$conf.int,ct3$conf.int,
ct4$conf.int,ct5$conf.int,ct6$conf.int,
ct7$conf.int,ct8$conf.int,ct9$conf.int,
ct10$conf.int,ct11$conf.int,ct12$conf.int,
ct13$conf.int,ct14$conf.int,ct15$conf.int,
ct16$conf.int,ct17$conf.int,ct18$conf.int)
c<-c(ct1$statistic,ct2$statistic,ct3$statistic,
ct4$statistic,ct5$statistic,ct6$statistic,
ct7$statistic,ct8$statistic,ct9$statistic,
ct10$statistic,ct11$statistic,ct12$statistic,
ct13$statistic,ct14$statistic,ct15$statistic,
ct16$statistic,ct17$statistic,ct18$statistic)
d<-c(ct1$p.value,ct2$p.value,ct3$p.value,
ct4$p.value,ct5$p.value,ct6$p.value,
ct7$p.value,ct8$p.value,ct9$p.value,
ct10$p.value,ct11$p.value,ct12$p.value,
ct13$p.value,ct14$p.value,ct15$p.value,
ct16$p.value,ct17$p.value,ct18$p.value)
Table1<-cbind(a,b,c,d)
colnames(Table1)<-c('Mean Difference','Lower Bound','Upper Bound','T','P-value')
rownames(Table1)<-c('Lot3_w1','Lot3_w2','Yellow_w1','Yellow_w2','Argyle_w1',
'Argyle_w2','Bediol_w1','Bediol_w2','Blue_w1','Blue_w2',
'Bedica_w1','Bedica_w2','BakerSt_w1','BakerSt_w2',
'RedIndica_w1','RedIndica_w2','RedSativa_w1','RedSativa_w2')
Table1
Table1<-format(round(Table1, 4), nsmall = 2)
write.csv(Table1, "Table1tsm.csv")
#```