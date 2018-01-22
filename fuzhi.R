library(ggplot2)
library(tidyverse)
library(gdata)
#options(digits = 3) 
df_ISI=read.xls ("Sleep Logs Data (Rested, sleep quality, latency, highness, tolerance).xlsx",header=TRUE)
df_ISI[,'Strain_1']<-as.factor(df_ISI[,'Strain_1'])
df_ISI[,'Strain_2']<-as.factor(df_ISI[,'Strain_2'])
df_ISI[,'Strain_3']<-as.factor(df_ISI[,'Strain_3'])
df_ISI[,'B_Sleep_Quality']<-as.numeric(df_ISI[,'B_Sleep_Quality'])
df_ISI[,'W1_Sleep_Quality']<-as.numeric(df_ISI[,'W1_Sleep_Quality'])
df_ISI[,'W2_Sleep_Quality']<-as.numeric(df_ISI[,'W2_Sleep_Quality'])
df_ISI[,'W3_Sleep_Quality']<-as.numeric(df_ISI[,'W3_Sleep_Quality'])
df_ISI[,'W4_Sleep_Quality']<-as.numeric(df_ISI[,'W4_Sleep_Quality'])
df_ISI[,'W5_Sleep_Quality']<-as.numeric(df_ISI[,'W5_Sleep_Quality'])
df_ISI[,'W6_Sleep_Quality']<-as.numeric(df_ISI[,'W6_Sleep_Quality'])
cISI<-as_data_frame(df_ISI[,c('Subject.ID','Strain_1','Strain_2','Strain_3','B_Sleep_Quality'
                              ,'W1_Sleep_Quality','W2_Sleep_Quality','W3_Sleep_Quality','W4_Sleep_Quality','W5_Sleep_Quality','W6_Sleep_Quality')])
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
#1. For Stain 1 = 1 named 'Lot 3' and Strain 2 = 3 named 'Argyle', comparing week 1 and week 3, week 2 and week 4.
#```{r}
St11_23<-cISI %>% filter(St1 %in% 1,St2 %in% 3)
St11_23
t2_1<-t.test(St11_23$ISI_w1,St11_23$ISI_w3,paired=TRUE)
t2_1
t2_2<-t.test(St11_23$ISI_w2,St11_23$ISI_w4,paired=TRUE)
t2_2
#```
#2. For Strain 1 = 1 named 'Lot 3' and Strain 2 = 4 named 'Bediol', comparing week 1 and week 3, week 2 and week 4.
#```{r}
St11_24<-cISI %>% filter(St1 %in% 1,St2 %in% 4)
St11_24
t2_3<-t.test(St11_24$ISI_w1,St11_24$ISI_w3,paired=TRUE)
t2_3
t2_4<-t.test(St11_24$ISI_w2,St11_24$ISI_w4,paired=TRUE)
t2_4
#```
#3. For Strain 1 = 1 named 'Lot 3' and Strain 2 = 5 named 'Blue', comparing week 1 and week 3, week 2 and week 4.
#```{r}
St11_25<-cISI %>% filter(St1 %in% 1,St2 %in% 5)
St11_25
t2_5<-t.test(St11_25$ISI_w1,St11_25$ISI_w3,paired=TRUE)
t2_5
t2_6<-t.test(St11_25$ISI_w2,St11_25$ISI_w4,paired=TRUE)
t2_6
#```
#4. For Strain 1 = 1 named 'Lot 3' and Strain 3 = 6 named 'Red Sativa', , comparing week 1 and week 5, week 2 and week 6.
#```{r}
St11_36<-cISI %>% filter(St1 %in% 1,St3%in% 6)
St11_36
t2_7<-t.test(St11_36$ISI_w1,St11_36$ISI_w5,paired=TRUE)
t2_8<-t.test(St11_36$ISI_w2,St11_36$ISI_w6,paired=TRUE)
#```
#5. For St1=1 named 'Lot 3' and St3=7 named 'Red Indica':
#  ```{r error=TRUE}
St11_37<-cISI %>% filter(St1 %in% 1,St3 %in% 7)
St11_37
t2_9<-t.test(St11_37$ISI_w1,St11_37$ISI_w5,paired=TRUE)
t2_10<-t.test(St11_37$ISI_w2,St11_37$ISI_w6,paired=TRUE)
#```
#6. For St1=1 named 'Lot 3' and St3=8 named 'Bedica':
#  ```{r error=TRUE}
St11_38<-cISI %>% filter(St1 %in% 1,St2 %in% 8)
St11_38
t2_11<-t.test(St11_38$ISI_w1,St11_38$ISI_w5,paired=TRUE)
t2_12<-t.test(St11_38$ISI_w2,St11_38$ISI_w6,paired=TRUE)
#```
#7.For St1=1 named 'Lot 3' and St3=9 named 'Bakerstreet':
#  ```{r error=TRUE}
St11_39<-cISI %>% filter(St1 %in% 2,St2 %in% 9)
St11_39
t2_13<-t.test(St11_39$ISI_w1,St11_39$ISI_w5,paired=TRUE)
t2_14<-t.test(St11_39$ISI_w2,St11_39$ISI_w6,paired=TRUE)

#```

#8. For St1=2 named 'Yellow ' and St2=3 named 'Argyle':
#  ```{r}
St12_23<-cISI %>% filter(St1 %in% 2,St2 %in% 3)
St12_23
t2_15<-t.test(St12_23$ISI_w1,St12_23$ISI_w3,paired=TRUE)
t2_16<-t.test(St12_23$ISI_w2,St12_23$ISI_w4,paired=TRUE)
#```
#9. For St1=2 named 'Yellow ' and St2=4 named 'Bediol':
#  ```{r}
St12_24<-cISI %>% filter(St1 %in% 2,St2 %in% 4)
St12_24
t2_17<-t.test(St12_24$ISI_w1,St12_24$ISI_w3,paired=TRUE)
t2_18<-t.test(St12_24$ISI_w2,St12_24$ISI_w4,paired=TRUE)
#```
#10. For St1=2 named 'Yellow ' and St2=5 named 'Blue':
#  ```{r}
St12_25<-cISI %>% filter(St1 %in% 2,St2 %in% 5)
St12_25
t2_19<-t.test(St12_25$ISI_w1,St12_25$ISI_w3,paired=TRUE)
t2_20<-t.test(St12_25$ISI_w2,St12_25$ISI_w4,paired=TRUE)
#```
#11. For St1=2 named 'Yellow ' and St3=6 named 'Red Sativa':
#  ```{r}
St12_36<-cISI %>% filter(St1 %in% 2,St3 %in% 6)
St12_36
t2_21<-t.test(St12_36$ISI_w1,St12_36$ISI_w5,paired=TRUE)
t2_22<-t.test(St12_36$ISI_w2,St12_36$ISI_w6,paired=TRUE)
#```
#12. For St1=2 named 'Yellow ' and St3=7 named 'Red Indica':
#  ```{r}
St12_37<-cISI %>% filter(St1 %in% 2,St3 %in% 7)
St12_37
t2_23<-t.test(St12_37$ISI_w1,St12_37$ISI_w5,paired=TRUE)
t2_24<-t.test(St12_37$ISI_w2,St12_37$ISI_w6,paired=TRUE)
#```
#13. For St1=2 named 'Yellow' and St3=8 named 'Bedica':
#  ```{r}
St12_38<-cISI %>% filter(St1 %in% 2,St3 %in% 8)
St12_38
t2_25<-t.test(St12_38$ISI_w1,St12_38$ISI_w5,paired=TRUE)
t2_26<-t.test(St12_38$ISI_w2,St12_38$ISI_w6,paired=TRUE)
#```
#14. For St1=2 named 'Yellow ' and St3=9 named 'Bakerstreet':
#  ```{r}
St12_39<-cISI %>% filter(St1 %in% 2,St3 %in% 9)
St12_39
t2_27<-t.test(St12_39$ISI_w1,St12_39$ISI_w5,paired=TRUE)
t2_28<-t.test(St12_39$ISI_w2,St12_39$ISI_w6,paired=TRUE)
#```
#15. For St2=3 named 'Argyle' and St3=6 named 'Red Sativa':
#  ```{r}
St23_36<-cISI %>% filter(St2 %in% 3,St3 %in% 6)
St23_36
t2_29<-t.test(St23_36$ISI_w3,St23_36$ISI_w5,paired=TRUE)
t2_30<-t.test(St23_36$ISI_w4,St23_36$ISI_w6,paired=TRUE)
#```
#16. For St2=3 named 'Argyle' and St3=7 named 'Red Indica':
#  ```{r error=TRUE}
St23_37<-cISI %>% filter(St2 %in% 3,St3 %in% 7)
St23_37
#t2_31<-t.test(St23_37$ISI_w3,St23_37$ISI_w5,paired=TRUE)
#t2_32<-t.test(St23_37$ISI_w4,St23_37$ISI_w6,paired=TRUE)
#```
#17. For St2=3 named 'Argyle' and St3=8 named 'Bedica':
#  ```{r}
St23_38<-cISI %>% filter(St2 %in% 3,St3 %in% 8)
St23_38
t2_33<-t.test(St23_38$ISI_w3,St23_38$ISI_w5,paired=TRUE)
t2_34<-t.test(St23_38$ISI_w4,St23_38$ISI_w6,paired=TRUE)
#```
#18. For St2=3 named 'Argyle' and St3=9 named 'Bakerstreet':
#  ```{r}
St23_39<-cISI %>% filter(St2 %in% 3,St3 %in% 9)
St23_39
t2_35<-t.test(St23_39$ISI_w3,St23_39$ISI_w5,paired=TRUE)
t2_36<-t.test(St23_39$ISI_w4,St23_39$ISI_w6,paired=TRUE)
#```
#19. For St2=4 named 'Bediol' and St3=6 named 'Red Sativa':
#  ```{r}
St24_36<-cISI %>% filter(St2 %in% 4,St3 %in% 6)
St24_36
t2_37<-t.test(St24_36$ISI_w3,St24_36$ISI_w5,paired=TRUE)
t2_38<-t.test(St24_36$ISI_w4,St24_36$ISI_w6,paired=TRUE)
#```
#20. For St2=4 named 'Bediol' and St3=7 named 'Red Indica':
#  ```{r error=TRUE}
St24_37<-cISI %>% filter(St2 %in% 4,St3 %in% 7)
St24_37
t2_39<-t.test(St24_37$ISI_w3,St24_37$ISI_w5,paired=TRUE)
t2_40<-t.test(St24_37$ISI_w4,St24_37$ISI_w6,paired=TRUE)
#```
#21. For St2=4 named 'Bediol' and St3=8 named 'Bedica':
#  ```{r}
St24_38<-cISI %>% filter(St2 %in% 4,St3 %in% 8)
St24_38
t2_41<-t.test(St24_38$ISI_w3,St24_38$ISI_w5,paired=TRUE)
t2_42<-t.test(St24_38$ISI_w4,St24_38$ISI_w6,paired=TRUE)
#```
#22. For St2=4 named 'Bediol' and St3=9 named 'Bakerstreet':
#  ```{r error=TRUE}
St23_39<-cISI %>% filter(St2 %in% 4,St3 %in% 9)
St23_39
t2_43<-t.test(St23_39$ISI_w3,St23_39$ISI_w5,paired=TRUE)
t2_44<-t.test(St23_39$ISI_w4,St23_39$ISI_w6,paired=TRUE)
#```
#23. For St2=5 named 'Blue' and St3=6 named 'Red Sativa':
#  ```{r}
St25_36<-cISI %>% filter(St2 %in% 5,St3 %in% 6)
St25_36
t2_45<-t.test(St25_36$ISI_w3,St25_36$ISI_w5,paired=TRUE)
t2_46<-t.test(St25_36$ISI_w4,St25_36$ISI_w6,paired=TRUE)
#```
#24. For St2=5 named 'Blue' and St3=7 named 'Red Indica':
#  ```{r}
St25_37<-cISI %>% filter(St2 %in% 5,St3 %in% 7)
St25_37
t2_47<-t.test(St25_37$ISI_w3,St25_37$ISI_w5,paired=TRUE)
t2_48<-t.test(St25_37$ISI_w4,St25_37$ISI_w6,paired=TRUE)
#```
#25. For St2=5 named 'Blue' and St3=8 named 'Bedica':
#  ```{r}
St25_38<-cISI %>% filter(St2 %in% 5,St3 %in% 8)
St25_38
t2_49<-t.test(St25_38$ISI_w3,St25_38$ISI_w5,paired=TRUE)
t2_50<-t.test(St25_38$ISI_w4,St25_38$ISI_w6,paired=TRUE)
#```
#26. For St2=5 named 'Blue' and St3=9 named 'Bakerstreet':
#  ```{r}
St25_39<-cISI %>% filter(St2 %in% 5,St3 %in% 9)
St25_39
t2_51<-t.test(St25_39$ISI_w3,St25_39$ISI_w5,paired=TRUE)
t2_52<-t.test(St25_39$ISI_w4,St25_39$ISI_w6,paired=TRUE)

a2<<-c(t2_1$estimate,t2_2$estimate,
          t2_3$estimate,t2_4$estimate,
          t2_5$estimate,t2_6$estimate,
          #t2_7$estimate,t2_8$estimate,
          NA,NA,
          t2_9$estimate,t2_10$estimate,
          NA,NA,
          NA,NA,
          t2_15$estimate,t2_16$estimate,
          t2_17$estimate,t2_18$estimate,
          t2_19$estimate,t2_20$estimate,
          t2_21$estimate,t2_22$estimate,
          t2_23$estimate,t2_24$estimate,
          t2_25$estimate,t2_26$estimate,
          t2_27$estimate,t2_28$estimate,
          t2_29$estimate,t2_30$estimate,
          #t2_31$estimate,t2_32$estimate
          NA,NA,
          t2_33$estimate,t2_34$estimate,
          t2_35$estimate,t2_36$estimate,
          #t2_37$estimate,t2_38$estimate,
          NA,NA,
          t2_39$estimate,t2_40$estimate,
          t2_41$estimate,t2_42$estimate,
          NA,NA,
          #t2_43$estimate,t2_44$estimate,
          t2_45$estimate,t2_46$estimate,
          t2_47$estimate,t2_48$estimate,
          t2_49$estimate,t2_50$estimate,
          t2_51$estimate,t2_52$estimate)
b2<-rbind(t2_1$conf.int,t2_2$conf.int,
            t2_3$conf.int,t2_4$conf.int,
            t2_5$conf.int,t2_6$conf.int,
            #t2_7$conf.int,t2_8$conf.int,
            NA,NA,
            t2_9$conf.int,t2_10$conf.int,
            NA,NA,
            NA,NA,
            t2_15$conf.int,t2_16$conf.int,
            t2_17$conf.int,t2_18$conf.int,
            t2_19$conf.int,t2_20$conf.int,
            t2_21$conf.int,t2_22$conf.int,
            t2_23$conf.int,t2_24$conf.int,
            t2_25$conf.int,t2_26$conf.int,
            t2_27$conf.int,t2_28$conf.int,
            t2_29$conf.int,t2_30$conf.int,
            #t2_31$conf.int,t2_32$conf.int
            NA,NA,
            t2_33$conf.int,t2_34$conf.int,
            t2_35$conf.int,t2_36$conf.int,
            #t2_37$conf.int,t2_38$conf.int,
            NA,NA,
            t2_39$conf.int,t2_40$conf.int,
            t2_41$conf.int,t2_42$conf.int,
            NA,NA,
            #t2_43$conf.int,t2_44$conf.int,
            t2_45$conf.int,t2_46$conf.int,
            t2_47$conf.int,t2_48$conf.int,
            t2_49$conf.int,t2_50$conf.int,
            t2_51$conf.int,t2_52$conf.int)
c2<-c(t2_1$statistic,t2_2$statistic,
      t2_3$statistic,t2_4$statistic,
      t2_5$statistic,t2_6$statistic,
      #t2_7$statistic,t2_8$statistic,
      NA,NA,
      t2_9$statistic,t2_10$statistic,
      NA,NA,
      NA,NA,
      t2_15$statistic,t2_16$statistic,
      t2_17$statistic,t2_18$statistic,
      t2_19$statistic,t2_20$statistic,
      t2_21$statistic,t2_22$statistic,
      t2_23$statistic,t2_24$statistic,
      t2_25$statistic,t2_26$statistic,
      t2_27$statistic,t2_28$statistic,
      t2_29$statistic,t2_30$statistic,
      #t2_31$statistic,t2_32$statistic
      NA,NA,
      t2_33$statistic,t2_34$statistic,
      t2_35$statistic,t2_36$statistic,
      #t2_37$statistic,t2_38$statistic,
      NA,NA,
      t2_39$statistic,t2_40$statistic,
      t2_41$statistic,t2_42$statistic,
      NA,NA,
      #t2_43$statistic,t2_44$statistic,
      t2_45$statistic,t2_46$statistic,
      t2_47$statistic,t2_48$statistic,
      t2_49$statistic,t2_50$statistic,
      t2_51$statistic,t2_52$statistic)
d2<-c(t2_1$p.value,t2_2$p.value,
      t2_3$p.value,t2_4$p.value,
      t2_5$p.value,t2_6$p.value,
      #t2_7$p.value,t2_8$p.value,
      NA,NA,
      t2_9$p.value,t2_10$p.value,
      NA,NA,
      NA,NA,
      t2_15$p.value,t2_16$p.value,
      t2_17$p.value,t2_18$p.value,
      t2_19$p.value,t2_20$p.value,
      t2_21$p.value,t2_22$p.value,
      t2_23$p.value,t2_24$p.value,
      t2_25$p.value,t2_26$p.value,
      t2_27$p.value,t2_28$p.value,
      t2_29$p.value,t2_30$p.value,
      #t2_31$p.value,t2_32$p.value
      NA,NA,
      t2_33$p.value,t2_34$p.value,
      t2_35$p.value,t2_36$p.value,
      #t2_37$p.value,t2_38$p.value,
      NA,NA,
      t2_39$p.value,t2_40$p.value,
      t2_41$p.value,t2_42$p.value,
      NA,NA,
      #t2_43$p.value,t2_44$p.value,
      t2_45$p.value,t2_46$p.value,
      t2_47$p.value,t2_48$p.value,
      t2_49$p.value,t2_50$p.value,
      t2_51$p.value,t2_52$p.value)
Table2<-cbind(a2,b2,c2,d2)
colnames(Table2)<-c('Mean Difference','Lower Bound','Upper Bound','T','P-value')
rownames(Table2)<-c(1:52)

Table2<-format(round(Table2, 4),nsmall = 2)
Table2
write.csv(Table2, "Table2tsm.csv")
