################################################################
#Title: Thermal History (TH) Analysis 
#Purpose: Determine relationship between Q10 and thermal history - all species - across all timepoints
#Created by: R. E. Rangel
#Created: August 2018
#Last edited: 15 April 2022
################################################################
##### Packages #####
library(tidyverse)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(lme4)
library(lmerTest)
library(emmeans)
library(pastecs)
library(lattice)
library(ggplot2)
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(ggthemes)
library(nortest) #For ad.test for normality
###Working Directory###--------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Thermal History")

#----------------------------------------------------------------------------
#THERMAL HISTORY ANALYSIS---------------------------------------------------
TH_Meta<-read.csv("RangelandSorte_ThermalHistory_All_Updated.csv", na.strings = "nd", header=T) 


str(TH_Meta)
#-------------------------------------------------------
#------------------------
#3MONTH ONLY------------------------------------------------------
#---------------------------------
ThreeMo<-filter(TH_Meta, TempDate == "3Month")
#all=c("#FDE725FF","#21908CFF","#440154FF")[ThreeMo$Species]
count(ThreeMo$Pool)
count(ThreeMo$Species)

#3 MONTHS---------Full model ----> Simplify----------------------
M3.Q10<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*PC2+Species*Timepoint+(1|Pool), data=ThreeMo)
summary(M3.Q10)
anova(M3.Q10) 
qqnorm(resid(M3.Q10))
qqline(resid(M3.Q10))
hist(resid(M3.Q10))
ad.test(resid(M3.Q10)) #Checking normality


#DROPPING TERMS ------------------------------------------------
M3.Q10.2<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*PC2+(1|Pool), data=ThreeMo) #Simplest model
summary(M3.Q10.2)
anova(M3.Q10.2)
#Check to see if you can drop term
anova(M3.Q10, M3.Q10.2) # Yes, can drop
qqnorm(resid(M3.Q10.2))
qqline(resid(M3.Q10.2))
hist(resid(M3.Q10.2))
ad.test(resid(M3.Q10.2)) #Checking normality

AIC(M3.Q10, M3.Q10.2)

          #df   AIC
#M3.Q10   21 152.0335
#M3.Q10.2 17 148.8028

#DROPPING TERMS ------------------------------------------------
M3.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+(1|Pool), data=ThreeMo)
summary(M3.Q10.3)
anova(M3.Q10.3)
#Check to see if you can drop term
anova(M3.Q10.2, M3.Q10.3) # Yes, can drop
qqnorm(resid(M3.Q10.3))
qqline(resid(M3.Q10.3))
hist(resid(M3.Q10.3))
ad.test(resid(M3.Q10.3)) #Checking normality

AIC(M3.Q10.2, M3.Q10.3)

#df   AIC
#M3.Q10.2 17 148.8028
#M3.Q10.3 15 138.8239


#DROPPING TERMS ------------------------------------------------
M3.Q10.4<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Species*PC1+(1|Pool), data=ThreeMo)
summary(M3.Q10.4)
anova(M3.Q10.4)
#Check to see if you can drop term
anova(M3.Q10.3, M3.Q10.4) # Yes, can drop
qqnorm(resid(M3.Q10.4))
qqline(resid(M3.Q10.4))
hist(resid(M3.Q10.4))
ad.test(resid(M3.Q10.4)) #Checking normality

AIC(M3.Q10.3, M3.Q10.4)

#df   AIC
#M3.Q10.3 15 138.8239
#M3.Q10.4 13 133.8902



#DROPPING TERMS ------------------------------------------------
M3.Q10.5<-lmer(Q10~PC1+PC2+Timepoint+Species+Species*PC1+(1|Pool), data=ThreeMo) #Simplest model
summary(M3.Q10.5)
anova(M3.Q10.5)
#Check to see if you can drop term
anova(M3.Q10.4, M3.Q10.5) # Yes, can drop
qqnorm(resid(M3.Q10.5))
qqline(resid(M3.Q10.5))
hist(resid(M3.Q10.5))
ad.test(resid(M3.Q10.5)) #Checking normality

AIC(M3.Q10.4, M3.Q10.5)

#df   AIC
#M3.Q10.4 13 133.8902
#M3.Q10.5 11 126.4839


#Post hoc - emmeans--------------------------------------------
#Interested in overall effect of Species
Spp.3mo<- emmeans(M3.Q10.5, ~Species*PC1)
pairs(Spp.3mo, by="PC1")

# contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine    0.080 0.0813 92.0 0.985   0.5881 
#Hermit - Mussel       0.267 0.0783 92.9 3.414   0.0027 
#Littorine - Mussel    0.187 0.0808 92.6 2.315   0.0586  


#------------------------
#2MONTH ONLY---------Full model ----> Simplify--------------------------------------
#---------------------------------
TwoMo<-filter(TH_Meta, TempDate == "2Month")

M2.Q10<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*PC2+Species*Timepoint+(1|Pool), data=TwoMo)
anova(M2.Q10)
summary(M2.Q10)
qqnorm(resid(M2.Q10))
qqline(resid(M2.Q10))
hist(resid(M2.Q10))
ad.test(resid(M2.Q10)) #Checking normality

#Remove interaction-------------------------------
M2.Q10.2<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*Timepoint+(1|Pool), data=TwoMo)
anova(M2.Q10.2)

qqnorm(resid(M2.Q10.2))
qqline(resid(M2.Q10.2))
hist(resid(M2.Q10.2))
ad.test(resid(M2.Q10.2))

anova(M2.Q10, M2.Q10.2) #Check Removal

AIC(M2.Q10, M2.Q10.2)
#df     AIC
#M2.Q10   21 150.1948
#M2.Q10.2 19 143.3810


#Remove interaction----------------------------------
M2.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*Timepoint+(1|Pool), data=TwoMo)
anova(M2.Q10.3)
qqnorm(resid(M2.Q10.3))
qqline(resid(M2.Q10.3))
hist(resid(M2.Q10.3))
ad.test(resid(M2.Q10.3))

anova(M2.Q10.2, M2.Q10.3)

AIC(M2.Q10.2, M2.Q10.3)
#         df      AIC
#M2.Q10.2 19 143.3810
#M2.Q10.3 17 134.2437


#Remove interaction----------------------------------
M2.Q10.4<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Species*Timepoint+(1|Pool), data=TwoMo)
anova(M2.Q10.4)

qqnorm(resid(M2.Q10.4))
qqline(resid(M2.Q10.4))
hist(resid(M2.Q10.4))
ad.test(resid(M2.Q10.4))

anova(M2.Q10.3, M2.Q10.4)

AIC(M2.Q10.3, M2.Q10.4)
#df      AIC
#M2.Q10.3 17 134.2437
#M2.Q10.4 15 131.2609

#Remove interaction----------------------------------
M2.Q10.5<-lmer(Q10~PC1+PC2+Timepoint+Species+Species*Timepoint+(1|Pool), data=TwoMo) #Best model for 2 months
anova(M2.Q10.5)
summary(M2.Q10.5)

qqnorm(resid(M2.Q10.5))
qqline(resid(M2.Q10.5))
hist(resid(M2.Q10.5))
ad.test(resid(M2.Q10.5))

anova(M2.Q10.5, M2.Q10.4)

AIC(M2.Q10.5, M2.Q10.4)
#df      AIC
#M2.Q10.5 13 123.4900
#M2.Q10.4 15 131.2609


#Post hoc - emmeans--------------------------------------------
#Interested in overall effect of Species
Spp.3mo<- emmeans(M3.Q10.5, ~Species*PC1)
pairs(Spp.3mo, by="PC1")

M2.Q10.emm.n<-emmeans(M2.Q10.5, ~Species*Timepoint)
M2.Q10.emm.n
pairs(M2.Q10.emm.n, by="Timepoint")

#Timepoint = 19-Jul:
#  contrast           estimate    SE   df t.ratio p.value
#Hermit - Littorine   0.0947 0.146 85.1  0.648  0.7938 
#Hermit - Mussel      0.2338 0.136 82.9  1.724  0.2022 
#Littorine - Mussel   0.1391 0.143 86.3  0.974  0.5954 

#Timepoint = 19-Mar:
#  contrast           estimate    SE   df t.ratio p.value
#Hermit - Littorine   0.1843 0.136 94.5  1.359  0.3664 
#Hermit - Mussel      0.1061 0.131 90.7  0.811  0.6971 
#Littorine - Mussel  -0.0782 0.125 90.1 -0.626  0.8062 

#Timepoint = 19-Sep:
#  contrast           estimate    SE   df t.ratio p.value
#Hermit - Littorine  -0.0598 0.142 79.8 -0.422  0.9066 
#Hermit - Mussel      0.4562 0.130 83.0  3.512  0.0021 **
#Littorine - Mussel   0.5160 0.145 85.6  3.553  0.0018 **



#------------------------
#1MONTH ONLY---------Full model ----> Simplify--------------------------------------
#---------------------------------
OneMo<-filter(TH_Meta, TempDate == "1Month")

M1.Q10<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+PC1*Species+PC2*Species+(1|Pool), data=OneMo)
anova(M1.Q10)
summary(M1.Q10)
qqnorm(resid(M1.Q10))
qqline(resid(M1.Q10))
hist(resid(M1.Q10))
shapiro.test(resid(M1.Q10))

#Remove interaction------------
M1.Q10.2<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+PC1*Species+(1|Pool), data=OneMo)
anova(M1.Q10.2)
qqnorm(resid(M1.Q10.2))
qqline(resid(M1.Q10.2))
hist(resid(M1.Q10.2))
shapiro.test(resid(M1.Q10.2))

anova(M1.Q10,M1.Q10.2)

AIC(M1.Q10,M1.Q10.2)

#df      AIC
#M1.Q10   17 149.4786
#M1.Q10.2 15 141.8544

#Remove interaction------------
M1.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+(1|Pool), data=OneMo)
anova(M1.Q10.3)
qqnorm(resid(M1.Q10.3))
qqline(resid(M1.Q10.3))
hist(resid(M1.Q10.3))
shapiro.test(resid(M1.Q10.3))


anova(M1.Q10.2,M1.Q10.3)

AIC(M1.Q10.2,M1.Q10.3)
#df      AIC
#M1.Q10.2 15 141.8544
#M1.Q10.3 13 129.7066

#Remove interaction------------
M1.Q10.4<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+(1|Pool), data=OneMo)
anova(M1.Q10.4)
qqnorm(resid(M1.Q10.4))
qqline(resid(M1.Q10.4))
hist(resid(M1.Q10.4))
shapiro.test(resid(M1.Q10.4))

anova(M1.Q10.3,M1.Q10.4) #Yes can drop

AIC(M1.Q10.3,M1.Q10.4)
#df      AIC
#M1.Q10.3 13 129.7066
#M1.Q10.4 11 122.9576

#Remove interaction------------
M1.Q10.5<-lmer(Q10~PC1+PC2+Timepoint+Species+(1|Pool), data=OneMo)
anova(M1.Q10.5)
summary(M1.Q10.5)

qqnorm(resid(M1.Q10.5))
qqline(resid(M1.Q10.5))
hist(resid(M1.Q10.5))
shapiro.test(resid(M1.Q10.5))

anova(M1.Q10.5,M1.Q10.4)

AIC(M1.Q10.5,M1.Q10.4)
#df      AIC
#M1.Q10.5  9 116.5948
#M1.Q10.4 11 122.9576


M1.Q10.emm.n<-emmeans(M1.Q10.5, "Species")
M1.Q10.emm.n
emmeans(M1.Q10.5, pairwise~Species)

#contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine    0.102 0.0905 92.0 1.126   0.5008 
#Hermit - Mussel       0.252 0.0846 90.7 2.983   0.0101 
#Littorine - Mussel    0.150 0.0837 95.3 1.798   0.1758 


#------------------------
#1 WEEK ONLY---------Full model ----> Simplify--------------------------------------
#---------------------------------
OneWeek<-filter(TH_Meta, TempDate == "1Week")


M1WK.Q10<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+PC1*Species+PC2*Species+(1|Pool), data=OneWeek)
anova(M1WK.Q10)
summary(M1WK.Q10)
qqnorm(resid(M1WK.Q10))
qqline(resid(M1WK.Q10))
hist(resid(M1WK.Q10))
ad.test(resid(M1WK.Q10))
shapiro.test(resid(M1WK.Q10))

#DROP A TERM------------------------------------------------Simplest Model
M1WK.Q10.2<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+PC1*Species+(1|Pool),data=OneWeek)
anova(M1WK.Q10.2)

summary(M1WK.Q10.2)
qqnorm(resid(M1WK.Q10.2))
qqline(resid(M1WK.Q10.2))
hist(resid(M1WK.Q10.2))
ad.test(resid(M1WK.Q10.2))
shapiro.test(resid(M1WK.Q10.2))

anova(M1WK.Q10,M1WK.Q10.2) #Yes can drop

AIC(M1WK.Q10,M1WK.Q10.2)
#df      AIC
#M1WK.Q10   17 140.1427
#M1WK.Q10.2 15 130.2312


#DROP A TERM------------------------------------------------
M1WK.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+(1|Pool),data=OneWeek)
anova(M1WK.Q10.3)

summary(M1WK.Q10.3)
qqnorm(resid(M1WK.Q10.3))
qqline(resid(M1WK.Q10.3))
hist(resid(M1WK.Q10.3))
ad.test(resid(M1WK.Q10.3))

anova(M1WK.Q10.2,M1WK.Q10.3) #Yes can drop

AIC(M1WK.Q10.2,M1WK.Q10.3)
#df      AIC
#M1WK.Q10.2 19 137.4170
#M1WK.Q10.3 17 128.4412


#DROP A TERM------------------------------------------------
M1WK.Q10.4<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+(1|Pool),data=OneWeek) #Overfits
anova(M1WK.Q10.4)

summary(M1WK.Q10.4)
qqnorm(resid(M1WK.Q10.4))
qqline(resid(M1WK.Q10.4))
hist(resid(M1WK.Q10.4))
ad.test(resid(M1WK.Q10.4))

anova(M1WK.Q10.3,M1WK.Q10.4) #Yes can drop

AIC(M1WK.Q10.3,M1WK.Q10.4)
#df      AIC
#M1WK.Q10.3 17 128.4412
#M1WK.Q10.4 13 120.1287

#######Interested in overall effect of Species---------

Spp.1wk<- emmeans(M1WK.Q10.2, ~Species*PC1)
pairs(Spp.1wk, by=c("PC1"))

#contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine   0.0537 0.0993 82.8 0.541   0.8514 
#Hermit - Mussel      0.3069 0.1063 87.9 2.887   0.0134 
#Littorine - Mussel   0.2532 0.1312 86.0 1.930   0.1365 



#------------------------
#1 DAY ONLY-------------------------------------------------------------------------------------
#---------------------------------
OneDay<-filter(TH_Meta, TempDate == "1Day")

#PC1 & PC2------------
M1DY.Q10<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+PC1*Species+PC2*Species+(1|Pool), data=OneDay)
anova(M1DY.Q10)
summary(M1DY.Q10)
qqnorm(resid(M1DY.Q10))
qqline(resid(M1DY.Q10))
hist(resid(M1DY.Q10))
shapiro.test(resid(M1DY.Q10))

#Simpler model - dropped one term
M1DY.Q10.2<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+PC1*Species+(1|Pool), data=OneDay)
anova(M1DY.Q10.2)
summary(M1DY.Q10.2)
qqnorm(resid(M1DY.Q10.2))
qqline(resid(M1DY.Q10.2))
hist(resid(M1DY.Q10.2))
ad.test(resid(M1DY.Q10.2))
shapiro.test(resid(M1DY.Q10.2)) 

#Check - can drop?
anova(M1DY.Q10,M1DY.Q10.2) #Yes can drop

AIC(M1DY.Q10,M1DY.Q10.2)
#df      AIC
#M1DY.Q10   21 150.7109
#M1DY.Q10.2 17 147.8277

#Simpler model - dropped another term
M1DY.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+(1|Pool), data=OneDay)
anova(M1DY.Q10.3)
summary(M1DY.Q10.3)
qqnorm(resid(M1DY.Q10.3))
qqline(resid(M1DY.Q10.3))
hist(resid(M1DY.Q10.3))
shapiro.test(resid(M1DY.Q10.3))


#Check - can drop?
anova(M1DY.Q10.2,M1DY.Q10.3) #Yes can drop

AIC(M1DY.Q10.2,M1DY.Q10.3)
#df      AIC
#M1DY.Q10.2 17 147.8277
#M1DY.Q10.3 15 139.9174


#Simpler model - dropped another term -- Simplest Model
M1DY.Q10.4<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+(1|Pool), data=OneDay)
anova(M1DY.Q10.4)
summary(M1DY.Q10.4)
qqnorm(resid(M1DY.Q10.4))
qqline(resid(M1DY.Q10.4))
hist(resid(M1DY.Q10.4))
shapiro.test(resid(M1DY.Q10.4))

#Check - can drop?
anova(M1DY.Q10.3,M1DY.Q10.4) #Yes can drop

AIC(M1DY.Q10.3,M1DY.Q10.4)
#df      AIC
#M1DY.Q10.3 13 125.4582
#M1DY.Q10.4 11 120.9025


#Species effect-----------------------
M1DY.Q10.emm.n<-emmeans(M1DY.Q10.4, ~Species)
pairs(M1DY.Q10.emm.n)

#contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine    0.160 0.0974 95.5 1.646   0.2315 
#Hermit - Mussel       0.274 0.0924 92.7 2.967   0.0106 
#Littorine - Mussel    0.114 0.0883 94.2 1.289   0.4047 


############################----------------------------------------------
#Counts for pools and Pool summary Stats -----------------------------------------------------------
#----------------------------------
library(pastecs) # gives descriptive stats
############3MONTHS---------------
PoolStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Mar")
PoolStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Jul")
PoolStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Sep")

stat.desc(PoolStats$Min)
stat.desc(PoolStats$Max)
stat.desc(PoolStats$Range)
stat.desc(PoolStats$Median)
stat.desc(PoolStats$Mean)
stat.desc(PoolStats$Var)
stat.desc(PoolStats$St.Dev)
stat.desc(PoolStats$X90max)
stat.desc(PoolStats$X95max)
stat.desc(PoolStats$X90min)
stat.desc(PoolStats$dailyavg)
stat.desc(PoolStats$dailymax)
stat.desc(PoolStats$daily90)
stat.desc(PoolStats$daily95)
stat.desc(PoolStats$dailymin)
stat.desc(PoolStats$dailyrange)

#2MONTHS-------------------
PoolStats<-filter(TH_Meta, TempDate == "2Month" & Timepoint == "19-Mar")
PoolStats<-filter(TH_Meta, TempDate == "2Month" & Timepoint == "19-Jul")
PoolStats<-filter(TH_Meta, TempDate == "2Month" & Timepoint == "19-Sep")

stat.desc(PoolStats$Min)
stat.desc(PoolStats$Max)
stat.desc(PoolStats$Range)
stat.desc(PoolStats$Median)
stat.desc(PoolStats$Mean)
stat.desc(PoolStats$Var)
stat.desc(PoolStats$St.Dev)
stat.desc(PoolStats$X90max)
stat.desc(PoolStats$X95max)
stat.desc(PoolStats$X90min)
stat.desc(PoolStats$dailyavg)
stat.desc(PoolStats$dailymax)
stat.desc(PoolStats$daily90)
stat.desc(PoolStats$daily95)
stat.desc(PoolStats$dailymin)
stat.desc(PoolStats$dailyrange)

#1MONTH--------------------
PoolStats<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Mar")
PoolStats<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Jul")
PoolStats<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Sep")

stat.desc(PoolStats$Min)
stat.desc(PoolStats$Max)
stat.desc(PoolStats$Range)
stat.desc(PoolStats$Median)
stat.desc(PoolStats$Mean)
stat.desc(PoolStats$Var)
stat.desc(PoolStats$St.Dev)
stat.desc(PoolStats$X90max)
stat.desc(PoolStats$X95max)
stat.desc(PoolStats$X90min)
stat.desc(PoolStats$dailyavg)
stat.desc(PoolStats$dailymax)
stat.desc(PoolStats$daily90)
stat.desc(PoolStats$daily95)
stat.desc(PoolStats$dailymin)
stat.desc(PoolStats$dailyrange)

#1WEEK
PoolStats<-filter(TH_Meta, TempDate == "1Week" & Timepoint == "19-Mar")
PoolStats<-filter(TH_Meta, TempDate == "1Week" & Timepoint == "19-Jul")
PoolStats<-filter(TH_Meta, TempDate == "1Week" & Timepoint == "19-Sep")

stat.desc(PoolStats$Min)
stat.desc(PoolStats$Max)
stat.desc(PoolStats$Range)
stat.desc(PoolStats$Median)
stat.desc(PoolStats$Mean)
stat.desc(PoolStats$Var)
stat.desc(PoolStats$St.Dev)
stat.desc(PoolStats$X90max)
stat.desc(PoolStats$X95max)
stat.desc(PoolStats$X90min)
stat.desc(PoolStats$dailyavg)
stat.desc(PoolStats$dailymax)
stat.desc(PoolStats$daily90)
stat.desc(PoolStats$daily95)
stat.desc(PoolStats$dailymin)
stat.desc(PoolStats$dailyrange)

#1DAY
PoolStats<-filter(TH_Meta, TempDate == "1Day" & Timepoint == "19-Mar")
PoolStats<-filter(TH_Meta, TempDate == "1Day" & Timepoint == "19-Jul")
PoolStats<-filter(TH_Meta, TempDate == "1Day" & Timepoint == "19-Sep")

stat.desc(PoolStats$Min)
stat.desc(PoolStats$Max)
stat.desc(PoolStats$Range)
stat.desc(PoolStats$Median)
stat.desc(PoolStats$Mean)
stat.desc(PoolStats$Var)
stat.desc(PoolStats$St.Dev)
stat.desc(PoolStats$X90max)
stat.desc(PoolStats$X95max)
stat.desc(PoolStats$X90min)
stat.desc(PoolStats$dailyavg)
stat.desc(PoolStats$dailymax)
stat.desc(PoolStats$daily90)
stat.desc(PoolStats$daily95)
stat.desc(PoolStats$dailymin)
stat.desc(PoolStats$dailyrange)

count(PoolStats$Pool)


####Average Q10
Mar19<-filter(OneWeek, Timepoint == "19-Mar")
Jul19<-filter(OneWeek, Timepoint == "19-Jul")
Sept19<-filter(OneWeek, Timepoint == "19-Sep")

stat.desc(Mar19$Q10) #Mar19: 1.32 +- 0.25
stat.desc(Jul19$Q10) #July19: 1.38 +- 0.34
stat.desc(Sept19$Q10) #Sept 19: 1.50 +- 0.46


#Q10 Average and SD-------------------------------------------

#Hermits-------------
QStats_HM19<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Mar" & Species == "Hermit")
QStats_HJ19<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Jul" & Species == "Hermit")
QStats_HS19<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Sep" & Species == "Hermit")

#Mussels----------------
QStats_MM19<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Mar" & Species == "Mussel")
QStats_MJ19<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Jul" & Species == "Mussel")
QStats_MS19<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Sep" & Species == "Mussel")

#Littorines------------------
QStats_LM19<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Mar" & Species == "Littorine")
QStats_LJ19<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Jul" & Species == "Littorine")
QStats_LS19<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Sep" & Species == "Littorine")

#Herms---------------
#Mar 19
stat.desc(QStats_HM19$Q10) #mean=1.404 sd=0.24
#Jul 19
stat.desc(QStats_HJ19$Q10) #mean=1.519 sd=0.22
#Sep 19
stat.desc(QStats_HS19$Q10) #mean=1.667 sd=0.40

#Mussels-----------------
#Mar 19
stat.desc(QStats_MM19$Q10) #mean=1.314 sd=0.29
#Jul 19
stat.desc(QStats_MJ19$Q10) #mean=1.263 sd=0.34
#Sep 19
stat.desc(QStats_MS19$Q10) #mean=1.219 sd=0.33

#Littorines-----------------
#Mar 19
stat.desc(QStats_LM19$Q10) #mean=1.254 sd=0.22
#Jul 19
stat.desc(QStats_LJ19$Q10) #mean=1.378 sd=0.39
#Sep 19
stat.desc(QStats_LS19$Q10) #mean=1.721 sd=0.58




#Wet Weightand Dry Weight Average and SD-------------------------------------------
O2_Meta<-read.csv("RangelandSorte_MO2_All.csv", na.strings = "nd", header=T)
#-------------------------------------------------------------

#Hermits-------------
MOStats_HM19<-filter(O2_Meta, Timepoint == "19-Mar" & Species == "Hermit" & Temp== "10")
MOStats_HJ19<-filter(O2_Meta, Timepoint == "19-Jul" & Species == "Hermit" & Temp== "10")
MOStats_HS19<-filter(O2_Meta, Timepoint == "19-Sep" & Species == "Hermit" & Temp== "10")

#Mussels----------------
MOStats_MM19<-filter(O2_Meta, Timepoint == "19-Mar" & Species == "Mussel" & Temp== "10")
MOStats_MJ19<-filter(O2_Meta, Timepoint == "19-Jul" & Species == "Mussel" & Temp== "10")
MOStats_MS19<-filter(O2_Meta, Timepoint == "19-Sep" & Species == "Mussel" & Temp== "10")

#Littorines------------------
MOStats_LM19<-filter(O2_Meta, Timepoint == "19-Mar" & Species == "Littorine" & Temp== "10")
MOStats_LJ19<-filter(O2_Meta, Timepoint == "19-Jul" & Species == "Littorine" & Temp== "10")
MOStats_LS19<-filter(O2_Meta, Timepoint == "19-Sep" & Species == "Littorine" & Temp== "10")

#Herms---------------
#Mar 19
stat.desc(MOStats_HM19$Wet_wt) #mean=0.268 sd=0.15
#Jul 19
stat.desc(MOStats_HJ19$Wet_wt) #mean=0.503 sd=0.34
#Sep 19
stat.desc(MOStats_HS19$Wet_wt) #mean=0.541 sd=0.40

#Mussels-----------------
#Mar 19
stat.desc(MOStats_MM19$Wet_wt) #mean=1.209 sd=0.24
#Jul 19
stat.desc(MOStats_MJ19$Wet_wt) #mean=1.481 sd=0.30
#Sep 19
stat.desc(MOStats_MS19$Wet_wt) #mean=1.292 sd=0.29

#Littorines-----------------
#Mar 19
stat.desc(MOStats_LM19$Wet_wt) #mean=0.579 sd=0.17
#Jul 19
stat.desc(MOStats_LJ19$Wet_wt) #mean=0.477 sd=0.15
#Sep 19
stat.desc(MOStats_LS19$Wet_wt) #mean=0.527 sd=0.18

#####DRY WEIGHT####-----------------------------------
#Herms---------------
#Mar 19
stat.desc(MOStats_HM19$Dry_wt) #mean=0.083 sd=0.03
#Jul 19
stat.desc(MOStats_HJ19$Dry_wt) #mean=0.05 sd=0.05
#Sep 19
stat.desc(MOStats_HS19$Dry_wt) #mean=0.10 sd=0.05

#Mussels-----------------
#Mar 19
stat.desc(MOStats_MM19$Dry_wt) #mean=0.04 sd=0.01
#Jul 19
stat.desc(MOStats_MJ19$Dry_wt) #mean=0.04 sd=0.01
#Sep 19
stat.desc(MOStats_MS19$Dry_wt) #mean=0.05 sd=0.02

#Littorines-----------------
#Mar 19
stat.desc(MOStats_LM19$Dry_wt) #mean=0.04 sd=0.01
#Jul 19
stat.desc(MOStats_LJ19$Dry_wt) #mean=0.03 sd=0.01
#Sep 19
stat.desc(MOStats_LS19$Dry_wt) #mean=0.04 sd=0.02




#####MO2####-----------------------------------
#10C
#Herms---------------
#Mar 19
stat.desc(MOStats_HM19$MO2) #mean=0.97 sd=0.23
#Jul 19
stat.desc(MOStats_HJ19$MO2) #mean=0.91 sd=0.23
#Sep 19
stat.desc(MOStats_HS19$MO2) #mean=0.75 sd=0.26

#Mussels-----------------
#Mar 19
stat.desc(MOStats_MM19$MO2) #mean=0.52 sd=0.28
#Jul 19
stat.desc(MOStats_MJ19$MO2) #mean=1.11 sd=0.22
#Sep 19
stat.desc(MOStats_MS19$MO2) #mean=0.95 sd=0.18

#Littorines-----------------
#Mar 19
stat.desc(MOStats_LM19$MO2) #mean=0.78 sd=0.42
#Jul 19
stat.desc(MOStats_LJ19$MO2) #mean=0.85 sd=0.43
#Sep 19
stat.desc(MOStats_LS19$MO2) #mean=0.56 sd=0.43

#Hermits-------------
MOStats_HM19<-filter(O2_Meta, Timepoint == "19-Mar" & Species == "Hermit" & Temp== "18")
MOStats_HJ19<-filter(O2_Meta, Timepoint == "19-Jul" & Species == "Hermit" & Temp== "18")
MOStats_HS19<-filter(O2_Meta, Timepoint == "19-Sep" & Species == "Hermit" & Temp== "18")

#Mussels----------------
MOStats_MM19<-filter(O2_Meta, Timepoint == "19-Mar" & Species == "Mussel" & Temp== "18")
MOStats_MJ19<-filter(O2_Meta, Timepoint == "19-Jul" & Species == "Mussel" & Temp== "18")
MOStats_MS19<-filter(O2_Meta, Timepoint == "19-Sep" & Species == "Mussel" & Temp== "18")

#Littorines------------------
MOStats_LM19<-filter(O2_Meta, Timepoint == "19-Mar" & Species == "Littorine" & Temp== "18")
MOStats_LJ19<-filter(O2_Meta, Timepoint == "19-Jul" & Species == "Littorine" & Temp== "18")
MOStats_LS19<-filter(O2_Meta, Timepoint == "19-Sep" & Species == "Littorine" & Temp== "18")

#18C
#Herms---------------
#Mar 19
stat.desc(MOStats_HM19$MO2) #mean=1.22 sd=0.38
#Jul 19
stat.desc(MOStats_HJ19$MO2) #mean=1.39 sd=0.59
#Sep 19
stat.desc(MOStats_HS19$MO2) #mean=0.98 sd=0.34

#Mussels-----------------
#Mar 19
stat.desc(MOStats_MM19$MO2) #mean=1.23 sd=0.59
#Jul 19
stat.desc(MOStats_MJ19$MO2) #mean=1.83 sd=0.44
#Sep 19
stat.desc(MOStats_MS19$MO2) #mean=1.65 sd=0.36

#Littorines-----------------
#Mar 19
stat.desc(MOStats_LM19$MO2) #mean=1.28 sd=0.66
#Jul 19
stat.desc(MOStats_LJ19$MO2) #mean=1.33 sd=0.60
#Sep 19
stat.desc(MOStats_LS19$MO2) #mean=1.03 sd=0.59


#26C
#Hermits-------------
MOStats_HM19<-filter(O2_Meta, Timepoint == "19-Mar" & Species == "Hermit" & Temp== "26")
MOStats_HJ19<-filter(O2_Meta, Timepoint == "19-Jul" & Species == "Hermit" & Temp== "26")
MOStats_HS19<-filter(O2_Meta, Timepoint == "19-Sep" & Species == "Hermit" & Temp== "26")

#Mussels----------------
MOStats_MM19<-filter(O2_Meta, Timepoint == "19-Mar" & Species == "Mussel" & Temp== "26")
MOStats_MJ19<-filter(O2_Meta, Timepoint == "19-Jul" & Species == "Mussel" & Temp== "26")
MOStats_MS19<-filter(O2_Meta, Timepoint == "19-Sep" & Species == "Mussel" & Temp== "26")

#Littorines------------------
MOStats_LM19<-filter(O2_Meta, Timepoint == "19-Mar" & Species == "Littorine" & Temp== "26")
MOStats_LJ19<-filter(O2_Meta, Timepoint == "19-Jul" & Species == "Littorine" & Temp== "26")
MOStats_LS19<-filter(O2_Meta, Timepoint == "19-Sep" & Species == "Littorine" & Temp== "26")

#26C
#Herms---------------
#Mar 19
stat.desc(MOStats_HM19$MO2) #mean=1.57 sd=0.36
#Jul 19
stat.desc(MOStats_HJ19$MO2) #mean=1.79 sd=0.55
#Sep 19
stat.desc(MOStats_HS19$MO2) #mean=1.46 sd=0.46

#Mussels-----------------
#Mar 19
stat.desc(MOStats_MM19$MO2) #mean=0.98 sd=0.49
#Jul 19
stat.desc(MOStats_MJ19$MO2) #mean=1.69 sd=0.82
#Sep 19
stat.desc(MOStats_MS19$MO2) #mean=1.41 sd=0.79

#Littorines-----------------
#Mar 19
stat.desc(MOStats_LM19$MO2) #mean=1.22 sd=0.58
#Jul 19
stat.desc(MOStats_LJ19$MO2) #mean=1.53 sd=0.55
#Sep 19
stat.desc(MOStats_LS19$MO2) #mean=1.34 sd=1.03

#PCA-------------------------------
####################################################  
#   Script to run a PCA                            #
#  Script adapted from P. Wallingford              #
####################################################

library(lattice)
library(ggplot2)
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(ggthemes)


YearData <- read.csv("RangelandSorte_ThermalHistory_All_Updated.csv")

#Make the PCA-----------------------

PCA <- prcomp(YearData[,c(6:21)], scale = TRUE, center = TRUE)

# Proportion of data explained by each axis
summary(PCA)

#Making a PDF of the PCA---------------------
#pdf("TempPCA.pdf",height=8,width=9)
#dev.off()

# plot
fviz_pca_biplot(PCA, xlab = "PC1 (73.7%)", ylab = "PC2 (21.3%)", repel = TRUE, col.var = "black", col.ind = "gray", title = " ", label = "var") + theme_base()

# Pull out the individual point scores 
PCAFrame<-data.frame(YearData$ID,YearData$Timepoint,YearData$TempDate,PCA$x[,1:2])

# We want to make sure the columns/ID line up correctly and then merge our data into one dataset
colnames(PCAFrame)[1:3]<-c('ID', "Timepoint","TempDate")

AllData <- merge(YearData, PCAFrame, by = c("ID","Timepoint","TempDate"), all.x = TRUE)

#Getting AllData csv - yay!
write.csv(AllData, file = "RangelandSorte_ThermalHistory_All_Updated.csv")


#Indv Variability Analysis-------------------------------
####################################################  
#   Adapted from Rangel and Johnson (2019) JEMBE   #
#                                                  #
####################################################
library(specr)


O2_Meta<-read.csv("RangelandSorte_MO2_All.csv", na.strings = "nd", header=T)


#Individual variability plots - Hermit Crabs
#March 2019------------------------------------
T10<-(O2_Meta[O2_Meta$Species=="Hermit"& O2_Meta$Timepoint=="19-Mar",]$Temp)/(10)
mherm.m<-lmer(log(MO2)~T10+(1|ID),data=O2_Meta[O2_Meta$Species=="Hermit"& O2_Meta$Timepoint=="19-Mar",])
summary(mherm.m)

icc_specs(mherm.m) %>%
  mutate_if(is.numeric, round, 2) #individual variation 60.1%


pdf("Herm_March_Indv.pdf",height=5,width=6)
par(mgp = c(2.5, 1, 0))
par(mar=c(4.5,5,3,0.5)) #
plot(MO2~Temp, data=O2_Meta[O2_Meta$Species=="Hermit" & O2_Meta$Timepoint=="19-Mar",], xlab="", xaxt="n", cex.axis=2.10, cex.lab=2.15,ylim=c(0, 4.0),ylab=expression("MO"[2]~(mg~O[2]~g~h^{-1})),pch=c(16), cex=2, col=adjustcolor("#FDE725FF", alpha=0.6))
axis(side=1, at=seq(10, 26, by=8), cex.axis=2.10)
text(12.5,3.9,labels = "March 2019", cex=1.9,font=4)
text(12.2,3.5,labels = "ID=60.1%", cex=1.3,font=4)

nherm<-dim(coef(mherm.m)$ID)[1]
for (i in 1:nherm){
  a<-coef(mherm.m)$ID[i,1]
  b<-exp(coef(mherm.m)$ID[i,2])
  r2<-function(x){exp(a)*b^((x-0)/10)}
  curve(r2,10,26,add=TRUE)
}

dev.off()

#July 2019------------------------------------
T10<-(O2_Meta[O2_Meta$Species=="Hermit"& O2_Meta$Timepoint=="19-Jul",]$Temp)/(10)
mherm.j<-lmer(log(MO2)~T10+(1|ID),data=O2_Meta[O2_Meta$Species=="Hermit"& O2_Meta$Timepoint=="19-Jul",])
summary(mherm.j)

icc_specs(mherm.j) %>%
  mutate_if(is.numeric, round, 2) #Individual variation is going to be 66.4%


pdf("Herm_July_Indv.pdf",height=5,width=6)
par(mgp = c(2.5, 1, 0))
par(mar=c(4.5,5,3,0.5)) #
plot(MO2~Temp, data=O2_Meta[O2_Meta$Species=="Hermit" & O2_Meta$Timepoint=="19-Jul",], xlab="", xaxt="n", cex.axis=2.10, cex.lab=2.15,ylim=c(0, 4.0),ylab=expression("MO"[2]~(mg~O[2]~g~h^{-1})),pch=c(16), cex=2, col=adjustcolor("#FDE725FF", alpha=0.6))
axis(side=1, at=seq(10, 26, by=8), cex.axis=2.10)
text(12,3.9,labels = "July 2019", cex=1.9,font=4)
text(11.8,3.5,labels = "ID=66.4%", cex=1.3,font=4)

nherm<-dim(coef(mherm.j)$ID)[1]
for (i in 1:nherm){
  a<-coef(mherm.j)$ID[i,1]
  b<-exp(coef(mherm.j)$ID[i,2])
  r2<-function(x){exp(a)*b^((x-0)/10)}
  curve(r2,10,26,add=TRUE)
}

dev.off()


#September 2019------------------------------------
T10<-(O2_Meta[O2_Meta$Species=="Hermit"& O2_Meta$Timepoint=="19-Sep",]$Temp)/(10)
mherm.s<-lmer(log(MO2)~T10+(1|ID),data=O2_Meta[O2_Meta$Species=="Hermit"& O2_Meta$Timepoint=="19-Sep",])
summary(mherm.s)

icc_specs(mherm.s) %>%
  mutate_if(is.numeric, round, 2) #Individual variation: 41.2%

pdf("Herm_Sept_Indv.pdf",height=5,width=6)
par(mgp = c(2.5, 1, 0))
par(mar=c(4.5,5,3,0.5)) #
plot(MO2~Temp, data=O2_Meta[O2_Meta$Species=="Hermit" & O2_Meta$Timepoint=="19-Sep",],xlab="", xaxt="n", cex.axis=2.10, cex.lab=2.15,ylim=c(0, 4.0),ylab=expression("MO"[2]~(mg~O[2]~g~h^{-1})),pch=c(16), cex=2, col=adjustcolor("#FDE725FF", alpha=0.6))
axis(side=1, at=seq(10, 26, by=8), cex.axis=2.10)
text(13.7,3.9,labels = "September 2019", cex=1.9,font=4)
text(13.4,3.5,labels = "ID=41.2%", cex=1.3,font=4)

nherm<-dim(coef(mherm.s)$ID)[1]
for (i in 1:nherm){
  a<-coef(mherm.s)$ID[i,1]
  b<-exp(coef(mherm.s)$ID[i,2])
  r2<-function(x){exp(a)*b^((x-0)/10)}
  curve(r2,10,26,add=TRUE)
}

dev.off()


#Individual variability plots - Littorines
#March 2019------------------------------------
T10<-(O2_Meta[O2_Meta$Species=="Littorine"& O2_Meta$Timepoint=="19-Mar",]$Temp)/(10)
mlitt.m<-lmer(log(MO2)~T10+(1|ID),data=O2_Meta[O2_Meta$Species=="Littorine"& O2_Meta$Timepoint=="19-Mar",])
summary(mlitt.m)

icc_specs(mlitt.m) %>%
  mutate_if(is.numeric, round, 2) #indiv var: 61.2%

pdf("Litt_March_Indv.pdf",height=5,width=6)
par(mgp = c(2.5, 1, 0))
par(mar=c(4.5,5,3,0.5)) #"#21908CFF"
plot(MO2~Temp, data=O2_Meta[O2_Meta$Species=="Littorine" & O2_Meta$Timepoint=="19-Mar",], xlab="", xaxt="n", cex.axis=2.10, cex.lab=2.15,ylim=c(0, 4.0),ylab=expression("MO"[2]~(mg~O[2]~g~h^{-1})),pch=c(17), cex=2, col=adjustcolor("#21908CFF", alpha=0.6))
axis(side=1, at=seq(10, 26, by=8), cex.axis=2.10)
text(12.5,3.9,labels = "March 2019", cex=1.9,font=4)
text(12.2,3.5,labels = "ID=61.2%", cex=1.3,font=4)

nlitt<-dim(coef(mlitt.m)$ID)[1]
for (i in 1:nlitt){
  a<-coef(mlitt.m)$ID[i,1]
  b<-exp(coef(mlitt.m)$ID[i,2])
  r2<-function(x){exp(a)*b^((x-0)/10)}
  curve(r2,10,26,add=TRUE)
}

dev.off()

#July 2019------------------------------------
T10<-(O2_Meta[O2_Meta$Species=="Littorine"& O2_Meta$Timepoint=="19-Jul",]$Temp)/(10)
mlitt.j<-lmer(log(MO2)~T10+(1|ID),data=O2_Meta[O2_Meta$Species=="Littorine"& O2_Meta$Timepoint=="19-Jul",])
summary(mlitt.j)

icc_specs(mlitt.j) %>%
  mutate_if(is.numeric, round, 2) #indv var: 47.3%

pdf("Litt_July_Indv.pdf",height=5,width=6)
par(mgp = c(2.5, 1, 0))
par(mar=c(4.5,5,3,0.5)) #
plot(MO2~Temp, data=O2_Meta[O2_Meta$Species=="Littorine" & O2_Meta$Timepoint=="19-Jul",],  xlab="", xaxt="n", cex.axis=2.10, cex.lab=2.15,ylim=c(0, 4.0),ylab=expression("MO"[2]~(mg~O[2]~g~h^{-1})),pch=c(17), cex=2, col=adjustcolor("#21908CFF", alpha=0.6))
axis(side=1, at=seq(10, 26, by=8), cex.axis=2.10)
text(12,3.9,labels = "July 2019", cex=1.9,font=4)
text(11.8,3.5,labels = "ID=47.3%", cex=1.3,font=4)

nlitt<-dim(coef(mlitt.j)$ID)[1]
for (i in 1:nlitt){
  a<-coef(mlitt.j)$ID[i,1]
  b<-exp(coef(mlitt.j)$ID[i,2])
  r2<-function(x){exp(a)*b^((x-0)/10)}
  curve(r2,10,26,add=TRUE)
}
dev.off()


#September 2019------------------------------------
T10<-(O2_Meta[O2_Meta$Species=="Littorine"& O2_Meta$Timepoint=="19-Sep",]$Temp)/(10)
mlitt.s<-lmer(log(MO2)~T10+(1|ID),data=O2_Meta[O2_Meta$Species=="Littorine"& O2_Meta$Timepoint=="19-Sep",])
summary(mlitt.s)

icc_specs(mlitt.s) %>%
  mutate_if(is.numeric, round, 2) #59.2%

pdf("Litt_Sept_Indv.pdf",height=5,width=6)
par(mgp = c(2.5, 1, 0))
par(mar=c(4.5,5,3,0.5)) #
plot(MO2~Temp, data=O2_Meta[O2_Meta$Species=="Littorine" & O2_Meta$Timepoint=="19-Sep",], xlab="", xaxt="n", cex.axis=2.10, cex.lab=2.15,ylim=c(0, 4.0),ylab=expression("MO"[2]~(mg~O[2]~g~h^{-1})),pch=c(17), cex=2, col=adjustcolor("#21908CFF", alpha=0.6))
axis(side=1, at=seq(10, 26, by=8), cex.axis=2.10)
text(13.6,3.9,labels = "September 2019", cex=1.9,font=4)
text(13.4,3.5,labels = "ID=59.2%", cex=1.3,font=4)

nlitt<-dim(coef(mlitt.s)$ID)[1]
for (i in 1:nlitt){
  a<-coef(mlitt.s)$ID[i,1]
  b<-exp(coef(mlitt.s)$ID[i,2])
  r2<-function(x){exp(a)*b^((x-0)/10)}
  curve(r2,10,26,add=TRUE)
}
dev.off()



#Individual variability plots - Mussels
#March 2019------------------------------------
T10<-(O2_Meta[O2_Meta$Species=="Mussel"& O2_Meta$Timepoint=="19-Mar",]$Temp)/(10)
mmuss.m<-lmer(log(MO2)~T10+(1|ID),data=O2_Meta[O2_Meta$Species=="Mussel"& O2_Meta$Timepoint=="19-Mar",])
summary(mmuss.m)

icc_specs(mmuss.m) %>%
  mutate_if(is.numeric, round, 2) #23.1%

pdf("Muss_March_Indv.pdf",height=5,width=6)
par(mgp = c(2.5, 1, 0))
par(mar=c(4.5,5,3,0.5)) ##440154FF
plot(MO2~Temp, data=O2_Meta[O2_Meta$Species=="Mussel" & O2_Meta$Timepoint=="19-Mar",], xlab="", xaxt="n", cex.axis=2.10, cex.lab=2.15,ylim=c(0, 4.0),ylab=expression("MO"[2]~(mg~O[2]~g~h^{-1})),pch=c(15), cex=2, col=adjustcolor("#440154FF", alpha=0.6))
axis(side=1, at=seq(10, 26, by=8), cex.axis=2.10)
text(12.5,3.9,labels = "March 2019", cex=1.9,font=4)
text(12.2,3.5,labels = "ID=23.1%", cex=1.3,font=4)

nmuss<-dim(coef(mmuss.m)$ID)[1]
for (i in 1:nmuss){
  a<-coef(mmuss.m)$ID[i,1]
  b<-exp(coef(mmuss.m)$ID[i,2])
  r2<-function(x){exp(a)*b^((x-0)/10)}
  curve(r2,10,26,add=TRUE)
}

dev.off()


#July 2019------------------------------------
T10<-(O2_Meta[O2_Meta$Species=="Mussel"& O2_Meta$Timepoint=="19-Jul",]$Temp)/(10)
mmuss.j<-lmer(log(MO2)~T10+(1|ID),data=O2_Meta[O2_Meta$Species=="Mussel"& O2_Meta$Timepoint=="19-Jul",])
summary(mmuss.j)

icc_specs(mmuss.j) %>%
  mutate_if(is.numeric, round, 2) #indv var: 6.03%


pdf("Muss_July_Indv.pdf",height=5,width=6)
par(mgp = c(2.5, 1, 0))
par(mar=c(4.5,5,3,0.5)) #
plot(MO2~Temp, data=O2_Meta[O2_Meta$Species=="Mussel" & O2_Meta$Timepoint=="19-Jul",], xlab="Temperature (°C)", xaxt="n", cex.axis=2.10, cex.lab=2.15,ylim=c(0, 4.0),ylab=expression("MO"[2]~(mg~O[2]~g~h^{-1})),pch=c(15), cex=2, col=adjustcolor("#440154FF", alpha=0.6))
axis(side=1, at=seq(10, 26, by=8), cex.axis=2.10)
text(12,3.9,labels = "July 2019", cex=1.9,font=4)
text(11.8,3.5,labels = "ID=6.0%", cex=1.3,font=4)

nmuss<-dim(coef(mmuss.j)$ID)[1]
for (i in 1:nmuss){
  a<-coef(mmuss.j)$ID[i,1]
  b<-exp(coef(mmuss.j)$ID[i,2])
  r2<-function(x){exp(a)*b^((x-0)/10)}
  curve(r2,10,26,add=TRUE)
}

dev.off()



#September 2019------------------------------------
T10<-(O2_Meta[O2_Meta$Species=="Mussel"& O2_Meta$Timepoint=="19-Sep",]$Temp)/(10)
mmuss.s<-lmer(log(MO2)~T10+(1|ID),data=O2_Meta[O2_Meta$Species=="Mussel"& O2_Meta$Timepoint=="19-Sep",])
summary(mmuss.s)

0.02535+0.31486
0.02535/0.34021 #7.45%

icc_specs(mmuss.s) %>%
  mutate_if(is.numeric, round, 2) #same same #7.45%


pdf("Muss_Sept_Indv.pdf",height=5,width=6)
par(mgp = c(2.5, 1, 0))
par(mar=c(4.5,5,3,0.5)) 
plot(MO2~Temp, data=O2_Meta[O2_Meta$Species=="Mussel" & O2_Meta$Timepoint=="19-Sep",], xlab="", xaxt="n", cex.axis=2.10, cex.lab=2.15,ylim=c(0, 4.0),ylab=expression("MO"[2]~(mg~O[2]~g~h^{-1})),pch=c(15), cex=2, col=adjustcolor("#440154FF", alpha=0.6))
axis(side=1, at=seq(10, 26, by=8), cex.axis=2.10)
text(13.6,3.9,labels = "September 2019", cex=1.9,font=4)
text(13.4,3.5,labels = "ID=7.5%", cex=1.3,font=4)

nmuss<-dim(coef(mmuss.s)$ID)[1]
for (i in 1:nmuss){
  a<-coef(mmuss.s)$ID[i,1]
  b<-exp(coef(mmuss.s)$ID[i,2])
  r2<-function(x){exp(a)*b^((x-0)/10)}
  curve(r2,10,26,add=TRUE)
}

dev.off()


