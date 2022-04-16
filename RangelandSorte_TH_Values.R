################################################################
#Title: Thermal History (TH) Analysis With Real Tide Pool Temp Values
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

#---------DAILY MAX------------------------------------------------------------------

#-------------------------------------------------------
#------------------------
#3MONTH ONLY------------------------------------------------------
#---------------------------------
ThreeMo<-filter(TH_Meta, TempDate == "3Month")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#3 MONTHS Daily Max---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M3.RV<-lmer(Q10~dailymax+Timepoint+Species+Timepoint*dailymax+Species*dailymax+Timepoint*Species+(1|Pool), data=ThreeMo)
summary(M3.RV)
anova(M3.RV) 
qqnorm(resid(M3.RV))
qqline(resid(M3.RV))
hist(resid(M3.RV))
ad.test(resid(M3.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------
M3.RV.2<-lmer(Q10~dailymax+Timepoint+Species+Timepoint*dailymax+Species*dailymax+(1|Pool), data=ThreeMo)
summary(M3.RV.2)
anova(M3.RV.2)
#Check to see if you can drop term
anova(M3.RV, M3.RV.2) # Yes, can drop
qqnorm(resid(M3.RV.2))
qqline(resid(M3.RV.2))
hist(resid(M3.RV.2))
ad.test(resid(M3.RV.2)) #Checking normality

AIC(M3.RV, M3.RV.2)

#df   AIC
#M3.RV   16 139.4217
#M3.RV.2 12 133.1245


#DROPPING TERMS ------------------------------------------------SIMPLEST MODEL**************
M3.RV.3<-lmer(Q10~dailymax+Timepoint+Species+Species*dailymax+(1|Pool), data=ThreeMo) 
summary(M3.RV.3)
anova(M3.RV.3)
#Check to see if you can drop term
anova(M3.RV.2, M3.RV.3) # Yes, can drop
qqnorm(resid(M3.RV.3))
qqline(resid(M3.RV.3))
hist(resid(M3.RV.3))
ad.test(resid(M3.RV.3)) #Checking normality

AIC(M3.RV.2, M3.RV.3)

#df   AIC
# M3.RV.2 12 133.1245
#M3.RV.3 10 123.5571

#DROPPING TERMS ------------------------------------------------
M3.RV.4<-lmer(Q10~dailymax+Timepoint+Species+(1|Pool), data=ThreeMo)
summary(M3.RV.4)
anova(M3.RV.4)
#Check to see if you can drop term
anova(M3.RV.3, M3.RV.4) # No, cannot drop
qqnorm(resid(M3.RV.4))
qqline(resid(M3.RV.4))
hist(resid(M3.RV.4))
ad.test(resid(M3.RV.4)) #Checking normality

#-------------------------------------------------------
#------------------------
#2MONTH ONLY------------------------------------------------------
#---------------------------------
TwoMo<-filter(TH_Meta, TempDate == "2Month")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#2 MONTHS---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M2.RV<-lmer(Q10~dailymax+Timepoint+Species+Timepoint*dailymax+Species*dailymax+Timepoint*Species+(1|Pool), data=TwoMo)
summary(M2.RV)
anova(M2.RV) 
qqnorm(resid(M2.RV))
qqline(resid(M2.RV))
hist(resid(M2.RV))
ad.test(resid(M2.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------
M2.RV.2<-lmer(Q10~dailymax+Timepoint+Species+Timepoint*dailymax+Species*dailymax+(1|Pool), data=TwoMo) 
summary(M2.RV.2)
anova(M2.RV.2)
#Check to see if you can drop term
anova(M2.RV, M2.RV.2) # Yes, can drop
qqnorm(resid(M2.RV.2))
qqline(resid(M2.RV.2))
hist(resid(M2.RV.2))
ad.test(resid(M2.RV.2)) #Checking normality

AIC(M2.RV, M2.RV.2)

#df   AIC
#M2.RV   16 140.6011
#M2.RV.2 12 135.4057


#DROPPING TERMS ------------------------------------------------SIMPLEST MODEL
M2.RV.3<-lmer(Q10~dailymax+Timepoint+Species+Species*dailymax+(1|Pool), data=TwoMo)
summary(M2.RV.3)
anova(M2.RV.3)
#Check to see if you can drop term
anova(M2.RV.2, M2.RV.3) # Yes, can drop
qqnorm(resid(M2.RV.3))
qqline(resid(M2.RV.3))
hist(resid(M2.RV.3))
ad.test(resid(M2.RV.3)) #Checking normality

AIC(M2.RV.2, M2.RV.3)

#df   AIC
#M2.RV.2 12 135.4057
#M2.RV.3 10 124.9988

#DROPPING TERMS ------------------------------------------------
M2.RV.4<-lmer(Q10~dailymax+Timepoint+Species+(1|Pool), data=TwoMo) 
summary(M2.RV.4)
anova(M2.RV.4)
#Check to see if you can drop term
anova(M2.RV.3, M2.RV.4) # No, cannot drop
qqnorm(resid(M2.RV.4))
qqline(resid(M2.RV.4))
hist(resid(M2.RV.4))
ad.test(resid(M2.RV.4)) #Checking normality


#-------------------------------------------------------
#------------------------
#1MONTH ONLY------------------------------------------------------
#---------------------------------
OneMo<-filter(TH_Meta, TempDate == "1Month")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#1 MONTHS---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M1.RV<-lmer(Q10~dailymax+Timepoint+Species+Timepoint*dailymax+Species*dailymax+(1|Pool), data=OneMo)
summary(M1.RV)
anova(M1.RV) 
qqnorm(resid(M1.RV))
qqline(resid(M1.RV))
hist(resid(M1.RV))
ad.test(resid(M1.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------
M1.RV.2<-lmer(Q10~dailymax+Timepoint+Species+Timepoint*dailymax+(1|Pool), data=OneMo) 
summary(M1.RV.2)
anova(M1.RV.2)
#Check to see if you can drop term
anova(M1.RV, M1.RV.2) # Yes, can drop
qqnorm(resid(M1.RV.2))
qqline(resid(M1.RV.2))
hist(resid(M1.RV.2))
ad.test(resid(M1.RV.2)) #Checking normality

AIC(M1.RV, M1.RV.2)

#df   AIC
#M1.RV   12 135.7675
#M1.RV.2 10 121.5423


#DROPPING TERMS ------------------------------------------------
M1.RV.3<-lmer(Q10~dailymax+Timepoint+Species+(1|Pool), data=OneMo) 
summary(M1.RV.3)
anova(M1.RV.3)
#Check to see if you can drop term
anova(M1.RV.2, M1.RV.3) # Yes, can drop
qqnorm(resid(M1.RV.3))
qqline(resid(M1.RV.3))
hist(resid(M1.RV.3))
ad.test(resid(M1.RV.3)) #Checking normality

AIC(M1.RV.2, M1.RV.3)

#df   AIC
#M1.RV.2 10 121.5423
#M1.RV.3  8 111.2050


M1.Q10.emm.n<-emmeans(M1.RV.3, "Species")
M1.Q10.emm.n
emmeans(M1.RV.3, pairwise~Species)

#contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine    0.103 0.0875 93.1 1.182   0.4669 
#Hermit - Mussel       0.256 0.0818 91.6 3.133   0.0065 
#Littorine - Mussel    0.153 0.0820 93.7 1.862   0.1555 



#-------------------------------------------------------
#------------------------
#1WEEK ONLY------------------------------------------------------
#---------------------------------
OneWk<-filter(TH_Meta, TempDate == "1Week")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#1 MONTHS---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M1WK.RV<-lmer(Q10~dailymax+Timepoint+Species+Timepoint*dailymax+Species*dailymax+(1|Pool), data=OneWk)
summary(M1WK.RV)
anova(M1WK.RV) 
qqnorm(resid(M1WK.RV))
qqline(resid(M1WK.RV))
hist(resid(M1WK.RV))
ad.test(resid(M1WK.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------
M1WK.RV.2<-lmer(Q10~dailymax+Timepoint+Species+Timepoint*dailymax+(1|Pool), data=OneWk) 
summary(M1WK.RV.2)
anova(M1WK.RV.2)
#Check to see if you can drop term
anova(M1WK.RV, M1WK.RV.2) # Yes, can drop
qqnorm(resid(M1WK.RV.2))
qqline(resid(M1WK.RV.2))
hist(resid(M1WK.RV.2))
ad.test(resid(M1WK.RV.2)) #Checking normality

AIC(M1WK.RV, M1WK.RV.2)

#df   AIC
#M1WK.RV   12 137.0267
#M1WK.RV.2 10 121.9377


#DROPPING TERMS ------------------------------------------------SIMPLEST MODEL
M1WK.RV.3<-lmer(Q10~dailymax+Timepoint+Species+(1|Pool), data=OneWk) 
summary(M1WK.RV.3)
anova(M1WK.RV.3)
#Check to see if you can drop term
anova(M1WK.RV.2, M1WK.RV.3) # Yes, can drop
qqnorm(resid(M1WK.RV.3))
qqline(resid(M1WK.RV.3))
hist(resid(M1WK.RV.3))
ad.test(resid(M1WK.RV.3)) #Checking normality

AIC(M1WK.RV.2, M1WK.RV.3)

#df   AIC
#M1WK.RV.2 10 121.9377
#M1WK.RV.3  8 112.3737


M1WK.Q10.emm.n<-emmeans(M1WK.RV.3, "Species")
M1WK.Q10.emm.n
emmeans(M1WK.RV.3, pairwise~Species)

#contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine    0.093 0.0870 90.8 1.070   0.5352 
#Hermit - Mussel       0.259 0.0814 90.7 3.183   0.0056 
#Littorine - Mussel    0.166 0.0813 91.0 2.043   0.1079 

#-------------------------------------------------------
#------------------------
#1DAY ONLY------------------------------------------------------
#---------------------------------
OneDay<-filter(TH_Meta, TempDate == "1Day")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#1 DAY--------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M1DY.RV<-lmer(Q10~dailymax+Timepoint+Species+Timepoint*dailymax+Species*dailymax+(1|Pool), data=OneDay)
summary(M1DY.RV)
anova(M1DY.RV) 
qqnorm(resid(M1DY.RV))
qqline(resid(M1DY.RV))
hist(resid(M1DY.RV))
ad.test(resid(M1DY.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------SIMPLEST MODEL
M1DY.RV.2<-lmer(Q10~dailymax+Timepoint+Species+Timepoint*dailymax+(1|Pool), data=OneDay) 
summary(M1DY.RV.2)
anova(M1DY.RV.2)
#Check to see if you can drop term
anova(M1DY.RV, M1DY.RV.2) # Yes, can drop
qqnorm(resid(M1DY.RV.2))
qqline(resid(M1DY.RV.2))
hist(resid(M1DY.RV.2))
ad.test(resid(M1DY.RV.2)) #Checking normality

AIC(M1DY.RV, M1DY.RV.2)

#df   AIC
#M1DY.RV   12 133.7402
#M1DY.RV.2 10 118.4012



#Post hoc - emmeans--------------------------------------------
#Interested in overall effect of Season
Spp.1day<- emmeans(M1DY.RV.2, ~Timepoint*dailymax)
pairs(Spp.1day, by="dailymax")

#contrast            estimate    SE   df t.ratio p.value
#(19-Jul) - (19-Mar)  -0.0464 0.188 87.5 -0.247  0.9669 
#(19-Jul) - (19-Sep)  -0.1986 0.131 84.2 -1.520  0.2869 
#(19-Mar) - (19-Sep)  -0.1521 0.181 84.8 -0.842  0.6779 


M1DY.Q10.emm.n<-emmeans(M1DY.RV.2, "Species")
M1DY.Q10.emm.n
emmeans(M1DY.RV.2, pairwise~Species)

#contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine    0.156 0.0904 96.4 1.722   0.2024 
#Hermit - Mussel       0.256 0.0858 91.1 2.987   0.0100 
#Littorine - Mussel    0.100 0.0879 94.5 1.144   0.4897 

################################################################
#DAILY 95 TEMP ONLY
################################################################



#----------------------------------------------------------------------------
#THERMAL HISTORY ANALYSIS---------------------------------------------------
TH_Meta<-read.csv("RangelandSorte_ThermalHistory_All.csv", na.strings = "nd", header=T)
str(TH_Meta)
#-------------------------------------------------------
#------------------------
#3MONTH ONLY------------------------------------------------------
#---------------------------------
ThreeMo<-filter(TH_Meta, TempDate == "3Month")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#3 MONTHS---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M3.RV<-lmer(Q10~Range+Timepoint+Species+Timepoint*daily95+Species*daily95+Species*Timepoint+(1|Pool), data=ThreeMo)
summary(M3.RV)
anova(M3.RV) 
qqnorm(resid(M3.RV))
qqline(resid(M3.RV))
hist(resid(M3.RV))
ad.test(resid(M3.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------Removed Species*Timepoint
M3.RV.2<-lmer(Q10~daily95+Timepoint+Species+Timepoint*daily95+Species*daily95+(1|Pool), data=ThreeMo)
summary(M3.RV.2)
anova(M3.RV.2)
#Check to see if you can drop term
anova(M3.RV, M3.RV.2) # Yes, can drop
qqnorm(resid(M3.RV.2))
qqline(resid(M3.RV.2))
hist(resid(M3.RV.2))
ad.test(resid(M3.RV.2)) #Checking normality

AIC(M3.RV, M3.RV.2)

#df   AIC
#M3.RV   16 139.0296
#M3.RV.2 12 132.8001


#DROPPING TERMS ------------------------------------------------Removed Timepoint*daily95---SIMPLEST MODEL
M3.RV.3<-lmer(Q10~daily95+Timepoint+Species+Species*daily95+(1|Pool), data=ThreeMo)
summary(M3.RV.3)
anova(M3.RV.3)
#Check to see if you can drop term
anova(M3.RV.2, M3.RV.3) # Yes, can drop
qqnorm(resid(M3.RV.3))
qqline(resid(M3.RV.3))
hist(resid(M3.RV.3))
ad.test(resid(M3.RV.3)) #Checking normality

AIC(M3.RV.2, M3.RV.3)

#df   AIC
#M3.RV.2 12 132.8001
#M3.RV.3 10 123.4620

#DROPPING TERMS ------------------------------------------------Removed Species*dailymax95 
M3.RV.4<-lmer(Q10~daily95+Timepoint+Species+(1|Pool), data=ThreeMo) #Simplest model
summary(M3.RV.4)
anova(M3.RV.4)
#Check to see if you can drop term
anova(M3.RV.3, M3.RV.4) # NO, cannot drop
qqnorm(resid(M3.RV.4))
qqline(resid(M3.RV.4))
hist(resid(M3.RV.4))
ad.test(resid(M3.RV.4)) #Checking normality


#-------------------------------------------------------
#------------------------
#2MONTH ONLY------------------------------------------------------
#---------------------------------
TwoMo<-filter(TH_Meta, TempDate == "2Month")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#2 MONTHS---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M2.RV<-lmer(Q10~daily95+Timepoint+Species+Timepoint*daily95+Species*daily95+Species*Timepoint+(1|Pool), data=TwoMo)
summary(M2.RV)
anova(M2.RV) 
qqnorm(resid(M2.RV))
qqline(resid(M2.RV))
hist(resid(M2.RV))
ad.test(resid(M2.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------Removed Species*Timepoint
M2.RV.2<-lmer(Q10~daily95+Timepoint+Species+Timepoint*daily95+Species*daily95+(1|Pool), data=TwoMo) 
summary(M2.RV.2)
anova(M2.RV.2)
#Check to see if you can drop term
anova(M2.RV, M2.RV.2) # Yes, can drop
qqnorm(resid(M2.RV.2))
qqline(resid(M2.RV.2))
hist(resid(M2.RV.2))
ad.test(resid(M2.RV.2)) #Checking normality

AIC(M2.RV, M2.RV.2)

#df   AIC
#M2.RV   16 140.1660
#M2.RV.2 12 135.1132


#DROPPING TERMS ------------------------------------------------Removed Timepoint*daily95 ----  -- SIMPLEST MODEL--
M2.RV.3<-lmer(Q10~daily95+Timepoint+Species+Species*daily95+(1|Pool), data=TwoMo)
summary(M2.RV.3)
anova(M2.RV.3)
#Check to see if you can drop term
anova(M2.RV.2, M2.RV.3) # Yes, can drop
qqnorm(resid(M2.RV.3))
qqline(resid(M2.RV.3))
hist(resid(M2.RV.3))
ad.test(resid(M2.RV.3)) #Checking normality

AIC(M2.RV.2, M2.RV.3)

#df   AIC
#M2.RV.2 12 135.1132
#M2.RV.3 10 124.9252

#DROPPING TERMS ------------------------------------------------Removed Species*daily95
M2.RV.4<-lmer(Q10~daily95+Timepoint+Species+(1|Pool), data=TwoMo) 
summary(M2.RV.4)
anova(M2.RV.4)
#Check to see if you can drop term
anova(M2.RV.3, M2.RV.4) # NOPE, cannot drop
qqnorm(resid(M2.RV.4))
qqline(resid(M2.RV.4))
hist(resid(M2.RV.4))
ad.test(resid(M2.RV.4)) #Checking normality


#-------------------------------------------------------
#------------------------
#1MONTH ONLY------------------------------------------------------
#---------------------------------
OneMo<-filter(TH_Meta, TempDate == "1Month")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#1 MONTHS---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M1.RV<-lmer(Q10~daily95+Timepoint+Species+Timepoint*daily95+Species*daily95+(1|Pool), data=OneMo)
summary(M1.RV)
anova(M1.RV) 
qqnorm(resid(M1.RV))
qqline(resid(M1.RV))
hist(resid(M1.RV))
ad.test(resid(M1.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------
M1.RV.2<-lmer(Q10~daily95+Timepoint+Species+Timepoint*daily95+(1|Pool), data=OneMo) 
summary(M1.RV.2)
anova(M1.RV.2)
#Check to see if you can drop term
anova(M1.RV, M1.RV.2) # Yes, can drop
qqnorm(resid(M1.RV.2))
qqline(resid(M1.RV.2))
hist(resid(M1.RV.2))
ad.test(resid(M1.RV.2)) #Checking normality

AIC(M1.RV, M1.RV.2)

#df   AIC
#M1.RV   12 135.3689
#M1.RV.2 10 121.1813


#DROPPING TERMS ------------------------------------------------SIMPLEST MODEL
M1.RV.3<-lmer(Q10~daily95+Timepoint+Species+(1|Pool), data=OneMo) 
summary(M1.RV.3)
anova(M1.RV.3)
#Check to see if you can drop term
anova(M1.RV.2, M1.RV.3) # Yes, can drop
qqnorm(resid(M1.RV.3))
qqline(resid(M1.RV.3))
hist(resid(M1.RV.3))
ad.test(resid(M1.RV.3)) #Checking normality

AIC(M1.RV.2, M1.RV.3)

#df   AIC
#M1.RV.2 10 121.1813
#M1.RV.3  8 111.1456


M1DY.Q10.emm.n<-emmeans(M1.RV.3, "Species")
M1DY.Q10.emm.n
emmeans(M1.RV.3, pairwise~Species)

#contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine    0.104 0.0875 93.2 1.186   0.4646 
#Hermit - Mussel       0.256 0.0817 91.5 3.134   0.0065 
#Littorine - Mussel    0.152 0.0821 93.8 1.857   0.1571 


#-------------------------------------------------------
#------------------------
#1WEEK ONLY------------------------------------------------------
#---------------------------------
OneWk<-filter(TH_Meta, TempDate == "1Week")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#1 MONTHS---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M1WK.RV<-lmer(Q10~daily95+Timepoint+Species+Timepoint*daily95+Species*daily95+(1|Pool), data=OneWk)
summary(M1WK.RV)
anova(M1WK.RV) 
qqnorm(resid(M1WK.RV))
qqline(resid(M1WK.RV))
hist(resid(M1WK.RV))
ad.test(resid(M1WK.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------
M1WK.RV.2<-lmer(Q10~daily95+Timepoint+Species+Timepoint*daily95+(1|Pool), data=OneWk) 
summary(M1WK.RV.2)
anova(M1WK.RV.2)
#Check to see if you can drop term
anova(M1WK.RV, M1WK.RV.2) # Yes, can drop
qqnorm(resid(M1WK.RV.2))
qqline(resid(M1WK.RV.2))
hist(resid(M1WK.RV.2))
ad.test(resid(M1WK.RV.2)) #Checking normality

AIC(M1WK.RV, M1WK.RV.2)

#df   AIC
#          df      AIC
#M1WK.RV   12 136.4234
#M1WK.RV.2 10 121.2463


#DROPPING TERMS ------------------------------------------------SIMPLEST MODEL
M1WK.RV.3<-lmer(Q10~daily95+Timepoint+Species+(1|Pool), data=OneWk) 
summary(M1WK.RV.3)
anova(M1WK.RV.3)
#Check to see if you can drop term
anova(M1WK.RV.2, M1WK.RV.3) # Yes, can drop
qqnorm(resid(M1WK.RV.3))
qqline(resid(M1WK.RV.3))
hist(resid(M1WK.RV.3))
ad.test(resid(M1WK.RV.3)) #Checking normality

AIC(M1WK.RV.2, M1WK.RV.3)

#df   AIC
#M1WK.RV.2 10 121.2463
#M1WK.RV.3  8 111.9167

#Post hoc - emmeans-------------------------------------------
#
M1WK.RV.emm.n<-emmeans(M1WK.RV.3, "Species")
M1WK.RV.emm.n
emmeans(M1WK.RV.3, pairwise~Species)

# contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine   0.0918 0.0865 90.2 1.061   0.5405 
#Hermit - Mussel      0.2583 0.0810 90.5 3.188   0.0055 
#Littorine - Mussel   0.1665 0.0808 90.4 2.061   0.1039  


#-------------------------------------------------------
#------------------------
#1DAY ONLY------------------------------------------------------
#---------------------------------
OneDay<-filter(TH_Meta, TempDate == "1Day")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#1 MONTHS---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M1DY.RV<-lmer(Q10~daily95+Timepoint+Species+Timepoint*daily95+Species*daily95+(1|Pool), data=OneDay)
summary(M1DY.RV)
anova(M1DY.RV) 
qqnorm(resid(M1DY.RV))
qqline(resid(M1DY.RV))
hist(resid(M1DY.RV))
ad.test(resid(M1DY.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------
M1DY.RV.2<-lmer(Q10~daily95+Timepoint+Species+Timepoint*daily95+(1|Pool), data=OneDay) 
summary(M1DY.RV.2)
anova(M1DY.RV.2)
#Check to see if you can drop term
anova(M1DY.RV, M1DY.RV.2) # Yes, can drop
qqnorm(resid(M1DY.RV.2))
qqline(resid(M1DY.RV.2))
hist(resid(M1DY.RV.2))
ad.test(resid(M1DY.RV.2)) #Checking normality

AIC(M1DY.RV, M1DY.RV.2)

#df   AIC
#M1DY.RV   12 132.9109
#M1DY.RV.2 10 117.8557


#Post hoc - emmeans--------------------------------------------
#Interested in overall effect of Season
Spp.1day<- emmeans(M1DY.RV.2, ~Timepoint*daily95)
pairs(Spp.1day, by="daily95")

# contrast            estimate    SE   df t.ratio p.value
#(19-Jul) - (19-Mar)  -0.0532 0.185 90.5 -0.288  0.9555 
#(19-Jul) - (19-Sep)  -0.2359 0.134 83.3 -1.767  0.1870 
#(19-Mar) - (19-Sep)  -0.1827 0.179 89.1 -1.019  0.5670  


M1DY.Q10.emm.n<-emmeans(M1DY.RV.2, "Species")
M1DY.Q10.emm.n
emmeans(M1DY.RV.2, pairwise~Species)


#contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine    0.158 0.0921 97.0 1.715   0.2050 
#Hermit - Mussel       0.266 0.0859 91.5 3.095   0.0073 
#Littorine - Mussel    0.108 0.0880 95.2 1.225   0.4414 



################################################################
#RANGE ONLY
################################################################



#----------------------------------------------------------------------------
#THERMAL HISTORY ANALYSIS---------------------------------------------------
TH_Meta<-read.csv("RangelandSorte_ThermalHistory_All.csv", na.strings = "nd", header=T)
str(TH_Meta)
#-------------------------------------------------------
#------------------------
#3MONTH ONLY------------------------------------------------------
#---------------------------------
ThreeMo<-filter(TH_Meta, TempDate == "3Month")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#3 MONTHS---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M3.RV<-lmer(Q10~Range+Timepoint+Species+Timepoint*Range+Species*Range+Species*Timepoint+(1|Pool), data=ThreeMo)
summary(M3.RV)
anova(M3.RV) 
qqnorm(resid(M3.RV))
qqline(resid(M3.RV))
hist(resid(M3.RV))
ad.test(resid(M3.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------Removed Species*Range
M3.RV.2<-lmer(Q10~Range+Timepoint+Species+Timepoint*Range+Species*Timepoint+(1|Pool), data=ThreeMo)
summary(M3.RV.2)
anova(M3.RV.2)
#Check to see if you can drop term
anova(M3.RV, M3.RV.2) # Yes, can drop
qqnorm(resid(M3.RV.2))
qqline(resid(M3.RV.2))
hist(resid(M3.RV.2))
ad.test(resid(M3.RV.2)) #Checking normality

AIC(M3.RV, M3.RV.2)

#df   AIC
#M3.RV   16 148.6283
#M3.RV.2 14 134.1816


#DROPPING TERMS ------------------------------------------------Removed Timepoint*Range---SIMPLEST MODEL
M3.RV.3<-lmer(Q10~Range+Timepoint+Species+Species*Timepoint+(1|Pool), data=ThreeMo)
summary(M3.RV.3)
anova(M3.RV.3)
#Check to see if you can drop term
anova(M3.RV.2, M3.RV.3) # Yes, can drop
qqnorm(resid(M3.RV.3))
qqline(resid(M3.RV.3))
hist(resid(M3.RV.3))
ad.test(resid(M3.RV.3)) #Checking normality

AIC(M3.RV.2, M3.RV.3)

#df   AIC
#M3.RV.2 14 134.1816
#M3.RV.3 12 120.5765


#Post hoc - emmeans--------------------------------------------
#Interested in overall effect of Season
Spp.1day<- emmeans(M3.RV.3, ~Timepoint*Species)
pairs(Spp.1day, by="Timepoint")

#Timepoint = 19-Jul:
#  contrast           estimate    SE   df t.ratio p.value
#Hermit - Littorine   0.0974 0.144 84.8  0.678  0.7772 
#Hermit - Mussel      0.2359 0.134 82.7  1.765  0.1876 
#Littorine - Mussel   0.1385 0.138 84.3  1.004  0.5764 

#Timepoint = 19-Mar:
#  contrast           estimate    SE   df t.ratio p.value
#Hermit - Littorine   0.1780 0.135 97.7  1.318  0.3887 
#Hermit - Mussel      0.1078 0.127 93.7  0.850  0.6728 
#Littorine - Mussel  -0.0702 0.125 93.6 -0.563  0.8399 

#Timepoint = 19-Sep:
#  contrast           estimate    SE   df t.ratio p.value
#Hermit - Littorine  -0.0592 0.141 80.2 -0.421  0.9071 
#Hermit - Mussel      0.4567 0.128 82.9  3.571  0.0017 
#Littorine - Mussel   0.5159 0.142 84.9  3.624  0.0014  


#Interested in overall effect of Species
Spp.3mo<- emmeans(M3.RV.3, ~Timepoint*Species)
pairs(Spp.3mo, by="Species")

#Species = Hermit:
#  contrast            estimate    SE   df t.ratio p.value
#(19-Jul) - (19-Mar)   0.0485 0.169 90.2  0.287  0.9557 
#(19-Jul) - (19-Sep)  -0.1985 0.150 99.9 -1.326  0.3841 
#(19-Mar) - (19-Sep)  -0.2469 0.135 93.8 -1.822  0.1679 

#Species = Littorine:
#  contrast            estimate    SE   df t.ratio p.value
#(19-Jul) - (19-Mar)   0.1291 0.194 63.5  0.664  0.7849 
#(19-Jul) - (19-Sep)  -0.3551 0.170 99.8 -2.086  0.0979 
#(19-Mar) - (19-Sep)  -0.4841 0.156 99.1 -3.103  0.0070 

#Species = Mussel:
#  contrast            estimate    SE   df t.ratio p.value
#(19-Jul) - (19-Mar)  -0.0795 0.161 73.5 -0.493  0.8748 
#(19-Jul) - (19-Sep)   0.0224 0.145 99.4  0.155  0.9869 
#(19-Mar) - (19-Sep)   0.1019 0.129 99.1  0.790  0.7102  


#-------------------------------------------------------
#------------------------
#2MONTH ONLY------------------------------------------------------
#---------------------------------
TwoMo<-filter(TH_Meta, TempDate == "2Month")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#2 MONTHS---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M2.RV<-lmer(Q10~Range+Timepoint+Species+Timepoint*Range+Species*Range+Species*Timepoint+(1|Pool), data=TwoMo)
summary(M2.RV)
anova(M2.RV) 
qqnorm(resid(M2.RV))
qqline(resid(M2.RV))
hist(resid(M2.RV))
ad.test(resid(M2.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------Removed Species*Range
M2.RV.2<-lmer(Q10~Range+Timepoint+Species+Timepoint*Range+Species*Timepoint+(1|Pool), data=TwoMo) 
summary(M2.RV.2)
anova(M2.RV.2)
#Check to see if you can drop term
anova(M2.RV, M2.RV.2) # Yes, can drop
qqnorm(resid(M2.RV.2))
qqline(resid(M2.RV.2))
hist(resid(M2.RV.2))
ad.test(resid(M2.RV.2)) #Checking normality

AIC(M2.RV, M2.RV.2)

#df   AIC
#M2.RV   16 148.8095
#M2.RV.2 14 134.4633


#DROPPING TERMS ------------------------------------------------Removed Timepoint*Range ----  -- SIMPLEST MODEL--
M2.RV.3<-lmer(Q10~Range+Timepoint+Species+Species*Timepoint+(1|Pool), data=TwoMo)
summary(M2.RV.3)
anova(M2.RV.3)
#Check to see if you can drop term
anova(M2.RV.2, M2.RV.3) # Yes, can drop
qqnorm(resid(M2.RV.3))
qqline(resid(M2.RV.3))
hist(resid(M2.RV.3))
ad.test(resid(M2.RV.3)) #Checking normality

AIC(M2.RV.2, M2.RV.3)

#df   AIC
#M2.RV.2 14 134.4633
#M2.RV.3 12 120.2325

Spp.2mo<- emmeans(M2.RV.3, ~Timepoint*Species)
pairs(Spp.2mo, by="Species")

Spp.2mo<- emmeans(M2.RV.3, ~Timepoint*Species)
pairs(Spp.2mo, by="Timepoint")

#DROPPING TERMS ------------------------------------------------Removed Species*Timepoint
M2.RV.4<-lmer(Q10~Range+Timepoint+Species+(1|Pool), data=TwoMo) 
summary(M2.RV.4)
anova(M2.RV.4)
#Check to see if you can drop term
anova(M2.RV.3, M2.RV.4) # Cannot, cannot drop
qqnorm(resid(M2.RV.4))
qqline(resid(M2.RV.4))
hist(resid(M2.RV.4))
ad.test(resid(M2.RV.4)) #Checking normality


#-------------------------------------------------------
#------------------------
#1MONTH ONLY------------------------------------------------------
#---------------------------------
OneMo<-filter(TH_Meta, TempDate == "1Month")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#1 MONTHS---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M1.RV<-lmer(Q10~Range+Timepoint+Species+Timepoint*Range+Species*Range+(1|Pool), data=OneMo)
summary(M1.RV)
anova(M1.RV) 
qqnorm(resid(M1.RV))
qqline(resid(M1.RV))
hist(resid(M1.RV))
ad.test(resid(M1.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------
M1.RV.2<-lmer(Q10~Range+Timepoint+Species+Timepoint*Range+(1|Pool), data=OneMo) 
summary(M1.RV.2)
anova(M1.RV.2)
#Check to see if you can drop term
anova(M1.RV, M1.RV.2) # Yes, can drop
qqnorm(resid(M1.RV.2))
qqline(resid(M1.RV.2))
hist(resid(M1.RV.2))
ad.test(resid(M1.RV.2)) #Checking normality

AIC(M1.RV, M1.RV.2)

#df   AIC
#M1.RV   12 138.8697
#M1.RV.2 10 123.4780


#DROPPING TERMS ------------------------------------------------
M1.RV.3<-lmer(Q10~Range+Timepoint+Species+(1|Pool), data=OneMo) 
summary(M1.RV.3)
anova(M1.RV.3)
#Check to see if you can drop term
anova(M1.RV.2, M1.RV.3) # Yes, can drop
qqnorm(resid(M1.RV.3))
qqline(resid(M1.RV.3))
hist(resid(M1.RV.3))
ad.test(resid(M1.RV.3)) #Checking normality

AIC(M1.RV.2, M1.RV.3)

#df   AIC
#M1.RV.2 10 123.4780
#M1.RV.3  8 113.6754

M1Mo.Q10.emm.n<-emmeans(M1.RV.3, "Species")
M1Mo.Q10.emm.n
emmeans(M1.RV.3, pairwise~Species)


#contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine   0.0956 0.0872 91.5 1.096   0.5191 
#Hermit - Mussel      0.2589 0.0821 90.9 3.152   0.0062 
#Littorine - Mussel   0.1633 0.0824 94.1 1.981   0.1226 



#-------------------------------------------------------
#------------------------
#1WEEK ONLY------------------------------------------------------
#---------------------------------
OneWk<-filter(TH_Meta, TempDate == "1Week")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#1 MWEEK--------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M1WK.RV<-lmer(Q10~Range+Timepoint+Species+Timepoint*Range+Species*Range+(1|Pool), data=OneWk)
summary(M1WK.RV)
anova(M1WK.RV) 
qqnorm(resid(M1WK.RV))
qqline(resid(M1WK.RV))
hist(resid(M1WK.RV))
ad.test(resid(M1WK.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------
M1WK.RV.2<-lmer(Q10~Range+Timepoint+Species+Timepoint*Range+(1|Pool), data=OneWk) 
summary(M1WK.RV.2)
anova(M1WK.RV.2)
#Check to see if you can drop term
anova(M1WK.RV, M1WK.RV.2) # Yes, can drop
qqnorm(resid(M1WK.RV.2))
qqline(resid(M1WK.RV.2))
hist(resid(M1WK.RV.2))
ad.test(resid(M1WK.RV.2)) #Checking normality

AIC(M1WK.RV, M1WK.RV.2)

#df   AIC
#          df      AIC
#M1WK.RV   12 138.3163
#M1WK.RV.2 10 124.3536


#DROPPING TERMS ------------------------------------------------
M1WK.RV.3<-lmer(Q10~Range+Timepoint+Species+(1|Pool), data=OneWk) 
summary(M1WK.RV.3)
anova(M1WK.RV.3)
#Check to see if you can drop term
anova(M1WK.RV.2, M1WK.RV.3) # Yes, can drop
qqnorm(resid(M1WK.RV.3))
qqline(resid(M1WK.RV.3))
hist(resid(M1WK.RV.3))
ad.test(resid(M1WK.RV.3)) #Checking normality

AIC(M1WK.RV.2, M1WK.RV.3)

#df   AIC
#M1WK.RV.2 10 124.3536
#M1WK.RV.3  8 113.1174


#Post hoc - emmeans-------------------------------------------
#
M1WK.RV.emm.n<-emmeans(M1WK.RV.3, "Species")
M1WK.RV.emm.n
emmeans(M1WK.RV.3, pairwise~Species)

# contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine   0.0997 0.0871 89.2 1.146   0.4888 
#Hermit - Mussel      0.2627 0.0816 90.4 3.220   0.0050 
#Littorine - Mussel   0.1629 0.0811 90.4 2.010   0.1157 


#-------------------------------------------------------
#------------------------
#1DAY ONLY------------------------------------------------------
#---------------------------------
OneDay<-filter(TH_Meta, TempDate == "1Day")
#count(ThreeMo$Pool) #Check counts
#count(ThreeMo$Species)


#1 DAY---------Full model ----> Simplify----------------------Using Real Pool Temp Values----------------------------
M1DY.RV<-lmer(Q10~Range+Timepoint+Species+Timepoint*Range+Species*Range+(1|Pool), data=OneDay)
summary(M1DY.RV)
anova(M1DY.RV) 
qqnorm(resid(M1DY.RV))
qqline(resid(M1DY.RV))
hist(resid(M1DY.RV))
ad.test(resid(M1DY.RV)) #Checking normality


#DROPPING TERMS ------------------------------------------------SIMPLEST MODEL
M1DY.RV.2<-lmer(Q10~Range+Timepoint+Species+Timepoint*Range+(1|Pool), data=OneDay) 
summary(M1DY.RV.2)
anova(M1DY.RV.2)
#Check to see if you can drop term
anova(M1DY.RV, M1DY.RV.2) # Yes, can drop
qqnorm(resid(M1DY.RV.2))
qqline(resid(M1DY.RV.2))
hist(resid(M1DY.RV.2))
ad.test(resid(M1DY.RV.2)) #Checking normality

AIC(M1DY.RV, M1DY.RV.2)

#df   AIC
#M1DY.RV   12 126.7225
#M1DY.RV.2 10 115.9732



#Post hoc - emmeans--------------------------------------------
#Interested in overall effect of Season
Spp.1day<- emmeans(M1DY.RV.2, ~Timepoint*Range)
pairs(Spp.1day, by="Range")

#contrast            estimate     SE   df t.ratio p.value
#(19-Jul) - (19-Mar)   0.0555 0.0820 89.5  0.677  0.7775 
#(19-Jul) - (19-Sep)  -0.0932 0.0825 79.7 -1.129  0.4993 
#(19-Mar) - (19-Sep)  -0.1487 0.0807 87.8 -1.843  0.1616 


M1DY.Q10.emm.n<-emmeans(M1DY.RV.2, "Species")
M1DY.Q10.emm.n
emmeans(M1DY.RV.2, pairwise~Species)

# contrast           estimate     SE   df t.ratio p.value
#Hermit - Littorine   0.1415 0.0879 94.8 1.609   0.2467 
#Hermit - Mussel      0.2359 0.0867 90.6 2.720   0.0212 
#Littorine - Mussel   0.0943 0.0852 94.2 1.107   0.5119 





