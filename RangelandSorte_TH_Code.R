################################################################
#Title: Thermal History (TH) Analysis 
#Purpose: Determine relationship between Q10 and thermal history - all species - across all timepoints
#Created by: R. E. Rangel
#Created: August 2018
#Last edited: 17 April 2021
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
###Working Directory###--------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Thermal History")

#----------------------------------------------------------------------------
#THERMAL HISTORY ANALYSIS---------------------------------------------------
TH_Meta<-read.csv("RangelandSorte_ThermalHistory_All.csv", na.strings = "nd", header=T)
str(TH_Meta)
TH_Meta<-subset(TH_Meta, ID!="Littorine") # Keep this for making figures - remove for analyses
#-------------------------------------------------------
#------------------------
#3MONTH ONLY------------------------------------------------------
#---------------------------------
ThreeMo<-filter(TH_Meta, TempDate == "3Month")
all=c("#FDE725FF","#21908CFF","#440154FF")[ThreeMo$Species]
count(ThreeMo$Pool)
count(ThreeMo$Species)

#3 MONTHS---------Full model ----> Simplify
M3.Q10<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*PC2+(1|Pool), data=ThreeMo)
anova(M3.Q10) 
summary(M3.Q10)
qqnorm(resid(M3.Q10))
hist(resid(M3.Q10))

#DROP TERMS -
M3.Q10.2<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+(1|Pool), data=ThreeMo)
anova(M3.Q10.2)
#Check to see if you can drop term
anova(M3.Q10, M3.Q10.2)
summary(M3.Q10.2)
qqnorm(resid(M3.Q10.2))
hist(resid(M3.Q10.2))

AIC(M3.Q10, M3.Q10.2)

M3.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+(1|Pool), data=ThreeMo)
anova(M3.Q10.3) 
#Check to see if you can drop term
anova(M3.Q10.2, M3.Q10.3)
summary(M3.Q10.3)
qqnorm(resid(M3.Q10.3))
hist(resid(M3.Q10.3))

AIC(M3.Q10.2, M3.Q10.3)

M3.Q10.4<-lmer(Q10~PC1+PC2+Species+Timepoint+Timepoint*PC1+(1|Pool), data=ThreeMo)
anova(M3.Q10.4)
summary(M3.Q10.4)
qqnorm(resid(M3.Q10.4))
hist(resid(M3.Q10.4))
#Check to see if you can drop term
anova(M3.Q10.3, M3.Q10.4)

AIC(M3.Q10.4, M3.Q10.3)


M3.Q10.5<-lmer(Q10~PC1+PC2+Timepoint+Species+(1|Pool), data=ThreeMo) 
anova(M3.Q10.5)
summary(M3.Q10.5)
#Check to see if can drop term
anova(M3.Q10.5, M3.Q10.4)

#Checking residuals and normality-------------------- 
qqnorm(resid(M3.Q10.5))
qqline(resid(M3.Q10.5))
hist(resid(M3.Q10.5))
shapiro.test(resid(M3.Q10.5))
ad.test(resid(M3.Q10.5))

#Post hoc - emmeans---------
M3.Q10.emm.n<-emmeans(M3.Q10.5, "Species")
M3.Q10.emm.n
emmeans(M3.Q10.5, pairwise~Species)


#------------------------
#2MONTH ONLY---------Full model ----> Simplify--------------------------------------
#---------------------------------
TwoMo<-filter(TH_Meta, TempDate == "2Month")


M2.Q10<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*PC2+(1|Pool), data=TwoMo)
anova(M2.Q10)
summary(M2.Q10)
qqnorm(resid(M2.Q10))
hist(resid(M2.Q10))

#Remove interaction-------------------------------
M2.Q10.2<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+(1|Pool), data=TwoMo)
anova(M2.Q10.2)

qqnorm(resid(M2.Q10.2))
hist(resid(M2.Q10.2))

anova(M2.Q10, M2.Q10.2) #Check Removal

AIC(M2.Q10, M2.Q10.2)

#Remove interaction----------------------------------
M2.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+(1|Pool), data=TwoMo)
anova(M2.Q10.3)

qqnorm(resid(M2.Q10.3))
hist(resid(M2.Q10.3))

anova(M2.Q10.2, M2.Q10.3)

AIC(M2.Q10.2, M2.Q10.3)

#Remove interaction----------------------------------
M2.Q10.4<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+(1|Pool), data=TwoMo)
anova(M2.Q10.4)

qqnorm(resid(M2.Q10.4))
hist(resid(M2.Q10.4))

anova(M2.Q10.3, M2.Q10.4)

AIC(M2.Q10.3, M2.Q10.4)

#Remove interaction----------------------------------
M2.Q10.5<-lmer(Q10~PC1+PC2+Timepoint+Species+(1|Pool), data=TwoMo)
anova(M2.Q10.5)
summary(M2.Q10.5)

qqnorm(resid(M2.Q10.5))
qqline(resid(M2.Q10.5))
hist(resid(M2.Q10.5))
ad.test(resid(M2.Q10.5))

anova(M2.Q10.5, M2.Q10.4)

AIC(M2.Q10.5, M2.Q10.4)

M2.Q10.emm.n<-emmeans(M2.Q10.5, "Species")
M2.Q10.emm.n
emmeans(M2.Q10.5, pairwise~Species)



#------------------------
#1MONTH ONLY------------------------------------------------------
#---------------------------------
OneMo<-filter(TH_Meta, TempDate == "1Month")

M1.Q10<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*PC2+(1|Pool), data=OneMo)
anova(M1.Q10)
summary(M1.Q10)
qqnorm(resid(M1.Q10))
hist(resid(M1.Q10))

#Remove interaction------------
M1.Q10.2<-lmer(Q10~PC1+PC2+Species+Timepoint+Timepoint*PC1+Timepoint*PC2+Species*PC1+(1|Pool), data=OneMo)
anova(M1.Q10.2)
qqnorm(resid(M1.Q10.2))
hist(resid(M1.Q10.2))
anova(M1.Q10,M1.Q10.2)

AIC(M1.Q10,M1.Q10.2)


M1.Q10.3<-lmer(Q10~PC1+PC2+Species+Timepoint+Timepoint*PC1+Timepoint*PC2+(1|Pool), data=OneMo)
anova(M1.Q10.3)
qqnorm(resid(M1.Q10.3))
hist(resid(M1.Q10.3))
anova(M1.Q10.2,M1.Q10.3)

AIC(M1.Q10.2,M1.Q10.3)


#Remove interaction------------
M1.Q10.4<-lmer(Q10~PC1+PC2+Species+Timepoint+Timepoint*PC1+(1|Pool), data=OneMo)
anova(M1.Q10.4)
qqnorm(resid(M1.Q10.4))
hist(resid(M1.Q10.4))
anova(M1.Q10.3,M1.Q10.4) #Yes can drop

AIC(M1.Q10.3,M1.Q10.4)


#Remove interaction------------
M1.Q10.5<-lmer(Q10~PC1+PC2+Species+Timepoint+(1|Pool), data=OneMo)
anova(M1.Q10.5)
summary(M1.Q10.5)

qqnorm(resid(M1.Q10.5))
qqline(resid(M1.Q10.5))
hist(resid(M1.Q10.5))
ad.test(resid(M1.Q10.5))

anova(M1.Q10.5,M1.Q10.4)

AIC(M1.Q10.5,M1.Q10.4)

M1.Q10.emm.n<-emmeans(M1.Q10.5, "Species")
M1.Q10.emm.n
emmeans(M1.Q10.5, pairwise~Species)


#------------------------
#1 WEEK ONLY------------------------------------------------------
#---------------------------------
OneWeek<-filter(TH_Meta, TempDate == "1Week")


#PC1 & PC2---------------------------------------------------
M1WK.Q10<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*PC2+(1|Pool), 
               data=OneWeek)
anova(M1WK.Q10)
summary(M1WK.Q10)
qqnorm(resid(M1WK.Q10))
hist(resid(M1WK.Q10))
anova(M1WK.Q10,M1.Q10.2) #Yes can drop

AIC(M1.Q10,M1.Q10.2)


#DROP A TERM------------------------------------------------Best Model for 1 Week
M1WK.Q10.2<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+(1|Pool),data=OneWeek)
anova(M1WK.Q10.2)

summary(M1WK.Q10.2)
qqnorm(resid(M1WK.Q10.2))
hist(resid(M1WK.Q10.2))
shapiro.test(resid(M1WK.Q10.2))
ad.test(resid(M1WK.Q10.2))

anova(M1WK.Q10,M1WK.Q10.2) #Yes can drop

AIC(M1WK.Q10,M1WK.Q10.2)

#Interested in overall effect of Season
M1WK.Q10.emm.n<-emmeans(M1WK.Q10.2, specs = pairwise~Timepoint)

Spp.1wk<- emmeans(M1WK.Q10.2, ~Species*PC1)
pairs(Spp.1wk, by="PC1")

M1WK.Q10.emm.n<-emmeans(M1WK.Q10.2, pairwise ~ Timepoint)
M1WK.Q10.emm.n$contrasts





############################----------------------------------------------
#Counts for pools and Pool summary Stats -----------------------------------------------------------
#----------------------------------
library(pastecs) # gives descriptive stats
#3MONTHS
PoolStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "18-Sep")
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

#2MONTHS
PoolStats<-filter(TH_Meta, TempDate == "2Month" & Timepoint == "18-Sep")
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

#1MONTH
PoolStats<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "18-Sep")
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
PoolStats<-filter(TH_Meta, TempDate == "1Week" & Timepoint == "18-Sep")
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
PoolStats<-filter(TH_Meta, TempDate == "1Day" & Timepoint == "18-Sep")
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


####Q10
Sept18<-filter(OneWeek, Timepoint == "18-Sep")
Mar19<-filter(OneWeek, Timepoint == "19-Mar")
Jul19<-filter(OneWeek, Timepoint == "19-Jul")
Sept19<-filter(OneWeek, Timepoint == "19-Sep")

stat.desc(Sept18$Q10) #Sept 18: 1.59 +- 0.24
stat.desc(Mar19$Q10) #Mar19: 1.32 +- 0.25
stat.desc(Jul19$Q10) #July19: 1.38 +- 0.34
stat.desc(Sept19$Q10) #Sept 19: 1.50 +- 0.46


#Q10 Average and SD-------------------------------------------

#Hermits-------------
QStats_HS18<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "18-Sep" & Species == "Hermit")
QStats_HM19<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Mar" & Species == "Hermit")
QStats_HJ19<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Jul" & Species == "Hermit")
QStats_HS19<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Sep" & Species == "Hermit")

#Mussels----------------
QStats_MS18<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "18-Sep" & Species == "Mussel")
QStats_MM19<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Mar" & Species == "Mussel")
QStats_MJ19<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Jul" & Species == "Mussel")
QStats_MS19<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Sep" & Species == "Mussel")

#Littorines------------------
QStats_LM19<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Mar" & Species == "Littorine")
QStats_LJ19<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Jul" & Species == "Littorine")
QStats_LS19<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Sep" & Species == "Littorine")

#Herms---------------
#Sept18
stat.desc(QStats_HS18$Q10) #mean=1.529 sd=0.20
#Mar 19
stat.desc(QStats_HM19$Q10) #mean=1.408 sd=0.22
#Jul 19
stat.desc(QStats_HJ19$Q10) #mean=1.498 sd=0.27
#Sep 19
stat.desc(QStats_HS19$Q10) #mean=1.667 sd=0.40

#Mussels-----------------
#Sept18
stat.desc(QStats_MS18$Q10) #mean=1.682 sd=0.29
#Mar 19
stat.desc(QStats_MM19$Q10) #mean=1.314 sd=0.29
#Jul 19
stat.desc(QStats_MJ19$Q10) #mean=1.263 sd=0.34
#Sep 19
stat.desc(QStats_MS19$Q10) #mean=1.219 sd=0.33

#Littorines-----------------
#Mar 19
stat.desc(QStats_LM19$Q10) #mean=1.241 sd=0.22
#Jul 19
stat.desc(QStats_LJ19$Q10) #mean=1.378 sd=0.39
#Sep 19
stat.desc(QStats_LS19$Q10) #mean=1.721 sd=0.58


#PCA-------------------------------
####################################################  
#   Script to run a PCA                            #
#                                                  #
####################################################

library(lattice)
library(ggplot2)
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(ggthemes)


YearData <- read.csv("RangelandSorte_ThermalHistory_All.csv")
YearData<-subset(YearData, ID!="Littorine") # If you haven't already removed the blank littorine - do it now

#Make the PCA-----------------------

PCA <- prcomp(YearData[,c(8:23)], scale = TRUE, center = TRUE)

# Proportion of data explained by each axis
summary(PCA)

#Making a PDF of the PCA---------------------
#pdf("TempPCA.pdf",height=8,width=9)
#dev.off()

# plot
fviz_pca_biplot(PCA, xlab = "PC1 (73.0%)", ylab = "PC2 (22.0%)", repel = TRUE, col.var = "black", col.ind = "gray", title = " ", label = "var") + theme_base()

# Pull out the individual point scores 
PCAFrame<-data.frame(YearData$ID,YearData$Timepoint,YearData$TempDate,-PCA$x[,1:2])

# We want to make sure the columns/ID line up correctly and then merge our data into one dataset
colnames(PCAFrame)[1:3]<-c('ID', "Timepoint","TempDate")

AllData <- merge(YearData, PCAFrame, by = c("ID","Timepoint","TempDate"), all.x = TRUE) #Fixed - It works!

#Getting AllData csv - yay!
write.csv(AllData, file = "RangelandSorte_ThermalHistory_All.csv")

