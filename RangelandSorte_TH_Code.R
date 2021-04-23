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
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(lme4)
library(lmerTest)
library(emmeans)
###Working Directory###--------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Thermal History")

#----------------------------------------------------------------------------
#THERMAL HISTORY ANALYSIS---------------------------------------------------
TH_Meta<-read.csv("ThermalHistory_All3.csv", na.strings = "nd", header=T)
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
#Check to see if you can drop interaction:YES No Sig p-value
anova(M3.Q10, M3.Q10.2)
summary(M3.Q10.2)
qqnorm(resid(M3.Q10.2))
hist(resid(M3.Q10.2))

AIC(M3.Q10, M3.Q10.2)

M3.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+(1|Pool), data=ThreeMo)
anova(M3.Q10.3) # NO SIG
#Check to see if you can drop interaction: YES No sig p-value
anova(M3.Q10.2, M3.Q10.3)
summary(M3.Q10.3)
qqnorm(resid(M3.Q10.3))
hist(resid(M3.Q10.3))

AIC(M3.Q10.2, M3.Q10.3)

####
M3.Q10.4<-lmer(Q10~PC1+PC2+Species+Timepoint+Timepoint*PC1+(1|Pool), data=ThreeMo)
anova(M3.Q10.4) #Species

summary(M3.Q10.4)
qqnorm(resid(M3.Q10.4))
hist(resid(M3.Q10.4))

anova(M3.Q10.3, M3.Q10.4)#Check to see if can drop timepoint: YES
AIC(M3.Q10.4, M3.Q10.3)


M3.Q10.5<-lmer(Q10~PC1+PC2+Timepoint+Species+(1|Pool), data=ThreeMo) 
anova(M3.Q10.5)
summary(M3.Q10.5)
anova(M3.Q10.5, M3.Q10.4)#Check to see if can drop timepoint : YES

#Checking residuals and normality-------------------- 
qqnorm(resid(M3.Q10.5))
qqline(resid(M3.Q10.5))
hist(resid(M3.Q10.5))
shapiro.test(resid(M3.Q10.5))
ad.test(resid(M3.Q10.5))


AIC(M3.Q10.4, M3.Q10.5)

M3.Q10.emm.n<-emmeans(M3.Q10.5, "Species")
M3.Q10.emm.n
emmeans(M3.Q10.5, pairwise~Species)


#------------------------
#2MONTH ONLY------------------------------------------------------
#---------------------------------
TwoMo<-filter(TH_Meta, TempDate == "2Month")
all=c("#FDE725FF","#21908CFF", "#440154FF")[TwoMo$Species]

#PC1 ONLY + Q10------------
M1.2Q10<-lmer(Q10~PC1+Timepoint+Timepoint*PC1+Species+(1|Pool), data=TwoMo)
anova(M1.2Q10)
#Summary output---------
#Mussels are sig p=0.0275 
anova(M1.2Q10)

#PC1 & PC2------------
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


#SLOPE LOG PC1---------------
M1.2SL<-lmer(slopelog~PC1+Timepoint+Species+Timepoint*PC1+(1|Pool), data=TwoMo)
summary(M1.2SL)
#Summary output---------
#Mussels are almost sig p= 0.0803

##GET BOUNDARY SINGULAR MESSAGE!!!!!!!!

#Summary output---------
#Mussels are almost sig p=0.0804

#Plots------------------
quartz()
par(mfrow= c(1,2))
plot(Q10~PC1,pch=21,lwd=1.5, col="black", main="Q10", bg=all,data=TwoMo)
plot(Q10~PC2,pch=21,lwd=1.5, col="black", main="Q10", bg=all,data=TwoMo)

plot(slopelog~PC1,pch=21,lwd=1.5, col="black",bg=all, main="SlopeLog",data=TwoMo)
plot(slopelog~PC2,pch=21,lwd=1.5, col="black",bg=all, main="SlopeLog",data=TwoMo)


#Q10 from 10-18 C ----------------------------------------------------
#All Timepoints and TempDates: 2 MONTHS---------Full model ----> Simplify
M2.Q10.18<-lmer(Q1018~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*PC2+(1|Pool), data=TwoMo)
anova(M2.Q10.18) 

qqnorm(resid(M2.Q10.18))
hist(resid(M2.Q10.18))


#------------------------
#1MONTH ONLY------------------------------------------------------
#---------------------------------
OneMo<-filter(TH_Meta, TempDate == "1Month")
all=c("#FDE725FF","#21908CFF", "#440154FF")[OneMo$Species]

#PC1 & PC2 ONLY + Q10------------
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

anova(M1.Q10,M1.Q10.2) #Yes can drop

AIC(M1.Q10,M1.Q10.2)

#Remove interaction------------
M1.Q10.3<-lmer(Q10~PC1+PC2+Species+Timepoint+Timepoint*PC1+Timepoint*PC2+(1|Pool), data=OneMo)
anova(M1.Q10.3)

qqnorm(resid(M1.Q10.3))
hist(resid(M1.Q10.3))

anova(M1.Q10.2,M1.Q10.3) #Yes can drop

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

anova(M1.Q10.5,M1.Q10.4) #Yes can drop

AIC(M1.Q10.5,M1.Q10.4)

M1.Q10.emm.n<-emmeans(M1.Q10.5, "Species")
M1.Q10.emm.n
emmeans(M1.Q10.5, pairwise~Species)

#Plots------------------
quartz()
par(mfrow= c(1,2))
plot(Q10~PC1,pch=21,lwd=1.5, col="black", main="Q10", bg=all,data=OneMo)
plot(Q10~PC2,pch=21,lwd=1.5, col="black", main="Q10", bg=all,data=OneMo)

plot(slopelog~PC1,pch=21,lwd=1.5, col="black",bg=all, main="SlopeLog",data=OneMo)
plot(slopelog~PC2,pch=21,lwd=1.5, col="black",bg=all, main="SlopeLog",data=OneMo)


#Q10 from 10-18 C ----------------------------------------------------
#All Timepoints and TempDates: 1 MONTH---------Full model ----> Simplify
M1.Q10.18<-lmer(Q1018~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*PC2+(1|Pool), data=OneMo)
anova(M1.Q10.18) 

qqnorm(resid(M1.Q10.18))
hist(resid(M1.Q10.18))

#------------------------
#1 WEEK ONLY------------------------------------------------------
#---------------------------------
OneWeek<-filter(TH_Meta, TempDate == "1Week")
#OneWeek<-subset(OneWeek, ID!="Mussel9")
s18<-filter(OneWeek, Timepoint == "18-Sep")
m19<-filter(OneWeek, Timepoint == "19-Mar")
j19<-filter(OneWeek, Timepoint == "19-Jul")
s19<-filter(OneWeek, Timepoint == "19-Sep")

count(OneWeek$Pool)
count(m19$Species)
count(j19$Species)

#all=c("#FDE725FF","#21908CFF", "#440154FF")[OneWeek$Species]
#all.s=c("#FDE725FF","#21908CFF", "#440154FF")[s19$Species]
#all.j=c("#FDE725FF","#21908CFF", "#440154FF")[j19$Species]
#all.m=c("#FDE725FF","#21908CFF", "#440154FF")[m19$Species]
#all.s2=c("#FDE725FF","#21908CFF", "#440154FF")[s18$Species]

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

anova(M1WK.Q10,M1WK.Q10.2) #Yes can drop

AIC(M1WK.Q10,M1WK.Q10.2)

#Interested in overall effect of Season
M1WK.Q10.emm.n<-emmeans(M1WK.Q10.2, specs = pairwise~Timepoint)



Spp.1wk<- emmeans(M1WK.Q10.2, ~Species*PC1)
pairs(Spp.1wk, by="PC1")

M1WK.Q10.emm.n<-emmeans(M1WK.Q10.2, pairwise ~ Timepoint)
M1WK.Q10.emm.n$contrasts


#DROP A TERM----------------------------------------------for 1 Week - Overfits
M1WK.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+(1|Pool),data=OneWeek)

# Gets rid of error message - control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1)))
anova(M1WK.Q10.3)
#Anova(M1WK.Q10.3, type=3)

isSingular(M1WK.Q10.3, tol = 1e-05) #TRUE

summary(M1WK.Q10.3)

qqnorm(resid(M1WK.Q10.3))
qqline(resid(M1WK.Q10.3))
hist(resid(M1WK.Q10.3))
plot(M1WK.Q10.3)
ad.test(resid(M1WK.Q10.3))

anova(M1WK.Q10.3,M1WK.Q10.2) #Yes can drop

AIC(M1WK.Q10.3,M1WK.Q10.2)


#DROP A TERM----------------------------------------------Can drop PC1* = AIC no that different (only 1)
#####------------
M1WK.Q10.4<-lmer(Q10~PC1+PC2+Species+Timepoint+Timepoint*PC2+(1|Pool),data=OneWeek)
anova(M1WK.Q10.4)

qqnorm(resid(M1WK.Q10.4))
qqline(resid(M1WK.Q10.4))
hist(resid(M1WK.Q10.4))
shapiro.test(resid(M1WK.Q10.4))
anova(M1WK.Q10.4,M1WK.Q10.3) #nope cant drop


AIC(M1WK.Q10.4,M1WK.Q10.3)


M1WK.Q10.emm.n<-emmeans(M1WK.Q10.3, pairwise~Species)

M1WK.Q10.emm.n
emmeans(M1WK.Q10.3, pairwise~Species)
#Hermit - Littorine    0.077 0.0937  69.5 0.822   0.6906 
#Hermit - Mussel       0.238 0.0884  65.2 2.692   0.0242 ***
#Littorine - Mussel    0.161 0.1000 101.9 1.611   0.2455 


################################--------------------------------
##Model predictions
M1WK.Q10.2<-lmer(Q10~PC1+PC2+Species+Timepoint+Timepoint*PC1+Timepoint*PC2+Species*PC1+(1|Pool), data=OneWeek)

summary(M1WK.Q10.2)
anova(M1WK.Q10.2)

newdat <- data.frame(PC1=3.5, PC2=seq(-2, 0.5, by=0.1), Species=c("Hermit"),Timepoint="19-Mar")
newdat.m <- data.frame(PC1=3.5, PC2=seq(-2, 0.5, by=0.1), Species=c("Mussel"),Timepoint="19-Mar")
newdat.l <- data.frame(PC1=3.5, PC2=seq(-2, 0.5, by=0.1), Species=c("Littorine"),Timepoint="19-Mar")

yn <- predict(M1WK.Q10.2, newdata=newdat, re.form = NA)
yn.m <- predict(M1WK.Q10.2, newdata=newdat.m, re.form = NA)
yn.l <- predict(M1WK.Q10.2, newdata=newdat.l, re.form = NA)

plot(yn, type="l")
lines(yn.m, col="red")
lines(yn.l, col="green")

plot(MO2~Range, pch=16, col=all.m, data=OneWeek[which(OneWeek$Timepoint=="19-Mar"),])
abline(M1WK.Q10.m, lwd=2)

plot(MO2~Temp, pch=16, col=all.m, data=m19)


plot(Q1018~PC2,pch=21,lwd=1.5, col="black", main="1 Week", bg=all,data=OneWeek)
plot(Q10~PC1,pch=21,lwd=1.5, col="black", main="1 Week", bg=all,data=OneWeek)

plot(Q10~X90max,pch=21,lwd=1.5, col="black", main="1 Week", bg=all,data=OneWeek)
plot(Q10~PC1,pch=21,lwd=1.5, col="black", main="1 Week", bg=all,data=OneWeek)


anova(M1WK.Q10.2)
predict(M1WK.Q10.2, newdata=)
summary(M1WK.Q10.2)
plot(M1WK.Q10.2)
qqnorm(resid(M1WK.Q10.2))
hist(resid(M1WK.Q10.2))
ranef(M1WK.Q10.2)


plot<-ggplot(data=OneWeek, aes(x=dailyavg, y=Q10, color=Species)) +geom_point()
plot

print(M1WK.Q10.2)
summary(M1WK.Q10.2)
fixef(M1WK.Q10.2)
confint(M1WK.Q10.2) # for fixed effects
ranef(M1WK.Q10.2) #random effects no CI 


#DROP A TERM: PC1 interaction with Species
M1WK.Q10.3<-lmer(Q10~PC1+PC2+Species+Timepoint+Timepoint*PC1+Timepoint*PC2+(1|Pool), data=OneWeek)
anova(M1WK.Q10.3)

plot(Q10~PC1,pch=21,lwd=1.5, col="black", main="1 Week", bg=all,data=OneWeek)


#Q10 from 10-18 C ----------------------------------------------------
#All Timepoints and TempDates: 1WEEK---------Full model ----> Simplify
M1WK.Q10.18<-lmer(Q1018~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*PC2+(1|Pool), data=OneWeek)
anova(M1WK.Q10.18) 

qqnorm(resid(M1WK.Q10.18))
hist(resid(M1WK.Q10.18))

M1WK.Q10.18<-lmer(log(MO218)~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+Species*PC1+Species*PC2+(1|Pool), data=OneWeek)
anova(M1WK.Q10.18) 

qqnorm(resid(M1WK.Q10.18))
hist(resid(M1WK.Q10.18))

###Plots----------
plot(Q10~Timepoint, data=OneWeek)

points(MO210~Temp10, pch=16, col=all, data=OneWeek)

plot(Q1018~PC1, pch=16, col=all, data=OneWeek)
points(Q10~Temp10, pch=16, col=all, data=OneWeek)

plot(log(MO218)~X90max, pch=16, col=all, data=OneWeek)


s18.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "18-Sep")
s18.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "18-Sep")

m19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Mar")
m19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Mar")
m19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Mar")

j19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Jul")
j19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Jul")
j19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Jul")

s19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Sep")
s19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Sep")
s19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Sep")

#COLORS - ("#FDE725FF" = Yellow ,"#21908CFF" = Blue, "#440154FF" = Purple)
quartz()
par(mfrow= c(2,2))
plot(Q10~X95max,pch=21,lwd=1.5, col="black", main="1 Week", bg=all,data=j19)
plot(Q10~X95max,pch=21,lwd=1.5, col="black", main="1 Week", bg=all,data=s19)

plot(Q10~Range,pch=21,lwd=1.5, col="black", main="1 Week Sept 18", bg=all.s2,data=s18)
Ms18.lm<-lm(Q10~Range, data=s18.2)
Hs18.lm<-lm(Q10~Range, data=s18.3)
abline(Ms18.lm, lwd=2, col="#440154FF")
abline(Hs18.lm, lwd=2, col="#FDE725FF")


plot(Q10~Range,pch=21,lwd=1.5, col="black", main="1 Week March", bg=all.m,data=m19)
Mm19.lm<-lm(Q10~Range, data=m19.2)
Hm19.lm<-lm(Q10~Range, data=m19.3)
Lm19.lm<-lm(Q10~Range, data=m19.4)
abline(Mm19.lm, lwd=2, col="#440154FF")
abline(Hm19.lm, lwd=2, col="#FDE725FF")
abline(Lm19.lm, lwd=2, col="#21908CFF")

plot(Q10~Range,pch=21,lwd=1.5, col="black", main="1 Week July", bg=all.j,data=j19)
Mj19.lm<-lm(Q10~Range, data=j19.2)
Hj19.lm<-lm(Q10~Range, data=j19.3)
Lj19.lm<-lm(Q10~Range, data=j19.4)
abline(Mj19.lm, lwd=2, col="#440154FF")
abline(Hj19.lm, lwd=2, col="#FDE725FF")
abline(Lj19.lm, lwd=2, col="#21908CFF")

plot(Q10~Range,pch=21,lwd=1.5, col="black", main="1 Week Sept 19", bg=all.s,data=s19)
Ms19.lm<-lm(Q10~Range, data=s19.2)
Hs19.lm<-lm(Q10~Range, data=s19.3)
Ls19.lm<-lm(Q10~Range, data=s19.4)
abline(Ms19.lm, lwd=2, col="#440154FF")
abline(Hs19.lm, lwd=2, col="#FDE725FF")
abline(Ls19.lm, lwd=2, col="#21908CFF")


coef(M1WK.Q10.4)
plot(M1WK.Q10.4)
qqnorm(resid(M1WK.Q10.4))
qqline(resid(M1WK.Q10.4)) 

quartz()
#Figure without Linear Model in Background-----------------------
#---------------------------------
#-------------------------------------------
s.18<-(ggplot() + 
         geom_point(data = s18,  size=5, aes(x = Range, y = Q10, shape=Species, color=Species))+
         labs(x = "", y = "", title = "") + scale_colour_manual(labels = c("Hermit Crab", "Snail", "Mussel"), values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_shape(labels = c("Hermit Crab", "Snail", "Mussel")) +
         scale_y_continuous(name="", limits=c(0.5, 3)) +
         scale_x_continuous(name="", limits=c(2, 18))+
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)


s.18<-s.18 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20)) + theme(legend.title = element_text(color = "black", size = 20)) + theme(legend.text = element_text(size=20))


s18.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "18-Sep")
s18.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "18-Sep")

s.18<-s.18 + geom_smooth(data= s18.2, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= s18.3, aes(x=Range, y=Q10), method="lm", linetype="dashed", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5)




#pdf("s.18.pdf",height=5,width=6)
s.18<-s.18 + annotate("text", x=4, y=3, label="Sept 2018", size=7) +
  theme(axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))
#dev.off()


m.19<-(ggplot() + 
         geom_point(data = m19, size=5,                    # adding the raw data (scaled values)
                    aes(x = Range, y = Q10, shape= Species, color=Species)) + 
         labs(x = "", title = "") + 
         scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name=" ", limits=c(0.5, 3)) +
         scale_x_continuous(name="", limits=c(2, 18))+
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)


#pdf("m.19.pdf",height=5,width=6)
m.19<-m.19 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20))

m19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Mar")
m19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Mar")
m19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Mar")

m.19<-m.19 + geom_smooth(data= m19.2, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= m19.3, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed") +
  geom_smooth(data= m19.4, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5, linetype="dotted")



m.19<- m.19 + annotate("text", x=4.4, y=3, label="March 2019", size=7)
#dev.off()

j.19<-(ggplot() + 
         geom_point(data = j19, size=5,                      # adding the raw data (scaled values)
                    aes(x = Range, y = Q10, shape = Species, color=Species)) + 
         labs(x = "", y = "", 
              title = "") + scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name="", limits=c(0.5, 3)) +
         scale_x_continuous(name="", limits=c(2, 18))+
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)

#pdf("j.19.pdf",height=5,width=6)
j.19<-j.19 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20))

j19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Jul")
j19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Jul")
j19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Jul")

j.19<-j.19 + geom_smooth(data= j19.2, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= j19.3, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed") +
  geom_smooth(data= j19.4, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5, linetype="dotted")


j.19<-j.19 + annotate("text", x=4, y=3, label="July 2019", size=7)

#dev.off()

#M1WK.Range<-lmer(Q10~Range+Species+(1|Pool), data=OneWeek)
# Extract the prediction data frame
#pred.mm <- ggpredict(M1WK.Range, terms = c("Range"))  # this gives overall predictions for the model

s.19<-(ggplot() + 
         geom_point(data = s19, size=5,                     # adding the raw data (scaled values)
                    aes(x = Range, y = Q10, shape = Species, color=Species)) + 
         labs(x = "", 
              title = "") + scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name="", limits=c(0.5, 3)) +
         scale_x_continuous(name="", limits=c(2, 18)) +
         theme_bw()+ 
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)


#pdf("s.19.pdf",height=5,width=6)
s.19<-s.19+ theme(axis.text.x = element_text(size=20),
                  axis.text.y = element_text(size=20)) 

s19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Sep")
s19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Sep")
s19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Sep")

s.19<-s.19 + geom_smooth(data= s19.2, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= s19.3, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed") +
  geom_smooth(data= s19.4, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5, linetype="dotted")


s.19<-s.19 + annotate("text", x=4, y=3, label="Sept 2019", size=7)

dev.off()



pdf("TH_ALL1WEEK.pdf",height=8,width=10)
#NEED Package - gridExtra() grid() and ggpubr()
figure<-ggarrange(s.18,m.19,j.19,s.19,
                  common.legend = TRUE, legend = "top", font.label = list(size=20, face="plain"))
annotate_figure(figure, bottom = text_grob("Range of Pool Temperatures (°C)", size=20, just="centre", vjust = -0.5), top = text_grob("", size=20,hjust=0.5), left = text_grob(expression(Q[10]), rot=90, size=35, vjust =0.9))

dev.off()

#grid.arrange(s.18,m.19,j.19,s.19) # Dont use this one 

library(gridExtra)
library(grid)
library(ggpubr)

# Plot the predictions (DOULBE CHECK MODEL OUT PUT!!)-------------------------------------------------------
#coef(summary(M1WK.Range))
#M1WK.Range<-lmer(Q10~Range+Timepoint+Species+(1|Pool), data=OneWeek)
# Extract the prediction data frame
#pred.mm <- ggpredict(M1WK.Range, terms = c("Range"))  # this gives overall predictions for the model

s.18<-(ggplot() + 
         geom_line(aes(x = x, y = predicted)) +          # slope
         geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                     fill = "lightgrey", alpha = 0.5) +  # error band
         geom_point(data = s18,  size=5, aes(x = PC2, y = Q10, shape=Species, color=Species))+
         labs(x = "", y = "", title = "") + scale_colour_manual(labels = c("Hermit", "Snail", "Mussel"), values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_shape(labels = c("Hermit", "Snail", "Mussel")) +
         scale_y_continuous(name="", limits=c(0.5, 2.5)) +
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)

s.18<-s.18 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20)) + theme(legend.title = element_text(color = "black", size = 20)) + theme(legend.text = element_text(size=20))


s18.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "18-Sep")
s18.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "18-Sep")

s.18<-s.18 + geom_smooth(data= s18.2, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= s18.3, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed")


#coef(lm(Q10~Range, data=s18.2))

pdf("s.18.pdf",height=5,width=6)
s.18+ theme(axis.text.x = element_text(size=20),
            axis.text.y = element_text(size=20))
#dev.off()

M1WK.Range<-lmer(Q10~Range+Species+(1|Pool), data=OneWeek)
# Extract the prediction data frame
pred.mm <- ggpredict(M1WK.Range, terms = c("Range"))  # this gives overall predictions for the model
m.19<-(ggplot(pred.mm) + 
         geom_line(aes(x = x, y = predicted)) +          # slope
         geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                     fill = "lightgrey", alpha = 0.5) +  # error band
         geom_point(data = m19, size=5,                    # adding the raw data (scaled values)
                    aes(x = Range, y = Q10, shape= Species, color=Species)) + 
         labs(x = "", title = "") + 
         scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name=" ", limits=c(0.5, 2.5)) +
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)


pdf("m.19.pdf",height=5,width=6)
m.19<-m.19 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20))

m19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Mar")
m19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Mar")
m19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Mar")

m.19<-m.19 + geom_smooth(data= m19.2, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= m19.3, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5) +
  geom_smooth(data= m19.4, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5)



m.19

M1WK.Range<-lmer(Q10~Range+Species+(1|Pool), data=OneWeek)
# Extract the prediction data frame
pred.mm <- ggpredict(M1WK.Range, terms = c("Range"))  # this gives overall predictions for the model
j.19<-(ggplot(pred.mm) + 
         geom_line(aes(x = x, y = predicted)) +          # slope
         geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                     fill = "lightgrey", alpha = 0.5) +  # error band
         geom_point(data = j19, size=5,                      # adding the raw data (scaled values)
                    aes(x = Range, y = Q10, shape = Species, color=Species)) + 
         labs(x = "", y = "", 
              title = "") + scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name="", limits=c(0.5, 2.5)) +
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)

pdf("j.19.pdf",height=5,width=6)
j.19<-j.19 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20))

j19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Jul")
j19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Jul")
j19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Jul")

j.19<-j.19 + geom_smooth(data= j19.2, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= j19.3, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5) +
  geom_smooth(data= j19.4, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5)


j.19

dev.off()

M1WK.Range<-lmer(Q10~Range+Species+(1|Pool), data=OneWeek)
# Extract the prediction data frame
pred.mm <- ggpredict(M1WK.Range, terms = c("Range"))  # this gives overall predictions for the model

s.19<-(ggplot(pred.mm) + 
         geom_line(aes(x = x, y = predicted)) +          # slope
         geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                     fill = "lightgrey", alpha = 0.5) +  # error band
         geom_point(data = s19, size=5,                     # adding the raw data (scaled values)
                    aes(x = Range, y = Q10, shape = Species, color=Species)) + 
         labs(x = "", 
              title = "") + scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name="", limits=c(0.5, 2.5)) +
         theme_bw()+ 
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)


pdf("s.19.pdf",height=5,width=6)
s.19<-s.19+ theme(axis.text.x = element_text(size=20),
                  axis.text.y = element_text(size=20)) 

s19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Sep")
s19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Sep")
s19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Sep")

s.19<-s.19 + geom_smooth(data= s19.2, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= s19.3, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5) +
  geom_smooth(data= s19.4, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5)


s.19

dev.off()



pdf("TH_ALL.pdf",height=8,width=11)

ggarrange(s.18,m.19,j.19,s.19,
          common.legend = TRUE, legend = "top", font.label = list(size=20, face="plain"))
dev.off()

grid.arrange(s.18,m.19,j.19,s.19)

# Plot the predictions 
M1WK.Range<-lmer(Q10~Range+Timepoint+(1|Pool), data=OneWeek)
# Extract the prediction data frame

pred.mm <- ggpredict(M1WK.Range, terms = c("Range"))  # this gives overall predictions for the model
m<-(ggplot(pred.mm) + 
      geom_line(aes(x = x, y = predicted)) +          # slope
      geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                  fill = "lightgrey", alpha = 0.5) +  # error band
      geom_point(data = H1,                      # adding the raw data (scaled values)
                 aes(x = Range, y = Q10, colour = Timepoint)) + 
      labs(x = "Range of Pool Temp °C", y = "Q10", 
           title = "") + 
      theme_minimal()
)





####All spp and all timepoints on one graph-----------------------
M1WK.Range<-lmer(Q10~Range+Timepoint+Species+(1|Pool), data=OneWeek)
pred.mm <- ggpredict(M1WK.Range, terms = c("Range"))

plot(Effect(c("Range"), M1WK.Range))

TH.all<-(ggplot(pred.mm) + 
           geom_line(aes(x = x, y = predicted)) +          # slope
           geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                       fill = "lightgrey", alpha = 0.5) +  # error band
           geom_point(data = OneWeek,  size=5, aes(x = Range, y = Q10, shape=Species, color=Species))+
           labs(x = "", y = "", title = "") + scale_colour_manual(labels = c("Hermit", "Snail", "Mussel"), values=c("#FDE725FF","#21908CFF","#440154FF")) +
           scale_shape(labels = c("Hermit", "Snail", "Mussel")) +
           scale_y_continuous(name="", limits=c(0.5, 2.5)) +
           theme_bw()+
           theme(axis.line = element_line(colour = "black"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
)

TH.all<-TH.all + theme(axis.text.x = element_text(size=20),
                       axis.text.y = element_text(size=20)) + theme(legend.title = element_text(color = "black", size = 20)) + theme(legend.text = element_text(size=20))


l<-filter(OneWeek, Species == "Littorine")
m<-filter(OneWeek, Species == "Mussel")
h<-filter(OneWeek, Species == "Hermit")

m.all<-TH.all + geom_smooth(data= m, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
                            color="#440154FF", size= 1.5) + 
  geom_smooth(data= m, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#440154FF", size= 1.5)


h.all<-m.all + geom_smooth(data= h, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
                           color="#440154FF", size= 1.5) + 
  geom_smooth(data= h, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5)
l.all<-h.all + geom_smooth(data= l, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
                           color="#21908CFF", size= 1.5) + 
  geom_smooth(data= l, aes(x=Range, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5)


pdf("TH_ALL_SPECIES.pdf",height=8,width=11)

l.all

dev.off()
8

M1WK.Q10.emm.n<-emmeans(M1WK.Q10.2, "Species")
M1WK.Q10.emm.n
emmeans(M1WK.Q10.2, pairwise~Species)
#Hermit - Littorine    0.077 0.0937  69.5 0.822   0.6906 
#Hermit - Mussel       0.238 0.0884  65.2 2.692   0.0242 ***
#Littorine - Mussel    0.161 0.1000 101.9 1.611   0.2455 



#####Maximum for Q10 response-------------------------------------- 
s.18<-(ggplot() + 
         geom_point(data = s18,  size=5, aes(x = dailymax, y = Q10, shape=Species, color=Species))+
         labs(x = "", y = "Q[10", title = "") + scale_colour_manual(labels = c("Hermit Crab", "Snail", "Mussel"), values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_shape(labels = c("Hermit Crab", "Snail", "Mussel")) +
         scale_y_continuous(name="", limits=c(0.5, 3.0)) +
         scale_x_continuous(name="", limits=c(5, 25))+
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)


s.18<-s.18 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20)) + theme(legend.title = element_text(color = "black", size = 20)) + theme(legend.text = element_text(size=20))


s18.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "18-Sep")
s18.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "18-Sep")

s.18<-s.18 + geom_smooth(data= s18.2, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= s18.3, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed")




#pdf("s.18.pdf",height=5,width=6)
s.18<-s.18+ theme(axis.text.x = element_text(size=20),
                  axis.text.y = element_text(size=20)) + annotate("text", x=7.9, y=3.0, label="Sept 2018", size=7)
#dev.off()


m.19<-(ggplot() + 
         geom_point(data = m19, size=5,                   
                    aes(x = dailymax, y = Q10, shape= Species, color=Species)) + 
         labs(x = "", title = "") + 
         scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name=" ", limits=c(0.5, 3.0)) +
         scale_x_continuous(name="", limits=c(5, 25))+
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)


#pdf("m.19.pdf",height=5,width=6)
m.19<-m.19 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20))

m19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Mar")
m19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Mar")
m19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Mar")

m.19<-m.19 + geom_smooth(data= m19.2, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= m19.3, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed") +
  geom_smooth(data= m19.4, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5, linetype="dotted")



m.19<-m.19+annotate("text", x=8, y=3.0, label="March 2019", size=7)
#dev.off()

j.19<-(ggplot() + 
         geom_point(data = j19, size=5,                      # adding the raw data (scaled values)
                    aes(x = dailymax, y = Q10, shape = Species, color=Species)) + 
         labs(x = "", y = "", 
              title = "") + scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name="", limits=c(0.5, 3.0)) +
         scale_x_continuous(name="", limits=c(5, 25))+
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)

#pdf("j.19.pdf",height=5,width=6)
j.19<-j.19 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20))

j19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Jul")
j19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Jul")
j19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Jul")

j.19<-j.19 + geom_smooth(data= j19.2, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= j19.3, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed") +
  geom_smooth(data= j19.4, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5, linetype="dotted")


j.19<-j.19+annotate("text", x=7.9, y=3.0, label="July 2019", size=7)

#dev.off()

#M1WK.dailymax<-lmer(Q10~dailymax+Species+(1|Pool), data=OneDay)
# Extract the prediction data frame
#pred.mm <- ggpredict(M1WK.dailymax, terms = c("dailymax"))  # this gives overall predictions for the model

s.19<-(ggplot() + 
         geom_point(data = s19, size=5,                     # adding the raw data (scaled values)
                    aes(x = dailymax, y = Q10, shape = Species, color=Species)) + 
         labs(x = "", 
              title = "") + scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name="", limits=c(0.5, 3.0))+
         scale_x_continuous(name="", limits=c(5, 25)) +
         theme_bw()+ 
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)


#pdf("s.19.pdf",height=5,width=6)
s.19<-s.19+ theme(axis.text.x = element_text(size=20),
                  axis.text.y = element_text(size=20)) 

s19.2<-filter(OneWeek, Species == "Mussel" & Timepoint == "19-Sep")
s19.3<-filter(OneWeek, Species == "Hermit" & Timepoint == "19-Sep")
s19.4<-filter(OneWeek, Species == "Littorine" & Timepoint == "19-Sep")

s.19<-s.19 + geom_smooth(data= s19.2, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= s19.3, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed") +
  geom_smooth(data= s19.4, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5, linetype="dotted")


s.19<-s.19+annotate("text", x=7.9, y=3.0, label="Sept 2019", size=7)

#dev.off()



pdf("TH_ALL1WEEKMAX.pdf",height=8,width=10)
#NEED Package - gridExtra() grid() and ggpubr()
figure<-ggarrange(s.18,m.19,j.19,s.19,
                  common.legend = TRUE, legend = "top", font.label = list(size=20, face="plain"))
annotate_figure(figure, bottom = text_grob("Daily Maximum of Pool Temperatures (°C)", size=20, just="centre", vjust = -0.5), top = text_grob("1-Week Time Interval", size=20,hjust=0.5),  left = text_grob(expression(Q[10]), rot=90, size=35, vjust =0.9))


dev.off()



#------------------------
#1 DAY ONLY-------------------------------------------------------------------------------------
#---------------------------------
OneDay<-filter(TH_Meta, TempDate == "1Day")
s18<-filter(OneDay, Timepoint == "18-Sep")
m19<-filter(OneDay, Timepoint == "19-Mar")
j19<-filter(OneDay, Timepoint == "19-Jul")
s19<-filter(OneDay, Timepoint == "19-Sep")

all=c("#FDE725FF","#21908CFF", "#440154FF")[OneDay$Species]


#PC1 & PC2------------
M1DY.Q10<-lmer(Q10~PC1+PC2+Timepoint+Timepoint*PC1+Timepoint*PC2+Species+Species*PC1+Species*PC2+(1|Pool), data=OneDay)
anova(M1DY.Q10)

summary(M1DY.Q10)
qqnorm(resid(M1DY.Q10))
hist(resid(M1DY.Q10))

#Simpler model - dropped one term
M1DY.Q10.2<-lmer(Q10~PC1+PC2+Timepoint+Timepoint*PC1+Timepoint*PC2+Species+Species*PC1+(1|Pool), data=OneDay)
anova(M1DY.Q10.2)

#Check - can drop?
anova(M1DY.Q10,M1DY.Q10.2) #Yes can drop

AIC(M1DY.Q10,M1DY.Q10.2)

#Simpler model - dropped another term
M1DY.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+Timepoint*PC1+Timepoint*PC2+Species+(1|Pool), data=OneDay)
anova(M1DY.Q10.3)

#Check - can drop?
anova(M1DY.Q10.2,M1DY.Q10.3) #Yes can drop

AIC(M1DY.Q10.2,M1DY.Q10.3)


#Simpler model - dropped another term
M1DY.Q10.4<-lmer(Q10~PC1+PC2+Timepoint+Timepoint*PC1+Species+(1|Pool), data=OneDay)
anova(M1DY.Q10.4)

summary(M1DY.Q10.4)
qqnorm(resid(M1DY.Q10.4))
qqline(resid(M1DY.Q10.4))
hist(resid(M1DY.Q10.4))
ad.test(resid(M1DY.Q10.4))

#Check - can drop?
anova(M1DY.Q10.3,M1DY.Q10.4) #Yes can drop

AIC(M1DY.Q10.3,M1DY.Q10.4)

Anova(M1DY.Q10.4, type=3)

#Season effect-----------------------
M1DY.Q10.emm.n<-emmeans(M1DY.Q10.4, specs = pairwise~Timepoint)
M1DY.Q10.emm.n

M1DY.Q10.emm.n<-emmeans(M1DY.Q10.4, pairwise~Species)

M1DY.Q10.emm.n
emmeans(M1DY.Q10.4, pairwise~Species)

#contrast           estimate     SE  df t.ratio p.value
#Hermit - Littorine   0.1392 0.0947 106 1.469   0.3098 
#Hermit - Mussel      0.2285 0.0862 103 2.651   0.0250 
#Littorine - Mussel   0.0894 0.0883 103 1.012   0.5708 

M1DY.Q10.emm.n<-emmeans(M1DY.Q10.4, ~Timepoint*PC1)
pairs(M1DY.Q10.emm.n, by=c("PC1"))

#Simpler model - dropped another term--------------CANT DROP IT  - NO
M1DY.Q10.5<-lmer(Q10~PC1+PC2+Timepoint+Species+(1|Pool), data=OneDay)
anova(M1DY.Q10.5)

#Check - can drop?
anova(M1DY.Q10.4,M1DY.Q10.5) #NO can drop


s.18<-(ggplot() + 
         geom_point(data = s18,  size=5, aes(x = dailymax, y = Q10, shape=Species, color=Species))+
         labs(x = "", y = "", title = "") + scale_colour_manual(labels = c("Hermit Crab", "Snail", "Mussel"), values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_shape(labels = c("Hermit Crab", "Snail", "Mussel")) +
         scale_y_continuous(name="", limits=c(0.5, 3)) +
         scale_x_continuous(name="", limits=c(5, 30))+
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)


s.18<-s.18 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20)) + theme(legend.title = element_text(color = "black", size = 20)) + theme(legend.text = element_text(size=20))


s18.2<-filter(OneDay, Species == "Mussel" & Timepoint == "18-Sep")
s18.3<-filter(OneDay, Species == "Hermit" & Timepoint == "18-Sep")

s.18<-s.18 + geom_smooth(data= s18.2, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= s18.3, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed")




#pdf("s.18.pdf",height=5,width=6)
s.18<-s.18+ theme(axis.text.x = element_text(size=20),
                  axis.text.y = element_text(size=20)) + annotate("text", x=8, y=3, label="Sept 2018", size=7)
#dev.off()


m.19<-(ggplot() + 
         geom_point(data = m19, size=5,                   
                    aes(x = dailymax, y = Q10, shape= Species, color=Species)) + 
         labs(x = "", title = "") + 
         scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name=" ", limits=c(0.5, 3)) +
         scale_x_continuous(name="", limits=c(5, 30))+
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)


#pdf("m.19.pdf",height=5,width=6)
m.19<-m.19 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20))

m19.2<-filter(OneDay, Species == "Mussel" & Timepoint == "19-Mar")
m19.3<-filter(OneDay, Species == "Hermit" & Timepoint == "19-Mar")
m19.4<-filter(OneDay, Species == "Littorine" & Timepoint == "19-Mar")

m.19<-m.19 + geom_smooth(data= m19.2, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= m19.3, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed") +
  geom_smooth(data= m19.4, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5, linetype="dotted")



m.19<-m.19+annotate("text", x=8.7, y=3, label="March 2019", size=7)
#dev.off()

j.19<-(ggplot() + 
         geom_point(data = j19, size=5,                      # adding the raw data (scaled values)
                    aes(x = dailymax, y = Q10, shape = Species, color=Species)) + 
         labs(x = "", y = "", 
              title = "") + scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name="", limits=c(0, 3)) +
         scale_x_continuous(name="", limits=c(5, 35))+
         theme_bw()+
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)

#pdf("j.19.pdf",height=5,width=6)
j.19<-j.19 + theme(axis.text.x = element_text(size=20),
                   axis.text.y = element_text(size=20))

j19.2<-filter(OneDay, Species == "Mussel" & Timepoint == "19-Jul")
j19.3<-filter(OneDay, Species == "Hermit" & Timepoint == "19-Jul")
j19.4<-filter(OneDay, Species == "Littorine" & Timepoint == "19-Jul")

j.19<-j.19 + geom_smooth(data= j19.2, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= j19.3, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed") +
  geom_smooth(data= j19.4, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5, linetype="dotted")


j.19<-j.19+annotate("text", x=8, y=3, label="July 2019", size=7)

#dev.off()

#M1WK.dailymax<-lmer(Q10~dailymax+Species+(1|Pool), data=OneDay)
# Extract the prediction data frame
#pred.mm <- ggpredict(M1WK.dailymax, terms = c("dailymax"))  # this gives overall predictions for the model

s.19<-(ggplot() + 
         geom_point(data = s19, size=5,                     # adding the raw data (scaled values)
                    aes(x = dailymax, y = Q10, shape = Species, color=Species)) + 
         labs(x = "", 
              title = "") + scale_colour_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
         scale_y_continuous(name="", limits=c(0.5, 3)) +
         scale_x_continuous(name="", limits=c(5, 30)) +
         theme_bw()+ 
         theme(axis.line = element_line(colour = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank())
)


#pdf("s.19.pdf",height=5,width=6)
s.19<-s.19+ theme(axis.text.x = element_text(size=20),
                  axis.text.y = element_text(size=20)) 

s19.2<-filter(OneDay, Species == "Mussel" & Timepoint == "19-Sep")
s19.3<-filter(OneDay, Species == "Hermit" & Timepoint == "19-Sep")
s19.4<-filter(OneDay, Species == "Littorine" & Timepoint == "19-Sep")

s.19<-s.19 + geom_smooth(data= s19.2, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
                         color="#440154FF", size= 1.5) + 
  geom_smooth(data= s19.3, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#FDE725FF", size= 1.5, linetype="dashed") +
  geom_smooth(data= s19.4, aes(x=dailymax, y=Q10), method="lm", formula = y~x, se=FALSE,
              color="#21908CFF", size= 1.5, linetype="dotted")


s.19<-s.19+annotate("text", x=8, y=3, label="Sept 2019", size=7)

#dev.off()



pdf("TH_ALL1DAY.pdf",height=8,width=10)
#NEED Package - gridExtra() grid() and ggpubr()
figure<-ggarrange(s.18,m.19,j.19,s.19,
                  common.legend = TRUE, legend = "top", font.label = list(size=20, face="plain"))
annotate_figure(figure, bottom = text_grob("Daily Maximum of Pool Temperatures (°C)", size=20, just="centre", vjust = -0.5), top = text_grob("1-Day Time Interval", size=20,hjust=0.5),  left = text_grob(expression(Q[10]), rot=90, size=35, vjust =0.9))


dev.off()


#BOXPLOTS-----------------------------------------------
grid.arrange(pc1,pc2)

quartz()

Qseas<-OneDay%>%
  mutate(Timepoint = fct_relevel(Timepoint, "18-Sep", "19-Mar", "19-Jul", 
                                 "19-Sep"))%>%
  ggplot(aes(x=Timepoint, y=Q10)) + labs(y=expression(Q[10])) +
  geom_boxplot(alpha=0) +
  geom_jitter(alpha=0.3,color="grey6")+
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=5, color="black") +
  scale_x_discrete(name = "Season", labels=c("Sept 2018", "March 2019", "July 2019", "Sept 2019")) +
  theme(axis.text.x = element_text(size=20), axis.title=element_text(size=20),
        axis.text.y = element_text(size=20)) + theme(legend.position = "none") 

Qseas

pdf("Qseasonbox.pdf",height=5,width=9)
Qseas

dev.off()



#SPECIES BOXPLOT--------------------------------------
grid.arrange(pc1,pc2)


spp<-ggplot(OneDay, aes(x=Species, y=Q10, fill=Species)) +
  geom_boxplot() + ylim(0.5,3.0) +
  scale_fill_manual(values=c("#FDE725FF","#21908CFF","#440154FF")) +
  geom_jitter(alpha=0.6)  + xlab("Species")+ labs(y=expression(Q[10]), size=15) +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_x_discrete(name = "Species", labels=c("Hermit Crab", "Snail", "Mussel"))

spp <- spp +theme(axis.text.x = element_text(size=20), axis.title=element_text(size=20),
                  axis.text.y = element_text(size=20)) + theme(legend.position = "none") +
  annotate("text", x=1, y=2.8, label="a", size=10) +
  annotate("text", x=2, y=2.8, label="ab", size=10) +
  annotate("text", x=3, y=2.8, label="b", size=10)
spp

pdf("sppbox.pdf",height=5,width=6)


dev.off()



pc1<-OneDay%>%
  mutate(Timepoint = fct_relevel(Timepoint, "18-Sep", "19-Mar", "19-Jul", 
                                 "19-Sep"))%>%
  ggplot(aes(x=Timepoint, y=Q10))+
  geom_boxplot(alpha=0) +
  geom_jitter(alpha=0.6,color="black")

pc1

ggplot(data = OneWeek, aes(x=Timepoint, y=Q10, color=Species))+
  geom_boxplot(alpha=0) +
  geom_jitter(alpha=0.6,color="black")


#PC2---------------
ggplot(data = OneWeek, aes(x=Timepoint, y=Range))+
  geom_boxplot(alpha=0) +
  geom_jitter(alpha=0.6,color="black")

ggplot(data = OneWeek, aes(x=Timepoint, y=St.Dev))+
  geom_boxplot(alpha=0) +
  geom_jitter(alpha=0.6,color="black")

pc2<-OneWeek%>%
  mutate(Timepoint = fct_relevel(Timepoint, "Sep-18", "Mar-19", "Jul-19", 
                                 "Sep-19"))%>%
  ggplot(aes(x=Timepoint, y=Var))+
  geom_boxplot(alpha=0) +
  geom_jitter(alpha=0.6,color="black")

OneWeek%>%
  mutate(Timepoint = fct_relevel(Timepoint, "Sep-18", "Mar-19", "Jul-19", 
                                 "Sep-19"))%>%
  ggplot(aes(x=Var, y=Q10))+
  geom_point(alpha=0) +
  geom_jitter(alpha=0.6,color="black")

OneWeek%>%
  mutate(Timepoint = fct_relevel(Timepoint, "Sep-18", "Mar-19", "Jul-19", 
                                 "Sep-19"))%>%
  ggplot(aes(x = Range, y = Q10, shape=Species)) + geom_point() +
  geom_smooth(method="lmer")+
  facet_wrap(~Timepoint, nrow=1)


#SLOPE LOG PC1---------------
M1WK.1SL<-lmer(slopelog~PC1+Timepoint+Species+Timepoint*PC1+(1|ID), data=OneWeek)
summary(M1WK.1SL)
#Summary output---------
#NO SIG beside intercept
anova(M1WK.1SL)

M1WK.1SL.2<-lmer(slopelog~PC1+PC2+Timepoint+Species+Timepoint*PC1+Timepoint*PC2+(1|ID), data=OneWeek)
summary(M1WK.1SL.2)
#Summary output---------


#Plots------------------
quartz()
par(mfrow= c(1,2))
plot(Q10~PC1,pch=21,lwd=1.5, col="black", main="Q10", bg=all,data=OneWeek)
plot(Q10~PC2,pch=21,lwd=1.5, col="black", main="Q10", bg=all,data=OneWeek)

plot(slopelog~PC1,pch=21,lwd=1.5, col="black",bg=all, main="SlopeLog",data=OneWeek)
plot(slopelog~PC2,pch=21,lwd=1.5, col="black",bg=all, main="SlopeLog",data=OneWeek)





############################----------------------------------------------
#Counts for pools and Pool summary Stats -----------------------------------------------------------
#----------------------------------
library(pastecs) # gives descriptive stats
#3MONTHS
PoolStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "18-Sep")
PoolStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Mar")
PoolStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Jul")
PoolStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Sep")

#2MONTHS
PoolStats<-filter(TH_Meta, TempDate == "2Month" & Timepoint == "18-Sep")
PoolStats<-filter(TH_Meta, TempDate == "2Month" & Timepoint == "19-Mar")
PoolStats<-filter(TH_Meta, TempDate == "2Month" & Timepoint == "19-Jul")
PoolStats<-filter(TH_Meta, TempDate == "2Month" & Timepoint == "19-Sep")

#1MONTH
PoolStats<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "18-Sep")
PoolStats<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Mar")
PoolStats<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Jul")
PoolStats<-filter(TH_Meta, TempDate == "1Month" & Timepoint == "19-Sep")

#1WEEK
PoolStats<-filter(TH_Meta, TempDate == "1Week" & Timepoint == "18-Sep")
PoolStats<-filter(TH_Meta, TempDate == "1Week" & Timepoint == "19-Mar")
PoolStats<-filter(TH_Meta, TempDate == "1Week" & Timepoint == "19-Jul")
PoolStats<-filter(TH_Meta, TempDate == "1Week" & Timepoint == "19-Sep")

#1DAY
PoolStats1<-filter(TH_Meta, TempDate == "1Day" & Timepoint == "18-Sep")
PoolStats2<-filter(TH_Meta, TempDate == "1Day" & Timepoint == "19-Mar")
PoolStats3<-filter(TH_Meta, TempDate == "1Day" & Timepoint == "19-Jul")
PoolStats4<-filter(TH_Meta, TempDate == "1Day" & Timepoint == "19-Sep")

stat.desc(PoolStats1$Q10) #Sept 18: 1.59 +- 0.24
stat.desc(PoolStats2$Q10) #Mar19: 1.32 +- 0.25
stat.desc(PoolStats3$Q10) #July19: 1.38 +- 0.34
stat.desc(PoolStats4$Q10) #Sept 19: 1.50 +- 0.46

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

stat.desc(TH_Meta$Q10)

#Q10 Average and SD-------------------------------------------

#Hermits-------------
QStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "18-Sep" & Species == "Hermit")
QStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Mar" & Species == "Hermit")
QStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Jul" & Species == "Hermit")
QStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Sep" & Species == "Hermit")

#Mussels----------------
QStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "18-Sep" & Species == "Mussel")
QStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Mar" & Species == "Mussel")
QStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Jul" & Species == "Mussel")
QStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Sep" & Species == "Mussel")

#Littorines------------------
QStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Mar" & Species == "Littorine")
QStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Jul" & Species == "Littorine")
QStats<-filter(TH_Meta, TempDate == "3Month" & Timepoint == "19-Sep" & Species == "Littorine")

#Herms---------------
#Sept18
stat.desc(QStats$Q10) #mean=1.529 sd=0.20
#Mar 19
stat.desc(QStats$Q10) #mean=1.408 sd=0.22
#Jul 19
stat.desc(QStats$Q10) #mean=1.498 sd=0.27
#Sep 19
stat.desc(QStats$Q10) #mean=1.667 sd=0.40

#Mussels-----------------
#Sept18
stat.desc(QStats$Q10) #mean=1.682 sd=0.29
#Mar 19
stat.desc(QStats$Q10) #mean=1.314 sd=0.29
#Jul 19
stat.desc(QStats$Q10) #mean=1.263 sd=0.34
#Sep 19
stat.desc(QStats$Q10) #mean=1.219 sd=0.33

#Littorines-----------------
#Mar 19
stat.desc(QStats$Q10) #mean=1.241 sd=0.22
#Jul 19
stat.desc(QStats$Q10) #mean=1.378 sd=0.39
#Sep 19
stat.desc(QStats$Q10) #mean=1.721 sd=0.58

#------------------------
#SPECIES SPECIFIC------------------------------------------------------
#---------------------------------
#------------------------

#LMER Mussel Models----------------------------------------
Muss_ThreeMo<-filter(TH_Meta, TempDate == "3Month" & Species == "Mussel" & Timepoint == "18-Sep")
Muss_ThreeMo<-subset(Muss_ThreeMo, Timepoint!="18-Sep")
Muss_ThreeMo<-subset(Muss_ThreeMo, Pool!="Pool27")
Muss_ThreeMo<-subset(Muss_ThreeMo, Pool!="Pool28")
Muss_ThreeMo<-subset(Muss_ThreeMo, Pool!="Pool7")
Muss_ThreeMo<-subset(Muss_ThreeMo, Pool!="Pool36")
Muss_ThreeMo<-subset(Muss_ThreeMo, Pool!="Pool6")
OneMo<-filter(TH_Meta, TempDate == "Muss_ThreeMo")
all=c( "#440154FF")



Muss_M3<-lmer(Q10~PC1+PC2+Timepoint+Timepoint*PC2+Timepoint*PC1+(1|Pool), data=Muss_ThreeMo)
summary(Muss_M3)
anova(Muss_M3)
hist(resid(Muss_M3))
qqnorm(resid(Muss_M3))

plot(Q10~PC1, data=Muss_ThreeMo)



###Simplify-----------------
Muss_M3.2<-lmer(Q10~PC1+PC2+Timepoint+Timepoint*PC2+(1|Pool), data=Muss_ThreeMo)
summary(Muss_M3.2)
anova(Muss_M3.2)

hist(resid(Muss_M3.2))
qqnorm(resid(Muss_M3.2))

anova(Muss_M3, Muss_M3.2)

AIC(Muss_M3, Muss_M3.2) #NOT big enough difference




Muss_M3.3<-lmer(Q10~PC1+PC2+Timepoint+(1|Pool), data=Muss_ThreeMo)
summary(Muss_M3.3)
anova(Muss_M3.3)
anova(Muss_M3.2, Muss_M3.3)

hist(resid(Muss_M3.3))
qqnorm(resid(Muss_M3.3)) # Better fit than Q10-18

AIC(Muss_M3.2, Muss_M3.3)

plot(Q10~Timepoint, col="purple", data=Muss_ThreeMo)

#####Not a GREAT fit..........--------------
Muss_M3.18<-lmer(Q1018~PC1+PC2+Timepoint*PC1*PC2+(1|Pool), data=Muss_ThreeMo)
summary(Muss_M3.18)
anova(Muss_M3.18)

hist(resid(Muss_M3.18))
qqnorm(resid(Muss_M3.18))
plot(Muss_M3.18)



#-------------------------------------
Muss_M3.18<-lmer(log(MO218)~PC1+PC2+Timepoint*PC1+Timepoint*PC2+(1|Pool), data=Muss_ThreeMo)
summary(Muss_M3.18)
anova(Muss_M3.18)

hist(resid(Muss_M3.18))
qqnorm(resid(Muss_M3.18))
plot(Muss_M3.18)

plot(MO210~PC2, pch=16, data=Muss_ThreeMo)

#Summmary output------
#No Sig

plot(Q10~Timepoint,pch=16,lwd=1.5, col="purple", main="Q10",data=Muss_ThreeMo)
plot(Q1018~PC1,pch=16,lwd=1.5, col="purple", main="Q10",data=Muss_ThreeMo)
plot(slopelog~PC2,pch=21,lwd=1.5, col="black", main="slopelog", bg=all,data=Muss_ThreeMo)

#Muss_M3.lm<-lm(Q10~PC1, data=Muss_ThreeMo)
#summary(Muss_M3.lm)

Muss_M3.lm2<-lm(slopelog~PC1, data=Muss_ThreeMo)
summary(Muss_M3.lm2)
abline(Muss_M3.lm2)

Muss_TwoMo<-filter(TH_Meta, TempDate == "2Month" & Species == "Mussel")
Muss_TwoMo<-subset(Muss_TwoMo, Pool!="Pool24")
Muss_M2<-lmer(Q10~PC1+PC2+Timepoint+Timepoint*PC1+Timepoint*PC2+(1|Pool), data=Muss_TwoMo)
anova(Muss_M2)

plot(Q10~PC2, pch=16, data=Muss_TwoMo)

Muss_OneMo<-filter(TH_Meta, TempDate == "1Month" & Species == "Mussel")
Muss_OneMo<-subset(Muss_OneMo, Pool!="Pool24")
Muss_M1<-lmer(Q10~PC1+PC2+Timepoint+PC1*Timepoint+Timepoint*PC2+(1|Pool), data=Muss_OneMo)
anova(Muss_M1)

Muss_OneWk<-filter(TH_Meta, TempDate == "1Week" & Species == "Mussel")
Muss_OneWk<-subset(Muss_OneWk, Pool!="Pool24")
Muss_M1.wk<-lmer(Q10~PC1+PC2+Timepoint+PC1+Timepoint*PC2+(1|Pool), data=Muss_OneWk)
anova(Muss_M1.wk)

quartz()
plot(Q10~PC2, pch=16, data=Muss_OneWk)
plot(Q10~PC1, pch=16, data=Muss_OneWk)


#GLMER Littorine Models----------------------------------------
Litt_ThreeMo<-filter(TH_Meta, TempDate == "3Month" & Species == "Littorine")
Litt_M3<-lmer(Q10~PC1+PC2+Timepoint+PC1*Timepoint+PC2*Timepoint+(1|Pool), data=Litt_ThreeMo)
anova(Litt_M3)

plot(Q10~PC1, pch=16, data=Litt_ThreeMo)

Litt_TwoMo<-filter(TH_Meta, TempDate == "2Month" & Species == "Littorine")
Litt_M2<-lmer(Q10~PC1+PC2+PC1*Timepoint+PC2*Timepoint+(1|Pool), data=Litt_TwoMo)
anova(Litt_M2)

Litt_OneMo<-filter(TH_Meta, TempDate == "1Month" & Species == "Littorine")
Litt_M1<-lmer(Q10~PC1+PC2+PC1*Timepoint+PC2*Timepoint+(1|Pool), data=Litt_OneMo)
anova(Litt_M1)

Litt_OneWk<-filter(TH_Meta, TempDate == "1Week" & Species == "Littorine")
Litt_M1.wk<-lmer(Q10~PC1+PC2+Timepoint+PC1*Timepoint+PC2*Timepoint+(1|Pool), data=Litt_OneWk)
anova(Litt_M1.wk)

Litt_OneDay<-filter(TH_Meta, TempDate == "1Day" & Species == "Littorine")
Litt_M1.dy<-lmer(Q10~PC1+PC2+Timepoint+PC1*Timepoint+PC2*Timepoint+(1|Pool), data=Litt_OneDay)
anova(Litt_M1.dy)


plot(Q10~PC1, pch=16, data=Litt_OneDay)

#GLMER Hermit Models----------------------------------------
Hermit_ThreeMo<-filter(TH_Meta, TempDate == "3Month" & Species == "Hermit")
Hermit_M3<-lm(Q10~PC1*PC2,data=Hermit_ThreeMo)
anova(Hermit_M3)

plot(Q10~PC2, pch=16, data=Hermit_ThreeMo)

Hermit_TwoMo<-filter(TH_Meta, TempDate == "2Month" & Species == "Hermit")
Hermit_M2<-lmer(Q10~PC1+PC2+Timepoint+Timepoint*PC1+Timepoint*PC2+(1|ID),data=Hermit_TwoMo)
anova(Hermit_M2)



Hermit_OneMo<-filter(TH_Meta, TempDate == "1Month" & Species == "Hermit")
Hermit_M1<-lmer(Q10~PC1+PC2+Timepoint+(1|Pool), data=Hermit_OneMo)
anova(Hermit_M1)

Hermit_M1<-lmer(Q10~PC1+PC2+Timepoint+Timepoint*PC1+Timepoint*PC2+(1|Pool), data=Hermit_OneMo)
summary(Hermit_M1)

plot(Q10~PC1, pch=16, data=Hermit_TwoMo)

Hermit_OneWk<-filter(TH_Meta, TempDate == "1Week" & Species == "Hermit")
Hermit_M1.wk<-lm(log(MO2)~PC1+PC2, data=Hermit_OneWk)
anova(Hermit_M1.wk)

qqnorm(resid(Hermit_M1.wk))

plot(Q10~PC2, pch=16, data=Hermit_OneWk)

#Plotting-------------------------
#Lines
quartz()
ggplot(data = ThreeMo, aes(x=Species, y=Q10))+
  geom_boxplot(alpha=0) +
  geom_jitter(alpha=0.3,color="grey56")

#Scatterplot
quartz()
ggplot(data = subset(TH_Meta,Species=="Mussel" & TempDate=="3Month"), mapping=aes(x=Timepoint, y=Q10))+
  geom_boxplot(alpha=0) +
  geom_jitter(alpha=0.3,color="grey56")

#Scatterplot
quartz()
ggplot(data = subset(TH_Meta,TempDate=="3Month"), mapping=aes(x=Timepoint, y=Q10, color=Species))+
  geom_boxplot(alpha=0) +
  geom_jitter(alpha=0.3,color="grey56")


pdf("Q10vs90thMax.pdf",height=8,width=9)
plot(Q10~X90max, pch=16, data=ThreeMo)

dev.off()


#Timepoints-------------
model3<-glm(slope~Max+Timepoint, data = subset(TH_Meta, Species=="Mussel" &TempDate=="3Month"))
summary(model3)

Muss_M3<-gam(Q10~PC1+PC2+TempDate+Species, data=Muss_ThreeMo)
summary(Muss_M3)

M_Mar<-ggplot(data= Muss_ThreeMo, aes(x=PC1, y=Q10))+
  geom_point(aes(color=Species))+
  ggtitle("MMarch")+
  xlab("PC1") +ylab("slopelog")

plot(M_Mar)

library(gridExtra)

quartz()
M<-ggplot(data = subset(TH_Meta, Species=="Mussel" & TempDate=="3Month"), aes(x=Max, y=slope, color=Timepoint))+
  geom_point()+
  geom_smooth(method="glm")+
  ggtitle("Mussels")+
  xlab("Maximum Pool Temperature°C") +ylab("Slope of MO2")

H<-ggplot(data = subset(TH_Meta, Species=="Hermit" & TempDate=="3Month"), aes(x=Max, y=slope, color=Timepoint))+
  geom_point()+
  geom_smooth(method="glm")+
  ggtitle("Hermits")+
  xlab("Maximum Pool Temperature°C") +ylab("Slope of MO2")

L<-ggplot(data = subset(TH_Meta, Species=="Littorine" & TempDate=="3Month"), aes(x=Max, y=slope, color=Timepoint))+
  geom_point()+
  geom_smooth(method="glm")+
  ggtitle("Littorines")+
  xlab("Maximum Pool Temperature°C") +ylab("Slope of MO2")


grid.arrange(M,H,L)

pdf("AllSpecies.pdf",height=8,width=9)
grid.arrange(M,H,L)
dev.off()


#------------------------
#SPECIES SPECIFIC------------------------------------------------------
#---------------------------------
#------------------------

#Hermit Crabs--------------
#3MONTH ONLY------------------------------------------------------
#---------------------------------
ThreeMo<-filter(TH_Meta, TempDate == "3Month")
Herms<-filter(ThreeMo, Species == "Hermit")
all=c("#FDE725FF","#21908CFF","#440154FF")[ThreeMo$Species]

M1H.Q10.1<-lmer(Q10~PC1+PC2+Timepoint+(1|Pool), data=Herms)
anova(M1H.Q10.1)

plot(Q10~PC2, pch=16, data=Herms)

#2MONTH ONLY------------------------------------------------------
#---------------------------------
TwoMo<-filter(TH_Meta, TempDate == "2Month")
Herms.2mo<-filter(TwoMo, Species == "Hermit")

M1H.Q10.2<-lmer(Q10~PC1+PC2+Timepoint+PC1*Timepoint+PC2*Timepoint+(1|Pool), data=Herms.2mo)
anova(M1H.Q10.2)

plot(Q10~PC2, pch=16, data=Herms.2mo)
plot(Q10~PC1, pch=16, data=Herms.2mo)


#1MONTH ONLY------------------------------------------------------
#---------------------------------
OneMo<-filter(TH_Meta, TempDate == "1Month")
Herms.1mo<-filter(OneMo, Species == "Hermit")

M1H.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+PC1*Timepoint+PC2*Timepoint+(1|Pool), data=Herms.1mo)
anova(M1H.Q10.3)

plot(Q10~PC2, pch=16, data=Herms.1mo)
plot(Q10~PC1, pch=16, data=Herms.1mo)


#1MONTH ONLY------------------------------------------------------
#---------------------------------
OneWeek<-filter(TH_Meta, TempDate == "1Week")
Herms.1wk<-filter(OneWeek, Species == "Hermit")

M1H.Q10.3<-lmer(Q10~PC1+PC2+Timepoint+PC1*Timepoint+PC2*Timepoint+(1|Pool), data=Herms.1wk)
anova(M1H.Q10.3)

plot(Q10~PC2, pch=16, data=Herms.1wk)
plot(Q10~PC1, pch=16, data=Herms.1wk)

OneWeek<-filter(TH_Meta, TempDate == "1Week")


#########################
#Adding more temp metrics-----------------------------
#########################
setwd("/Users/racinerangel/Desktop/Temperature")
musselJuly1WK<-read.csv("FullTempJuly1WKM2.csv")
setwd("/Users/racinerangel/Desktop/Thermal History")
TH_Meta<-read.csv("ThermalHistory_All.csv", na.strings = "nd", header=T)
TH_Meta<-subset(TH_Meta, ID!="Littorine")


mussju<-musselJuly1WK %>% select(Pool, DailyMaxTemp, Daily90MaxTemp, Daily95MaxTemp, DailyMinTemp, DailyRangeTemp)

Muss_OneWk<-filter(TH_Meta, Timepoint=="19-Jul" & TempDate == "1Week" & Species == "Mussel")

Total<-merge(Muss_OneWk, mussju, by="Pool", all.x=FALSE)
Total<-Total %>% select(Pool,Timepoint, TempDate, DailyMaxTemp, Daily90MaxTemp, Daily95MaxTemp, DailyMinTemp, DailyRangeTemp)

TH_Meta<-merge(TH_Meta, Total, all=TRUE)

write.csv(TH_Meta, file = "TH_Meta_All.csv")

#########################
#Adding Jul 2 Month Temp Metrics-----------------------------
#########################
setwd("/Users/racinerangel/Desktop/Temperature")
musselJuly2MO<-read.csv("FullTempJuly2moM2.csv")
setwd("/Users/racinerangel/Desktop/Thermal History")
TH_Meta<-read.csv("TH_Meta_All.csv", na.strings = "nd", header=T)


mussju<-musselJuly2MO %>% select(Pool, DailyMaxTemp, Daily90MaxTemp, Daily95MaxTemp, DailyMinTemp, DailyRangeTemp)


Muss_TwoMo<-filter(TH_Meta, Timepoint=="19-Jul" & TempDate == "2Month" & Species == "Mussel")


merge(TH_Meta, Total, by=c("Pool","DailyMaxTemp", "Daily90MaxTemp", "Daily95MaxTemp", "DailyMinTemp", "DailyRangeTemp"), all.x=TRUE)

#Muss_TwoMo<-Muss_TwoMo %>% select(-DailyMaxTemp, -Daily90MaxTemp, -Daily95MaxTemp, -DailyMinTemp, -DailyRangeTemp)


Total<-merge(Muss_TwoMo, mussju, by="Pool", all=FALSE)
Total<-Total %>% select(Pool,Timepoint, TempDate, DailyMaxTemp, Daily90MaxTemp, Daily95MaxTemp, DailyMinTemp, DailyRangeTemp)


merge(TH_Meta, Total, by=c("Pool","DailyMaxTemp", "Daily90MaxTemp", "Daily95MaxTemp", "DailyMinTemp", "DailyRangeTemp"), all.x=TRUE)


#by=c("DailyMaxTemp", "Daily90MaxTemp", "Daily95MaxTemp", "DailyMinTemp", "DailyRangeTemp")


#PCA-------------------------------
####################################################  
#   Script to run a PCA                            #
#   09/21/19 From Piper                           #
####################################################

library(lattice)
library(ggplot2)
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(ggthemes)

# clear workspace
rm(list=ls())

# Research Question: How does long-term temperature affect where we find mussels in the intertidal? 

# The Data - We measured the highest point on the shore where mussels are found at 19 sites along the West Coast. We also collected yearly temperature data at each site, measured as means, maximums, and ranges at different time persiods. 

## Mean = Yearly Average
## P90 = Yearly 90th Percentile
## Max = Yearly Maximum
## MonAvg = Monthly Average
## MeanMonMax = Mean Monthly Maximum
## DailyMean = Daily Average
## MeanDMax = Average Daily Maximum
## Range = Yearly Range
## DailyRange = Average Daily Range
## MeanRange = Average Monthly Range


YearData <- read.csv("ThermalHistory_All3.csv")
YearData<-subset(YearData, ID!="Littorine")
YearData<-subset(YearData, ID!="Mussel6")
# We want to characterize the sites by their temperature, but we don't know which of our measurements is most important. A PCA reduces the dimensionality and can tell us which variables drive the patterns

# PCAs

PCA <- prcomp(YearData[,c(8:23)], scale = TRUE, center = TRUE)

# Proportion of data explained by each axis
summary(PCA)

pdf("TempPCA.pdf",height=8,width=9)
dev.off()

# plot
fviz_pca_biplot(PCA, xlab = "PC1 (73.0%)", ylab = "PC2 (22.0%)", repel = TRUE, col.var = "black", col.ind = "gray", title = " ", label = "var") + theme_base()


# We want to use our PCA as a predictor variable for how temperature affects the upper limits of the mussel bed at each of our  sites 

# Pull out the individual point scores 
PCAFrame<-data.frame(YearData$ID,YearData$Timepoint,YearData$TempDate,-PCA$x[,1:2])

# We want to make sure the sites line up correctly and then merge our data into one dataset
colnames(PCAFrame)[1:3]<-c('ID', "Timepoint","TempDate")

AllData <- merge(YearData, PCAFrame, by = c("ID","Timepoint","TempDate"), all.x = TRUE) #Fixed - It works!

#Getting AllData csv
write.csv(AllData, file = "ThermalHistory_All3.csv")


# Now let's compare how temperature affects Q10 using our individual metrics

LM.Mean <- lm(TH_Meta$Q10 ~ TH_Meta$Mean) #average
summary(LM.Mean)
LM.Max <- lm(Q10 ~ Max, data = TH_Meta) #max
summary(LM.Max)
LM.Range <- lm(Q10 ~ St.Dev, data = TH_Meta) #range
summary(LM.Range)


# Now try with daily max. Based on your results above, do you think daily max would be significant?

LM.X90max <- lm(Q10 ~ X90max, data = AllData) 
summary(LM.X90max)



# We get variable answers depending on which metric we use. But we know that maximums, means, and ranges all contribute to our site's temperature though. What happens when we use our PCA?

LM.PC1 <- lm(Q10 ~ PC1, data = TH_Meta) #range
summary(LM.PC1)


# Is PC2 significant?

LM.PC2 <-
  
  
  # Now we can plot how Q10 change with temperature (as described by our PCA)
  
  plot <- plot(TH_Meta$PC1, TH_Meta$Q10)
new.Y<-predict(LM.PC1, se = TRUE, interval = 'confidence') 
x <- TH_Meta$PC1
x.ind<-order(x)
lines(x[x.ind], new.Y$fit[x.ind,1])

# Now let's make the plot pretty by adding a title and axis legends


#---------------------------
plot(PC1~X90max, pch=16, data=TH_Meta)
