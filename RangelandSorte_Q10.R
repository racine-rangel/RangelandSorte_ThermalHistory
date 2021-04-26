#################################################################
#Title: Q10 Analysis
#Purpose: Determine Q10 values for Hermit Crabs, Snails, and Mussels
#Created by: R. E. Rangel
#Created: Sept 2020
#Last edited: 26 April 2021
################################################################
##### Packages #####
#Clear workspace

#load libraries
library(respirometry) #Calculates Q10
library(nlme)     # non linear mixed effects model 
library(dplyr, warn.conflicts = FALSE) # filtering
library(lubridate) # date/time wrangling

#-----------------------------------------------------------------------------
####################################
########Q10 SEPTEMBER 2018##########
########Hermit Crabs and Mussels####
####################################
#----------------------------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Thermal History")
#----------------------------------------------------------------------------
MO2<- read.csv("RangelandSorte_MO2_All.csv", na.strings = "nd", header=T)

#Filter for Hermit Crabs first----------------------------------
Sept18<-filter(MO2, Timepoint == "18-Sep" & Species == "Hermit")

#Check Data-------------
str(Sept18)
Pool<-as.factor(Sept18$Pool)
Temp<-as.numeric(Sept18$Temp)

#This runs a function that calculates Q10 across experimental temperatures from 10C, 18C, and 26C
new_Q<-NULL
for(ind in unique(Sept18$Pool)){
  temp_dat<-Sept18[which(Sept18$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

#Matches Q10 with Pool ID
colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

#Double check everything lined up
head(data.frame(Q10_H))

#Hermit Crab Q10 values
write.csv(data.frame(Q10_H), file = "Q10_H_Sept2018.csv")

###########################
#####Mussels-----------------------------------------
############################
#Filter for Mussels----------------------------------
#This runs a function that calculates Q10 across experimental temperatures from 10C, 18C, and 26C
Sept18<-filter(MO2, Timepoint == "18-Sep" & Species == "Mussel")

new_Q<-NULL
for(ind in unique(Sept18$Pool)){
  temp_dat<-Sept18[which(Sept18$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_M_Sept2018.csv")



#-----------------------------------------------------------------------------
####################################
########Q10 MARCH 2019##########
########Hermit Crabs, Mussels, and Snails####
####################################
#----------------------------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Thermal History")
#----------------------------------------------------------------------------
MO2<- read.csv("RangelandSorte_MO2_All.csv", na.strings = "nd", header=T)

#Filter for Hermit Crabs first----------------------------------
March19<-filter(MO2, Timepoint == "19-Mar" & Species == "Hermit")

#Check Data-------------
str(March19)
Pool<-as.factor(March19$Pool)
Temp<-as.numeric(March19$Temp)

#This runs a function that calculates Q10 across experimental temperatures from 10C, 18C, and 26C
new_Q<-NULL
for(ind in unique(March19$Pool)){
  temp_dat<-March19[which(March19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

#Matches Q10 with Pool ID
colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

#Double check everything lined up
head(data.frame(Q10_H))

#Hermit Crab Q10 values
write.csv(data.frame(Q10_H), file = "Q10_H_March2019.csv")

###########################
#####Mussels-----------------------------------------
############################
#Filter for Mussels----------------------------------
#This runs a function that calculates Q10 across experimental temperatures from 10C, 18C, and 26C
March19<-filter(MO2, Timepoint == "19-Mar" & Species == "Mussel")

new_Q<-NULL
for(ind in unique(March19$Pool)){
  temp_dat<-March19[which(March19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_M_March2019.csv")


###########################
#####Snails-----------------------------------------
############################
#Filter for Snails----------------------------------
#This runs a function that calculates Q10 across experimental temperatures from 10C, 18C, and 26C
March19<-filter(MO2, Timepoint == "19-Mar" & Species == "Littorine")

new_Q<-NULL
for(ind in unique(March19$Pool)){
  temp_dat<-March19[which(March19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_L_March2019.csv")


#-----------------------------------------------------------------------------
####################################
########Q10 JULY 2019##########
########Hermit Crabs, Mussels, and Snails####
####################################
#----------------------------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Thermal History")
#----------------------------------------------------------------------------
MO2<- read.csv("RangelandSorte_MO2_All.csv", na.strings = "nd", header=T)

#Filter for Hermit Crabs first----------------------------------
July19<-filter(MO2, Timepoint == "19-Jul" & Species == "Hermit")

#Check Data-------------
str(July19)
Pool<-as.factor(July19$Pool)
Temp<-as.numeric(July19$Temp)

#This runs a function that calculates Q10 across experimental temperatures from 10C, 18C, and 26C
new_Q<-NULL
for(ind in unique(July19$Pool)){
  temp_dat<-July19[which(July19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

#Matches Q10 with Pool ID
colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

#Double check everything lined up
head(data.frame(Q10_H))

#Hermit Crab Q10 values
write.csv(data.frame(Q10_H), file = "Q10_H_July2019.csv")

###########################
#####Mussels-----------------------------------------
############################
#Filter for Mussels----------------------------------
#This runs a function that calculates Q10 across experimental temperatures from 10C, 18C, and 26C
July19<-filter(MO2, Timepoint == "19-Jul" & Species == "Mussel")

new_Q<-NULL
for(ind in unique(July19$Pool)){
  temp_dat<-July19[which(July19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_M_July2019.csv")


###########################
#####Snails-----------------------------------------
############################
#Filter for Snails----------------------------------
#This runs a function that calculates Q10 across experimental temperatures from 10C, 18C, and 26C
July19<-filter(MO2, Timepoint == "19-Jul" & Species == "Littorine")

new_Q<-NULL
for(ind in unique(July19$Pool)){
  temp_dat<-July19[which(July19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_L_July2019.csv")



#-----------------------------------------------------------------------------
####################################
########Q10 SEPTEMBER 2019##########
########Hermit Crabs, Mussels, and Snails####
####################################
#----------------------------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Thermal History")
#----------------------------------------------------------------------------
MO2<- read.csv("RangelandSorte_MO2_All.csv", na.strings = "nd", header=T)

#Filter for Hermit Crabs first----------------------------------
Sept19<-filter(MO2, Timepoint == "19-Sep" & Species == "Hermit")

#Check Data-------------
str(Sept19)
Pool<-as.factor(Sept19$Pool)
Temp<-as.numeric(Sept19$Temp)

#This runs a function that calculates Q10 across experimental temperatures from 10C, 18C, and 26C
new_Q<-NULL
for(ind in unique(Sept19$Pool)){
  temp_dat<-Sept19[which(Sept19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

#Matches Q10 with Pool ID
colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

#Double check everything lined up
head(data.frame(Q10_H))

#Hermit Crab Q10 values
write.csv(data.frame(Q10_H), file = "Q10_H_Sept2019.csv")

###########################
#####Mussels-----------------------------------------
############################
#Filter for Mussels----------------------------------
#This runs a function that calculates Q10 across experimental temperatures from 10C, 18C, and 26C
Sept19<-filter(MO2, Timepoint == "19-Sep" & Species == "Mussel")

new_Q<-NULL
for(ind in unique(Sept19$Pool)){
  temp_dat<-Sept19[which(Sept19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_M_Sept2019.csv")


###########################
#####Snails-----------------------------------------
############################
#Filter for Snails----------------------------------
#This runs a function that calculates Q10 across experimental temperatures from 10C, 18C, and 26C
Sept19<-filter(MO2, Timepoint == "19-Sep" & Species == "Littorine")

new_Q<-NULL
for(ind in unique(Sept19$Pool)){
  temp_dat<-Sept19[which(Sept19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_L_Sept2019.csv")

