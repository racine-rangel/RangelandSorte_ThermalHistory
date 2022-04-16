#################################################################
#Title: Q10 Analysis for Publication
#Purpose: Determine relationship between experimental temperatures and MO2
#Created by: R. E. Rangel
#Created: Sept 2020
#Last edited: 18 March 2022
################################################################
##### Packages #####
library(respirometry)
packageVersion('respirometry') #last ran using version 1.3.0
install.packages('respirometry') #Update package
#-----------------------------------------------------------------------------
####################################
########Q10 MARCH 2019##########
####################################
#----------------------------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Thermal History")
#----------------------------------------------------------------------------
TH<- read.csv("RangelandSorte_MO2_All.csv", na.strings = "nd", header=T)

####################
#####LITTORINES#######
####################

Litt_M19<-filter(TH, Species == "Littorine" & Timepoint == '19-Mar')

new_Q<-NULL
for(ind in unique(Litt_M19$Pool)){
  temp_dat<-Litt_M19[which(Litt_M19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_L_March2019_TH.3.csv")

########Q10 JULY 2019##########
#----------------------------------------------------------------------------

Litt_J19<-filter(TH, Species == "Littorine" & Timepoint == '19-Jul')

new_Q<-NULL
for(ind in unique(Litt_J19$Pool)){
  temp_dat<-Litt_J19[which(Litt_J19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_L_July2019_TH.3.csv")

########Q10 SEPTEMBER 2019##########
#----------------------------------------------------------------------------

Litt_S19<-filter(TH, Species == "Littorine" & Timepoint == '19-Sep')

new_Q<-NULL
for(ind in unique(Litt_S19$Pool)){
  temp_dat<-Litt_S19[which(Litt_S19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_L_Sept2019_TH.3.csv")

#----------------------------------------------------------------------------

####################
#####MUSSELS#######
####################

#----------------------------------------------------------------------------

Muss_M19<-filter(TH, Species == "Mussel" & Timepoint == '19-Mar')

new_Q<-NULL
for(ind in unique(Muss_M19$Pool)){
  temp_dat<-Muss_M19[which(Muss_M19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_M_March2019_TH.3.csv")

########Q10 JULY 2019##########
#----------------------------------------------------------------------------

Muss_J19<-filter(TH, Species == "Mussel" & Timepoint == '19-Jul')

new_Q<-NULL
for(ind in unique(Muss_J19$Pool)){
  temp_dat<-Muss_J19[which(Muss_J19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_M_July2019_TH.3.csv")

########Q10 SEPTEMBER 2019##########
#----------------------------------------------------------------------------

Muss_S19<-filter(TH, Species == "Mussel" & Timepoint == '19-Sep')

new_Q<-NULL
for(ind in unique(Muss_S19$Pool)){
  temp_dat<-Muss_S19[which(Muss_S19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_M_Sept2019_TH.3.csv")



#----------------------------------------------------------------------------

####################
#####HERMIT CRABS#######
####################

#----------------------------------------------------------------------------

Herm_M19<-filter(TH, Species == "Hermit" & Timepoint == '19-Mar')

new_Q<-NULL
for(ind in unique(Herm_M19$Pool)){
  temp_dat<-Herm_M19[which(Herm_M19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

head(data.frame(Q10_H))
write.csv(data.frame(Q10_H), file = "Q10_H_March2019_TH.3.csv")

########Q10 JULY 2019##########
#----------------------------------------------------------------------------

Herm_J19<-filter(TH, Species == "Hermit" & Timepoint == '19-Jul')

new_Q<-NULL
for(ind in unique(Herm_J19$Pool)){
  temp_dat<-Herm_J19[which(Herm_J19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

head(data.frame(Q10_H))
write.csv(data.frame(Q10_H), file = "Q10_H_July2019_TH.3.csv")

########Q10 SEPTEMBER 2019##########
#----------------------------------------------------------------------------

Herm_S19<-filter(TH, Species == "Hermit" & Timepoint == '19-Sep')

new_Q<-NULL
for(ind in unique(Herm_S19$Pool)){
  temp_dat<-Herm_S19[which(Herm_S19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

head(data.frame(Q10_H))
write.csv(data.frame(Q10_H), file = "Q10_H_Sept2019_TH.3.csv")




#Merging 
TH<- read.csv("RangelandSorte_ThermalHistory_All_Updated.csv", na.strings = "nd", header=T)
Q<- read.csv("Q10_All.csv", na.strings = "nd", header=T)

TH_All<-merge(TH, Q, by = c("ID","Timepoint"), all.x = TRUE)


write.csv(TH_All, file = "FullTH.csv") 








#-----------------------------------------------------------------------------
#Q10 10-18 and 18-26
####################################
########Q10 MARCH 2019##########
####################################
#----------------------------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Thermal History")
#----------------------------------------------------------------------------
TH<- read.csv("RangelandSorte_MO2_All.csv", na.strings = "nd", header=T)

####################
#####LITTORINES#######
#######10-18#########

Litt_M19<-filter(TH, Species == "Littorine" & Timepoint == '19-Mar')
Litt_M19<-filter(Litt_M19, Temp %in% c("10", "18"))

new_Q<-NULL
for(ind in unique(Litt_M19$Pool)){
  temp_dat<-Litt_M19[which(Litt_M19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Litt_M19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_10_18_L_March2019_TH.csv")

####################
#####LITTORINES#######
#######18-26#########

Litt_M19<-filter(TH, Species == "Littorine" & Timepoint == '19-Mar')
Litt_M19<-filter(Litt_M19, Temp %in% c("18", "26"))

new_Q<-NULL
for(ind in unique(Litt_M19$Pool)){
  temp_dat<-Litt_M19[which(Litt_M19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Litt_M19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_18_26_L_March2019_TH.csv")

########Q10 JULY 2019##########
#----------------------------------------------------------------------------
#10-18

Litt_J19<-filter(TH, Species == "Littorine" & Timepoint == '19-Jul')
Litt_J19<-filter(Litt_J19, Temp %in% c("10", "18"))

new_Q<-NULL
for(ind in unique(Litt_J19$Pool)){
  temp_dat<-Litt_J19[which(Litt_J19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}


colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_10_18_L_July2019_TH.csv")

#----------------------------------------------------------------------------
#18-26

Litt_J19<-filter(TH, Species == "Littorine" & Timepoint == '19-Jul')
Litt_J19<-filter(Litt_J19, Temp %in% c("18", "26"))

new_Q<-NULL
for(ind in unique(Litt_J19$Pool)){
  temp_dat<-Litt_J19[which(Litt_J19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Litt_J19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_18_26_L_July2019_TH.csv")

########Q10 SEPTEMBER 2019##########
#----------------------------------------------------------------------------
#10-18C

Litt_S19<-filter(TH, Species == "Littorine" & Timepoint == '19-Sep')
Litt_S19<-filter(Litt_S19, Temp %in% c("10", "18"))

new_Q<-NULL
for(ind in unique(Litt_S19$Pool)){
  temp_dat<-Litt_S19[which(Litt_S19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Litt_S19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_10_18_L_Sept2019_TH.csv")


#----------------------------------------------------------------------------
#18-26C

Litt_S19<-filter(TH, Species == "Littorine" & Timepoint == '19-Sep')
Litt_S19<-filter(Litt_S19, Temp %in% c("18", "26"))

new_Q<-NULL
for(ind in unique(Litt_S19$Pool)){
  temp_dat<-Litt_S19[which(Litt_S19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Litt_S19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_L<- new_Q

head(data.frame(Q10_L))
write.csv(data.frame(Q10_L), file = "Q10_18_26_L_Sept2019_TH.csv")

#----------------------------------------------------------------------------

####################
#####MUSSELS#######
####################

#----------------------------------------------------------------------------
#10-18C 

Muss_M19<-filter(TH, Species == "Mussel" & Timepoint == '19-Mar')
Muss_M19<-filter(Muss_M19, Temp %in% c("10", "18"))

new_Q<-NULL
for(ind in unique(Muss_M19$Pool)){
  temp_dat<-Muss_M19[which(Muss_M19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Muss_M19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_10_18_M_March2019_TH.csv")


#----------------------------------------------------------------------------
#18-26C 

Muss_M19<-filter(TH, Species == "Mussel" & Timepoint == '19-Mar')
Muss_M19<-filter(Muss_M19, Temp %in% c("18", "26"))

new_Q<-NULL
for(ind in unique(Muss_M19$Pool)){
  temp_dat<-Muss_M19[which(Muss_M19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Muss_M19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_18_26_M_March2019_TH.csv")

########Q10 JULY 2019##########
#----------------------------------------------------------------------------
#10-18C

Muss_J19<-filter(TH, Species == "Mussel" & Timepoint == '19-Jul')
Muss_J19<-filter(Muss_J19, Temp %in% c("10", "18"))

new_Q<-NULL
for(ind in unique(Muss_J19$Pool)){
  temp_dat<-Muss_J19[which(Muss_J19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Muss_J19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_10_18_M_July2019_TH.csv")

#----------------------------------------------------------------------------
#18-26C

Muss_J19<-filter(TH, Species == "Mussel" & Timepoint == '19-Jul')
Muss_J19<-filter(Muss_J19, Temp %in% c("18", "26"))

new_Q<-NULL
for(ind in unique(Muss_J19$Pool)){
  temp_dat<-Muss_J19[which(Muss_J19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Muss_J19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_18_26_M_July2019_TH.csv")

########Q10 SEPTEMBER 2019##########
#----------------------------------------------------------------------------
#10-18C

Muss_S19<-filter(TH, Species == "Mussel" & Timepoint == '19-Sep')
Muss_S19<-filter(Muss_S19, Temp %in% c("10", "18"))

new_Q<-NULL
for(ind in unique(Muss_S19$Pool)){
  temp_dat<-Muss_S19[which(Muss_S19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Muss_S19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_10_18_M_Sept2019_TH.csv")


#----------------------------------------------------------------------------
#10-18C

Muss_S19<-filter(TH, Species == "Mussel" & Timepoint == '19-Sep')
Muss_S19<-filter(Muss_S19, Temp %in% c("18", "26"))

new_Q<-NULL
for(ind in unique(Muss_S19$Pool)){
  temp_dat<-Muss_S19[which(Muss_S19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Muss_S19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_M<- new_Q

head(data.frame(Q10_M))
write.csv(data.frame(Q10_M), file = "Q10_18_26_M_Sept2019_TH.csv")


#----------------------------------------------------------------------------

####################
#####HERMIT CRABS#######
####################

#----------------------------------------------------------------------------
#10-18C

Herm_M19<-filter(TH, Species == "Hermit" & Timepoint == '19-Mar')
Herm_M19<-filter(Herm_M19, Temp %in% c("10", "18"))

new_Q<-NULL
for(ind in unique(Herm_M19$Pool)){
  temp_dat<-Herm_M19[which(Herm_M19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Herm_M19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

head(data.frame(Q10_H))
write.csv(data.frame(Q10_H), file = "Q10_10_18_H_March2019_TH.csv")


#----------------------------------------------------------------------------
#18-26C

Herm_M19<-filter(TH, Species == "Hermit" & Timepoint == '19-Mar')
Herm_M19<-filter(Herm_M19, Temp %in% c("18", "26"))

new_Q<-NULL
for(ind in unique(Herm_M19$Pool)){
  temp_dat<-Herm_M19[which(Herm_M19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Herm_M19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

head(data.frame(Q10_H))
write.csv(data.frame(Q10_H), file = "Q10_18_26_H_March2019_TH.csv")

########Q10 JULY 2019##########
#----------------------------------------------------------------------------
#10-18C

Herm_J19<-filter(TH, Species == "Hermit" & Timepoint == '19-Jul')
Herm_J19<-filter(Herm_J19, Temp %in% c("10", "18"))

new_Q<-NULL
for(ind in unique(Herm_J19$Pool)){
  temp_dat<-Herm_J19[which(Herm_J19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Herm_J19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

head(data.frame(Q10_H))
write.csv(data.frame(Q10_H), file = "Q10_10_18_H_July2019_TH.csv")

#----------------------------------------------------------------------------
#18-26C

Herm_J19<-filter(TH, Species == "Hermit" & Timepoint == '19-Jul')
Herm_J19<-filter(Herm_J19, Temp %in% c("18", "26"))

new_Q<-NULL
for(ind in unique(Herm_J19$Pool)){
  temp_dat<-Herm_J19[which(Herm_J19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Herm_J19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

head(data.frame(Q10_H))
write.csv(data.frame(Q10_H), file = "Q10_18_26_H_July2019_TH.csv")

########Q10 SEPTEMBER 2019##########
#----------------------------------------------------------------------------
#10-18C

Herm_S19<-filter(TH, Species == "Hermit" & Timepoint == '19-Sep')
Herm_S19<-filter(Herm_S19, Temp %in% c("10", "18"))

new_Q<-NULL
for(ind in unique(Herm_S19$Pool)){
  temp_dat<-Herm_S19[which(Herm_S19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(10,18))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Herm_S19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

head(data.frame(Q10_H))
write.csv(data.frame(Q10_H), file = "Q10_10_18_H_Sept2019_TH.csv")

#----------------------------------------------------------------------------
#18-26C

Herm_S19<-filter(TH, Species == "Hermit" & Timepoint == '19-Sep')
Herm_S19<-filter(Herm_S19, Temp %in% c("18", "26"))

new_Q<-NULL
for(ind in unique(Herm_S19$Pool)){
  temp_dat<-Herm_S19[which(Herm_S19$Pool == ind),]
  temp10<-Q10(R_vec = c(unique(temp_dat$MO2)),
              T_vec= c(18,26))
  new_Q<-rbind(new_Q, cbind(temp10, paste(ind)))
}

plot(MO2~Temp, Herm_S19, pch=16)

colnames(new_Q)<-c("Q10", "PoolID")
Q10_H<- new_Q

head(data.frame(Q10_H))
write.csv(data.frame(Q10_H), file = "Q10_18_26_H_Sept2019_TH.csv")


#Merging 
TH<- read.csv("RangelandSorte_ThermalHistory_All_Updated.csv", na.strings = "nd", header=T)
Q<- read.csv("Q10_All.csv", na.strings = "nd", header=T)

TH_All<-merge(TH, Q, by = c("ID","Timepoint"), all.x = TRUE)


write.csv(TH_All, file = "FullTH.csv") 

