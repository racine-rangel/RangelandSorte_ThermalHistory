################################################################
#Title: Mass-specific Oxygen Consumption (MO2) for all species
#Purpose: Determine the MO2 values - for all species - across all timepoints
#Created by: R. E. Rangel
#Created: August 2018
#Last edited: 13 March 2022
################################################################
##### Packages #####



################################################################
#PERIWINKLE SNAILS (Littorina sitkana)
################################################################

#MARCH 2019-----------------------------------------------------------------------

setwd("/Users/racinerangel/Desktop/Sitka March 2019/SitkaL-23March2019/Littorine-10C")
#######################################################
####################Littorines 10C#########################
##################23 March 2019########################
####################
#####Snail 5####### Removed R2 <0.90
####################
LittTP5<-read.csv("Littorine_TP05_10C_23March2019.csv",header=TRUE)


LittTP5$Date<-as.POSIXct(LittTP5$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP5, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.5<-lm(Oxygen~Date,data=LittTP5)
summary(m10.5)
abline(m10.5,lwd=2, lty=1)
slope10.5<-coef(m10.5)[2]*60 #converts to Mins
slope10.5

####################
#####Snail 29#######
####################
LittTP29<-read.csv("Littorine_TP29_10C_23March2019.csv",header=TRUE)


LittTP29$Date<-as.POSIXct(LittTP29$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP29, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.29<-lm(Oxygen~Date,data=LittTP29)
summary(m10.29)
abline(m10.29,lwd=2, lty=1)
slope10.29<-coef(m10.29)[2]*60 #converts to Mins
slope10.29

####################
#####Snail 8#######
####################
LittTP8<-read.csv("Littorine_TP08_10C_23March2019.csv",header=TRUE)


LittTP8$Date<-as.POSIXct(LittTP8$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP8, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.8<-lm(Oxygen~Date,data=LittTP8)
summary(m10.8)
abline(m10.8,lwd=2, lty=1)
slope10.8<-coef(m10.8)[2]*60 #converts to Mins
slope10.8


####################
#####Snail 9#######
####################
LittTP9<-read.csv("Littorine_TP09_10C_23March2019.csv",header=TRUE)


LittTP9$Date<-as.POSIXct(LittTP9$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP9, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.9<-lm(Oxygen~Date,data=LittTP9)
summary(m10.9)
abline(m10.9,lwd=2, lty=1)
slope10.9<-coef(m10.9)[2]*60 #converts to Mins
slope10.9


####################
#####Snail 16#######
####################
LittTP16<-read.csv("Littorine_TP16_10C_23March2019.csv",header=TRUE)


LittTP16$Date<-as.POSIXct(LittTP16$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.16<-lm(Oxygen~Date,data=LittTP16)
summary(m10.16)
abline(m10.16,lwd=2, lty=1)
slope10.16<-coef(m10.16)[2]*60 #converts to Mins
slope10.16


####################
#####Snail 26#######
####################
LittTP26<-read.csv("Littorine_TP26_10C_23March2019.csv",header=TRUE)


LittTP26$Date<-as.POSIXct(LittTP26$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.26<-lm(Oxygen~Date,data=LittTP26)
summary(m10.26)
abline(m10.26,lwd=2, lty=1)
slope10.26<-coef(m10.26)[2]*60 #converts to Mins
slope10.26

####################
#####Snail 35#######
####################
LittTP35<-read.csv("Littorine_TP35_10C_23March2019.csv",header=TRUE)


LittTP35$Date<-as.POSIXct(LittTP35$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP35, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.35<-lm(Oxygen~Date,data=LittTP35)
summary(m10.35)
abline(m10.35,lwd=2, lty=1)
slope10.35<-coef(m10.35)[2]*60 #converts to Mins
slope10.35

####################
#####BLANK 10C #######
####################
LittBL1.10<-read.csv("Littorine_BLANK_10C_23March2019.csv",header=TRUE)


LittBL1.10$Date<-as.POSIXct(LittBL1.10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl1<-lm(Oxygen~Date,data=LittBL1.10)
summary(m10.bl1)
abline(m10.bl1,lwd=2, lty=1)
slope10.bl1<-coef(m10.bl1)[2]*60 #converts to Mins
slope10.bl1


####################
#####BLANK 7 10C #######
####################
LittBL7.10<-read.csv("Littorine_BLANK7_10C_23March2019.csv",header=TRUE)


LittBL7.10$Date<-as.POSIXct(LittBL7.10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL7.10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl7<-lm(Oxygen~Date,data=LittBL7.10)
summary(m10.bl7)
abline(m10.bl7,lwd=2, lty=1)
slope10.bl7<-coef(m10.bl7)[2]*60 #converts to Mins
slope10.bl7 #

#Avg Blank
b<-c(0.004530824,0.01260123 )
mean(b)

#######################################################
####################Littorines 18C#########################
##################23 March 2019########################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/SitkaL-23March2019/Littorine-18C")

####################
#####Snail 5#######Removed R2<0.90
####################
LittTP5<-read.csv("Littorine_TP05_18C_23March2019.csv",header=TRUE)


LittTP5$Date<-as.POSIXct(LittTP5$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP5, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.5<-lm(Oxygen~Date,data=LittTP5)
summary(m18.5)
abline(m18.5,lwd=2, lty=1)
slope18.5<-coef(m18.5)[2]*60 #converts to Mins
slope18.5

####################
#####Snail 29#######
####################
LittTP29<-read.csv("Littorine_TP29_18C_23March2019.csv",header=TRUE)


LittTP29$Date<-as.POSIXct(LittTP29$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP29, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.29<-lm(Oxygen~Date+Seconds,data=LittTP29)
summary(m18.29)
abline(m18.29,lwd=2, lty=1)
slope18.29<-coef(m18.29)[3]*60 #converts to Mins
slope18.29

####################
#####Snail 8#######
####################
LittTP8<-read.csv("Littorine_TP08_18C_23March2019.csv",header=TRUE)


LittTP8$Date<-as.POSIXct(LittTP8$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP8, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.8<-lm(Oxygen~Date,data=LittTP8)
summary(m18.8)
abline(m18.8,lwd=2, lty=1)
slope18.8<-coef(m18.8)[2]*60 #converts to Mins
slope18.8


####################
#####Snail 9#######
####################
LittTP9<-read.csv("Littorine_TP09_18C_23March2019.csv",header=TRUE)


LittTP9$Date<-as.POSIXct(LittTP9$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP9, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.9<-lm(Oxygen~Date,data=LittTP9)
summary(m18.9)
abline(m18.9,lwd=2, lty=1)
slope18.9<-coef(m18.9)[2]*60 #converts to Mins
slope18.9


####################
#####Snail 16#######
####################
LittTP16<-read.csv("Littorine_TP16_18C_23March2019.csv",header=TRUE)


LittTP16$Date<-as.POSIXct(LittTP16$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.16<-lm(Oxygen~Date,data=LittTP16)
summary(m18.16)
abline(m18.16,lwd=2, lty=1)
slope18.16<-coef(m18.16)[2]*60 #converts to Mins
slope18.16

####################
#####Snail 26#######
####################
LittTP26<-read.csv("Littorine_TP26_18C_23March2019.csv",header=TRUE)


LittTP26$Date<-as.POSIXct(LittTP26$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.26<-lm(Oxygen~Date,data=LittTP26)
summary(m18.26)
abline(m18.26,lwd=2, lty=1)
slope18.26<-coef(m18.26)[2]*60 #converts to Mins
slope18.26

####################
#####Snail 35#######
####################
LittTP35<-read.csv("Littorine_TP35_18C_23March2019.csv",header=TRUE)


LittTP35$Date<-as.POSIXct(LittTP35$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP35, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.35<-lm(Oxygen~Date+Seconds,data=LittTP35)
summary(m18.35)
abline(m18.35,lwd=2, lty=1)
slope18.35<-coef(m18.35)[3]*60 #converts to Mins
slope18.35

####################
#####BLANK 18C #######
####################
LittBL1.18<-read.csv("Littorine_BLANK_18C_23March2019.csv",header=TRUE)


LittBL1.18$Date<-as.POSIXct(LittBL1.18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl1<-lm(Oxygen~Date,data=LittBL1.18)
summary(m18.bl1)
abline(m18.bl1,lwd=2, lty=1)
slope18.bl1<-coef(m18.bl1)[2]*60 #converts to Mins
slope18.bl1


####################
#####BLANK 7 10C #######
####################
LittBL7.18<-read.csv("Littorine_BLANK7_18C_23March2019.csv",header=TRUE)


LittBL7.18$Date<-as.POSIXct(LittBL7.18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL7.18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl7<-lm(Oxygen~Date,data=LittBL7.18)
summary(m18.bl7)
abline(m18.bl7,lwd=2, lty=1)
slope18.bl7<-coef(m18.bl7)[2]*60 #converts to Mins
slope18.bl7 #

#Avg Blank
b<-c(0.002224514,0.001668065)
mean(b)

#######################################################
####################Littorines 26C#########################
##################23 March 2019########################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/SitkaL-23March2019/Littorine-26C")

####################
#####Snail 5#######Removed R2<0.90
####################
LittTP5<-read.csv("Littorine_TP05_26C_23March2019.csv",header=TRUE)


LittTP5$Date<-as.POSIXct(LittTP5$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP5, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.5<-lm(Oxygen~Date,data=LittTP5)
summary(m26.5)
abline(m26.5,lwd=2, lty=1)
slope26.5<-coef(m26.5)[2]*60 #converts to Mins
slope26.5

####################
#####Snail 29#######
####################
LittTP29<-read.csv("Littorine_TP29_26C_23March2019.csv",header=TRUE)


LittTP29$Date<-as.POSIXct(LittTP29$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP29, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.29<-lm(Oxygen~Date,data=LittTP29)
summary(m26.29)
abline(m26.29,lwd=2, lty=1)
slope26.29<-coef(m26.29)[2]*60 #converts to Mins
slope26.29

####################
#####Snail 8#######
####################
LittTP8<-read.csv("Littorine_TP08_26C_23March2019.csv",header=TRUE)


LittTP8$Date<-as.POSIXct(LittTP8$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP8, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.8<-lm(Oxygen~Date,data=LittTP8)
summary(m26.8)
abline(m26.8,lwd=2, lty=1)
slope26.8<-coef(m26.8)[2]*60 #converts to Mins
slope26.8


####################
#####Snail 9#######
####################
LittTP9<-read.csv("Littorine_TP09_26C_23March2019.csv",header=TRUE)


LittTP9$Date<-as.POSIXct(LittTP9$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP9, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.9<-lm(Oxygen~Date,data=LittTP9)
summary(m26.9)
abline(m26.9,lwd=2, lty=1)
slope26.9<-coef(m26.9)[2]*60 #converts to Mins
slope26.9


####################
#####Snail 16#######
####################
LittTP16<-read.csv("Littorine_TP16_26C_23March2019.csv",header=TRUE)


LittTP16$Date<-as.POSIXct(LittTP16$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.16<-lm(Oxygen~Date,data=LittTP16)
summary(m26.16)
abline(m26.16,lwd=2, lty=1)
slope26.16<-coef(m26.16)[2]*60 #converts to Mins
slope26.16


####################
#####Snail 26#######
####################
LittTP26<-read.csv("Littorine_TP26_26C_23March2019.csv",header=TRUE)


LittTP26$Date<-as.POSIXct(LittTP26$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.26<-lm(Oxygen~Date,data=LittTP26)
summary(m26.26)
abline(m26.26,lwd=2, lty=1)
slope26.26<-coef(m26.26)[2]*60 #converts to Mins
slope26.26

####################
#####Snail 35#######
####################
LittTP35<-read.csv("Littorine_TP35_26C_23March2019.csv",header=TRUE)


LittTP35$Date<-as.POSIXct(LittTP35$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP35, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.35<-lm(Oxygen~Date,data=LittTP35)
summary(m26.35)
abline(m26.35,lwd=2, lty=1)
slope26.35<-coef(m26.35)[2]*60 #converts to Mins
slope26.35

####################
#####BLANK 26C #######
####################
LittBL1.26<-read.csv("Littorine_BLANK_26C_23March2019.csv",header=TRUE)


LittBL1.26$Date<-as.POSIXct(LittBL1.26$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl1<-lm(Oxygen~Date,data=LittBL1.26)
summary(m26.bl1)
abline(m26.bl1,lwd=2, lty=1)
slope26.bl1<-coef(m26.bl1)[2]*60 #converts to Mins
slope26.bl1


####################
#####BLANK 7 26C #######
####################
LittBL7.26<-read.csv("Littorine_BLANK7_26C_23March2019.csv",header=TRUE)


LittBL7.26$Date<-as.POSIXct(LittBL7.26$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL7.26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl7<-lm(Oxygen~Date,data=LittBL7.26)
summary(m26.bl7)
abline(m26.bl7,lwd=2, lty=1)
slope26.bl7<-coef(m26.bl7)[2]*60 #converts to Mins
slope26.bl7 #

#Avg Blank
b<-c(0.00225038,0.006824638)
mean(b)

#######################################################
####################Littorines 10C#########################
##################25 March 2019########################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/SitkaL-25March2019/Littorine-10C")
####################
#####Snail 07#######
####################
LittTP07<-read.csv("Littorine_TP07_10C_25March2019.csv",header=TRUE)


LittTP07$Date<-as.POSIXct(LittTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.07<-lm(Oxygen~Date,data=LittTP07)
summary(m10.07)
abline(m10.07,lwd=2, lty=1)
slope10.07<-coef(m10.07)[2]*60 #converts to mins
slope10.07 

####################
#####Snail 10#######
####################
LittTP10<-read.csv("Littorine_TP10_10C_25March2019.csv",header=TRUE)


LittTP10$Date<-as.POSIXct(LittTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.10<-lm(Oxygen~Date,data=LittTP10)
summary(m10.10)
abline(m10.10,lwd=2, lty=1)
slope10.10<-coef(m10.10)[2]*60
slope10.10 


####################
#####Snail 11#######
####################
LittTP11<-read.csv("Littorine_TP11_10C_25March2019.csv",header=TRUE)


LittTP11$Date<-as.POSIXct(LittTP11$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP11, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.11<-lm(Oxygen~Date,data=LittTP11)
summary(m10.11)
abline(m10.11,lwd=2, lty=1)
slope10.11<-coef(m10.11)[2]*60 #converts to Mins
slope10.11

####################
#####Snail 13#######
####################
LittTP13<-read.csv("Littorine_TP13_10C_25March2019.csv",header=TRUE)


LittTP13$Date<-as.POSIXct(LittTP13$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.13<-lm(Oxygen~Date,data=LittTP13)
summary(m10.13)
abline(m10.13,lwd=2, lty=1)
slope10.13<-coef(m10.13)[2]*60 #converts to Mins
slope10.13


####################
#####Snail 21#######
####################
LittTP21<-read.csv("Littorine_TP21_10C_25March2019.csv",header=TRUE)


LittTP21$Date<-as.POSIXct(LittTP21$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP21, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.21<-lm(Oxygen~Date,data=LittTP21)
summary(m10.21)
abline(m10.21,lwd=2, lty=1)
slope10.21<-coef(m10.21)[2]*60 #converts to Mins
slope10.21


####################
#####Snail 30#######
####################
LittTP30<-read.csv("Littorine_TP30_10C_25March2019.csv",header=TRUE)


LittTP30$Date<-as.POSIXct(LittTP30$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP30, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.30<-lm(Oxygen~Date,data=LittTP30)
summary(m10.30)
abline(m10.30,lwd=2, lty=1)
slope10.30<-coef(m10.30)[2]*60 #converts to Mins
slope10.30


####################
#####Snail 31#######
####################
LittTP31<-read.csv("Littorine_TP31_10C_25March2019.csv",header=TRUE)


LittTP31$Date<-as.POSIXct(LittTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.31<-lm(Oxygen~Date,data=LittTP31)
summary(m10.31)
abline(m10.31,lwd=2, lty=1)
slope10.31<-coef(m10.31)[2]*60 #converts to Mins
slope10.31



####################
#####Snail 33#######
####################
LittTP33<-read.csv("Littorine_TP33_10C_25March2019.csv",header=TRUE)


LittTP33$Date<-as.POSIXct(LittTP33$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.33<-lm(Oxygen~Date,data=LittTP33)
summary(m10.33)
abline(m10.33,lwd=2, lty=1)
slope10.33<-coef(m10.33)[2]*60 #converts to Mins
slope10.31

####################
#####BLANK 10C #######
####################
LittBL1.10<-read.csv("Littorine_BLANK_10C_25March2019.csv",header=TRUE)


LittBL1.10$Date<-as.POSIXct(LittBL.10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl1<-lm(Oxygen~Date,data=LittBL1.10)
summary(m10.bl1)
abline(m10.bl1,lwd=2, lty=1)
slope10.bl1<-coef(m10.bl1)[2]*60 #converts to Mins
slope10.bl1 #0.003171619

#######################################################
####################Littorines 18C#########################
##################25 March 2019########################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/SitkaL-25March2019/Littorine-18C")

####################
#####Snail 07#######
####################
LittTP07<-read.csv("Littorine_TP07_18C_25March2019.csv",header=TRUE)


LittTP07$Date<-as.POSIXct(LittTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.07<-lm(Oxygen~Date,data=LittTP07)
summary(m18.07)
abline(m18.07,lwd=2, lty=1)
slope18.07<-coef(m18.07)[2]*60 #converts to Mins
slope18.07 

####################
#####Snail 10#######
####################
LittTP10<-read.csv("Littorine_TP10_18C_25March2019.csv",header=TRUE)


LittTP10$Date<-as.POSIXct(LittTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.10<-lm(Oxygen~Date,data=LittTP10)
summary(m18.10)
abline(m18.10,lwd=2, lty=1)
slope18.10<-coef(m18.10)[2]*60 #converts to Mins
slope18.10 


####################
#####Snail 11#######
####################
LittTP11<-read.csv("Littorine_TP11_18C_25March2019.csv",header=TRUE)


LittTP11$Date<-as.POSIXct(LittTP11$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP11, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.11<-lm(Oxygen~Date,data=LittTP11)
summary(m18.11)
abline(m18.11,lwd=2, lty=1)
slope18.11<-coef(m18.11)[2]*60 #converts to Mins
slope18.11

####################
#####Snail 13#######
####################
LittTP13<-read.csv("Littorine_TP13_18C_25March2019.csv",header=TRUE)


LittTP13$Date<-as.POSIXct(LittTP13$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.13<-lm(Oxygen~Date,data=LittTP13)
summary(m18.13)
abline(m18.13,lwd=2, lty=1)
slope18.13<-coef(m18.13)[2]*60 #converts to Mins
slope18.13


####################
#####Snail 21#######
####################
LittTP21<-read.csv("Littorine_TP21_18C_25March2019.csv",header=TRUE)


LittTP21$Date<-as.POSIXct(LittTP21$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP21, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.21<-lm(Oxygen~Date,data=LittTP21)
summary(m18.21)
abline(m18.21,lwd=2, lty=1)
slope18.21<-coef(m18.21)[2]*60 #converts to Mins
slope18.21


####################
#####Snail 30#######
####################
LittTP30<-read.csv("Littorine_TP30_18C_25March2019.csv",header=TRUE)


LittTP30$Date<-as.POSIXct(LittTP30$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP30, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.30<-lm(Oxygen~Date,data=LittTP30)
summary(m18.30)
abline(m18.30,lwd=2, lty=1)
slope18.30<-coef(m18.30)[2]*60 #converts to Mins
slope18.30


####################
#####Snail 31#######
####################
LittTP31<-read.csv("Littorine_TP31_18C_25March2019.csv",header=TRUE)


LittTP31$Date<-as.POSIXct(LittTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.31<-lm(Oxygen~Date,data=LittTP31)
summary(m18.31)
abline(m18.31,lwd=2, lty=1)
slope18.31<-coef(m18.31)[2]*60 #converts to Mins
slope18.31



####################
#####Snail 33#######
####################
LittTP33<-read.csv("Littorine_TP33_18C_25March2019.csv",header=TRUE)


LittTP33$Date<-as.POSIXct(LittTP33$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.33<-lm(Oxygen~Date,data=LittTP33)
summary(m18.33)
abline(m18.33,lwd=2, lty=1)
slope18.33<-coef(m18.33)[2]*60 #converts to Mins
slope18.33

####################
#####BLANK 18C #######
####################
LittBL1.26<-read.csv("Littorine_BLANK_18C_25March2019.csv",header=TRUE)


LittBL1.26$Date<-as.POSIXct(LittBL1.26$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl1<-lm(Oxygen~Date,data=LittBL1.26)
summary(m26.bl1)
abline(m26.bl1,lwd=2, lty=1)
slope26.bl1<-coef(m26.bl1)[2]*60 #converts to Mins
slope26.bl1 #0.003992849 


####################
#####BLANK 7 18C #######
####################
LittorineBL7<-read.csv("Littorine_BLANK7_18C_25March2019.csv",header=TRUE)


LittorineBL7$Date<-as.POSIXct(LittorineBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittorineBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl7<-lm(Oxygen~Date,data=LittorineBL7)
summary(m26.bl7)
abline(m26.bl7,lwd=2, lty=1)
slope26.bl7<-coef(m26.bl7)[2]*60 #converts to Mins
slope26.bl7 #0.001783257 

#Avg Blank
b<-c(0.003992849,0.001783257)
mean(b) #0.002888053

#######################################################
####################Littorines 26C#########################
##################25 March 2019########################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/SitkaL-25March2019/Littorine-26C")
####################
#####Snail 07#######
####################
LittTP07<-read.csv("Littorine_TP07_26C_25March2019.csv",header=TRUE)


LittTP07$Date<-as.POSIXct(LittTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.07<-lm(Oxygen~Date,data=LittTP07)
summary(m26.07)
abline(m26.07,lwd=2, lty=1)
slope26.07<-coef(m26.07)[2]*60 #converts to Mins
slope26.07 

####################
#####Snail 10#######
####################
LittTP10<-read.csv("Littorine_TP10_26C_25March2019.csv",header=TRUE)


LittTP10$Date<-as.POSIXct(LittTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.10<-lm(Oxygen~Date,data=LittTP10)
summary(m26.10)
abline(m26.10,lwd=2, lty=1)
slope26.10<-coef(m26.10)[2]*60 #converts to Mins
slope26.10 


####################
#####Snail 11#######
####################
LittTP11<-read.csv("Littorine_TP11_26C_25March2019.csv",header=TRUE)


LittTP11$Date<-as.POSIXct(LittTP11$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP11, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.11<-lm(Oxygen~Date,data=LittTP11)
summary(m26.11)
abline(m26.11,lwd=2, lty=1)
slope26.11<-coef(m26.11)[2]*60 #converts to Mins
slope26.11

####################
#####Snail 13#######
####################
LittTP13<-read.csv("Littorine_TP13_26C_25March2019.csv",header=TRUE)


LittTP13$Date<-as.POSIXct(LittTP13$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.13<-lm(Oxygen~Date,data=LittTP13)
summary(m26.13)
abline(m26.13,lwd=2, lty=1)
slope26.13<-coef(m26.13)[2]*60 #converts to Mins
slope26.13


####################
#####Snail 21#######
####################
LittTP21<-read.csv("Littorine_TP21_26C_25March2019.csv",header=TRUE)


LittTP21$Date<-as.POSIXct(LittTP21$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP21, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.21<-lm(Oxygen~Date,data=LittTP21)
summary(m26.21)
abline(m26.21,lwd=2, lty=1)
slope26.21<-coef(m26.21)[2]*60 #converts to Mins
slope26.21


####################
#####Snail 30#######
####################
LittTP30<-read.csv("Littorine_TP30_26C_25March2019.csv",header=TRUE)


LittTP30$Date<-as.POSIXct(LittTP30$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP30, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.30<-lm(Oxygen~Date,data=LittTP30)
summary(m26.30)
abline(m26.30,lwd=2, lty=1)
slope26.30<-coef(m26.30)[2]*60 #converts to Mins
slope26.30


####################
#####Snail 31#######
####################
LittTP31<-read.csv("Littorine_TP31_26C_25March2019.csv",header=TRUE)


LittTP31$Date<-as.POSIXct(LittTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.31<-lm(Oxygen~Date,data=LittTP31)
summary(m26.31)
abline(m26.31,lwd=2, lty=1)
slope26.31<-coef(m26.31)[2]*60 #converts to Mins
slope26.31



####################
#####Snail 33#######
####################
LittTP33<-read.csv("Littorine_TP33_26C_25March2019.csv",header=TRUE)


LittTP33$Date<-as.POSIXct(LittTP33$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.33<-lm(Oxygen~Date,data=LittTP33)
summary(m26.33)
abline(m26.33,lwd=2, lty=1)
slope26.33<-coef(m26.33)[2]*60 #converts to Mins
slope26.33

####################
#####BLANK 26C #######
####################
LittBL1.26<-read.csv("Littorine_BLANK_26C_25March2019.csv",header=TRUE)


LittBL1.26$Date<-as.POSIXct(LittBL1.26$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl1<-lm(Oxygen~Date,data=LittBL1.26)
summary(m26.bl1)
abline(m26.bl1,lwd=2, lty=1)
slope26.bl1<-coef(m26.bl1)[2]*60 #converts to Mins
slope26.bl1 #0.003171619


####################
#####BLANK 7 26C #######
####################
LittorineBL7<-read.csv("Littorine_BLANK7_26C_25March2019.csv",header=TRUE)


LittorineBL7$Date<-as.POSIXct(LittorineBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittorineBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl7<-lm(Oxygen~Date,data=LittorineBL7)
summary(m26.bl7)
abline(m26.bl7,lwd=2, lty=1)
slope26.bl7<-coef(m26.bl7)[2]*60 #converts to Mins
slope26.bl7 #0.009030267 

#Avg Blank
b<-c(0.009030267 ,0.007897022)
mean(b)




#######################################################
####################Littorines 10C#########################
##################27 March 2019########################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/SitkaL-27March2019/Littorine-10C")
####################
#####Snail 15#######
####################
LittTP15<-read.csv("Littorine_TP15_10C_27March2019.csv",header=TRUE)


LittTP15$Date<-as.POSIXct(LittTP15$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.15<-lm(Oxygen~Date,data=LittTP15)
summary(m10.15)
abline(m10.15,lwd=2, lty=1)
slope10.15<-coef(m10.15)[2]*60 #converts to Mins
slope10.15

####################
#####Snail 20#######
####################
LittTP20<-read.csv("Littorine_TP20_10C_27March2019.csv",header=TRUE)


LittTP20$Date<-as.POSIXct(LittTP20$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP20, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.20<-lm(Oxygen~Date,data=LittTP20)
summary(m10.20)
abline(m10.20,lwd=2, lty=1)
slope10.20<-coef(m10.20)[2]*60 #converts to Mins
slope10.20


####################
#####Snail 27#######
####################
LittTP27<-read.csv("Littorine_TP27_10C_27March2019.csv",header=TRUE) 


LittTP27$Date<-as.POSIXct(LittTP27$Date, format="%d/%m/%y %H:%M:%S") 

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP27, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.27<-lm(Oxygen~Date,data=LittTP27)
summary(m10.27)
abline(m10.27,lwd=2, lty=1)
slope10.27<-coef(m10.27)[2]*60 #converts to Mins
slope10.27


####################
#####Snail 28#######
####################
LittTP28<-read.csv("Littorine_TP28_10C_27March2019.csv",header=TRUE) 


LittTP28$Date<-as.POSIXct(LittTP28$Date, format="%d/%m/%y %H:%M:%S") 

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.28<-lm(Oxygen~Date,data=LittTP28)
summary(m10.28)
abline(m10.28,lwd=2, lty=1)
slope10.28<-coef(m10.28)[2]*60 #converts to Mins
slope10.28


####################
#####Snail 36#######
####################
LittTP36<-read.csv("Littorine_TP36_10C_27March2019.csv",header=TRUE) 


LittTP36$Date<-as.POSIXct(LittTP36$Date, format="%d/%m/%y %H:%M:%S") 

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP36, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.36<-lm(Oxygen~Date,data=LittTP36)
summary(m10.36)
abline(m10.36,lwd=2, lty=1)
slope10.36<-coef(m10.36)[2]*60 #converts to Mins
slope10.36

####################
#####BLANK 10C #######
####################
LittBL1.10<-read.csv("Littorine_BLANK_10C_27March2019.csv",header=TRUE)


LittBL1.10$Date<-as.POSIXct(LittBL1.10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl1<-lm(Oxygen~Date,data=LittBL1.10)
summary(m10.bl1)
abline(m10.bl1,lwd=2, lty=1)
slope10.bl1<-coef(m10.bl1)[2]*60 #converts to Mins
slope10.bl1 #0.003254193









#######################################################
####################Littorines 18C#########################
##################27 March 2019########################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/SitkaL-27March2019/Littorine-18C")
####################
#####Snail 15#######
####################
LittTP15<-read.csv("Littorine_TP15_18C_27March2019.csv",header=TRUE)


LittTP15$Date<-as.POSIXct(LittTP15$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.15<-lm(Oxygen~Date,data=LittTP15)
summary(m18.15)
abline(m18.15,lwd=2, lty=1)
slope18.15<-coef(m18.15)[2]*60 #converts to Mins
slope18.15

####################
#####Snail 20#######
####################
LittTP20<-read.csv("Littorine_TP20_18C_27March2019.csv",header=TRUE)


LittTP20$Date<-as.POSIXct(LittTP20$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP20, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.20<-lm(Oxygen~Date,data=LittTP20)
summary(m18.20)
abline(m18.20,lwd=2, lty=1)
slope18.20<-coef(m18.20)[2]*60 #converts to Mins
slope18.20


####################
#####Snail 27#######
####################
LittTP27<-read.csv("Littorine_TP27_18C_27March2019.csv",header=TRUE) 


LittTP27$Date<-as.POSIXct(LittTP27$Date, format="%d/%m/%y %H:%M:%S") 

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP27, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.27<-lm(Oxygen~Date,data=LittTP27)
summary(m18.27)
abline(m18.27,lwd=2, lty=1)
slope18.27<-coef(m18.27)[2]*60 #converts to Mins
slope18.27


####################
#####Snail 28#######
####################
LittTP28<-read.csv("Littorine_TP28_18C_27March2019.csv",header=TRUE) 


LittTP28$Date<-as.POSIXct(LittTP28$Date, format="%d/%m/%y %H:%M:%S") 

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.28<-lm(Oxygen~Date,data=LittTP28)
summary(m18.28)
abline(m18.28,lwd=2, lty=1)
slope18.28<-coef(m18.28)[2]*60 #converts to Mins
slope18.28


####################
#####Snail 36#######
####################
LittTP36<-read.csv("Littorine_TP36_18C_27March2019.csv",header=TRUE) 


LittTP36$Date<-as.POSIXct(LittTP36$Date, format="%d/%m/%y %H:%M:%S") 

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP36, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.36<-lm(Oxygen~Date,data=LittTP36)
summary(m18.36)
abline(m18.36,lwd=2, lty=1)
slope18.36<-coef(m18.36)[2]*60 #converts to Mins
slope18.36

####################
#####BLANK 18C #######
####################
LittBL.18<-read.csv("Littorine_BLANK_18C_27March2019.csv",header=TRUE)


LittBL.18$Date<-as.POSIXct(LittBL.18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL.18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl<-lm(Oxygen~Date,data=LittBL.18)
summary(m18.bl)
abline(m18.bl,lwd=2, lty=1)
slope18.bl<-coef(m18.bl)[2]*60 #converts to Mins
slope18.bl #0.003254193




#######################################################
####################Littorines 26C#########################
##################27 March 2019########################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/SitkaL-27March2019/Littorine-26C")
####################
#####Snail 15#######
####################
LittTP15<-read.csv("Littorine_TP15_26C_27March2019.csv",header=TRUE)


LittTP15$Date<-as.POSIXct(LittTP15$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.15<-lm(Oxygen~Date,data=LittTP15)
summary(m26.15)
abline(m26.15,lwd=2, lty=1)
slope26.15<-coef(m26.15)[2]*60 #converts to Mins
slope26.15

####################
#####Snail 20#######
####################
LittTP20<-read.csv("Littorine_TP20_26C_27March2019.csv",header=TRUE)


LittTP20$Date<-as.POSIXct(LittTP20$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP20, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.20<-lm(Oxygen~Date,data=LittTP20)
summary(m26.20)
abline(m26.20,lwd=2, lty=1)
slope26.20<-coef(m26.20)[2]*60 #converts to Mins
slope26.20


####################
#####Snail 27#######
####################
LittTP27<-read.csv("Littorine_TP27_26C_27March2019.csv",header=TRUE) 


LittTP27$Date<-as.POSIXct(LittTP27$Date, format="%d/%m/%y %H:%M:%S") 

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP27, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.27<-lm(Oxygen~Date,data=LittTP27)
summary(m26.27)
abline(m26.27,lwd=2, lty=1)
slope26.27<-coef(m26.27)[2]*60 #converts to Mins
slope26.27


####################
#####Snail 28#######
####################
LittTP28<-read.csv("Littorine_TP28_26C_27March2019.csv",header=TRUE) 


LittTP28$Date<-as.POSIXct(LittTP28$Date, format="%d/%m/%y %H:%M:%S") 

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.28<-lm(Oxygen~Date,data=LittTP28)
summary(m26.28)
abline(m26.28,lwd=2, lty=1)
slope26.28<-coef(m26.28)[2]*60 #converts to Mins
slope26.28


####################
#####Snail 36#######
####################
LittTP36<-read.csv("Littorine_TP36_26C_27March2019.csv",header=TRUE) 


LittTP36$Date<-as.POSIXct(LittTP36$Date, format="%d/%m/%y %H:%M:%S") 

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP36, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.36<-lm(Oxygen~Date,data=LittTP36)
summary(m26.36)
abline(m26.36,lwd=2, lty=1)
slope26.36<-coef(m26.36)[2]*60 #converts to Mins
slope26.36

####################
#####BLANK 26C #######
####################
LittBL.26<-read.csv("Littorine_BLANK_26C_27March2019.csv",header=TRUE)


LittBL.26$Date<-as.POSIXct(LittBL.26$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL.26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl<-lm(Oxygen~Date,data=LittBL.26)
summary(m26.bl)
abline(m26.bl,lwd=2, lty=1)
slope26.bl<-coef(m26.bl)[2]*60 #converts to Mins
slope26.bl #0.0005079167




################################################################
#MUSSELS (Mytilus trossulus)
################################################################

#MARCH 2019-----------------------------------------------------------------------


###Working Directory###--------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka_M-28March2019/Mussel-10C")

#######################################################
####################MUSSEL 10C#########################
##################28 March 2019########################

####################
#####Mussel 8#######
####################
MusselTP08<-read.csv("Mussel_TP08_10C_28March2019.csv",header=TRUE)


MusselTP08$Date<-as.POSIXct(MusselTP08$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(7.6,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP08, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.08<-lm(Oxygen~Date,data=MusselTP08)
summary(m10.08)
abline(m10.08,lwd=2, lty=1)
slope10.08<-coef(m10.08)[2]*60 #converts to Mins
slope10.08


####################
#####Mussel 5#######
####################
MusselTP05<-read.csv("Mussel_TP05_10C_28March2019.csv",header=TRUE)


MusselTP05$Date<-as.POSIXct(MusselTP05$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP05, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.05<-lm(Oxygen~Date,data=MusselTP05)
summary(m10.05)
abline(m10.05,lwd=2, lty=1)
slope10.05<-coef(m10.05)[2]*60 #converts to Mins
slope10.05


####################
#####Mussel 9#######
####################
MusselTP09<-read.csv("Mussel_TP09_10C_28March2019.csv",header=TRUE)


MusselTP09$Date<-as.POSIXct(MusselTP09$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP09, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.09<-lm(Oxygen~Date,data=MusselTP09)
summary(m10.09)
abline(m10.09,lwd=2, lty=1)
slope10.09<-coef(m10.09)[2]*60 #converts to Mins
slope10.09


####################
#####Mussel 13#######
####################
MusselTP13<-read.csv("Mussel_TP13_10C_28March2019.csv",header=TRUE)


MusselTP13$Date<-as.POSIXct(MusselTP13$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.13<-lm(Oxygen~Date,data=MusselTP13)
summary(m10.13)
abline(m10.13,lwd=2, lty=1)
slope10.13<-coef(m10.13)[2]*60 #converts to Mins
slope10.13

####################
#####Mussel 15#######
####################
MusselTP15<-read.csv("Mussel_TP15_10C_28March2019.csv",header=TRUE)


MusselTP15$Date<-as.POSIXct(MusselTP15$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.15<-lm(Oxygen~Date,data=MusselTP15)
summary(m10.15)
abline(m10.15,lwd=2, lty=1)
slope10.15<-coef(m10.15)[2]*60 #converts to Mins
slope10.15




####################
#####Mussel 16#######
####################
MusselTP16<-read.csv("Mussel_TP16_10C_28March2019.csv",header=TRUE)


MusselTP16$Date<-as.POSIXct(MusselTP16$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.16<-lm(Oxygen~Date,data=MusselTP16)
summary(m10.16)
abline(m10.16,lwd=2, lty=1)
slope10.16<-coef(m10.16)[2]*60 #converts to Mins
slope10.16



####################
#####Mussel 31#######
####################
MusselTP31<-read.csv("Mussel_TP31_10C_28March2019.csv",header=TRUE)


MusselTP31$Date<-as.POSIXct(MusselTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.31<-lm(Oxygen~Date,data=MusselTP31)
summary(m10.31)
abline(m10.31,lwd=2, lty=1)
slope10.31<-coef(m10.31)[2]*60 #converts to Mins
slope10.31


####################
#####Mussel 35#######
####################
MusselTP35<-read.csv("Mussel_TP35_10C_28March2019.csv",header=TRUE)


MusselTP35$Date<-as.POSIXct(MusselTP35$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP35, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.35<-lm(Oxygen~Date,data=MusselTP35)
summary(m10.35)
abline(m10.35,lwd=2, lty=1)
slope10.35<-coef(m10.35)[2]*60 #converts to Mins
slope10.35

####################
#####BLANK 10C #######
####################
MusselBL<-read.csv("Mussel_BLANK_10C_28March2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m10.bl)
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.003752299


####################
#####BLANK 7 10C #######
####################
MusselBL7<-read.csv("Mussel_BLANK7_10C_28March2019.csv",header=TRUE)


MusselBL7$Date<-as.POSIXct(MusselBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl7<-lm(Oxygen~Date,data=MusselBL7)
summary(m10.bl7)
abline(m10.bl7,lwd=2, lty=1)
slope10.bl7<-coef(m10.bl7)[2]*60 #converts to Mins
slope10.bl7 #0.01118598

#Avg Blank
b<-c(0.01118598,0.003752299)
mean(b)

#############################################################################
##############################18 C###########################################
#############################################################################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka_M-28March2019/Mussel-18C")
####################
#####Mussel 5#######
####################
MusselTP05<-read.csv("Mussel_TP05_18C_28March2019.csv",header=TRUE)


MusselTP05$Date<-as.POSIXct(MusselTP05$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP05, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.05<-lm(Oxygen~Date,data=MusselTP05)
summary(m18.05)
abline(m18.05,lwd=2, lty=1)
slope18.05<-coef(m18.05)[2]*60 #converts to Mins
slope18.05

####################
#####Mussel 8#######
####################
MusselTP08<-read.csv("Mussel_TP08_18C_28March2019.csv",header=TRUE)


MusselTP08$Date<-as.POSIXct(MusselTP08$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP08, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.08<-lm(Oxygen~Date,data=MusselTP08)
summary(m18.08)
abline(m18.08,lwd=2, lty=1)
slope18.08<-coef(m18.08)[2]*60 #converts to Mins
slope18.08

####################
#####Mussel 9#######
####################
MusselTP09<-read.csv("Mussel_TP09_18C_28March2019.csv",header=TRUE)


MusselTP09$Date<-as.POSIXct(MusselTP09$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP09, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.09<-lm(Oxygen~Date,data=MusselTP09)
summary(m18.09)
abline(m18.09,lwd=2, lty=1)
slope18.09<-coef(m18.09)[2]*60 #converts to Mins
slope18.09


####################
#####Mussel 13#######
####################
MusselTP13<-read.csv("Mussel_TP13_18C_28March2019.csv",header=TRUE)


MusselTP13$Date<-as.POSIXct(MusselTP13$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.13<-lm(Oxygen~Date,data=MusselTP13)
summary(m18.13)
abline(m18.13,lwd=2, lty=1)
slope18.13<-coef(m18.13)[2]*60 #converts to Mins
slope18.13

####################
#####Mussel 15#######
####################
MusselTP15<-read.csv("Mussel_TP15_18C_28March2019.csv",header=TRUE)


MusselTP15$Date<-as.POSIXct(MusselTP15$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.15<-lm(Oxygen~Date,data=MusselTP15)
summary(m18.15)
abline(m18.15,lwd=2, lty=1)
slope18.15<-coef(m18.15)[2]*60 #converts to Mins
slope18.15




####################
#####Mussel 16#######
####################
MusselTP16<-read.csv("Mussel_TP16_18C_28March2019.csv",header=TRUE)


MusselTP16$Date<-as.POSIXct(MusselTP16$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.16<-lm(Oxygen~Date,data=MusselTP16)
summary(m18.16)
abline(m18.16,lwd=2, lty=1)
slope18.16<-coef(m18.16)[2]*60 #converts to Mins
slope18.16



####################
#####Mussel 31#######
####################
MusselTP31<-read.csv("Mussel_TP31_18C_28March2019.csv",header=TRUE)


MusselTP31$Date<-as.POSIXct(MusselTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.31<-lm(Oxygen~Date,data=MusselTP31)
summary(m18.31)
abline(m18.31,lwd=2, lty=1)
slope18.31<-coef(m18.31)[2]*60 #converts to Mins
slope18.31


####################
#####Mussel 35#######
####################
MusselTP35<-read.csv("Mussel_TP35_18C_28March2019.csv",header=TRUE)


MusselTP35$Date<-as.POSIXct(MusselTP35$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP35, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.35<-lm(Oxygen~Date,data=MusselTP35)
summary(m18.35)
abline(m18.35,lwd=2, lty=1)
slope18.35<-coef(m18.35)[2]*60 #converts to Mins
slope18.35

####################
#####BLANK 18C #######
####################
MusselBL<-read.csv("Mussel_BLANK_18C_28March2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m18.bl)
abline(m18.bl,lwd=2, lty=1)
slope18.bl<-coef(m18.bl)[2]*60 #converts to Mins
slope18.bl #0.007897022


####################
#####BLANK 7 18C #######
####################
MusselBL7<-read.csv("Mussel_BLANK7_18C_28March2019.csv",header=TRUE)


MusselBL7$Date<-as.POSIXct(MusselBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl7<-lm(Oxygen~Date,data=MusselBL7)
summary(m18.bl7)
abline(m18.bl7,lwd=2, lty=1)
slope18.bl7<-coef(m18.bl7)[2]*60 #converts to Mins
slope18.bl7 #0.009030267 

#Avg Blank
b<-c(0.009030267 ,0.007897022)
mean(b)


#############################################################################
##############################26 C###########################################
#############################################################################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka_M-28March2019/Mussel-26C")
####################
#####Mussel 5#######
####################
MusselTP05<-read.csv("Mussel_TP05_26C_28March2019.csv",header=TRUE)


MusselTP05$Date<-as.POSIXct(MusselTP05$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP05, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.05<-lm(Oxygen~Date,data=MusselTP05)
summary(m26.05)
abline(m26.05,lwd=2, lty=1)
slope26.05<-coef(m26.05)[2]*60 #converts to Mins
slope26.05

####################
#####Mussel 8#######
####################
MusselTP08<-read.csv("Mussel_TP08_26C_28March2019.csv",header=TRUE)


MusselTP08$Date<-as.POSIXct(MusselTP08$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP08, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.08<-lm(Oxygen~Date,data=MusselTP08)
summary(m26.08)
abline(m26.08,lwd=2, lty=1)
slope26.08<-coef(m26.08)[2]*60 #converts to Mins
slope26.08

####################
#####Mussel 9#######
####################
MusselTP09<-read.csv("Mussel_TP09_26C_28March2019.csv",header=TRUE)


MusselTP09$Date<-as.POSIXct(MusselTP09$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP09, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.09<-lm(Oxygen~Date,data=MusselTP09)
summary(m26.09)
abline(m26.09,lwd=2, lty=1)
slope26.09<-coef(m26.09)[2]*60 #converts to Mins
slope26.09


####################
#####Mussel 13#######
####################
MusselTP13<-read.csv("Mussel_TP13_26C_28March2019.csv",header=TRUE)


MusselTP13$Date<-as.POSIXct(MusselTP13$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.13<-lm(Oxygen~Date,data=MusselTP13)
summary(m26.13)
abline(m26.13,lwd=2, lty=1)
slope26.13<-coef(m26.13)[2]*60 #converts to Mins
slope26.13

####################
#####Mussel 15#######
####################
MusselTP15<-read.csv("Mussel_TP15_26C_28March2019.csv",header=TRUE)


MusselTP15$Date<-as.POSIXct(MusselTP15$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.15<-lm(Oxygen~Date,data=MusselTP15)
summary(m26.15)
abline(m26.15,lwd=2, lty=1)
slope26.15<-coef(m26.15)[2]*60 #converts to Mins
slope26.15




####################
#####Mussel 16#######
####################
MusselTP16<-read.csv("Mussel_TP16_26C_28March2019.csv",header=TRUE)


MusselTP16$Date<-as.POSIXct(MusselTP16$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.16<-lm(Oxygen~Date,data=MusselTP16)
summary(m26.16)
abline(m26.16,lwd=2, lty=1)
slope26.16<-coef(m26.16)[2]*60 #converts to Mins
slope26.16



####################
#####Mussel 31#######
####################
MusselTP31<-read.csv("Mussel_TP31_26C_28March2019.csv",header=TRUE)


MusselTP31$Date<-as.POSIXct(MusselTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.31<-lm(Oxygen~Date,data=MusselTP31)
summary(m26.31)
abline(m26.31,lwd=2, lty=1)
slope26.31<-coef(m26.31)[2]*60 #converts to Mins
slope26.31


####################
#####Mussel 35#######
####################
MusselTP35<-read.csv("Mussel_TP35_26C_28March2019.csv",header=TRUE)


MusselTP35$Date<-as.POSIXct(MusselTP35$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP35, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.35<-lm(Oxygen~Date,data=MusselTP35)
summary(m26.35)
abline(m26.35,lwd=2, lty=1)
slope26.35<-coef(m26.35)[2]*60 #converts to Mins
slope26.35


####################
#####BLANK 7 26C #######
####################
MusselBL7<-read.csv("Mussel_BLANK7_26C_28March2019.csv",header=TRUE)


MusselBL7$Date<-as.POSIXct(MusselBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl7<-lm(Oxygen~Date,data=MusselBL7)
summary(m26.bl7)
abline(m26.bl7,lwd=2, lty=1)
slope26.bl7<-coef(m26.bl7)[2]*60 #converts to Mins
slope26.bl7 #0.009663924


#######################################################
####################MUSSEL 10C#########################
##################29 March 2019########################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka_M-29March2019/Mussel-10C")

####################
#####Mussel 3#######
####################
MusselTP03<-read.csv("Mussel_TP03_10C_29March2019.csv",header=TRUE)


MusselTP03$Date<-as.POSIXct(MusselTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.03<-lm(Oxygen~Date,data=MusselTP03)
summary(m10.03)
abline(m10.03,lwd=2, lty=1)
slope10.03<-coef(m10.03)[2]*60 #converts to Mins
slope10.03


####################
#####Mussel 7#######
####################
MusselTP07<-read.csv("Mussel_TP07_10C_29March2019.csv",header=TRUE)


MusselTP07$Date<-as.POSIXct(MusselTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.07<-lm(Oxygen~Date,data=MusselTP07)
summary(m10.07)
abline(m10.07,lwd=2, lty=1)
slope10.07<-coef(m10.07)[2]*60 #converts to Mins
slope10.07


####################
#####Mussel 10#######
####################
MusselTP10<-read.csv("Mussel_TP10_10C_29March2019.csv",header=TRUE)


MusselTP10$Date<-as.POSIXct(MusselTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.10<-lm(Oxygen~Date,data=MusselTP10)
summary(m10.10)
abline(m10.10,lwd=2, lty=1)
slope10.10<-coef(m10.10)[2]*60 #converts to Mins
slope10.10


####################
#####Mussel 11#######
####################
MusselTP11<-read.csv("Mussel_TP11_10C_29March2019.csv",header=TRUE)


MusselTP11$Date<-as.POSIXct(MusselTP11$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP11, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.11<-lm(Oxygen~Date,data=MusselTP11)
summary(m10.11)
abline(m10.11,lwd=2, lty=1)
slope10.11<-coef(m10.11)[2]*60 #converts to Mins
slope10.11

####################
#####Mussel 27#######
####################
MusselTP27<-read.csv("Mussel_TP27_10C_29March2019.csv",header=TRUE)


MusselTP27$Date<-as.POSIXct(MusselTP27$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP27, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.27<-lm(Oxygen~Date,data=MusselTP27)
summary(m10.27)
abline(m10.27,lwd=2, lty=1)
slope10.27<-coef(m10.27)[2]*60 #converts to Mins
slope10.27




####################
#####Mussel 28#######
####################
MusselTP28<-read.csv("Mussel_TP28_10C_29March2019.csv",header=TRUE)


MusselTP28$Date<-as.POSIXct(MusselTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.28<-lm(Oxygen~Date,data=MusselTP28)
summary(m10.28)
abline(m10.28,lwd=2, lty=1)
slope10.28<-coef(m10.28)[2]*60 #converts to Mins
slope10.28



####################
#####Mussel 33#######
####################
MusselTP33<-read.csv("Mussel_TP33_10C_29March2019.csv",header=TRUE)


MusselTP33$Date<-as.POSIXct(MusselTP33$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.33<-lm(Oxygen~Date,data=MusselTP33)
summary(m10.33)
abline(m10.33,lwd=2, lty=1)
slope10.33<-coef(m10.33)[2]*60 #converts to Mins
slope10.33


####################
#####Mussel 36#######
####################
MusselTP36<-read.csv("Mussel_TP36_10C_29March2019.csv",header=TRUE)


MusselTP36$Date<-as.POSIXct(MusselTP36$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP36, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.36<-lm(Oxygen~Date,data=MusselTP36)
summary(m10.36)
abline(m10.36,lwd=2, lty=1)
slope10.36<-coef(m10.36)[2]*60 #converts to Mins
slope10.36

####################
#####BLANK 10C #######
####################
MusselBL<-read.csv("Mussel_BLANK_10C_29March2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m10.bl)
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.007339236 


####################
#####BLANK 7 10C #######
####################
MusselBL7<-read.csv("Mussel_BLANK7_10C_29March2019.csv",header=TRUE)


MusselBL7$Date<-as.POSIXct(MusselBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl7<-lm(Oxygen~Date,data=MusselBL7)
summary(m10.bl7)
abline(m10.bl7,lwd=2, lty=1)
slope10.bl7<-coef(m10.bl7)[2]*60 #converts to Mins
slope10.bl7 #0.005090481

#Avg Blank
b<-c(0.007339236,0.005090481)
mean(b)

#############################################################################
##############################18 C###########################################
#############################################################################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka_M-29March2019/Mussel-18C")
####################
#####Mussel 3#######
####################
MusselTP03<-read.csv("Mussel_TP03_18C_29March2019.csv",header=TRUE)


MusselTP03$Date<-as.POSIXct(MusselTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.03<-lm(Oxygen~Date,data=MusselTP03)
summary(m18.03)
abline(m18.03,lwd=2, lty=1)
slope18.03<-coef(m18.03)[2]*60 #converts to Mins
slope18.03

####################
#####Mussel 7#######
####################
MusselTP07<-read.csv("Mussel_TP07_18C_29March2019.csv",header=TRUE)


MusselTP07$Date<-as.POSIXct(MusselTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.07<-lm(Oxygen~Date,data=MusselTP07)
summary(m18.07)
abline(m18.07,lwd=2, lty=1)
slope18.07<-coef(m18.07)[2]*60 #converts to Mins
slope18.07

####################
#####Mussel 10#######
####################
MusselTP10<-read.csv("Mussel_TP10_18C_29March2019.csv",header=TRUE)


MusselTP10$Date<-as.POSIXct(MusselTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.10<-lm(Oxygen~Date,data=MusselTP10)
summary(m18.10)
abline(m18.10,lwd=2, lty=1)
slope18.10<-coef(m18.10)[2]*60 #converts to Mins
slope18.10


####################
#####Mussel 11#######
####################
MusselTP11<-read.csv("Mussel_TP11_18C_29March2019.csv",header=TRUE)


MusselTP11$Date<-as.POSIXct(MusselTP11$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP11, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.11<-lm(Oxygen~Date,data=MusselTP11)
summary(m18.11)
abline(m18.11,lwd=2, lty=1)
slope18.11<-coef(m18.11)[2]*60 #converts to Mins
slope18.11

####################
#####Mussel 27#######
####################
MusselTP27<-read.csv("Mussel_TP27_18C_29March2019.csv",header=TRUE)


MusselTP27$Date<-as.POSIXct(MusselTP27$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP27, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.27<-lm(Oxygen~Date,data=MusselTP27)
summary(m18.27)
abline(m18.27,lwd=2, lty=1)
slope18.27<-coef(m18.27)[2]*60 #converts to Mins
slope18.27




####################
#####Mussel 28#######
####################
MusselTP28<-read.csv("Mussel_TP28_18C_29March2019.csv",header=TRUE)


MusselTP28$Date<-as.POSIXct(MusselTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.28<-lm(Oxygen~Date,data=MusselTP28)
summary(m18.28)
abline(m18.28,lwd=2, lty=1)
slope18.28<-coef(m18.28)[2]*60 #converts to Mins
slope18.28



####################
#####Mussel 33#######
####################
MusselTP33<-read.csv("Mussel_TP33_18C_29March2019.csv",header=TRUE)


MusselTP33$Date<-as.POSIXct(MusselTP33$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.33<-lm(Oxygen~Date,data=MusselTP33)
summary(m18.33)
abline(m18.33,lwd=2, lty=1)
slope18.33<-coef(m18.33)[2]*60 #converts to Mins
slope18.33


####################
#####Mussel 36#######
####################
MusselTP36<-read.csv("Mussel_TP36_18C_29March2019.csv",header=TRUE)


MusselTP36$Date<-as.POSIXct(MusselTP36$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP36, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.36<-lm(Oxygen~Date,data=MusselTP36)
summary(m18.36)
abline(m18.36,lwd=2, lty=1)
slope18.36<-coef(m18.36)[2]*60 #converts to Mins
slope18.36

####################
#####BLANK 18C #######
####################
MusselBL<-read.csv("Mussel_BLANK_18C_29March2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m18.bl)
abline(m18.bl,lwd=2, lty=1)
slope18.bl<-coef(m18.bl)[2]*60 #converts to Mins
slope18.bl #0.0119997 


####################
#####BLANK 7 18C #######
####################
MusselBL7<-read.csv("Mussel_BLANK7_18C_29March2019.csv",header=TRUE)


MusselBL7$Date<-as.POSIXct(MusselBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl7<-lm(Oxygen~Date,data=MusselBL7)
summary(m18.bl7)
abline(m18.bl7,lwd=2, lty=1)
slope18.bl7<-coef(m18.bl7)[2]*60 #converts to Mins
slope18.bl7 #0.01152481 

#Avg Blank
b<-c(0.0119997,0.01152481)
mean(b)


#############################################################################
##############################26 C###########################################
#############################################################################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka_M-29March2019/Mussel-26C")
####################
#####Mussel 3#######
####################
MusselTP03<-read.csv("Mussel_TP03_26C_29March2019.csv",header=TRUE)


MusselTP03$Date<-as.POSIXct(MusselTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.03<-lm(Oxygen~Date,data=MusselTP03)
summary(m26.03)
abline(m26.03,lwd=2, lty=1)
slope26.03<-coef(m26.03)[2]*60 #converts to Mins
slope26.03

####################
#####Mussel 7#######
####################
MusselTP07<-read.csv("Mussel_TP07_26C_29March2019.csv",header=TRUE)


MusselTP07$Date<-as.POSIXct(MusselTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.07<-lm(Oxygen~Date,data=MusselTP07)
summary(m26.07)
abline(m26.07,lwd=2, lty=1)
slope26.07<-coef(m26.07)[2]*60 #converts to Mins
slope26.07

####################
#####Mussel 10#######
####################
MusselTP10<-read.csv("Mussel_TP10_26C_29March2019.csv",header=TRUE)


MusselTP10$Date<-as.POSIXct(MusselTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.10<-lm(Oxygen~Date,data=MusselTP10)
summary(m26.10)
abline(m26.10,lwd=2, lty=1)
slope26.10<-coef(m26.10)[2]*60 #converts to Mins
slope26.10


####################
#####Mussel 11#######
####################
MusselTP11<-read.csv("Mussel_TP11_26C_29March2019.csv",header=TRUE)


MusselTP11$Date<-as.POSIXct(MusselTP11$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP11, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.11<-lm(Oxygen~Date,data=MusselTP11)
summary(m26.11)
abline(m26.11,lwd=2, lty=1)
slope26.11<-coef(m26.11)[2]*60 #converts to Mins
slope26.11

####################
#####Mussel 27#######
####################
MusselTP27<-read.csv("Mussel_TP27_26C_29March2019.csv",header=TRUE)


MusselTP27$Date<-as.POSIXct(MusselTP27$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP27, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.27<-lm(Oxygen~Date,data=MusselTP27)
summary(m26.27)
abline(m26.27,lwd=2, lty=1)
slope26.27<-coef(m26.27)[2]*60 #converts to Mins
slope26.27




####################
#####Mussel 28#######
####################
MusselTP28<-read.csv("Mussel_TP28_26C_29March2019.csv",header=TRUE)


MusselTP28$Date<-as.POSIXct(MusselTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.28<-lm(Oxygen~Date,data=MusselTP28)
summary(m26.28)
abline(m26.28,lwd=2, lty=1)
slope26.28<-coef(m26.28)[2]*60 #converts to Mins
slope26.28



####################
#####Mussel 33#######
####################
MusselTP33<-read.csv("Mussel_TP33_26C_29March2019.csv",header=TRUE)


MusselTP33$Date<-as.POSIXct(MusselTP33$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.33<-lm(Oxygen~Date,data=MusselTP33)
summary(m26.33)
abline(m26.33,lwd=2, lty=1)
slope26.33<-coef(m26.33)[2]*60 #converts to Mins
slope26.33


####################
#####Mussel 36#######
####################
MusselTP36<-read.csv("Mussel_TP36_26C_29March2019.csv",header=TRUE)


MusselTP36$Date<-as.POSIXct(MusselTP36$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP36, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.36<-lm(Oxygen~Date,data=MusselTP36)
summary(m26.36)
abline(m26.36,lwd=2, lty=1)
slope26.36<-coef(m26.36)[2]*60 #converts to Mins
slope26.36

####################
#####BLANK 26C #######
####################
MusselBL<-read.csv("Mussel_BLANK_26C_29March2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m26.bl)
abline(m26.bl,lwd=2, lty=1)
slope26.bl<-coef(m26.bl)[2]*60 #converts to Mins
slope26.bl #0.01129679

####################
#####BLANK 7 26C #######DONT USE
####################
MusselBL7<-read.csv("Mussel_BLANK7_26C_29March2019.csv",header=TRUE)


MusselBL7$Date<-as.POSIXct(MusselBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl7<-lm(Oxygen~Date,data=MusselBL7)
summary(m26.bl7)
abline(m26.bl7,lwd=2, lty=1)
slope26.bl7<-coef(m26.bl7)[2]*60 #converts to Mins
slope26.bl7 #0.009663924







################################################################
#HERMIT CRABS (Pagurus hirsitusculus)
################################################################

#MARCH 2019-----------------------------------------------------------------------


###Working Directory###--------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka H-30March2019/Hermits-10C")

#######################################################
####################HERMITS 10C########################
##################30 March 2019########################

####################
#####Hermit 10######
####################
HermitTP10<-read.csv("Hermit_TP10_10C_30March2019.csv",header=TRUE)


HermitTP10$Date<-as.POSIXct(HermitTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.10<-lm(Oxygen~Date,data=HermitTP10)
summary(m10.10)
abline(m10.10,lwd=2, lty=1)
slope10.10<-coef(m10.10)[2]*60 #converts to Mins
slope10.10


####################
#####Hermit 16#######
####################
HermitTP16<-read.csv("Hermit_TP16_10C_30March2019.csv",header=TRUE)


HermitTP16$Date<-as.POSIXct(HermitTP16$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.16<-lm(Oxygen~Date,data=HermitTP16)
summary(m10.16)
abline(m10.16,lwd=2, lty=1)
slope10.16<-coef(m10.16)[2]*60 #converts to Mins
slope10.16


####################
#####Hermit 20#######
####################
HermitTP20<-read.csv("Hermit_TP20_10C_30March2019.csv",header=TRUE)


HermitTP20$Date<-as.POSIXct(HermitTP20$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP20, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.20<-lm(Oxygen~Date,data=HermitTP20)
summary(m10.20)
abline(m10.20,lwd=2, lty=1)
slope10.20<-coef(m10.20)[2]*60 #converts to Mins
slope10.20


####################
#####Hermit 29#######
####################
HermitTP29<-read.csv("Hermit_TP29_10C_30March2019.csv",header=TRUE)


HermitTP29$Date<-as.POSIXct(HermitTP29$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP29, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.29<-lm(Oxygen~Date,data=HermitTP29)
summary(m10.29)
abline(m10.29,lwd=2, lty=1)
slope10.29<-coef(m10.29)[2]*60 #converts to Mins
slope10.29


####################
#####Hermit 31#######
####################
HermitTP31<-read.csv("Hermit_TP31_10C_30March2019.csv",header=TRUE)


HermitTP31$Date<-as.POSIXct(HermitTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.31<-lm(Oxygen~Date,data=HermitTP31)
summary(m10.31)
abline(m10.31,lwd=2, lty=1)
slope10.31<-coef(m10.31)[2]*60 #converts to Mins
slope10.31


####################
#####Hermit 33####### REMOVE R2<0.90
####################
HermitTP33<-read.csv("Hermit_TP33_10C_30March2019.csv",header=TRUE)


HermitTP33$Date<-as.POSIXct(HermitTP33$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.33<-lm(Oxygen~Date,data=HermitTP33)
summary(m10.33)
abline(m10.33,lwd=2, lty=1)
slope10.33<-coef(m10.33)[2]*60 #converts to Mins
slope10.33

####################
#####BLANK 10C #######
####################
HermitBL<-read.csv("Hermit_BLANK_10C_30March2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m10.bl)
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.003437721



#######################################################
####################HERMITS 18C#########################
##################30 March 2019########################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka H-30March2019/Hermits-18C")
####################
#####Hermit 18#######
####################
HermitTP10<-read.csv("Hermit_TP10_18C_30March2019.csv",header=TRUE)


HermitTP10$Date<-as.POSIXct(HermitTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.10<-lm(Oxygen~Date,data=HermitTP10)
summary(m10.10)
abline(m10.10,lwd=2, lty=1)
slope10.10<-coef(m10.10)[2]*60 #converts to Mins
slope10.10


####################
#####Hermit 16#######
####################
HermitTP16<-read.csv("Hermit_TP16_18C_30March2019.csv",header=TRUE)


HermitTP16$Date<-as.POSIXct(HermitTP16$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.16<-lm(Oxygen~Date,data=HermitTP16)
summary(m10.16)
abline(m10.16,lwd=2, lty=1)
slope10.16<-coef(m10.16)[2]*60 #converts to Mins
slope10.16


####################
#####Hermit 20#######
####################
HermitTP20<-read.csv("Hermit_TP20_18C_30March2019.csv",header=TRUE)


HermitTP20$Date<-as.POSIXct(HermitTP20$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP20, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.20<-lm(Oxygen~Date,data=HermitTP20)
summary(m10.20)
abline(m10.20,lwd=2, lty=1)
slope10.20<-coef(m10.20)[2]*60 #converts to Mins
slope10.20


####################
#####Hermit 29#######
####################
HermitTP29<-read.csv("Hermit_TP29_18C_30March2019.csv",header=TRUE)


HermitTP29$Date<-as.POSIXct(HermitTP29$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP29, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.29<-lm(Oxygen~Date,data=HermitTP29)
summary(m10.29)
abline(m10.29,lwd=2, lty=1)
slope10.29<-coef(m10.29)[2]*60 #converts to Mins
slope10.29


####################
#####Hermit 31#######
####################
HermitTP31<-read.csv("Hermit_TP31_18C_30March2019.csv",header=TRUE)


HermitTP31$Date<-as.POSIXct(HermitTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.31<-lm(Oxygen~Date,data=HermitTP31)
summary(m10.31)
abline(m10.31,lwd=2, lty=1)
slope10.31<-coef(m10.31)[2]*60 #converts to Mins
slope10.31


####################
#####Hermit 33####### Don't Include R2<0.90
####################
HermitTP33<-read.csv("Hermit_TP33_18C_30March2019.csv",header=TRUE)


HermitTP33$Date<-as.POSIXct(HermitTP33$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.33<-lm(Oxygen~Date,data=HermitTP33)
summary(m10.33)
abline(m10.33,lwd=2, lty=1)
slope10.33<-coef(m10.33)[2]*60 #converts to Mins
slope10.33

####################
#####BLANK 10C #######
####################
HermitBL<-read.csv("Hermit_BLANK_18C_30March2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m10.bl)
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.003437721


####################
#####BLANK 7 10C #######
####################
HermitBL7<-read.csv("Hermit_BLANK7_18C_30March2019.csv",header=TRUE)


HermitBL7$Date<-as.POSIXct(HermitBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl7<-lm(Oxygen~Date,data=HermitBL7)
summary(m10.bl7)
abline(m10.bl7,lwd=2, lty=1)
slope10.bl7<-coef(m10.bl7)[2]*60 #converts to Mins
slope10.bl7 #0.00688296 

#Avg Blank
b<-c(0.01118598,0.003752299)
mean(b)



#######################################################
####################HERMITS 26C#########################
##################30 March 2019########################
setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka H-30March2019/Hermits-26C")
####################
#####Hermit 18#######
####################
HermitTP10<-read.csv("Hermit_TP10_26C_30March2019.csv",header=TRUE)


HermitTP10$Date<-as.POSIXct(HermitTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.10<-lm(Oxygen~Date,data=HermitTP10)
summary(m10.10)
abline(m10.10,lwd=2, lty=1)
slope10.10<-coef(m10.10)[2]*60 #converts to Mins
slope10.10


####################
#####Hermit 16#######
####################
HermitTP16<-read.csv("Hermit_TP16_26C_30March2019.csv",header=TRUE)


HermitTP16$Date<-as.POSIXct(HermitTP16$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.16<-lm(Oxygen~Date,data=HermitTP16)
summary(m10.16)
abline(m10.16,lwd=2, lty=1)
slope10.16<-coef(m10.16)[2]*60 #converts to Mins
slope10.16


####################
#####Hermit 20#######
####################
HermitTP20<-read.csv("Hermit_TP20_26C_30March2019.csv",header=TRUE)


HermitTP20$Date<-as.POSIXct(HermitTP20$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP20, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.20<-lm(Oxygen~Date,data=HermitTP20)
summary(m10.20)
abline(m10.20,lwd=2, lty=1)
slope10.20<-coef(m10.20)[2]*60 #converts to Mins
slope10.20


####################
#####Hermit 29#######
####################
HermitTP29<-read.csv("Hermit_TP29_26C_30March2019.csv",header=TRUE)


HermitTP29$Date<-as.POSIXct(HermitTP29$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP29, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.29<-lm(Oxygen~Date,data=HermitTP29)
summary(m10.29)
abline(m10.29,lwd=2, lty=1)
slope10.29<-coef(m10.29)[2]*60 #converts to Mins
slope10.29


####################
#####Hermit 31#######
####################
HermitTP31<-read.csv("Hermit_TP31_26C_30March2019.csv",header=TRUE)


HermitTP31$Date<-as.POSIXct(HermitTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.31<-lm(Oxygen~Date,data=HermitTP31)
summary(m10.31)
abline(m10.31,lwd=2, lty=1)
slope10.31<-coef(m10.31)[2]*60 #converts to Mins
slope10.31


####################
#####Hermit 33#######Remove R2<0.90
####################
HermitTP33<-read.csv("Hermit_TP33_26C_30March2019.csv",header=TRUE)


HermitTP33$Date<-as.POSIXct(HermitTP33$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.33<-lm(Oxygen~Date,data=HermitTP33)
summary(m26.33)
abline(m26.33,lwd=2, lty=1)
slope26.33<-coef(m26.33)[2]*60 #converts to Mins
slope26.33

####################
#####BLANK 26C #######
####################
HermitBL<-read.csv("Hermit_BLANK_26C_30March2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m26.bl)
abline(m26.bl,lwd=2, lty=1)
slope26.bl<-coef(m26.bl)[2]*60 #converts to Mins
slope26.bl #0.008290018 


####################
#####BLANK 7 10C #######
####################
HermitBL7<-read.csv("Hermit_BLANK7_26C_30March2019.csv",header=TRUE)


HermitBL7$Date<-as.POSIXct(HermitBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl7<-lm(Oxygen~Date,data=HermitBL7)
summary(m10.bl7)
abline(m10.bl7,lwd=2, lty=1)
slope10.bl7<-coef(m10.bl7)[2]*60 #converts to Mins
slope10.bl7 #0.004067572 

#Avg Blank
b<-c(0.004067572,0.008290018)
mean(b)





setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka H-31March2019/Hermits-10C")
#######################################################
####################HERMITS 10C#########################
##################31 March 2019########################

####################
#####Hermit 03######
####################
HermitTP03<-read.csv("Hermit_TP03_10C_31March2019.csv",header=TRUE)


HermitTP03$Date<-as.POSIXct(HermitTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.03<-lm(Oxygen~Date,data=HermitTP03)
summary(m10.03)
abline(m10.03,lwd=2, lty=1)
slope10.03<-coef(m10.03)[2]*60 #converts to Mins
slope10.03


####################
#####Hermit 06#######Removed R2<0.90
####################
HermitTP06<-read.csv("Hermit_TP06_10C_31March2019.csv",header=TRUE)


HermitTP06$Date<-as.POSIXct(HermitTP06$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP06, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.06<-lm(Oxygen~Date,data=HermitTP06)
summary(m10.06)
abline(m10.06,lwd=2, lty=1)
slope10.06<-coef(m10.06)[2]*60 #converts to Mins
slope10.06


####################
#####Hermit 13#######
####################
HermitTP13<-read.csv("Hermit_TP13_10C_31March2019.csv",header=TRUE)


HermitTP13$Date<-as.POSIXct(HermitTP13$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.13<-lm(Oxygen~Date,data=HermitTP13)
summary(m10.13)
abline(m10.13,lwd=2, lty=1)
slope10.13<-coef(m10.13)[2]*60 #converts to Mins
slope10.13


####################
#####Hermit 15#######
####################
HermitTP15<-read.csv("Hermit_TP15_10C_31March2019.csv",header=TRUE)


HermitTP15$Date<-as.POSIXct(HermitTP15$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.15<-lm(Oxygen~Date,data=HermitTP15)
summary(m10.15)
abline(m10.15,lwd=2, lty=1)
slope10.15<-coef(m10.15)[2]*60 #converts to Mins
slope10.15


####################
#####Hermit 18#######
####################
HermitTP18<-read.csv("Hermit_TP18_10C_31March2019.csv",header=TRUE)


HermitTP18$Date<-as.POSIXct(HermitTP18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.18<-lm(Oxygen~Date,data=HermitTP18)
summary(m10.18)
abline(m10.18,lwd=2, lty=1)
slope10.18<-coef(m10.18)[2]*60 #converts to Mins
slope10.18


####################
#####Hermit 19#######
####################
HermitTP19<-read.csv("Hermit_TP19_10C_31March2019.csv",header=TRUE)


HermitTP19$Date<-as.POSIXct(HermitTP19$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP19, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.19<-lm(Oxygen~Date,data=HermitTP19)
summary(m10.19)
abline(m10.19,lwd=2, lty=1)
slope10.19<-coef(m10.19)[2]*60 #converts to Mins
slope10.19

####################
#####Hermit 28#######
####################
HermitTP28<-read.csv("Hermit_TP28_10C_31March2019.csv",header=TRUE)


HermitTP28$Date<-as.POSIXct(HermitTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.28<-lm(Oxygen~Date,data=HermitTP28)
summary(m10.28)
abline(m10.28,lwd=2, lty=1)
slope10.28<-coef(m10.28)[2]*60 #converts to Mins
slope10.28


####################
#####Hermit 32#######
####################
HermitTP32<-read.csv("Hermit_TP32_10C_31March2019.csv",header=TRUE)


HermitTP32$Date<-as.POSIXct(HermitTP32$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP32, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.32<-lm(Oxygen~Date,data=HermitTP32)
summary(m10.32)
abline(m10.32,lwd=2, lty=1)
slope10.32<-coef(m10.32)[2]*60 #converts to Mins
slope10.32
####################
#####BLANK 10C #######
####################
HermitBL<-read.csv("Hermit_BLANK_10C_31March2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m10.bl) 
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.003437721

####################
#####BLANK 7 10C #######
####################
HermitBL7<-read.csv("Hermit_BLANK7_10C_31March2019.csv",header=TRUE)


HermitBL7$Date<-as.POSIXct(HermitBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl7<-lm(Oxygen~Date,data=HermitBL7)
summary(m10.bl7)
abline(m10.bl7,lwd=2, lty=1)
slope10.bl7<-coef(m10.bl7)[2]*60 #converts to Mins
slope10.bl7 #0.00688296 




setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka H-31March2019/Hermits-18C")
#######################################################
####################HERMITS 18C#########################
##################31 March 2019########################

####################
#####Hermit 03######
####################
HermitTP03<-read.csv("Hermit_TP03_18C_31March2019.csv",header=TRUE)


HermitTP03$Date<-as.POSIXct(HermitTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.03<-lm(Oxygen~Date,data=HermitTP03)
summary(m18.03)
abline(m18.03,lwd=2, lty=1)
slope18.03<-coef(m18.03)[2]*60 #converts to Mins
slope18.03


####################
#####Hermit 06#######Remove R2<0.90
####################
HermitTP06<-read.csv("Hermit_TP06_18C_31March2019.csv",header=TRUE)


HermitTP06$Date<-as.POSIXct(HermitTP06$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP06, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.06<-lm(Oxygen~Date,data=HermitTP06)
summary(m18.06)
abline(m18.06,lwd=2, lty=1)
slope18.06<-coef(m18.06)[2]*60 #converts to Mins
slope18.06


####################
#####Hermit 13#######
####################
HermitTP13<-read.csv("Hermit_TP13_18C_31March2019.csv",header=TRUE)


HermitTP13$Date<-as.POSIXct(HermitTP13$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.13<-lm(Oxygen~Date,data=HermitTP13)
summary(m18.13)
abline(m18.13,lwd=2, lty=1)
slope18.13<-coef(m18.13)[2]*60 #converts to Mins
slope18.13


####################
#####Hermit 15#######
####################
HermitTP15<-read.csv("Hermit_TP15_18C_31March2019.csv",header=TRUE)


HermitTP15$Date<-as.POSIXct(HermitTP15$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.15<-lm(Oxygen~Date,data=HermitTP15)
summary(m18.15)
abline(m18.15,lwd=2, lty=1)
slope18.15<-coef(m18.15)[2]*60 #converts to Mins
slope18.15


####################
#####Hermit 18#######
####################
HermitTP18<-read.csv("Hermit_TP18_18C_31March2019.csv",header=TRUE)


HermitTP18$Date<-as.POSIXct(HermitTP18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.18<-lm(Oxygen~Date,data=HermitTP18)
summary(m18.18)
abline(m18.18,lwd=2, lty=1)
slope18.18<-coef(m18.18)[2]*60 #converts to Mins
slope18.18


####################
#####Hermit 19#######
####################
HermitTP19<-read.csv("Hermit_TP19_18C_31March2019.csv",header=TRUE)


HermitTP19$Date<-as.POSIXct(HermitTP19$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP19, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.19<-lm(Oxygen~Date,data=HermitTP19)
summary(m18.19)
abline(m18.19,lwd=2, lty=1)
slope18.19<-coef(m18.19)[2]*60 #converts to Mins
slope18.19

####################
#####Hermit 28#######
####################
HermitTP28<-read.csv("Hermit_TP28_18C_31March2019.csv",header=TRUE)


HermitTP28$Date<-as.POSIXct(HermitTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.28<-lm(Oxygen~Date,data=HermitTP28)
summary(m18.28)
abline(m18.28,lwd=2, lty=1)
slope18.28<-coef(m18.28)[2]*60 #converts to Mins
slope18.28


####################
#####Hermit 32#######
####################
HermitTP32<-read.csv("Hermit_TP32_18C_31March2019.csv",header=TRUE)


HermitTP32$Date<-as.POSIXct(HermitTP32$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP32, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.32<-lm(Oxygen~Date,data=HermitTP32)
summary(m18.32)
abline(m18.32,lwd=2, lty=1)
slope18.32<-coef(m18.32)[2]*60 #converts to Mins
slope18.32
####################
#####BLANK 10C #######
####################
HermitBL<-read.csv("Hermit_BLANK_18C_31March2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m18.bl) 
abline(m18.bl,lwd=2, lty=1)
slope18.bl<-coef(m18.bl)[2]*60 #converts to Mins
slope18.bl #0.01057273 

####################
#####BLANK 7 10C #######
####################
HermitBL7<-read.csv("Hermit_BLANK7_18C_31March2019.csv",header=TRUE)


HermitBL7$Date<-as.POSIXct(HermitBL7$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl7<-lm(Oxygen~Date,data=HermitBL7)
summary(m18.bl7)
abline(m18.bl7,lwd=2, lty=1)
slope18.bl7<-coef(m18.bl7)[2]*60 #converts to Mins
slope18.bl7 #0.007518365

#Avg Blank
b<-c(0.01057273,0.007518365)
mean(b)


setwd("/Users/racinerangel/Desktop/Sitka March 2019/Sitka H-31March2019/Hermits-26C")
#######################################################
####################HERMITS 26C#########################
##################31 March 2019########################

####################
#####Hermit 03######
####################
HermitTP03<-read.csv("Hermit_TP03_26C_31March2019.csv",header=TRUE)


HermitTP03$Date<-as.POSIXct(HermitTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.03<-lm(Oxygen~Date,data=HermitTP03)
summary(m26.03)
abline(m26.03,lwd=2, lty=1)
slope26.03<-coef(m26.03)[2]*60 #converts to Mins
slope26.03


####################
#####Hermit 06#######Removed R2<0.90
####################
HermitTP06<-read.csv("Hermit_TP06_26C_31March2019.csv",header=TRUE)


HermitTP06$Date<-as.POSIXct(HermitTP06$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~HermitTP06$Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP06, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.06<-lm(Oxygen~HermitTP06$Date,data=HermitTP06)
summary(m26.06)
abline(m26.06,lwd=2, lty=1)
slope26.06<-coef(m26.06)[2]*60 #converts to Mins
slope26.06


####################
#####Hermit 13#######
####################
HermitTP13<-read.csv("Hermit_TP13_26C_31March2019.csv",header=TRUE)


HermitTP13$Date<-as.POSIXct(HermitTP13$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.13<-lm(Oxygen~Date,data=HermitTP13)
summary(m26.13)
abline(m26.13,lwd=2, lty=1)
slope26.13<-coef(m26.13)[2]*60 #converts to Mins
slope26.13


####################
#####Hermit 15#######
####################
HermitTP15<-read.csv("Hermit_TP15_26C_31March2019.csv",header=TRUE)


HermitTP15$Date<-as.POSIXct(HermitTP15$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.15<-lm(Oxygen~Date,data=HermitTP15)
summary(m26.15)
abline(m26.15,lwd=2, lty=1)
slope26.15<-coef(m26.15)[2]*60 #converts to Mins
slope26.15


####################
#####Hermit 18#######
####################
HermitTP18<-read.csv("Hermit_TP18_26C_31March2019.csv",header=TRUE)


HermitTP18$Date<-as.POSIXct(HermitTP18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.18<-lm(Oxygen~Date,data=HermitTP18)
summary(m26.18)
abline(m26.18,lwd=2, lty=1)
slope26.18<-coef(m26.18)[2]*60 #converts to Mins
slope26.18


####################
#####Hermit 19#######
####################
HermitTP19<-read.csv("Hermit_TP19_26C_31March2019.csv",header=TRUE)


HermitTP19$Date<-as.POSIXct(HermitTP19$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP19, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.19<-lm(Oxygen~Date,data=HermitTP19)
summary(m26.19)
abline(m26.19,lwd=2, lty=1)
slope26.19<-coef(m26.19)[2]*60 #converts to Mins
slope26.19

####################
#####Hermit 28#######
####################
HermitTP28<-read.csv("Hermit_TP28_26C_31March2019.csv",header=TRUE)


HermitTP28$Date<-as.POSIXct(HermitTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.28<-lm(Oxygen~Date,data=HermitTP28)
summary(m26.28)
abline(m26.28,lwd=2, lty=1)
slope26.28<-coef(m26.28)[2]*60 #converts to Mins
slope26.28


####################
#####Hermit 32#######
########### #########
HermitTP32<-read.csv("Hermit_TP32_26C_31March2019.csv",header=TRUE)


HermitTP32$Date<-as.POSIXct(HermitTP32$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP32, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.32<-lm(Oxygen~Date,data=HermitTP32)
summary(m26.32)
abline(m26.32,lwd=2, lty=1)
slope26.32<-coef(m26.32)[2]*60 #converts to Mins
slope26.32
####################
#####BLANK 10C #######
####################
HermitBL<-read.csv("Hermit_BLANK_26C_31March2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m26.bl) 
abline(m26.bl,lwd=2, lty=1)
slope26.bl<-coef(m26.bl)[2]*60 #converts to Mins
slope26.bl #0.007316067 




################################################################
#MUSSELS (Mytilus trossulus)
################################################################

#JULY 2019-----------------------------------------------------------------------

setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-17July2019/Mussel-10C")

#######################################################
####################MUSSEL 10C#########################
##################17 July 2019########################


####################
#####Mussel 7 #######
####################
MusselTP07<-read.csv("Mussel_TP7_10C_17July2019.csv",header=TRUE)


MusselTP07$Date<-as.POSIXct(MusselTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.07<-lm(Oxygen~Date,data=MusselTP07)
summary(m10.07)
abline(m10.07,lwd=2, lty=1)
slope10.07<-coef(m10.07)[2]*60 #converts to Mins
slope10.07


####################
#####Mussel 10 #######
####################
MusselTP10<-read.csv("Mussel_TP10_10C_17July2019.csv",header=TRUE)


MusselTP10$Date<-as.POSIXct(MusselTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(6,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.10<-lm(Oxygen~Date,data=MusselTP10)
summary(m10.10)
abline(m10.10,lwd=2, lty=1)
slope10.10<-coef(m10.10)[2]*60 #converts to Mins
slope10.10


####################
#####BLANK 10C #######
####################
MusselBL<-read.csv("Mussel_BLANK_10C_17July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

MusselBL<-MusselBL[-c(1:273),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m10.bl)
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.001942634 0.84

####################
#####BLANK 2 10C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_10C_17July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

MusselBL2<-MusselBL2[-c(1),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m10.bl2)
abline(m10.bl2,lwd=2, lty=1)
slope10.bl2<-coef(m10.bl2)[2]*60 #converts to Mins
slope10.bl2 #0.001182782 0.71

#Avg Blank
b<-c(0.004052301, 0.004810577)
mean(b) #0.001562708


#############################################################################
##############################18 C###########################################
#############################################################################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-17July2019/Mussel-18C")

####################
#####Mussel 7#######
####################
MusselTP07<-read.csv("Mussel_TP7_18C_17July2019.csv",header=TRUE)


MusselTP07$Date<-as.POSIXct(MusselTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.07<-lm(Oxygen~Date,data=MusselTP07)
summary(m18.07)
abline(m18.07,lwd=2, lty=1)
slope18.07<-coef(m18.07)[2]*60 #converts to Mins
slope18.07


####################
#####Mussel 10#######
####################
MusselTP10<-read.csv("Mussel_TP10_18C_17July2019.csv",header=TRUE)


MusselTP10$Date<-as.POSIXct(MusselTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.10<-lm(Oxygen~Date,data=MusselTP10)
summary(m18.10)
abline(m18.10,lwd=2, lty=1)
slope18.10<-coef(m18.10)[2]*60 #converts to Mins
slope18.10



####################
#####BLANK 18C #######
####################
MusselBL<-read.csv("Mussel_BLANK_18C_17July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m18.bl)
abline(m18.bl,lwd=2, lty=1)
slope18.bl<-coef(m18.bl)[2]*60 #converts to Mins
slope18.bl #0.00463668  0.90

####################
#####BLANK 2 18C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_18C_17July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m18.bl2)
abline(m18.bl2,lwd=2, lty=1)
slope18.bl2<-coef(m18.bl2)[2]*60 #converts to Mins
slope18.bl2 #0.003507674  0.85

#Avg Blank
b<-c(0.00463668, 0.003507674)
mean(b) #0.004072177


#############################################################################
##############################26 C###########################################
#############################################################################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-17July2019/Mussel-26C")


####################
#####Mussel 7#######
####################
MusselTP07<-read.csv("Mussel_TP7_26C_17July2019.csv",header=TRUE)


MusselTP07$Date<-as.POSIXct(MusselTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.07<-lm(Oxygen~Date,data=MusselTP07)
summary(m26.07)
abline(m26.07,lwd=2, lty=1)
slope26.07<-coef(m26.07)[2]*60 #converts to Mins
slope26.07



####################
#####Mussel 10#######
####################
MusselTP10<-read.csv("Mussel_TP10_26C_17July2019.csv",header=TRUE)


MusselTP10$Date<-as.POSIXct(MusselTP10$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.10<-lm(Oxygen~Date,data=MusselTP10)
summary(m26.10)
abline(m26.10,lwd=2, lty=1)
slope26.10<-coef(m26.10)[2]*60 #converts to Mins
slope26.10




####################
#####BLANK 26C #######
####################
MusselBL<-read.csv("Mussel_BLANK_26C_17July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m26.bl)
abline(m26.bl,lwd=2, lty=1)
slope26.bl<-coef(m26.bl)[2]*60 #converts to Mins
slope26.bl #0.0009556809  0.23

####################
#####BLANK 2 26C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_26C_17July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m26.bl2)
abline(m26.bl2,lwd=2, lty=1)
slope26.bl2<-coef(m26.bl2)[2]*60 #converts to Mins
slope26.bl2 #0.003507674  0.85

#Avg Blank
b<-c(0.0009556809, 0.0004798192)
mean(b) #0.00071775



setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-18July2019/Mussel-10C")
#######################################################
####################MUSSEL 10C#########################
##################18 July 2019########################

####################
#####Mussel 6 #######
####################
MusselTP06<-read.csv("Mussel_TP6_10C_18July2019.csv",header=TRUE)


MusselTP06$Date<-as.POSIXct(MusselTP06$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP06, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.06<-lm(Oxygen~Date,data=MusselTP06)
summary(m10.06)
abline(m10.06,lwd=2, lty=1)
slope10.06<-coef(m10.06)[2]*60 #converts to Mins
slope10.06



####################
#####Mussel 31#######
####################
MusselTP31<-read.csv("Mussel_TP31_10C_18July2019.csv",header=TRUE)


MusselTP31$Date<-as.POSIXct(MusselTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.31<-lm(Oxygen~Date,data=MusselTP31)
summary(m10.31)
abline(m10.31,lwd=2, lty=1)
slope10.31<-coef(m10.31)[2]*60 #converts to Mins
slope10.31



####################
#####BLANK 10C #######
####################
MusselBL<-read.csv("Mussel_BLANK_10C_18July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m10.bl)
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.00690857 0..61

####################
#####BLANK 2 10C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_10C_18July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m10.bl2)
abline(m10.bl2,lwd=2, lty=1)
slope10.bl2<-coef(m10.bl2)[2]*60 #converts to Mins
slope10.bl2 #0.003415792 0.71

#Avg Blank
b<-c(0.00690857, 0.003415792)
mean(b) #0.005162181


#############################################################################
##############################18 C###########################################
#############################################################################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-18July2019/Mussel-18C")
####################
#####Mussel 6#######
####################
MusselTP06<-read.csv("Mussel_TP6_18C_18July2019.csv",header=TRUE)


MusselTP06$Date<-as.POSIXct(MusselTP06$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP06, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.06<-lm(Oxygen~Date,data=MusselTP06)
summary(m18.06)
abline(m18.06,lwd=2, lty=1)
slope18.06<-coef(m18.06)[2]*60 #converts to Mins
slope18.06



####################
#####Mussel 31#######
####################
MusselTP31<-read.csv("Mussel_TP31_18C_18July2019.csv",header=TRUE)


MusselTP31$Date<-as.POSIXct(MusselTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.31<-lm(Oxygen~Date,data=MusselTP31)
summary(m18.31)
abline(m18.31,lwd=2, lty=1)
slope18.31<-coef(m18.31)[2]*60 #converts to Mins
slope18.31


####################
#####BLANK 18C #######
####################
MusselBL<-read.csv("Mussel_BLANK_18C_18July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m18.bl)
abline(m18.bl,lwd=2, lty=1)
slope18.bl<-coef(m18.bl)[2]*60 #converts to Mins
slope18.bl #0.003411263 

####################
#####BLANK 2 18C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_18C_18July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m18.bl2)
abline(m18.bl2,lwd=2, lty=1)
slope18.bl2<-coef(m18.bl2)[2]*60 #converts to Mins
slope18.bl2 #0.002091633

#Avg Blank
b<-c(0.003411263,0.002091633)
mean(b) #0.002751448


#############################################################################
##############################26 C###########################################
#############################################################################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-18July2019/Mussel-26C")

####################
#####Mussel 6#######
####################
MusselTP06<-read.csv("Mussel_TP6_26C_18July2019.csv",header=TRUE)


MusselTP06$Date<-as.POSIXct(MusselTP06$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP06, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.06<-lm(Oxygen~Date,data=MusselTP06)
summary(m26.06)
abline(m26.06,lwd=2, lty=1)
slope26.06<-coef(m26.06)[2]*60 #converts to Mins
slope26.06



####################
#####Mussel 31#######
####################
MusselTP31<-read.csv("Mussel_TP31_26C_18July2019.csv",header=TRUE)


MusselTP31$Date<-as.POSIXct(MusselTP31$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP31, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.31<-lm(Oxygen~Date,data=MusselTP31)
summary(m26.31)
abline(m26.31,lwd=2, lty=1)
slope26.31<-coef(m26.31)[2]*60 #converts to Mins
slope26.31


####################
#####BLANK 26C #######
####################
MusselBL<-read.csv("Mussel_BLANK_26C_18July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m26.bl)
abline(m26.bl,lwd=2, lty=1)
slope26.bl<-coef(m26.bl)[2]*60 #converts to Mins
slope26.bl #0.00393212  0.23

####################
#####BLANK 2 26C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_26C_18July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m26.bl2)
abline(m26.bl2,lwd=2, lty=1)
slope26.bl2<-coef(m26.bl2)[2]*60 #converts to Mins
slope26.bl2 #0.00599035   0.85

#Avg Blank
b<-c(0.00393212, 0.00599035)
mean(b) #0.004961235



#######################################################
####################MUSSEL 10C#########################
##################19 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-19July2019/Mussel-10C")
####################
#####Mussel 1 #######
####################
MusselTP01<-read.csv("Mussel_TP1_10C_19July2019.csv",header=TRUE)


MusselTP01$Date<-as.POSIXct(MusselTP01$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP01, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.01<-lm(Oxygen~Date,data=MusselTP01)
summary(m10.01)
abline(m10.01,lwd=2, lty=1)
slope10.01<-coef(m10.01)[2]*60 #converts to Mins
slope10.01


####################
#####Mussel 2#######
####################
MusselTP02<-read.csv("Mussel_TP2_10C_19July2019.csv",header=TRUE)


MusselTP02$Date<-as.POSIXct(MusselTP02$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP02, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.02<-lm(Oxygen~Date,data=MusselTP02)
summary(m10.02)
abline(m10.02,lwd=2, lty=1)
slope10.02<-coef(m10.02)[2]*60 #converts to Mins
slope10.02


####################
#####Mussel 14#######
####################
MusselTP14<-read.csv("Mussel_TP14_10C_19July2019.csv",header=TRUE)


MusselTP14$Date<-as.POSIXct(MusselTP14$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP14, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.14<-lm(Oxygen~Date,data=MusselTP14)
summary(m10.14)
abline(m10.14,lwd=2, lty=1)
slope10.14<-coef(m10.14)[2]*60 #converts to Mins
slope10.14

####################
#####Mussel 22#######
####################
MusselTP22<-read.csv("Mussel_TP22_10C_19July2019.csv",header=TRUE)


MusselTP22$Date<-as.POSIXct(MusselTP22$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP22, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.22<-lm(Oxygen~Date,data=MusselTP22)
summary(m10.22)
abline(m10.22,lwd=2, lty=1)
slope10.22<-coef(m10.22)[2]*60 #converts to Mins
slope10.22

####################
#####Mussel 23#######
####################
MusselTP23<-read.csv("Mussel_TP23_10C_19July2019.csv",header=TRUE)


MusselTP23$Date<-as.POSIXct(MusselTP23$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP23, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.23<-lm(Oxygen~Date,data=MusselTP23)
summary(m10.23)
abline(m10.23,lwd=2, lty=1)
slope10.23<-coef(m10.23)[2]*60 #converts to Mins
slope10.23

####################
#####Mussel 24#######
####################
MusselTP24<-read.csv("Mussel_TP24_10C_19July2019.csv",header=TRUE)


MusselTP24$Date<-as.POSIXct(MusselTP24$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP24, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.24<-lm(Oxygen~Date,data=MusselTP24)
summary(m10.24)
abline(m10.24,lwd=2, lty=1)
slope10.24<-coef(m10.24)[2]*60 #converts to Mins
slope10.24


####################
#####Mussel 28#######
####################
MusselTP28<-read.csv("Mussel_TP28_10C_19July2019.csv",header=TRUE)


MusselTP28$Date<-as.POSIXct(MusselTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.28<-lm(Oxygen~Date,data=MusselTP28)
summary(m10.28)
abline(m10.28,lwd=2, lty=1)
slope10.28<-coef(m10.28)[2]*60 #converts to Mins
slope10.28

####################
#####Mussel 32#######
####################
MusselTP32<-read.csv("Mussel_TP32_10C_19July2019.csv",header=TRUE)


MusselTP32$Date<-as.POSIXct(MusselTP32$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP32, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.32<-lm(Oxygen~Date,data=MusselTP32)
summary(m10.32)
abline(m10.32,lwd=2, lty=1)
slope10.32<-coef(m10.32)[2]*60 #converts to Mins
slope10.32

####################
#####BLANK 10C #######
####################
MusselBL<-read.csv("Mussel_BLANK_10C_19July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m10.bl)
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.003038853

####################
#####BLANK 2 10C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_10C_19July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m10.bl2)
abline(m10.bl2,lwd=2, lty=1)
slope10.bl2<-coef(m10.bl2)[2]*60 #converts to Mins
slope10.bl2 #0.000605001 

#Avg Blank
b<-c(0.003038853, 0.000605001)
mean(b) #0.001821927

#############################################################################
##############################18 C###########################################
#############################################################################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-19July2019/Mussel-18C")
####################
#####Mussel 1#######
####################
MusselTP1<-read.csv("Mussel_TP1_18C_19July2019.csv",header=TRUE)


MusselTP1$Date<-as.POSIXct(MusselTP1$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP1, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.1<-lm(Oxygen~Date,data=MusselTP1)
summary(m18.1)
abline(m18.1,lwd=2, lty=1)
slope18.1<-coef(m18.1)[2]*60 #converts to Mins
slope18.1


####################
#####Mussel 2#######
####################
MusselTP2<-read.csv("Mussel_TP2_18C_19July2019.csv",header=TRUE)


MusselTP2$Date<-as.POSIXct(MusselTP2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.2<-lm(Oxygen~Date,data=MusselTP2)
summary(m18.2)
abline(m18.2,lwd=2, lty=1)
slope18.2<-coef(m18.2)[2]*60 #converts to Mins
slope18.2

####################
#####Mussel 14#######
####################
MusselTP14<-read.csv("Mussel_TP14_18C_19July2019.csv",header=TRUE)


MusselTP14$Date<-as.POSIXct(MusselTP14$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP14, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.14<-lm(Oxygen~Date,data=MusselTP14)
summary(m18.14)
abline(m18.14,lwd=2, lty=1)
slope18.14<-coef(m18.14)[2]*60 #converts to Mins
slope18.14

####################
#####Mussel 22#######
####################
MusselTP22<-read.csv("Mussel_TP22_18C_19July2019.csv",header=TRUE)


MusselTP22$Date<-as.POSIXct(MusselTP22$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP22, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.22<-lm(Oxygen~Date,data=MusselTP22)
summary(m18.22)
abline(m18.22,lwd=2, lty=1)
slope18.22<-coef(m18.22)[2]*60 #converts to Mins
slope18.22

####################
#####Mussel 23#######
####################
MusselTP23<-read.csv("Mussel_TP23_18C_19July2019.csv",header=TRUE)


MusselTP23$Date<-as.POSIXct(MusselTP23$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP23, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.23<-lm(Oxygen~Date,data=MusselTP23)
summary(m18.23)
abline(m18.23,lwd=2, lty=1)
slope18.23<-coef(m18.23)[2]*60 #converts to Mins
slope18.23

####################
#####Mussel 24#######
####################
MusselTP24<-read.csv("Mussel_TP24_18C_19July2019.csv",header=TRUE)


MusselTP24$Date<-as.POSIXct(MusselTP24$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP24, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.24<-lm(Oxygen~Date,data=MusselTP24)
summary(m18.24)
abline(m18.24,lwd=2, lty=1)
slope18.24<-coef(m18.24)[2]*60 #converts to Mins
slope18.24

####################
#####Mussel 28#######
####################
MusselTP28<-read.csv("Mussel_TP28_18C_19July2019.csv",header=TRUE)


MusselTP28$Date<-as.POSIXct(MusselTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.28<-lm(Oxygen~Date,data=MusselTP28)
summary(m18.28)
abline(m18.28,lwd=2, lty=1)
slope18.28<-coef(m18.28)[2]*60 #converts to Mins
slope18.28

####################
#####Mussel 32#######
####################
MusselTP32<-read.csv("Mussel_TP32_18C_19July2019.csv",header=TRUE)


MusselTP32$Date<-as.POSIXct(MusselTP32$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP32, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.32<-lm(Oxygen~Date,data=MusselTP32)
summary(m18.32)
abline(m18.32,lwd=2, lty=1)
slope18.32<-coef(m18.32)[2]*60 #converts to Mins
slope18.32


####################
#####BLANK 18C #######
####################
MusselBL<-read.csv("Mussel_BLANK_18C_19July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m18.bl)
abline(m18.bl,lwd=2, lty=1)
slope18.bl<-coef(m18.bl)[2]*60 #converts to Mins
slope18.bl #0.006888164 

####################
#####BLANK 2 18C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_18C_19July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m18.bl2)
abline(m18.bl2,lwd=2, lty=1)
slope18.bl2<-coef(m18.bl2)[2]*60 #converts to Mins
slope18.bl2 #0.008163069 

#Avg Blank
b<-c(0.006888164 ,0.008163069)
mean(b) #0.007525617


#############################################################################
##############################26 C###########################################
#############################################################################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-19July2019/Mussel-26C")

####################
#####Mussel 1#######
####################
MusselTP01<-read.csv("Mussel_TP1_26C_19July2019.csv",header=TRUE)


MusselTP01$Date<-as.POSIXct(MusselTP01$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP01, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.01<-lm(Oxygen~Date,data=MusselTP01)
summary(m26.01)
abline(m26.01,lwd=2, lty=1)
slope26.01<-coef(m26.01)[2]*60 #converts to Mins
slope26.01


####################
#####Mussel 2#######
####################
MusselTP02<-read.csv("Mussel_TP2_26C_19July2019.csv",header=TRUE)


MusselTP02$Date<-as.POSIXct(MusselTP02$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP02, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.02<-lm(Oxygen~Date,data=MusselTP02)
summary(m26.02)
abline(m26.02,lwd=2, lty=1)
slope26.02<-coef(m26.02)[2]*60 #converts to Mins
slope26.02

####################
#####Mussel 14#######
####################
MusselTP14<-read.csv("Mussel_TP14_26C_19July2019.csv",header=TRUE)


MusselTP14$Date<-as.POSIXct(MusselTP14$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP14, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.14<-lm(Oxygen~Date,data=MusselTP14)
summary(m26.14)
abline(m26.14,lwd=2, lty=1)
slope26.14<-coef(m26.14)[2]*60 #converts to Mins
slope26.14

####################
#####Mussel 22#######
####################
MusselTP22<-read.csv("Mussel_TP22_26C_19July2019.csv",header=TRUE)


MusselTP22$Date<-as.POSIXct(MusselTP22$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP22, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.22<-lm(Oxygen~Date,data=MusselTP22)
summary(m26.22)
abline(m26.22,lwd=2, lty=1)
slope26.22<-coef(m26.22)[2]*60 #converts to Mins
slope26.22


####################
#####Mussel 23#######
#################### 
MusselTP23<-read.csv("Mussel_TP23_26C_19July2019.csv",header=TRUE)


MusselTP23$Date<-as.POSIXct(MusselTP23$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP23, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.23<-lm(Oxygen~Date+Seconds,data=MusselTP23)
summary(m26.23)
abline(m26.23,lwd=2, lty=1)
slope26.23<-coef(m26.23)[3]*60 #converts to Mins
slope26.23


####################
#####Mussel 24#######
####################
MusselTP24<-read.csv("Mussel_TP24_26C_19July2019.csv",header=TRUE)


MusselTP24$Date<-as.POSIXct(MusselTP24$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP24, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.24<-lm(Oxygen~Date,data=MusselTP24)
summary(m26.24)
abline(m26.24,lwd=2, lty=1)
slope26.24<-coef(m26.24)[2]*60 #converts to Mins
slope26.24

####################
#####Mussel 28#######
####################
MusselTP28<-read.csv("Mussel_TP28_26C_19July2019.csv",header=TRUE)


MusselTP28$Date<-as.POSIXct(MusselTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.28<-lm(Oxygen~Date,data=MusselTP28)
summary(m26.28)
abline(m26.28,lwd=2, lty=1)
slope26.28<-coef(m26.28)[2]*60 #converts to Mins
slope26.28

####################
#####Mussel 32#######
####################
MusselTP32<-read.csv("Mussel_TP32_26C_19July2019.csv",header=TRUE)


MusselTP32$Date<-as.POSIXct(MusselTP32$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP32, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.32<-lm(Oxygen~Date,data=MusselTP32)
summary(m26.32)
abline(m26.32,lwd=2, lty=1)
slope26.32<-coef(m26.32)[2]*60 #converts to Mins
slope26.32

####################
#####BLANK 26C #######
####################
MusselBL<-read.csv("Mussel_BLANK_26C_19July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m26.bl)
abline(m26.bl,lwd=2, lty=1)
slope26.bl<-coef(m26.bl)[2]*60 #converts to Mins
slope26.bl #0.007533756

####################
#####BLANK 2 26C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_26C_19July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m26.bl2)
abline(m26.bl2,lwd=2, lty=1)
slope26.bl2<-coef(m26.bl2)[2]*60 #converts to Mins
slope26.bl2 #0.005204177

#Avg Blank
b<-c(0.007533756,0.005204177)
mean(b) # 0.006368966


#######################################################
####################MUSSEL 10C#########################
##################20 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-20July2019/Mussel-10C")
####################
#####Mussel 3#######
####################
MusselTP03<-read.csv("Mussel_TP3_10C_20July2019.csv",header=TRUE)


MusselTP03$Date<-as.POSIXct(MusselTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.03<-lm(Oxygen~Date,data=MusselTP03)
summary(m10.03)
abline(m10.03,lwd=2, lty=1)
slope10.03<-coef(m10.03)[2]*60 #converts to Mins
slope10.03

####################
#####Mussel 18#######
####################
MusselTP18<-read.csv("Mussel_TP18_10C_20July2019.csv",header=TRUE)


MusselTP18$Date<-as.POSIXct(MusselTP18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.18<-lm(Oxygen~Date,data=MusselTP18)
summary(m10.18)
abline(m10.18,lwd=2, lty=1)
slope10.18<-coef(m10.18)[2]*60 #converts to Mins
slope10.18

####################
#####BLANK 10C #######
####################
MusselBL<-read.csv("Mussel_BLANK_10C_20July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m10.bl)
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.008550104

####################
#####BLANK 2 10C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_10C_20July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m10.bl2)
abline(m10.bl2,lwd=2, lty=1)
slope10.bl2<-coef(m10.bl2)[2]*60 #converts to Mins
slope10.bl2 #0.002534168 

#Avg Blank
b<-c(0.008550104, 0.002534168)
mean(b) #0.005542136



#######################################################
####################MUSSEL 18C#########################
##################20 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-20July2019/Mussel-18C")
####################
#####Mussel 3#######
####################
MusselTP03<-read.csv("Mussel_TP3_18C_20July2019.csv",header=TRUE)


MusselTP03$Date<-as.POSIXct(MusselTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.03<-lm(Oxygen~Date,data=MusselTP03)
summary(m18.03)
abline(m18.03,lwd=2, lty=1)
slope18.03<-coef(m18.03)[2]*60 #converts to Mins
slope18.03

####################
#####Mussel 18#######
####################
MusselTP18<-read.csv("Mussel_TP18_18C_20July2019.csv",header=TRUE)


MusselTP18$Date<-as.POSIXct(MusselTP18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.18<-lm(Oxygen~Date,data=MusselTP18)
summary(m18.18)
abline(m18.18,lwd=2, lty=1)
slope18.18<-coef(m18.18)[2]*60 #converts to Mins
slope18.18

####################
#####BLANK 18C #######
####################
MusselBL<-read.csv("Mussel_BLANK_18C_20July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m18.bl)
abline(m18.bl,lwd=2, lty=1)
slope18.bl<-coef(m18.bl)[2]*60 #converts to Mins
slope18.bl #0.009527832

####################
#####BLANK 2 10C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_18C_20July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m18.bl2)
abline(m18.bl2,lwd=2, lty=1)
slope18.bl2<-coef(m18.bl2)[2]*60 #converts to Mins
slope18.bl2 #0.006991155  

#Avg Blank
b<-c(0.009527832, 0.006991155)
mean(b) #0.008259493


#######################################################
####################MUSSEL 26C#########################
##################20 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Mussels/SitkaM-20July2019/Mussel-26C")
####################
#####Mussel 3#######
####################
MusselTP03<-read.csv("Mussel_TP3_26C_20July2019.csv",header=TRUE)


MusselTP03$Date<-as.POSIXct(MusselTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.03<-lm(Oxygen~Date,data=MusselTP03)
summary(m26.03)
abline(m26.03,lwd=2, lty=1)
slope26.03<-coef(m26.03)[2]*60 #converts to Mins
slope26.03

####################
#####Mussel 18#######
####################
MusselTP18<-read.csv("Mussel_TP18_26C_20July2019.csv",header=TRUE)


MusselTP18$Date<-as.POSIXct(MusselTP18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylim=c(4,10), ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.18<-lm(Oxygen~Date,data=MusselTP18)
summary(m26.18)
abline(m26.18,lwd=2, lty=1)
slope26.18<-coef(m26.18)[2]*60 #converts to Mins
slope26.18

####################
#####BLANK 26C #######
####################
MusselBL<-read.csv("Mussel_BLANK_26C_20July2019.csv",header=TRUE)


MusselBL$Date<-as.POSIXct(MusselBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl<-lm(Oxygen~Date,data=MusselBL)
summary(m26.bl)
abline(m26.bl,lwd=2, lty=1)
slope26.bl<-coef(m26.bl)[2]*60 #converts to Mins
slope26.bl #0.0006338717

####################
#####BLANK 2 10C #######
####################
MusselBL2<-read.csv("Mussel_BLANK2_26C_20July2019.csv",header=TRUE)


MusselBL2$Date<-as.POSIXct(MusselBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=MusselBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl2<-lm(Oxygen~Date,data=MusselBL2)
summary(m26.bl2)
abline(m26.bl2,lwd=2, lty=1)
slope26.bl2<-coef(m26.bl2)[2]*60 #converts to Mins
slope26.bl2 #0.001648802 

#Avg Blank
b<-c(0.0006338717, 0.001648802)
mean(b) #0.001141337



################################################################
#HERMIT CRABS (Pagurus hirsitisculus)
################################################################

#JULY 2019-----------------------------------------------------------------------

setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Hermits/SitkaH-21July2019/Hermit-10C")

#######################################################
####################HERMITS 10C#########################
##################21 July 2019########################

####################
#####Hermit 30#######
####################
HermitTP30<-read.csv("Hermit_TP30_10C_21July2019.csv",header=TRUE)


HermitTP30$Date<-as.POSIXct(HermitTP30$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP30, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.30<-lm(Oxygen~Date,data=HermitTP30)
summary(m10.30)
abline(m10.30,lwd=2, lty=1)
slope10.30<-coef(m10.30)[2]*60 #converts to Mins
slope10.30


####################
#####Hermit 7#######
####################
HermitTP07<-read.csv("Hermit_TP7_10C_21July2019.csv",header=TRUE)


HermitTP07$Date<-as.POSIXct(HermitTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.07<-lm(Oxygen~Date,data=HermitTP07)
summary(m10.07)
abline(m10.07,lwd=2, lty=1)
slope10.07<-coef(m10.07)[2]*60 #converts to Mins
slope10.07


####################
#####BLANK 10C #######
####################
HermitBL<-read.csv("Hermit_BLANK_10C_21July2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m10.bl)
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.01181592 


#######################################################
####################HERMITS 18C#########################
##################21 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Hermits/SitkaH-21July2019/Hermit-18C")
####################
#####Hermit 30#######
####################
HermitTP30<-read.csv("Hermit_TP30_18C_21July2019.csv",header=TRUE)


HermitTP30$Date<-as.POSIXct(HermitTP30$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP30, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.30<-lm(Oxygen~Date,data=HermitTP30)
summary(m18.30)
abline(m18.30,lwd=2, lty=1)
slope18.30<-coef(m18.30)[2]*60 #converts to Mins
slope18.30


####################
#####Hermit 7#######
####################
HermitTP07<-read.csv("Hermit_TP7_18C_21July2019.csv",header=TRUE)


HermitTP07$Date<-as.POSIXct(HermitTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.07<-lm(Oxygen~Date+Seconds,data=HermitTP07)
summary(m18.07)
abline(m18.07,lwd=2, lty=1)
slope18.07<-coef(m18.07)[2]*60 #converts to Mins
slope18.07



####################
#####BLANK 18C #######
####################
HermitBL<-read.csv("Hermit_BLANK_18C_21July2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m18.bl)
abline(m18.bl,lwd=2, lty=1)
slope18.bl<-coef(m18.bl)[2]*60 #converts to Mins
slope18.bl  


#######################################################
####################HERMITS 26C#########################
##################21 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Hermits/SitkaH-21July2019/Hermit-26C")
####################
#####Hermit 30#######
####################
HermitTP30<-read.csv("Hermit_TP30_26C_21July2019.csv",header=TRUE)


HermitTP30$Date<-as.POSIXct(HermitTP30$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP30, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.30<-lm(Oxygen~Date,data=HermitTP30)
summary(m26.30)
abline(m26.30,lwd=2, lty=1)
slope26.30<-coef(m26.30)[2]*60 #converts to Mins
slope26.30


####################
#####Hermit 7#######
####################
HermitTP07<-read.csv("Hermit_TP7_26C_21July2019_Test.csv",header=TRUE)


HermitTP07$Date<-as.POSIXct(HermitTP07$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP07, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.07<-lm(Oxygen~Date,data=HermitTP07)
summary(m26.07)
abline(m26.07,lwd=2, lty=1)
slope26.07<-coef(m26.07)[2]*60 #converts to Mins
slope26.07


####################
#####BLANK 26C #######
####################
HermitBL<-read.csv("Hermit_BLANK_26C_21July2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m26.bl)
abline(m26.bl,lwd=2, lty=1)
slope26.bl<-coef(m26.bl)[2]*60 #converts to Mins
slope26.bl #0.01181592




#######################################################
####################HERMITS 10C#########################
##################22 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Hermits/SitkaH-22July2019/Hermit-10C")

####################
#####Hermit 18#######
####################
HermitTP18<-read.csv("Hermit_TP18_10C_22July2019.csv",header=TRUE)


HermitTP18$Date<-as.POSIXct(HermitTP18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.18<-lm(Oxygen~Date+Seconds,data=HermitTP18)
summary(m10.18)
abline(m10.18,lwd=2, lty=1)
slope10.18<-coef(m10.18)[3]*60 #converts to Mins
slope10.18

####################
#####Hermit 19#######
####################
HermitTP19<-read.csv("Hermit_TP19_10C_22July2019.csv",header=TRUE)


HermitTP19$Date<-as.POSIXct(HermitTP19$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP19, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.19<-lm(Oxygen~Date,data=HermitTP19)
summary(m10.19)
abline(m10.19,lwd=2, lty=1)
slope10.19<-coef(m10.19)[2]*60 #converts to Mins
slope10.19

####################
#####Hermit 23#######
####################
HermitTP23<-read.csv("Hermit_TP23_10C_22July2019.csv",header=TRUE)


HermitTP23$Date<-as.POSIXct(HermitTP23$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP23, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.23<-lm(Oxygen~Date,data=HermitTP23)
summary(m10.23)
abline(m10.23,lwd=2, lty=1)
slope10.23<-coef(m10.23)[2]*60 #converts to Mins
slope10.23


####################
#####Hermit 24#######
####################
HermitTP24<-read.csv("Hermit_TP24_10C_22July2019.csv",header=TRUE)


HermitTP24$Date<-as.POSIXct(HermitTP24$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP24, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.24<-lm(Oxygen~Date,data=HermitTP24)
summary(m10.24)
abline(m10.24,lwd=2, lty=1)
slope10.24<-coef(m10.24)[2]*60 #converts to Mins
slope10.24


####################
#####Hermit 28#######
####################
HermitTP28<-read.csv("Hermit_TP28_10C_22July2019.csv",header=TRUE)


HermitTP28$Date<-as.POSIXct(HermitTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.28<-lm(Oxygen~Date,data=HermitTP28)
summary(m10.28)
abline(m10.28,lwd=2, lty=1)
slope10.28<-coef(m10.28)[2]*60 #converts to Mins
slope10.28


####################
#####Hermit 32#######
####################
HermitTP32<-read.csv("Hermit_TP32_10C_22July2019.csv",header=TRUE)


HermitTP32$Date<-as.POSIXct(HermitTP32$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP32, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.32<-lm(Oxygen~Date,data=HermitTP32)
summary(m10.32)
abline(m10.32,lwd=2, lty=1)
slope10.32<-coef(m10.32)[2]*60 #converts to Mins
slope10.32

####################
#####BLANK 10C #######
####################
HermitBL<-read.csv("Hermit_BLANK_10C_22July2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m10.bl)
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.008641511 


####################
#####BLANK 2 10C#######
####################
HermitBL2<-read.csv("Hermit_BLANK2_10C_22July2019.csv",header=TRUE)


HermitBL2$Date<-as.POSIXct(HermitBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl2<-lm(Oxygen~Date,data=HermitBL2)
summary(m10.bl2)
abline(m10.bl2,lwd=2, lty=1)
slope10.bl2<-coef(m10.bl2)[2]*60 #converts to Mins
slope10.bl2 #0.00901968  

#Avg Blank
b<-c(0.008641511, 0.00901968)
mean(b) #0.008830595



#######################################################
####################HERMITS 18C#########################
##################22 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Hermits/SitkaH-22July2019/Hermit-18C")
####################
#####Hermit 18#######
####################
HermitTP18<-read.csv("Hermit_TP18_18C_22July2019.csv",header=TRUE)


HermitTP18$Date<-as.POSIXct(HermitTP18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~HermitTP18$Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.18<-lm(Oxygen~HermitTP18$Date,data=HermitTP18)
summary(m18.18)
abline(m18.18,lwd=2, lty=1)
slope18.18<-coef(m18.18)[2]*60 #converts to Mins
slope18.18

####################
#####Hermit 19#######
####################
HermitTP19<-read.csv("Hermit_TP19_18C_22July2019.csv",header=TRUE)


HermitTP19$Date<-as.POSIXct(HermitTP19$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~HermitTP19$Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP19, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.19<-lm(Oxygen~Date,data=HermitTP19)
summary(m18.19)
abline(m18.19,lwd=2, lty=1)
slope18.19<-coef(m18.19)[2]*60 #converts to Mins
slope18.19


####################
#####Hermit 23#######
####################
HermitTP23<-read.csv("Hermit_TP23_18C_22July2019.csv",header=TRUE)


HermitTP23$Date<-as.POSIXct(HermitTP23$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP23, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.23<-lm(Oxygen~Date,data=HermitTP23)
summary(m18.23)
abline(m18.23,lwd=2, lty=1)
slope18.23<-coef(m18.23)[2]*60 #converts to Mins
slope18.23


####################
#####Hermit 24#######
####################
HermitTP24<-read.csv("Hermit_TP24_18C_22July2019.csv",header=TRUE)


HermitTP24$Date<-as.POSIXct(HermitTP24$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP24, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.24<-lm(Oxygen~Date,data=HermitTP24)
summary(m18.24)
abline(m18.24,lwd=2, lty=1)
slope18.24<-coef(m18.24)[2]*60 #converts to Mins
slope18.24


####################
#####Hermit 28#######
####################
HermitTP28<-read.csv("Hermit_TP28_18C_22July2019.csv",header=TRUE)


HermitTP28$Date<-as.POSIXct(HermitTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.28<-lm(Oxygen~Date,data=HermitTP28)
summary(m18.28)
abline(m18.28,lwd=2, lty=1)
slope18.28<-coef(m18.28)[2]*60 #converts to Mins
slope18.28



####################
#####Hermit 32#######
####################
HermitTP32<-read.csv("Hermit_TP32_18C_22July2019.csv",header=TRUE)


HermitTP32$Date<-as.POSIXct(HermitTP32$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP32, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.32<-lm(Oxygen~Date,data=HermitTP32)
summary(m18.32)
abline(m18.32,lwd=2, lty=1)
slope18.32<-coef(m18.32)[2]*60 #converts to Mins
slope18.32


####################
#####BLANK 18C#######
####################
HermitBL<-read.csv("Hermit_BLANK_18C_22July2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m18.bl)
abline(m18.bl,lwd=2, lty=1)
slope18.bl<-coef(m18.bl)[2]*60 #converts to Mins
slope18.bl  #0.003458441 


####################
#####BLANK 2 18C#######
####################
HermitBL2<-read.csv("Hermit_BLANK2_18C_22July2019.csv",header=TRUE)


HermitBL2$Date<-as.POSIXct(HermitBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl2<-lm(Oxygen~Date,data=HermitBL2)
summary(m10.bl2)
abline(m10.bl2,lwd=2, lty=1)
slope10.bl2<-coef(m10.bl2)[2]*60 #converts to Mins
slope10.bl2 #0.006564736  

#Avg Blank
b<-c(0.003458441,0.006564736)
mean(b) #0.005011588


#######################################################
####################HERMITS 26C#########################
##################22 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Hermits/SitkaH-22July2019/Hermit-26C")

####################
#####Hermit 18#######
####################
HermitTP18<-read.csv("Hermit_TP18_26C_22July2019.csv",header=TRUE)


HermitTP18$Date<-as.POSIXct(HermitTP18$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.18<-lm(Oxygen~Date+Seconds,data=HermitTP18)
summary(m26.18)
abline(m26.18,lwd=2, lty=1)
slope26.18<-coef(m26.18)[3]*60 #converts to Mins
slope26.18


####################
#####Hermit 19#######Removed R2<0.90
####################
HermitTP19<-read.csv("Hermit_TP19_26C_22July2019.csv",header=TRUE)


HermitTP19$Date<-as.POSIXct(HermitTP19$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP19, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.19<-lm(Oxygen~Date,data=HermitTP19)
summary(m26.19)
abline(m26.19,lwd=2, lty=1)
slope26.19<-coef(m26.19)[2]*60 #converts to Mins
slope26.19


####################
#####Hermit 23#######
####################
HermitTP23<-read.csv("Hermit_TP23_26C_22July2019.csv",header=TRUE)


HermitTP23$Date<-as.POSIXct(HermitTP23$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP23, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.23<-lm(Oxygen~Date+Seconds,data=HermitTP23)
summary(m26.23)
abline(m26.23,lwd=2, lty=1)
slope26.23<-coef(m26.23)[3]*60 #converts to Mins
slope26.23



####################
#####Hermit 24#######
####################
HermitTP24<-read.csv("Hermit_TP24_26C_22July2019.csv",header=TRUE)


HermitTP24$Date<-as.POSIXct(HermitTP24$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP24, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.24<-lm(Oxygen~Date,data=HermitTP24)
summary(m26.24)
abline(m26.24,lwd=2, lty=1)
slope26.24<-coef(m26.24)[2]*60 #converts to Mins
slope26.24


####################
#####Hermit 32#######
####################
HermitTP32<-read.csv("Hermit_TP32_26C_22July2019.csv",header=TRUE)


HermitTP32$Date<-as.POSIXct(HermitTP32$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP32, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.32<-lm(Oxygen~Date+Seconds,data=HermitTP32)
summary(m26.32)
abline(m26.32,lwd=2, lty=1)
slope26.32<-coef(m26.32)[3]*60 #converts to Mins
slope26.32


####################
#####Hermit 28#######
####################
HermitTP28<-read.csv("Hermit_TP28_26C_22July2019.csv",header=TRUE)


HermitTP28$Date<-as.POSIXct(HermitTP28$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.28<-lm(Oxygen~Date+Seconds,data=HermitTP28)
summary(m26.28)
abline(m26.28,lwd=2, lty=1)
slope26.28<-coef(m26.28)[3]*60 #converts to Mins
slope26.28


####################
#####BLANK 26C#######
####################
HermitBL<-read.csv("Hermit_BLANK_26C_22July2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m26.bl)
abline(m26.bl,lwd=2, lty=1)
slope26.bl<-coef(m26.bl)[2]*60 #converts to Mins
slope26.bl  #0.003202639


####################
#####BLANK 2 26C#######
####################
HermitBL2<-read.csv("Hermit_BLANK2_26C_22July2019.csv",header=TRUE)


HermitBL2$Date<-as.POSIXct(HermitBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl2<-lm(Oxygen~Date,data=HermitBL2)
summary(m26.bl2)
abline(m26.bl2,lwd=2, lty=1)
slope26.bl2<-coef(m26.bl2)[2]*60 #converts to Mins
slope26.bl2 #0.005327222  

#Avg Blank
b<-c(0.003202639,0.005327222)
mean(b) #0.00426493


#######################################################
####################HERMITS 10C#########################
##################23 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Hermits/SitkaH-23July2019/Hermit-10C")
####################
#####Hermit 2#######
####################
HermitTP02<-read.csv("Hermit_TP2_10C_23July2019.csv",header=TRUE)


HermitTP02$Date<-as.POSIXct(HermitTP02$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP02, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.02<-lm(Oxygen~Date,data=HermitTP02)
summary(m10.02)
abline(m10.02,lwd=2, lty=1)
slope10.02<-coef(m10.02)[2]*60 #converts to Mins
slope10.02

####################
#####Hermit 6#######
####################
HermitTP06<-read.csv("Hermit_TP6_10C_23July2019.csv",header=TRUE)


HermitTP06$Date<-as.POSIXct(HermitTP06$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP06, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.06<-lm(Oxygen~Date,data=HermitTP06)
summary(m10.06)
abline(m10.06,lwd=2, lty=1)
slope10.06<-coef(m10.06)[2]*60 #converts to Mins
slope10.06


####################
#####Hermit 12#######
####################
HermitTP12<-read.csv("Hermit_TP12_10C_23July2019.csv",header=TRUE)


HermitTP12$Date<-as.POSIXct(HermitTP12$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP12, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.12<-lm(Oxygen~Date,data=HermitTP12)
summary(m10.12)
abline(m10.12,lwd=2, lty=1)
slope10.12<-coef(m10.12)[2]*60 #converts to Mins
slope10.12


####################
#####Hermit 22#######
####################
HermitTP22<-read.csv("Hermit_TP22_10C_23July2019.csv",header=TRUE)


HermitTP22$Date<-as.POSIXct(HermitTP22$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP22, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.22<-lm(Oxygen~Date,data=HermitTP22)
summary(m10.22)
abline(m10.22,lwd=2, lty=1)
slope10.22<-coef(m10.22)[2]*60 #converts to Mins
slope10.22

####################
#####Hermit 3#######
####################
HermitTP03<-read.csv("Hermit_TP3_10C_23July2019.csv",header=TRUE)


HermitTP03$Date<-as.POSIXct(HermitTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.03<-lm(Oxygen~Date,data=HermitTP03)
summary(m10.03)
abline(m10.03,lwd=2, lty=1)
slope10.03<-coef(m10.03)[2]*60 #converts to Mins
slope10.03

####################
#####BLANK 10C#######
####################
HermitBL<-read.csv("Hermit_BLANK_10C_23July2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m10.bl)
abline(m10.bl,lwd=2, lty=1)
slope10.bl<-coef(m10.bl)[2]*60 #converts to Mins
slope10.bl #0.007448228


####################
#####BLANK 2 10C#######
####################
HermitBL2<-read.csv("Hermit_BLANK2_10C_23July2019.csv",header=TRUE)


HermitBL2$Date<-as.POSIXct(HermitBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl2<-lm(Oxygen~Date,data=HermitBL2)
summary(m10.bl2)
abline(m10.bl2,lwd=2, lty=1)
slope10.bl2<-coef(m10.bl2)[2]*60 #converts to Mins
slope10.bl2 #0.009076242  

#Avg Blank
b<-c(0.007448228,0.009076242)
mean(b) #0.008262235


#######################################################
####################HERMITS 18C#########################
##################23 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Hermits/SitkaH-23July2019/Hermit-18C")
####################
#####Hermit 02#######
####################
HermitTP02<-read.csv("Hermit_TP2_18C_23July2019.csv",header=TRUE)


HermitTP02$Date<-as.POSIXct(HermitTP02$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~HermitTP02$Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP02, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.02<-lm(Oxygen~Date,data=HermitTP02)
summary(m18.02)
abline(m18.02,lwd=2, lty=1)
slope18.02<-coef(m18.02)[2]*60 #converts to Mins
slope18.02


####################
#####Hermit 03#######
####################
HermitTP03<-read.csv("Hermit_TP3_18C_23July2019.csv",header=TRUE)


HermitTP03$Date<-as.POSIXct(HermitTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~HermitTP03$Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.03<-lm(Oxygen~Date,data=HermitTP03)
summary(m18.03)
abline(m18.03,lwd=2, lty=1)
slope18.03<-coef(m18.03)[2]*60 #converts to Mins
slope18.03




####################
#####Hermit 06#######
####################
HermitTP06<-read.csv("Hermit_TP6_18C_23July2019.csv",header=TRUE)


HermitTP06$Date<-as.POSIXct(HermitTP06$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP06, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.06<-lm(Oxygen~Date,data=HermitTP06)
summary(m18.06)
abline(m18.06,lwd=2, lty=1)
slope18.06<-coef(m18.06)[2]*60 #converts to Mins
slope18.06



####################
#####Hermit 12#######
####################
HermitTP12<-read.csv("Hermit_TP12_18C_23July2019.csv",header=TRUE)


HermitTP12$Date<-as.POSIXct(HermitTP12$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP12, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.12<-lm(Oxygen~Date+Seconds,data=HermitTP12)
summary(m18.12)
abline(m18.12,lwd=2, lty=1)
slope18.12<-coef(m18.12)[3]*60 #converts to Mins
slope18.12



####################
#####Hermit 22#######
####################
HermitTP22<-read.csv("Hermit_TP22_18C_23July2019.csv",header=TRUE)


HermitTP22$Date<-as.POSIXct(HermitTP22$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP22, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.22<-lm(Oxygen~Date,data=HermitTP22)
summary(m18.22)
abline(m18.22,lwd=2, lty=1)
slope18.22<-coef(m18.22)[2]*60 #converts to Mins
slope18.22


####################
#####BLANK 18C#######
####################
HermitBL<-read.csv("Hermit_BLANK_18C_23July2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m18.bl)
abline(m18.bl,lwd=2, lty=1)
slope18.bl<-coef(m18.bl)[2]*60 #converts to Mins
slope18.bl  #0.00547094 


####################
#####BLANK 2 18C#######
####################
HermitBL2<-read.csv("Hermit_BLANK2_18C_23July2019.csv",header=TRUE)


HermitBL2$Date<-as.POSIXct(HermitBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl2<-lm(Oxygen~Date,data=HermitBL2)
summary(m18.bl2)
abline(m18.bl2,lwd=2, lty=1)
slope18.bl2<-coef(m18.bl2)[2]*60 #converts to Mins
slope18.bl2 #0.009076242  

#Avg Blank
b<-c(0.00547094,0.009076242)
mean(b) #0.007273591


#######################################################
####################HERMITS 26C#########################
##################23 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Hermits/SitkaH-23July2019/Hermit-26C")
####################
#####Hermit 02#######
####################
HermitTP02<-read.csv("Hermit_TP2_26C_23July2019.csv",header=TRUE)


HermitTP02$Date<-as.POSIXct(HermitTP02$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP02, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.02<-lm(Oxygen~Date,data=HermitTP02)
summary(m26.02)
abline(m26.02,lwd=2, lty=1)
slope26.02<-coef(m26.02)[2]*60 #converts to Mins
slope26.02


####################
#####Hermit 03#######
####################
HermitTP03<-read.csv("Hermit_TP3_26C_23July2019.csv",header=TRUE)


HermitTP03$Date<-as.POSIXct(HermitTP03$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP03, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.03<-lm(Oxygen~Date_Seconds,data=HermitTP03)
summary(m26.03)
abline(m26.03,lwd=2, lty=1)
slope26.03<-coef(m26.03)[3]*60 #converts to Mins
slope26.03


####################
#####Hermit 06#######
####################
HermitTP06<-read.csv("Hermit_TP6_26C_23July2019.csv",header=TRUE)


HermitTP06$Date<-as.POSIXct(HermitTP06$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP06, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.06<-lm(Oxygen~Date,data=HermitTP06)
summary(m26.06)
abline(m26.06,lwd=2, lty=1)
slope26.06<-coef(m26.06)[2]*60 #converts to Mins
slope26.06

####################
#####Hermit 12#######
####################
HermitTP12<-read.csv("Hermit_TP12_26C_23July2019.csv",header=TRUE)


HermitTP12$Date<-as.POSIXct(HermitTP12$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP12, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.12<-lm(Oxygen~Date+Seconds,data=HermitTP12)
summary(m26.12)
abline(m26.12,lwd=2, lty=1)
slope26.12<-coef(m26.12)[3]*60 #converts to Mins
slope26.12


####################
#####Hermit 22#######
####################
HermitTP22<-read.csv("Hermit_TP22_26C_23July2019.csv",header=TRUE)


HermitTP22$Date<-as.POSIXct(HermitTP22$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitTP22, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.22<-lm(Oxygen~Date+Seconds,data=HermitTP22)
summary(m26.22)
abline(m26.22,lwd=2, lty=1)
slope26.22<-coef(m26.22)[3]*60 #converts to Mins
slope26.22

####################
#####BLANK 26C#######
####################
HermitBL<-read.csv("Hermit_BLANK_26C_23July2019.csv",header=TRUE)


HermitBL$Date<-as.POSIXct(HermitBL$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl<-lm(Oxygen~Date,data=HermitBL)
summary(m26.bl)
abline(m26.bl,lwd=2, lty=1)
slope26.bl<-coef(m26.bl)[2]*60 #converts to Mins
slope26.bl  #0.001082654


####################
#####BLANK 2 26C#######
####################
HermitBL2<-read.csv("Hermit_BLANK2_26C_23July2019.csv",header=TRUE)


HermitBL2$Date<-as.POSIXct(HermitBL2$Date, format="%d/%m/%y %H:%M:%S")

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=HermitBL2, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl2<-lm(Oxygen~Date,data=HermitBL2)
summary(m26.bl2)
abline(m26.bl2,lwd=2, lty=1)
slope26.bl2<-coef(m26.bl2)[2]*60 #converts to Mins
slope26.bl2 #0.005327222  

#Avg Blank
b<-c(0.001082654,0.004022305)
mean(b) #0.00255248


################################################################
#PERIWINKLE SNAILS (Littorina sitkana)
################################################################

#JULY 2019-----------------------------------------------------------------------

#######################################################
####################Littorines 10C#########################
##################24, 25, 26 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Littorines/Thermal History Pools/Littorines-10C-TH") 

####################
#####Snail 10#######
####################
LittTP10<-read.csv("Littorine_TP10_10C_24July2019.csv",header=TRUE)


LittTP10$Date<-as.POSIXct(LittTP10$Date, format="%d/%m/%y %H:%M:%S")

LittTP10<-LittTP10[-c(1,2),]
subset(LittTP10, Temperature==19) 

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.10<-lm(Oxygen~Date,data=LittTP10)
summary(m10.10)
abline(m10.10,lwd=2, lty=1)
slope10.10<-coef(m10.10)[2]*60 #converts to Mins
slope10.10

####################
#####Snail 3#######
####################
LittTP3<-read.csv("Littorine_TP3_10C_25July2019.csv",header=TRUE)


LittTP3$Date<-as.POSIXct(LittTP3$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP3<-LittTP3[-c(1,2),]
#subset(LittTP5, Temperature==19)

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP3, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.3<-lm(Oxygen~Date,data=LittTP3)
summary(m10.3)
abline(m10.3,lwd=2, lty=1)
slope10.3<-coef(m10.3)[2]*60 #converts to Mins
slope10.3

####################
#####Snail 6#######
####################
LittTP6<-read.csv("Littorine_TP6_10C_25July2019.csv",header=TRUE)


LittTP6$Date<-as.POSIXct(LittTP6$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP3<-LittTP3[-c(1,2),]
#subset(LittTP5, Temperature==19)

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP6, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.6<-lm(Oxygen~Date,data=LittTP6)
summary(m10.6)
abline(m10.6,lwd=2, lty=1)
slope10.6<-coef(m10.6)[2]*60 #converts to Mins
slope10.6


####################
#####Snail 7#######
####################
LittTP7<-read.csv("Littorine_TP7_10C_25July2019.csv",header=TRUE)


LittTP7$Date<-as.POSIXct(LittTP7$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.7<-lm(Oxygen~Date,data=LittTP7)
summary(m10.7)
abline(m10.7,lwd=2, lty=1)
slope10.7<-coef(m10.7)[2]*60 #converts to Mins
slope10.7

####################
#####Snail 14#######
####################
LittTP14<-read.csv("Littorine_TP14_10C_26July2019.csv",header=TRUE)


LittTP14$Date<-as.POSIXct(LittTP14$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP14, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.14<-lm(Oxygen~Date,data=LittTP14)
summary(m10.14)
abline(m10.14,lwd=2, lty=1)
slope10.14<-coef(m10.14)[2]*60 #converts to Mins
slope10.14

####################
#####Snail 18#######
####################
LittTP18<-read.csv("Littorine_TP18_10C_26July2019.csv",header=TRUE)


LittTP18$Date<-as.POSIXct(LittTP18$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.18<-lm(Oxygen~Date,data=LittTP18)
summary(m10.18)
abline(m10.18,lwd=2, lty=1)
slope10.18<-coef(m10.18)[2]*60 #converts to Mins
slope10.18


####################
#####Snail 19#######
####################
LittTP19<-read.csv("Littorine_TP19_10C_26July2019.csv",header=TRUE)


LittTP19$Date<-as.POSIXct(LittTP19$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP19, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.19<-lm(Oxygen~Date,data=LittTP19)
summary(m10.19)
abline(m10.19,lwd=2, lty=1)
slope10.19<-coef(m10.19)[2]*60 #converts to Mins
slope10.19


####################
#####Snail 22#######
####################
LittTP22<-read.csv("Littorine_TP22_10C_26July2019.csv",header=TRUE)


LittTP22$Date<-as.POSIXct(LittTP22$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP22, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.22<-lm(Oxygen~Date,data=LittTP22)
summary(m10.22)
abline(m10.22,lwd=2, lty=1)
slope10.22<-coef(m10.22)[2]*60 #converts to Mins
slope10.22



####################
#####Snail 23#######
####################
LittTP23<-read.csv("Littorine_TP23_10C_26July2019.csv",header=TRUE)


LittTP23$Date<-as.POSIXct(LittTP23$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP23, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.23<-lm(Oxygen~Date,data=LittTP23)
summary(m10.23)
abline(m10.23,lwd=2, lty=1)
slope10.23<-coef(m10.23)[2]*60 #converts to Mins
slope10.23


####################
#####Snail 24#######
####################
LittTP24<-read.csv("Littorine_TP24_10C_26July2019.csv",header=TRUE)


LittTP24$Date<-as.POSIXct(LittTP24$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP24, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.24<-lm(Oxygen~Date,data=LittTP24)
summary(m10.24)
abline(m10.24,lwd=2, lty=1)
slope10.24<-coef(m10.24)[2]*60 #converts to Mins
slope10.24


####################
#####Snail 28#######
####################
LittTP28<-read.csv("Littorine_TP28_10C_26July2019.csv",header=TRUE)


LittTP28$Date<-as.POSIXct(LittTP28$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.28<-lm(Oxygen~Date,data=LittTP28)
summary(m10.28)
abline(m10.28,lwd=2, lty=1)
slope10.28<-coef(m10.28)[2]*60 #converts to Mins
slope10.28

####################
#####Snail 30#######
####################
LittTP30<-read.csv("Littorine_TP30_10C_24July2019.csv",header=TRUE)


LittTP30$Date<-as.POSIXct(LittTP30$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP30, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.30<-lm(Oxygen~Date,data=LittTP30)
summary(m10.30)
abline(m10.30,lwd=2, lty=1)
slope10.30<-coef(m10.30)[2]*60 #converts to Mins
slope10.30


####################
#####Snail 36#######
####################
LittTP36<-read.csv("Littorine_TP36_10C_24July2019.csv",header=TRUE)


LittTP36$Date<-as.POSIXct(LittTP36$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP36<-LittTP36[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP36, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.36<-lm(Oxygen~Date,data=LittTP36)
summary(m10.36)
abline(m10.36,lwd=2, lty=1)
slope10.36<-coef(m10.36)[2]*60 #converts to Mins
slope10.36


####################
#####Snail 8#######
####################
LittTP8<-read.csv("Littorine_TP8_10C_24July2019.csv",header=TRUE)


LittTP8$Date<-as.POSIXct(LittTP8$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP8<-LittTP8[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP8, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.8<-lm(Oxygen~Date,data=LittTP8)
summary(m10.8)
abline(m10.8,lwd=2, lty=1)
slope10.8<-coef(m10.8)[2]*60 #converts to Mins
slope10.8


####################
#####Snail 29#######
####################
LittTP29<-read.csv("Littorine_TP29_10C_24July2019.csv",header=TRUE)


LittTP29$Date<-as.POSIXct(LittTP29$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP29<-LittTP29[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP29, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.290, 0.47, paste("13°C"), font=2)
m10.29<-lm(Oxygen~Date,data=LittTP29)
summary(m10.29)
abline(m10.29,lwd=2, lty=1)
slope10.29<-coef(m10.29)[2]*60 #converts to Mins
slope10.29


####################
#####Snail 11#######
####################
LittTP11<-read.csv("Littorine_TP11_10C_24July2019.csv",header=TRUE)


LittTP11$Date<-as.POSIXct(LittTP11$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP11<-LittTP11[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP11, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.110, 0.47, paste("13°C"), font=2)
m10.11<-lm(Oxygen~Date,data=LittTP11)
summary(m10.11)
abline(m10.11,lwd=2, lty=1)
slope10.11<-coef(m10.11)[2]*60 #converts to Mins
slope10.11

####################
#####Snail 13#######
####################
LittTP13<-read.csv("Littorine_TP13_10C_24July2019.csv",header=TRUE)


LittTP13$Date<-as.POSIXct(LittTP13$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP13<-LittTP13[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.130, 0.47, paste("13°C"), font=2)
m10.13<-lm(Oxygen~Date,data=LittTP13)
summary(m10.13)
abline(m10.13,lwd=2, lty=1)
slope10.13<-coef(m10.13)[2]*60 #converts to Mins
slope10.13

####################
#####Snail 33#######
####################
LittTP33<-read.csv("Littorine_TP33_10C_24July2019.csv",header=TRUE)


LittTP33$Date<-as.POSIXct(LittTP33$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP33<-LittTP33[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.330, 0.47, paste("33°C"), font=2)
m10.33<-lm(Oxygen~Date,data=LittTP33)
summary(m10.33)
abline(m10.33,lwd=2, lty=1)
slope10.33<-coef(m10.33)[2]*60 #converts to Mins
slope10.33

####################
#####Snail 27#######
####################
LittTP27<-read.csv("Littorine_TP27_10C_25July2019.csv",header=TRUE)


LittTP27$Date<-as.POSIXct(LittTP27$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP27<-LittTP27[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP27, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.270, 0.47, paste("27°C"), font=2)
m10.27<-lm(Oxygen~Date,data=LittTP27)
summary(m10.27)
abline(m10.27,lwd=2, lty=1)
slope10.27<-coef(m10.27)[2]*60 #converts to Mins
slope10.27

####################
#####Snail 5#######
####################
LittTP5<-read.csv("Littorine_TP5_10C_25July2019.csv",header=TRUE)


LittTP5$Date<-as.POSIXct(LittTP5$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP5<-LittTP5[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP5, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.50, 0.47, paste("5°C"), font=2)
m10.5<-lm(Oxygen~Date,data=LittTP5)
summary(m10.5)
abline(m10.5,lwd=2, lty=1)
slope10.5<-coef(m10.5)[2]*60 #converts to Mins
slope10.5


####################
#####Snail 9#######
####################
LittTP9<-read.csv("Littorine_TP9_10C_25July2019.csv",header=TRUE)


LittTP9$Date<-as.POSIXct(LittTP9$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP9<-LittTP9[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP9, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.90, 0.47, paste("9°C"), font=2)
m10.9<-lm(Oxygen~Date,data=LittTP9)
summary(m10.9)
abline(m10.9,lwd=2, lty=1)
slope10.9<-coef(m10.9)[2]*60 #converts to Mins
slope10.9


####################
#####Snail 15#######
####################
LittTP15<-read.csv("Littorine_TP15_10C_25July2019.csv",header=TRUE)


LittTP15$Date<-as.POSIXct(LittTP15$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP15<-LittTP15[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.150, 0.47, paste("15°C"), font=2)
m10.15<-lm(Oxygen~Date,data=LittTP15)
summary(m10.15)
abline(m10.15,lwd=2, lty=1)
slope10.15<-coef(m10.15)[2]*60 #converts to Mins
slope10.15


####################
#####Snail 26#######
####################
LittTP26<-read.csv("Littorine_TP26_10C_25July2019.csv",header=TRUE)


LittTP26$Date<-as.POSIXct(LittTP26$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP26<-LittTP26[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.260, 0.47, paste("26°C"), font=2)
m10.26<-lm(Oxygen~Date,data=LittTP26)
summary(m10.26)
abline(m10.26,lwd=2, lty=1)
slope10.26<-coef(m10.26)[2]*60 #converts to Mins
slope10.26



####################
#####Snail 16#######
####################
LittTP16<-read.csv("Littorine_TP16_10C_26July2019.csv",header=TRUE)


LittTP16$Date<-as.POSIXct(LittTP16$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP16<-LittTP16[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.160, 0.47, paste("16°C"), font=2)
m10.16<-lm(Oxygen~Date,data=LittTP16)
summary(m10.16)
abline(m10.16,lwd=2, lty=1)
slope10.16<-coef(m10.16)[2]*60 #converts to Mins
slope10.16

####################
#####BLANK 10C #######
####################
LittBL1.10<-read.csv("Littorine_BLANK1_10C_24July2019.csv",header=TRUE)


LittBL1.10$Date<-as.POSIXct(LittBL1.10$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.10<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl1<-lm(Oxygen~Date,data=LittBL1.10)
summary(m10.bl1)
abline(m10.bl1,lwd=2, lty=1)
slope10.bl1<-coef(m10.bl1)[2]*60 #converts to Mins
slope10.bl1 #0.003171619


####################
#####BLANK 10C #######
####################
LittBL1.10<-read.csv("Littorine_BLANK1_10C_25July2019.csv",header=TRUE)


LittBL1.10$Date<-as.POSIXct(LittBL1.10$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.10<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl1<-lm(Oxygen~Date,data=LittBL1.10)
summary(m10.bl1)
abline(m10.bl1,lwd=2, lty=1)
slope10.bl1<-coef(m10.bl1)[2]*60 #converts to Mins
slope10.bl1 #0.003171619

####################
#####BLANK 10C #######
####################
LittBL1.10<-read.csv("Littorine_BLANK1_10C_26July2019.csv",header=TRUE)


LittBL1.10$Date<-as.POSIXct(LittBL1.10$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.10<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl1<-lm(Oxygen~Date,data=LittBL1.10)
summary(m10.bl1)
abline(m10.bl1,lwd=2, lty=1)
slope10.bl1<-coef(m10.bl1)[2]*60 #converts to Mins
slope10.bl1 #0.003171619

####################
#####BLANK 10C #######
####################
LittBL2.10<-read.csv("Littorine_BLANK2_10C_26July2019.csv",header=TRUE)


LittBL2.10$Date<-as.POSIXct(LittBL2.10$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.10<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL2.10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m10.bl2<-lm(Oxygen~Date,data=LittBL2.10)
summary(m10.bl2)
abline(m10.bl2,lwd=2, lty=1)
slope10.bl2<-coef(m10.bl2)[2]*60 #converts to Mins
slope10.bl2 #0.003171619

bl<-c(0.004779948,0.005443598)
mean(bl)




#######################################################
####################Littorines 18C#########################
##################24, 25, 26 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Littorines/Thermal History Pools/Littorines-18C-TH")
####################
#####Snail 10#######
####################
LittTP10<-read.csv("Littorine_TP10_18C_24July2019.csv",header=TRUE)


LittTP10$Date<-as.POSIXct(LittTP10$Date, format="%d/%m/%y %H:%M:%S")

#LittTP10<-LittTP10[-c(1,2),]
#subset(LittTP10, Temperature==19)

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.10<-lm(Oxygen~Date,data=LittTP10)
summary(m18.10)
abline(m18.10,lwd=2, lty=1)
slope18.10<-coef(m18.10)[2]*60 #converts to Mins
slope18.10

####################
#####Snail 3#######
####################
LittTP3<-read.csv("Littorine_TP3_18C_25July2019.csv",header=TRUE)


LittTP3$Date<-as.POSIXct(LittTP3$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP3<-LittTP3[-c(1,2),]
#subset(LittTP5, Temperature==19)

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP3, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.3<-lm(Oxygen~Date,data=LittTP3)
summary(m18.3)
abline(m18.3,lwd=2, lty=1)
slope18.3<-coef(m18.3)[2]*60 #converts to Mins
slope18.3

####################
#####Snail 6#######
####################
LittTP6<-read.csv("Littorine_TP6_18C_25July2019.csv",header=TRUE)


LittTP6$Date<-as.POSIXct(LittTP6$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP3<-LittTP3[-c(1,2),]
#subset(LittTP5, Temperature==19)

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP6, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.6<-lm(Oxygen~Date,data=LittTP6)
summary(m18.6)
abline(m18.6,lwd=2, lty=1)
slope18.6<-coef(m18.6)[2]*60 #converts to Mins
slope18.6


####################
#####Snail 7#######
####################
LittTP7<-read.csv("Littorine_TP7_18C_25July2019.csv",header=TRUE)


LittTP7$Date<-as.POSIXct(LittTP7$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.7<-lm(Oxygen~Date,data=LittTP7)
summary(m18.7)
abline(m18.7,lwd=2, lty=1)
slope18.7<-coef(m18.7)[2]*60 #converts to Mins
slope18.7

####################
#####Snail 14#######
####################
LittTP14<-read.csv("Littorine_TP14_18C_26July2019.csv",header=TRUE)


LittTP14$Date<-as.POSIXct(LittTP14$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP14, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.14<-lm(Oxygen~Date,data=LittTP14)
summary(m18.14)
abline(m18.14,lwd=2, lty=1)
slope18.14<-coef(m18.14)[2]*60 #converts to Mins
slope18.14

####################
#####Snail 18#######
####################
LittTP18<-read.csv("Littorine_TP18_18C_26July2019.csv",header=TRUE)


LittTP18$Date<-as.POSIXct(LittTP18$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.18<-lm(Oxygen~Date,data=LittTP18)
summary(m18.18)
abline(m18.18,lwd=2, lty=1)
slope18.18<-coef(m18.18)[2]*60 #converts to Mins
slope18.18


####################
#####Snail 19#######
####################
LittTP19<-read.csv("Littorine_TP19_18C_26July2019.csv",header=TRUE)


LittTP19$Date<-as.POSIXct(LittTP19$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP19, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.19<-lm(Oxygen~Date,data=LittTP19)
summary(m18.19)
abline(m18.19,lwd=2, lty=1)
slope18.19<-coef(m18.19)[2]*60 #converts to Mins
slope18.19


####################
#####Snail 22#######
####################
LittTP22<-read.csv("Littorine_TP22_18C_26July2019.csv",header=TRUE)


LittTP22$Date<-as.POSIXct(LittTP22$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP22, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.22<-lm(Oxygen~Date,data=LittTP22)
summary(m18.22)
abline(m18.22,lwd=2, lty=1)
slope18.22<-coef(m18.22)[2]*60 #converts to Mins
slope18.22



####################
#####Snail 23#######
####################
LittTP23<-read.csv("Littorine_TP23_18C_26July2019.csv",header=TRUE)


LittTP23$Date<-as.POSIXct(LittTP23$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP23, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.23<-lm(Oxygen~Date,data=LittTP23)
summary(m18.23)
abline(m18.23,lwd=2, lty=1)
slope18.23<-coef(m18.23)[2]*60 #converts to Mins
slope18.23


####################
#####Snail 24#######
####################
LittTP24<-read.csv("Littorine_TP24_18C_26July2019.csv",header=TRUE)


LittTP24$Date<-as.POSIXct(LittTP24$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP24, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.24<-lm(Oxygen~Date,data=LittTP24)
summary(m18.24)
abline(m18.24,lwd=2, lty=1)
slope18.24<-coef(m18.24)[2]*60 #converts to Mins
slope18.24


####################
#####Snail 28#######
####################
LittTP28<-read.csv("Littorine_TP28_18C_26July2019.csv",header=TRUE)


LittTP28$Date<-as.POSIXct(LittTP28$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.28<-lm(Oxygen~Date,data=LittTP28)
summary(m18.28)
abline(m18.28,lwd=2, lty=1)
slope18.28<-coef(m18.28)[2]*60 #converts to Mins
slope18.28

####################
#####Snail 30#######
####################
LittTP30<-read.csv("Littorine_TP30_18C_24July2019.csv",header=TRUE)


LittTP30$Date<-as.POSIXct(LittTP30$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP30, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.30<-lm(Oxygen~Date,data=LittTP30)
summary(m18.30)
abline(m18.30,lwd=2, lty=1)
slope18.30<-coef(m18.30)[2]*60 #converts to Mins
slope18.30



####################
#####Snail 36#######
####################
LittTP36<-read.csv("Littorine_TP36_18C_24July2019.csv",header=TRUE)


LittTP36$Date<-as.POSIXct(LittTP36$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP36<-LittTP36[-c(1,292),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP36, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.36<-lm(Oxygen~Date,data=LittTP36)
summary(m18.36)
abline(m18.36,lwd=2, lty=1)
slope18.36<-coef(m18.36)[2]*60 #converts to Mins
slope18.36


####################
#####Snail 8#######
####################
LittTP8<-read.csv("Littorine_TP8_18C_24July2019.csv",header=TRUE)


LittTP8$Date<-as.POSIXct(LittTP8$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP8<-LittTP8[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP8, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.8<-lm(Oxygen~Date,data=LittTP8)
summary(m18.8)
abline(m18.8,lwd=2, lty=1)
slope18.8<-coef(m18.8)[2]*60 #converts to Mins
slope18.8


####################
#####Snail 29#######
####################
LittTP29<-read.csv("Littorine_TP29_18C_24July2019.csv",header=TRUE)


LittTP29$Date<-as.POSIXct(LittTP29$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP29<-LittTP29[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP29, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.290, 0.47, paste("13°C"), font=2)
m18.29<-lm(Oxygen~Date,data=LittTP29)
summary(m18.29)
abline(m18.29,lwd=2, lty=1)
slope18.29<-coef(m18.29)[2]*60 #converts to Mins
slope18.29


####################
#####Snail 11#######
####################
LittTP11<-read.csv("Littorine_TP11_18C_24July2019.csv",header=TRUE)


LittTP11$Date<-as.POSIXct(LittTP11$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP11<-LittTP11[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP11, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.118, 0.47, paste("13°C"), font=2)
m18.11<-lm(Oxygen~Date,data=LittTP11)
summary(m18.11)
abline(m18.11,lwd=2, lty=1)
slope18.11<-coef(m18.11)[2]*60 #converts to Mins
slope18.11

####################
#####Snail 13#######
####################
LittTP13<-read.csv("Littorine_TP13_18C_24July2019.csv",header=TRUE)


LittTP13$Date<-as.POSIXct(LittTP13$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP13<-LittTP13[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.130, 0.47, paste("13°C"), font=2)
m18.13<-lm(Oxygen~Date,data=LittTP13)
summary(m18.13)
abline(m18.13,lwd=2, lty=1)
slope18.13<-coef(m18.13)[2]*60 #converts to Mins
slope18.13

####################
#####Snail 33#######
####################
LittTP33<-read.csv("Littorine_TP33_18C_24July2019.csv",header=TRUE)


LittTP33$Date<-as.POSIXct(LittTP33$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP33<-LittTP33[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.330, 0.47, paste("33°C"), font=2)
m18.33<-lm(Oxygen~Date,data=LittTP33)
summary(m18.33)
abline(m18.33,lwd=2, lty=1)
slope18.33<-coef(m18.33)[2]*60 #converts to Mins
slope18.33

####################
#####Snail 27#######
####################
LittTP27<-read.csv("Littorine_TP27_18C_25July2019.csv",header=TRUE)


LittTP27$Date<-as.POSIXct(LittTP27$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP27<-LittTP27[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP27, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.270, 0.47, paste("27°C"), font=2)
m18.27<-lm(Oxygen~Date,data=LittTP27)
summary(m18.27)
abline(m18.27,lwd=2, lty=1)
slope18.27<-coef(m18.27)[2]*60 #converts to Mins
slope18.27

####################
#####Snail 5#######
####################
LittTP5<-read.csv("Littorine_TP5_18C_25July2019.csv",header=TRUE)


LittTP5$Date<-as.POSIXct(LittTP5$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP5<-LittTP5[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP5, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.50, 0.47, paste("5°C"), font=2)
m18.5<-lm(Oxygen~Date,data=LittTP5)
summary(m18.5)
abline(m18.5,lwd=2, lty=1)
slope18.5<-coef(m18.5)[2]*60 #converts to Mins
slope18.5


####################
#####Snail 9#######
####################
LittTP9<-read.csv("Littorine_TP9_18C_25July2019.csv",header=TRUE)


LittTP9$Date<-as.POSIXct(LittTP9$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP9<-LittTP9[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP9, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.90, 0.47, paste("9°C"), font=2)
m18.9<-lm(Oxygen~Date,data=LittTP9)
summary(m18.9)
abline(m18.9,lwd=2, lty=1)
slope18.9<-coef(m18.9)[2]*60 #converts to Mins
slope18.9


####################
#####Snail 15#######
####################
LittTP15<-read.csv("Littorine_TP15_18C_25July2019.csv",header=TRUE)


LittTP15$Date<-as.POSIXct(LittTP15$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP15<-LittTP15[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.150, 0.47, paste("15°C"), font=2)
m18.15<-lm(Oxygen~Date,data=LittTP15)
summary(m18.15)
abline(m18.15,lwd=2, lty=1)
slope18.15<-coef(m18.15)[2]*60 #converts to Mins
slope18.15


####################
#####Snail 26#######
####################
LittTP26<-read.csv("Littorine_TP26_18C_25July2019.csv",header=TRUE)


LittTP26$Date<-as.POSIXct(LittTP26$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP26<-LittTP26[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.260, 0.47, paste("26°C"), font=2)
m18.26<-lm(Oxygen~Date,data=LittTP26)
summary(m18.26)
abline(m18.26,lwd=2, lty=1)
slope18.26<-coef(m18.26)[2]*60 #converts to Mins
slope18.26



####################
#####Snail 16#######
####################
LittTP16<-read.csv("Littorine_TP16_18C_26July2019.csv",header=TRUE)


LittTP16$Date<-as.POSIXct(LittTP16$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP16<-LittTP16[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.160, 0.47, paste("16°C"), font=2)
m18.16<-lm(Oxygen~Date,data=LittTP16)
summary(m18.16)
abline(m18.16,lwd=2, lty=1)
slope18.16<-coef(m18.16)[2]*60 #converts to Mins
slope18.16






####################
#####BLANK 18C #######
####################
LittBL1.18<-read.csv("Littorine_BLANK1_18C_24July2019.csv",header=TRUE)


LittBL1.18$Date<-as.POSIXct(LittBL1.10$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.10<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl1<-lm(Oxygen~Date,data=LittBL1.18)
summary(m18.bl1)
abline(m18.bl1,lwd=2, lty=1)
slope18.bl1<-coef(m18.bl1)[2]*60 #converts to Mins
slope18.bl1 #0.003171619


####################
#####BLANK 18C #######
####################
LittBL1.18<-read.csv("Littorine_BLANK1_18C_25July2019.csv",header=TRUE)


LittBL1.18$Date<-as.POSIXct(LittBL1.18$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.10<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl1<-lm(Oxygen~Date,data=LittBL1.18)
summary(m18.bl1)
abline(m18.bl1,lwd=2, lty=1)
slope18.bl1<-coef(m18.bl1)[2]*60 #converts to Mins
slope18.bl1 

####################
#####BLANK 18C #######
####################
LittBL1.18<-read.csv("Littorine_BLANK1_18C_26July2019.csv",header=TRUE)


LittBL1.18$Date<-as.POSIXct(LittBL1.18$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.10<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl1<-lm(Oxygen~Date,data=LittBL1.18)
summary(m18.bl1)
abline(m18.bl1,lwd=2, lty=1)
slope18.bl1<-coef(m18.bl1)[2]*60 #converts to Mins
slope18.bl1 #0.003171619

####################
#####BLANK 18C #######
####################
LittBL2.18<-read.csv("Littorine_BLANK2_18C_26July2019.csv",header=TRUE)


LittBL2.18$Date<-as.POSIXct(LittBL2.18$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.10<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL2.18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m18.bl2<-lm(Oxygen~Date,data=LittBL2.18)
summary(m18.bl2)
abline(m18.bl2,lwd=2, lty=1)
slope18.bl2<-coef(m18.bl2)[2]*60 #converts to Mins
slope18.bl2 #0.003171619

bl<-c(0.003897785,0.007445995)
mean(bl)



#######################################################
####################Littorines 26C#########################
##################24, 25, 26 July 2019########################
setwd("/Users/racinerangel/Desktop/Sitka July 2019/Sitka July 2019/Littorines/Thermal History Pools/Littorines-26C-TH")
####################
#####Snail 10#######
####################
LittTP10<-read.csv("Littorine_TP10_26C_24July2019.csv",header=TRUE)


LittTP10$Date<-as.POSIXct(LittTP10$Date, format="%d/%m/%y %H:%M:%S")

#LittTP10<-LittTP10[-c(1,2),]
#subset(LittTP10, Temperature==19)

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP10, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.10<-lm(Oxygen~Date,data=LittTP10)
summary(m26.10)
abline(m26.10,lwd=2, lty=1)
slope26.10<-coef(m26.10)[2]*60 #converts to Mins
slope26.10

####################
#####Snail 3#######
####################
LittTP3<-read.csv("Littorine_TP3_26C_25July2019.csv",header=TRUE)


LittTP3$Date<-as.POSIXct(LittTP3$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP3<-LittTP3[-c(1,2),]
#subset(LittTP5, Temperature==19)

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP3, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.3<-lm(Oxygen~Date,data=LittTP3)
summary(m26.3)
abline(m26.3,lwd=2, lty=1)
slope26.3<-coef(m26.3)[2]*60 #converts to Mins
slope26.3

####################
#####Snail 6#######
####################
LittTP6<-read.csv("Littorine_TP6_26C_25July2019.csv",header=TRUE)


LittTP6$Date<-as.POSIXct(LittTP6$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP3<-LittTP3[-c(1,2),]
#subset(LittTP5, Temperature==19)

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP6, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.6<-lm(Oxygen~Date,data=LittTP6)
summary(m26.6)
abline(m26.6,lwd=2, lty=1)
slope26.6<-coef(m26.6)[2]*60 #converts to Mins
slope26.6


####################
#####Snail 7#######
####################
LittTP7<-read.csv("Littorine_TP7_26C_25July2019.csv",header=TRUE)


LittTP7$Date<-as.POSIXct(LittTP7$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP7, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.7<-lm(Oxygen~Date,data=LittTP7)
summary(m26.7)
abline(m26.7,lwd=2, lty=1)
slope26.7<-coef(m26.7)[2]*60 #converts to Mins
slope26.7

####################
#####Snail 14#######
####################
LittTP14<-read.csv("Littorine_TP14_26C_26July2019.csv",header=TRUE)


LittTP14$Date<-as.POSIXct(LittTP14$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP14, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.14<-lm(Oxygen~Date,data=LittTP14)
summary(m26.14)
abline(m26.14,lwd=2, lty=1)
slope26.14<-coef(m26.14)[2]*60 #converts to Mins
slope26.14

####################
#####Snail 18#######
####################
LittTP18<-read.csv("Littorine_TP18_26C_26July2019.csv",header=TRUE)


LittTP18$Date<-as.POSIXct(LittTP18$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP18, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.18<-lm(Oxygen~Date,data=LittTP18)
summary(m26.18)
abline(m26.18,lwd=2, lty=1)
slope26.18<-coef(m26.18)[2]*60 #converts to Mins
slope26.18


####################
#####Snail 19#######
####################
LittTP19<-read.csv("Littorine_TP19_26C_26July2019.csv",header=TRUE)


LittTP19$Date<-as.POSIXct(LittTP19$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP19<-LittTP19[-c(182),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP19, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.19<-lm(Oxygen~Date,data=LittTP19)
summary(m26.19)
abline(m26.19,lwd=2, lty=1)
slope26.19<-coef(m26.19)[2]*60 #converts to Mins
slope26.19


####################
#####Snail 22#######
####################
LittTP22<-read.csv("Littorine_TP22_26C_26July2019.csv",header=TRUE)


LittTP22$Date<-as.POSIXct(LittTP22$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP22, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.22<-lm(Oxygen~Date,data=LittTP22)
summary(m26.22)
abline(m26.22,lwd=2, lty=1)
slope26.22<-coef(m26.22)[2]*60 #converts to Mins
slope26.22



####################
#####Snail 23#######
####################
LittTP23<-read.csv("Littorine_TP23_26C_26July2019.csv",header=TRUE)


LittTP23$Date<-as.POSIXct(LittTP23$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP23<-LittTP23[-c(188,189,190,191,192,193,194,195,196),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP23, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.23<-lm(Oxygen~Date,data=LittTP23)
summary(m26.23)
abline(m26.23,lwd=2, lty=1)
slope26.23<-coef(m26.23)[2]*60 #converts to Mins
slope26.23


####################
#####Snail 24#######
####################
LittTP24<-read.csv("Littorine_TP24_26C_26July2019.csv",header=TRUE)


LittTP24$Date<-as.POSIXct(LittTP24$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP24, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.24<-lm(Oxygen~Date,data=LittTP24)
summary(m26.24)
abline(m26.24,lwd=2, lty=1)
slope26.24<-coef(m26.24)[2]*60 #converts to Mins
slope26.24


####################
#####Snail 28#######
####################
LittTP28<-read.csv("Littorine_TP28_26C_26July2019.csv",header=TRUE)


LittTP28$Date<-as.POSIXct(LittTP28$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP28, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.28<-lm(Oxygen~Date,data=LittTP28)
summary(m26.28)
abline(m26.28,lwd=2, lty=1)
slope26.28<-coef(m26.28)[2]*60 #converts to Mins
slope26.28

####################
#####Snail 30#######
####################
LittTP30<-read.csv("Littorine_TP30_26C_24July2019.csv",header=TRUE)


LittTP30$Date<-as.POSIXct(LittTP30$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
#LittTP7<-LittTP7[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP30, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.30<-lm(Oxygen~Date,data=LittTP30)
summary(m26.30)
abline(m26.30,lwd=2, lty=1)
slope26.30<-coef(m26.30)[2]*60 #converts to Mins
slope26.30


####################
#####Snail 36#######
####################
LittTP36<-read.csv("Littorine_TP36_26C_24July2019.csv",header=TRUE)


LittTP36$Date<-as.POSIXct(LittTP36$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP36<-LittTP36[-c(1,292),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP36, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.36<-lm(Oxygen~Date,data=LittTP36)
summary(m26.36)
abline(m26.36,lwd=2, lty=1)
slope26.36<-coef(m26.36)[2]*60 #converts to Mins
slope26.36


####################
#####Snail 8#######
####################
LittTP8<-read.csv("Littorine_TP8_26C_24July2019.csv",header=TRUE)


LittTP8$Date<-as.POSIXct(LittTP8$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP8<-LittTP8[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP8, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.8<-lm(Oxygen~Date,data=LittTP8)
summary(m26.8)
abline(m26.8,lwd=2, lty=1)
slope26.8<-coef(m26.8)[2]*60 #converts to Mins
slope26.8


####################
#####Snail 29#######
####################
LittTP29<-read.csv("Littorine_TP29_26C_24July2019.csv",header=TRUE)


LittTP29$Date<-as.POSIXct(LittTP29$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP29<-LittTP29[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP29, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.290, 0.47, paste("13°C"), font=2)
m26.29<-lm(Oxygen~Date,data=LittTP29)
summary(m26.29)
abline(m26.29,lwd=2, lty=1)
slope26.29<-coef(m26.29)[2]*60 #converts to Mins
slope26.29


####################
#####Snail 11#######
####################
LittTP11<-read.csv("Littorine_TP11_26C_24July2019.csv",header=TRUE)


LittTP11$Date<-as.POSIXct(LittTP11$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP11<-LittTP11[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP11, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.126, 0.47, paste("13°C"), font=2)
m26.11<-lm(Oxygen~Date,data=LittTP11)
summary(m26.11)
abline(m26.11,lwd=2, lty=1)
slope26.11<-coef(m26.11)[2]*60 #converts to Mins
slope26.11

####################
#####Snail 13#######
####################
LittTP13<-read.csv("Littorine_TP13_26C_24July2019.csv",header=TRUE)


LittTP13$Date<-as.POSIXct(LittTP13$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP13<-LittTP13[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP13, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.130, 0.47, paste("13°C"), font=2)
m26.13<-lm(Oxygen~Date,data=LittTP13)
summary(m26.13)
abline(m26.13,lwd=2, lty=1)
slope26.13<-coef(m26.13)[2]*60 #converts to Mins
slope26.13

####################
#####Snail 33#######
####################
LittTP33<-read.csv("Littorine_TP33_26C_24July2019.csv",header=TRUE)


LittTP33$Date<-as.POSIXct(LittTP33$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP33<-LittTP33[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP33, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.330, 0.47, paste("33°C"), font=2)
m26.33<-lm(Oxygen~Date,data=LittTP33)
summary(m26.33)
abline(m26.33,lwd=2, lty=1)
slope26.33<-coef(m26.33)[2]*60 #converts to Mins
slope26.33

####################
#####Snail 27#######
####################
LittTP27<-read.csv("Littorine_TP27_26C_25July2019.csv",header=TRUE)


LittTP27$Date<-as.POSIXct(LittTP27$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP27<-LittTP27[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP27, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.270, 0.47, paste("27°C"), font=2)
m26.27<-lm(Oxygen~Date,data=LittTP27)
summary(m26.27)
abline(m26.27,lwd=2, lty=1)
slope26.27<-coef(m26.27)[2]*60 #converts to Mins
slope26.27

####################
#####Snail 5#######
####################
LittTP5<-read.csv("Littorine_TP5_26C_25July2019.csv",header=TRUE)


LittTP5$Date<-as.POSIXct(LittTP5$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP5<-LittTP5[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP5, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.50, 0.47, paste("5°C"), font=2)
m26.5<-lm(Oxygen~Date,data=LittTP5)
summary(m26.5)
abline(m26.5,lwd=2, lty=1)
slope26.5<-coef(m26.5)[2]*60 #converts to Mins
slope26.5


####################
#####Snail 9#######
####################
LittTP9<-read.csv("Littorine_TP9_26C_25July2019.csv",header=TRUE)


LittTP9$Date<-as.POSIXct(LittTP9$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP9<-LittTP9[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP9, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.90, 0.47, paste("9°C"), font=2)
m26.9<-lm(Oxygen~Date,data=LittTP9)
summary(m26.9)
abline(m26.9,lwd=2, lty=1)
slope26.9<-coef(m26.9)[2]*60 #converts to Mins
slope26.9


####################
#####Snail 15#######
####################
LittTP15<-read.csv("Littorine_TP15_26C_25July2019.csv",header=TRUE)


LittTP15$Date<-as.POSIXct(LittTP15$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP15<-LittTP15[-c(1,2),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP15, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.150, 0.47, paste("15°C"), font=2)
m26.15<-lm(Oxygen~Date,data=LittTP15)
summary(m26.15)
abline(m26.15,lwd=2, lty=1)
slope26.15<-coef(m26.15)[2]*60 #converts to Mins
slope26.15


####################
#####Snail 26#######
####################
LittTP26<-read.csv("Littorine_TP26_26C_25July2019.csv",header=TRUE)


LittTP26$Date<-as.POSIXct(LittTP26$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP26<-LittTP26[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.260, 0.47, paste("26°C"), font=2)
m26.26<-lm(Oxygen~Date,data=LittTP26)
summary(m26.26)
abline(m26.26,lwd=2, lty=1)
slope26.26<-coef(m26.26)[2]*60 #converts to Mins
slope26.26



####################
#####Snail 16#######
####################
LittTP16<-read.csv("Littorine_TP16_26C_26July2019.csv",header=TRUE)


LittTP16$Date<-as.POSIXct(LittTP16$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittTP16<-LittTP16[-c(853),]


plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittTP16, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.160, 0.47, paste("16°C"), font=2)
m26.16<-lm(Oxygen~Date,data=LittTP16)
summary(m26.16)
abline(m26.16,lwd=2, lty=1)
slope26.16<-coef(m26.16)[2]*60 #converts to Mins
slope26.16




####################
#####BLANK 26C #######
####################
LittBL1.26<-read.csv("Littorine_BLANK1_26C_24July2019.csv",header=TRUE)


LittBL1.26$Date<-as.POSIXct(LittBL1.26$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.26<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl1<-lm(Oxygen~Date,data=LittBL1.26)
summary(m26.bl1)
abline(m26.bl1,lwd=2, lty=1)
slope26.bl1<-coef(m26.bl1)[2]*60 #converts to Mins
slope26.bl1 


####################
#####BLANK 26C#######
####################
LittBL1.26<-read.csv("Littorine_BLANK1_26C_25July2019.csv",header=TRUE)


LittBL1.26$Date<-as.POSIXct(LittBL1.26$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.26<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl1<-lm(Oxygen~Date,data=LittBL1.26)
summary(m26.bl1)
abline(m26.bl1,lwd=2, lty=1)
slope26.bl1<-coef(m26.bl1)[2]*60 #converts to Mins
slope26.bl1 


####################
#####BLANK 26C#######
####################
LittBL1.26<-read.csv("Littorine_BLANK1_26C_26July2019.csv",header=TRUE)


LittBL1.26$Date<-as.POSIXct(LittBL1.26$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.26<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL1.26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl1<-lm(Oxygen~Date,data=LittBL1.26)
summary(m26.bl1)
abline(m26.bl1,lwd=2, lty=1)
slope26.bl1<-coef(m26.bl1)[2]*60 #converts to Mins
slope26.bl1 

####################
#####BLANK 18C #######
####################
LittBL2.26<-read.csv("Littorine_BLANK2_26C_26July2019.csv",header=TRUE)


LittBL2.26$Date<-as.POSIXct(LittBL2.26$Date, format="%d/%m/%y %H:%M:%S")

#To Omit
LittBL1.10<-LittBL1.10[-c(1,2),]

plot(Oxygen~Date, type="l", ylab=expression("O"[2]~(mg~O[2]~L)),
     xlab="Time", data=LittBL2.26, col="blue")

#axis(side=1, at=seq(0.1, 2.0, by=0.2))
#text(1.80, 0.47, paste("13°C"), font=2)
m26.bl2<-lm(Oxygen~Date,data=LittBL2.26)
summary(m26.bl2)
abline(m26.bl2,lwd=2, lty=1)
slope26.bl2<-coef(m26.bl2)[2]*60 #converts to Mins
slope26.bl2 

bl<-c(0.007880913,0.006407942)
mean(bl)




