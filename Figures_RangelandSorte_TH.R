################################################################
#Title: Thermal History (TH) Figures 
#Purpose: Create figures for the manuscript "Local-scale thermal history influences metabolic response of marine invertebrates"
#Created by: R. E. Rangel
#Created: August 2018
#Last edited: 17 April 2021
################################################################
##### Packages #####
library(gridExtra)
library(grid)
library(ggpubr)
library(ggplot2)
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
#TH_Meta<-subset(TH_Meta, ID!="Littorine") # Keep this for making figures - remove for analyses
#-------------------------------------------------------
#------------------------
#------------------------
#1 WEEK ONLY------------------------------------------------------
#---------------------------------
OneWeek<-filter(TH_Meta, TempDate == "1Week")

#Filter for specific seasons
s18<-filter(OneWeek, Timepoint == "18-Sep") 
m19<-filter(OneWeek, Timepoint == "19-Mar")
j19<-filter(OneWeek, Timepoint == "19-Jul")
s19<-filter(OneWeek, Timepoint == "19-Sep")

#COLORS - ("#FDE725FF" = Yellow ,"#21908CFF" = Blue, "#440154FF" = Purple)
#quartz()


#####1WEEK - Range Pool temperatures figure - Figure 3-------------------------------------- 
#---------------------------------
#-------------------------------------------
s.18<-(ggplot() + 
         geom_point(data = s18,  size=3, aes(x = Range, y = Q10, shape=Species, color=Species))+
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
         geom_point(data = m19, size=3,                    # adding the raw data (scaled values)
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
         geom_point(data = j19, size=3,                      # adding the raw data (scaled values)
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

s.19<-(ggplot() + 
         geom_point(data = s19, size=3,                    
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

#dev.off()


#pdf("Figure1.pdf",height=8,width=10)
#NEED Package - gridExtra() grid() and ggpubr()
#Warning messages are for the blank littorine values - but this way the key is consisten across seasons
figure<-ggarrange(s.18,m.19,j.19,s.19,
                  common.legend = TRUE, legend = "top", font.label = list(size=20, face="plain"))
annotate_figure(figure, bottom = text_grob("Range of Pool Temperatures (°C)", size=20, just="centre", vjust = -0.5), top = text_grob("", size=20,hjust=0.5), left = text_grob("", rot=90, size=25, vjust =0.9))

#dev.off()



#####1Week - Maximum Pool temperatures figure - Supplementary figure 1-------------------------------------- 
s.18<-(ggplot() + 
         geom_point(data = s18,  size=3, aes(x = dailymax, y = Q10, shape=Species, color=Species))+
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
         geom_point(data = m19, size=3,                   
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
         geom_point(data = j19, size=3,                      # adding the raw data (scaled values)
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

s.19<-(ggplot() + 
         geom_point(data = s19, size=3,                     # adding the raw data (scaled values)
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

#pdf("SuppFig1.pdf",height=8,width=10)
#NEED Package - gridExtra() grid() and ggpubr()
figure<-ggarrange(s.18,m.19,j.19,s.19,
                  common.legend = TRUE, legend = "top", font.label = list(size=20, face="plain"))
annotate_figure(figure, bottom = text_grob("Daily Maximum of Pool Temperatures (°C)", size=20, just="centre", vjust = -0.5), top = text_grob("1-Week Time Interval", size=20,hjust=0.5),  left = text_grob(""))


dev.off()



#------------------------
#1 DAY ONLY- Supplementary Figure 2------------------------------------------------------------------------------------
#---------------------------------
OneDay<-filter(TH_Meta, TempDate == "1Day")
s18<-filter(OneDay, Timepoint == "18-Sep")
m19<-filter(OneDay, Timepoint == "19-Mar")
j19<-filter(OneDay, Timepoint == "19-Jul")
s19<-filter(OneDay, Timepoint == "19-Sep")


s.18<-(ggplot() + 
         geom_point(data = s18,  size=3, aes(x = dailymax, y = Q10, shape=Species, color=Species))+
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
         geom_point(data = m19, size=3,                   
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
         geom_point(data = j19, size=3,                      # adding the raw data (scaled values)
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

s.19<-(ggplot() + 
         geom_point(data = s19, size=3,                     # adding the raw data (scaled values)
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
               panel.background = element_blank()))


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


pdf("SuppFig2.pdf",height=8,width=10)
#NEED Package - gridExtra() grid() and ggpubr()
figure<-ggarrange(s.18,m.19,j.19,s.19,
                  common.legend = TRUE, legend = "top", font.label = list(size=20, face="plain"))
annotate_figure(figure, bottom = text_grob("Daily Maximum of Pool Temperatures (°C)", size=20, just="centre", vjust = -0.5), top = text_grob("1-Day Time Interval", size=20,hjust=0.5),  left = text_grob(""))


#dev.off()


#BOXPLOTS-----------------------------------------------
#quartz()
#Supplementary Figure 3------
#Warning messages are for the blank littorine values/ignore
Qseas<-OneWeek%>%
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

#pdf("SuppFig3.pdf",height=5,width=9)
#Qseas

#dev.off()



#SPECIES BOXPLOT--------------------------------------
#Figure 4 
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

#pdf("Fig4.pdf",height=5,width=6)
#dev.off()



####TEMPERATURE FIGURE- Figure 1---------------------------------------------------------------------------------------
setwd("/Users/racinerangel/Desktop/Temperature")
TotalT<-read.csv("Sitka_Master_Temp.csv")

#Format Date and Times
TotalT$day<-format(TotalT$Date.Time,format="%Y-%m-%d")
TotalT$month<-format(TotalT$Date,format="%m")
TotalT$Date<-as.POSIXct(TotalT$Date, format="%m/%d/%Y")


#pdf("Figure1.pdf",height=5,width=8)
par(mgp = c(2.6, 1, 0))
plot(TotalT$Pool.1~Date,lwd=0.2, xaxt="n", cex.axis=1.5,cex.lab=1.5, ylim=c(-7,40), xlab="Date", ylab="Temperature (°C)", type="l",
     col="dodgerblue2", data=TotalT)

# REMOVING NAs - NOT PRETTY but not in the mood to fight R-------------------------------------------------
TotalT <-TotalT %>% filter(!is.na(Pool.1))
TotalT <-TotalT %>% filter(!is.na(Pool.2))
TotalT <-TotalT %>% filter(!is.na(Pool.3))
TotalT <-TotalT %>% filter(!is.na(Pool.5))
TotalT <-TotalT %>% filter(!is.na(Pool.6))
TotalT <-TotalT %>% filter(!is.na(Pool.7))
TotalT <-TotalT %>% filter(!is.na(Pool.8))
TotalT <-TotalT %>% filter(!is.na(Pool.9))
TotalT <-TotalT %>% filter(!is.na(Pool.10))
TotalT <-TotalT %>% filter(!is.na(Pool.11))
TotalT <-TotalT %>% filter(!is.na(Pool.12))
TotalT <-TotalT %>% filter(!is.na(Pool.13))
TotalT <-TotalT %>% filter(!is.na(Pool.14))
TotalT <-TotalT %>% filter(!is.na(Pool.15))
TotalT <-TotalT %>% filter(!is.na(Pool.16))
TotalT <-TotalT %>% filter(!is.na(Pool.18))
TotalT <-TotalT %>% filter(!is.na(Pool.19))
TotalT <-TotalT %>% filter(!is.na(Pool.20))
TotalT <-TotalT %>% filter(!is.na(Pool.21))
TotalT <-TotalT %>% filter(!is.na(Pool.22))
TotalT <-TotalT %>% filter(!is.na(Pool.23))
TotalT <-TotalT %>% filter(!is.na(Pool.24))
TotalT <-TotalT %>% filter(!is.na(Pool.25))
TotalT <-TotalT %>% filter(!is.na(Pool.26))
TotalT <-TotalT %>% filter(!is.na(Pool.27))
TotalT <-TotalT %>% filter(!is.na(Pool.28))
TotalT <-TotalT %>% filter(!is.na(Pool.29))
TotalT <-TotalT %>% filter(!is.na(Pool.30))
TotalT <-TotalT %>% filter(!is.na(Pool.31))
TotalT <-TotalT %>% filter(!is.na(Pool.32))
TotalT <-TotalT %>% filter(!is.na(Pool.33))
TotalT <-TotalT %>% filter(!is.na(Pool.35))
TotalT <-TotalT %>% filter(!is.na(Pool.36))

#Make x-axis
r <- as.POSIXct(round(range(TotalT$Date), "months"))
axis.POSIXct(1, at = seq(r[1], r[2], by = "month"), format = "%b %y")

lines(TotalT$Pool.2~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.3~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.5~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.6~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.7~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.8~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.9~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.10~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.11~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.12~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.13~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.14~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.15~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.16~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.18~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.19~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.20~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.21~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.22~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.23~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.24~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.27~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.28~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.29~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.30~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.31~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.32~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.34~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.35~Date, type="l", col="dodgerblue2", data=TotalT)
lines(TotalT$Pool.36~Date, type="l", col="dodgerblue2", data=TotalT)



abline(v=as.numeric(TotalT$Date[20533], lwd=1))
abline(v=as.numeric(TotalT$Date[22546], lwd=1))
abline(v=as.numeric(TotalT$Date[78677], lwd=1))
abline(v=as.numeric(TotalT$Date[80671], lwd=1))
abline(v=as.numeric(TotalT$Date[110612], lwd=1))
abline(v=as.numeric(TotalT$Date[112628], lwd=1))
abline(v=as.numeric(TotalT$Date[127316], lwd=1))
abline(v=as.numeric(TotalT$Date[129332], lwd=1))


#dev.off()

#####################----------------------------------------------
###PCA Plot Figure 2

YearData <- read.csv("RangelandSorte_ThermalHistory_All.csv")
YearData<-subset(YearData, ID!="Littorine") #  Remove the blank littorine 

#Make the PCA-----------------------

PCA <- prcomp(YearData[,c(8:23)], scale = TRUE, center = TRUE)

# Proportion of data explained by each axis
summary(PCA)

#Making a PDF of the PCA---------------------
#pdf("TempPCA.pdf",height=8,width=9)


# plot
fviz_pca_biplot(PCA, xlab = "PC1 (73.0%)", ylab = "PC2 (22.0%)", repel = TRUE, col.var = "black", col.ind = "gray", title = " ", label = "var") + theme_base()

#dev.off()
