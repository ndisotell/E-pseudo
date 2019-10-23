#Average guarding male length by year Summary table
library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "Avg. guarding male length", 
                            col_types = c("text", "numeric"))
View(JMIH_R_Tables)

head(JMIH_R_Tables)
names(JMIH_R_Tables)
summary(JMIH_R_Tables)

library(dplyr)
JMIH_R_Tables %>% group_by(Year) %>%
  summarise(meanMaleTL = mean(MaleTL))

JMIH_R_Tables %>%
  group_by (Year) %>%
  summarise(
    nMaleTL = n(),
    meanMaleTL = mean(MaleTL),
    sdMaleTL = sd(MaleTL))
                                            #ANSWER 2015: n(36), mean(75.2), SD(5.99) 2019: n(23), mean(74.6), SD(5.18)
#Average guarding male TL Visual & Boxplot
library(ggplot2)
ggplot(JMIH_R_Tables,
       aes(x = Year, y = MaleTL)) +
  geom_point() +
  ylab("Male Total Length (mm)") +
  theme_bw()
library(colorspace)

ggplot(JMIH_R_Tables, aes(x=Year, y=MaleTL)) + geom_boxplot() + theme_classic(base_size = 14) + ylab("Male TL (mm)") + aes(fill= Year) + scale_fill_manual(values=c("#3E9DD1", "#E5A859"), guide=FALSE) + scale_y_continuous(breaks=seq(60, 95, 5)) + theme(axis.text.x = element_text(colour = 'black',  size = 12, face = 'bold')) + theme(axis.text.y = element_text(colour='black', size=12, face="bold")) + theme(axis.text.x = element_text(face="bold", color="black", size=12),axis.text.y = element_text(face="bold", color="black",size=12)) +theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))


#Average Clutch Size by year Summary table
library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "Avg. Clutch size", col_types = c("text", 
                                                                      "numeric"))
View(JMIH_R_Tables)
summary(JMIH_R_Tables)
library(dplyr)
JMIH_R_Tables %>%
  group_by (Year) %>%
  summarise(
    n = n(),
    meanClutchSize = mean(ClutchSize),
    sdClutchSize = sd(ClutchSize))
                                          #Answer: 2015 n(29), mean(1.59), SD(0.568) 2019 n(23), mean(1.13), SD(0.344)
#Average Clutch Size by year barchart
library(dplyr)
library(ggplot2)

library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "Avg. Clutch size", col_types = c("text", 
                                                                      "numeric"))
View(JMIH_R_Tables)

B <- table(JMIH_R_Tables$Year, JMIH_R_Tables$ClutchSize)
C <- as.data.frame(B)

colnames(C)
names(C)[names(C) == "Var1"] <- "Year"
names(C)[names(C) == "Var2"] <- "ClutchSize"
names(C)[names(C) == "Freq"] <- "Total_number"
C

legend_title <- "Clutch Size"
ggplot(C, aes(x = Year, y =Total_number, ylab="Frequency", fill=ClutchSize)) + geom_histogram(stat = "identity", position = 'dodge', binwidth = 5) + 
  theme_classic() + scale_fill_manual("Clutch Size", values = c("1"="#3E9DD1", "2"="#E5A859", "3"="#A57296")) + labs(x="Year",y="Frequency of Clutch Size") + 
  theme(axis.line = element_line(colour = "black"),axis.text.x = element_text(face="bold", color="black", size=12),axis.text.y = element_text(face="bold", color="black", size=12)) + 
  scale_y_continuous(breaks=seq(0, 20, 2)) + theme(axis.text.x = element_text(face="bold", color="black", size=12),axis.text.y = element_text(face="bold", color="black",size=12)) +
  theme(axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12,face = "bold")) +theme(legend.text=element_text(size=10, face = "bold"), legend.title=element_text(size=12, face="bold"))
                                                                                                                                                                                                                                                                                                                                                                                                                                                       
#Average Eggs to Rock size (SA)
library(ggplot2)
library(dplyr)
library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "Eggs to Rock Area", col_types = c("skip", 
                                                                       "numeric", "numeric"))
View(JMIH_R_Tables)
c<-JMIH_R_Tables
ggplot(c, aes(x = TotalEggs, y = SA, color = TotalEggs)) + geom_point() + theme_classic() + geom_point(size = 0.5) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + guides(color= FALSE) 

SAEgg_lm <- lm(SA ~ TotalEggs, data=c)
autoplot(SAEgg_lm, smooth.colour = NA)
SAEggsum = summary(SAEgg_lm)
plot(SA ~ TotalEggs, data = c)+ abline(SAEgg_lm) 

r2 = SAEggsum$adj.r.squared
SAEggsum$coefficients
my.p=SAEggsum$coefficients[2,4]
                                  #r2 0.0330 p=0.205

#Egg amount to male size 
library(ggplot2)
library(dplyr)
library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "Number of Eggs to male size", 
                            col_types = c("skip", "numeric", "numeric"))
View(JMIH_R_Tables)
mean(data.matrix(JMIH_R_Tables$TotalEggs))
sd(data.matrix(JMIH_R_Tables$TotalEggs))

a<-JMIH_R_Tables
ggplot(a, aes(x = MaleSize, y = TotalEggs, color = MaleSize, cex.axis=1.5)) + theme_classic() + geom_point() + 
  geom_point(size = 2, color="#3E9DD1") + geom_smooth(method=lm, col = "black", se=FALSE, fullrange=TRUE) + guides(color= FALSE) + xlab("Male TL (mm)") + ylab("No. of Eggs") + theme(axis.text.x = element_text(face="bold", color="black", size=12),axis.text.y = element_text(face="bold", color="black",size=12)) +theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

MaleEgg_lm <- lm(TotalEggs ~ MaleSize, data=a)
autoplot(MaleEgg_lm, smooth.colour = NA)
MEsum = summary(MaleEgg_lm)
plot(TotalEggs ~ MaleSize, data = a)+ abline(MaleEgg_lm) 
summary(MaleEgg_lm)
r2 = MEsum$adj.r.squared
MEsum$coefficients
my.p=MEsum$coefficients[2,4]
                                  #ANSWER r2 0.0586 p=0.144

#Surface Area to number of Eggs 
library(dplyr)
library(ggfortify)
library(ggplot2)
library(emmeans)
library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "Eggs to Rock Area", col_types = c("text", 
                                                                       "numeric", "numeric"))
View(JMIH_R_Tables)
b<-JMIH_R_Tables
ggplot(b, aes(x = TotalEggs, y = SA, color = TotalEggs, cex.axis=1.5)) + geom_point(size=2) + theme_classic() +
         geom_point(size = 0.5) + geom_smooth(method=lm, col = "#9d0000",se=FALSE, fullrange=TRUE) + guides(color= FALSE) + xlab("Egg Total") + ylab("Nesting Rock Surface Area")

SAE_lm <- lm(SA ~ TotalEggs, data=b)
autoplot(SAE_lm, smooth.colour = NA)
SAEsum = summary(SAE_lm)
plot(SA ~ TotalEggs, data = b)+ abline(SAE_lm)        

anova(SAE_lm)

r2 = SAEsum$adj.r.squared
SAEsum$coefficients
my.p=SAEsum$coefficients[2,4]

                                          #ANSWER r2 0.033 p = 0.205
                              #NEW FIGURES                                  
#TL to #of Clutches pooled
library(dplyr)
library(ggfortify)
library(ggplot2)
library(emmeans)
library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "TL pooled to Clutches", col_types = c("numeric", 
                                                                           "numeric"))
View(JMIH_R_Tables)
e<-JMIH_R_Tables

ggplot(e, aes(x = MaleTL, y = Clutch, color = Clutch, cex.axis=1.5)) + geom_point(size=2) + theme_classic() +
geom_point(size = 0.5,) + geom_smooth(method=lm, col = "#9d0000",se=FALSE, fullrange=TRUE) + guides(color= FALSE) + xlab("Male TL (mm)") + ylab("Clutch Size")

MC_lm <- lm(Clutch ~ MaleTL, data=e)
autoplot(MC_lm, smooth.colour = NA)
MCsum = summary(MC_lm)
plot(Clutch ~ MaleTL, data = e)+ abline(MC_lm) 

anova(Clutch_lm)

r2 = MCsum$adj.r.squared
MCsum$coefficients
my.p=MCsum$coefficients[2,4]
                                  #r2 0.299 p=0.011

#TL to #of Clutches by year
library(dplyr)
library(ggfortify)
library(ggplot2)
library(emmeans)
library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "TL Year to Clutches", col_types = c("text", 
                                                                         "numeric", "numeric"))
View(JMIH_R_Tables)
f<-JMIH_R_Tables
ggplot(f, aes(x = TL, y = Clutch, color = Year, cex.axis=1.5)) + geom_point(size=2) + theme_classic() +
  geom_point(size = 0.5) + geom_smooth(method=lm, col = "#9d0000",se=FALSE, fullrange=TRUE) + guides(color= FALSE) + xlab("Male TL (mm)") + ylab("Clutch")

                                                            #r2 0.299 p=0.011
#TL to NestSA
library(dplyr)
library(ggfortify)
library(ggplot2)
library(emmeans)
library(wesanderson)
library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "TL to SA", col_types = c("text", 
                                                              "numeric", "numeric"))
View(JMIH_R_Tables)

g<-JMIH_R_Tables
ggplot(g, aes(x = TL, y = SA, cex.axis=1.5)) + theme_classic() +
  geom_point(size = 2, color="#3E9DD1") + geom_smooth(method=lm, col = "black",se=FALSE, fullrange=TRUE) + guides(color= FALSE) + xlab("Male TL (mm)") + ylab("Nest Rock Size") + theme(axis.text.x = element_text(face="bold", color="black", size=12),axis.text.y = element_text(face="bold", color="black",size=12)) +theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold")) 
                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                
                                                                                                                                                                           
                                                                                                                                                                                                      
#Scatter Plot scale_color_manual
#BP scale_fill_manual(values=wes_palette(n=3, name="Zissou FantasticFox Darjeeling"))
TS_lm <- lm(SA ~ TL, data=g)
autoplot(TS_lm, smooth.colour = NA)
TSsum = summary(TS_lm)
plot(SA ~ TL, data = g)+ abline(TS_lm) 

anova(TS_lm)
summary(TS_lm)
r2 = TSsum$adj.r.squared
TSsum$coefficients
my.p=TSsum$coefficients[2,4]
                                    #r2 = -0.0197 p=0.637
#NestSA to # of Clutches
library(dplyr)
library(ggfortify)
library(ggplot2)
library(emmeans)
library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "Avg. clutch size to Rock size", 
                            col_types = c("text", "numeric", "numeric"))
View(JMIH_R_Tables)

mean(data.matrix(JMIH_R_Tables$SA))
sd(data.matrix(JMIH_R_Tables$SA))
d<-JMIH_R_Tables

ggplot(d, aes(x = ClutchSize, y = SA, color = Year, cex.axis=1.5)) + geom_point(size=2) + theme_classic() +
  geom_point(size = 0.5) + geom_smooth(method=lm, col = "#9d0000",se=FALSE, fullrange=TRUE) + guides(color= FALSE) + xlab("Clutch Size") + ylab("Rock Surface Area")

Clutch_lm <- lm(SA ~ ClutchSize, data=d)
autoplot(Clutch_lm, smooth.colour = NA)
Clutchsum = summary(Clutch_lm)
plot(SA ~ ClutchSize, data = d)+ abline(Clutch_lm) 

anova(Clutch_lm)

r2 = Clutchsum$adj.r.squared
Clutchsum$coefficients
my.p=Clutchsum$coefficients[2,4]
                                  #R2 -0.0021 P=0.349

install.packages("wesanderson")
