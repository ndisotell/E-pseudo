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


#Guarding vs nonGuarding
library(dplyr)
library(ggplot2)
library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "GMTL to NGMTL", col_types = c("text", 
                                                                   "numeric"))
View(JMIH_R_Tables)

ggplot(JMIH_R_Tables, aes(x=Male, y=TL)) + geom_boxplot() + theme_classic(base_size = 14) + ylab("Male TL (mm)") + aes(fill= Male) + scale_fill_manual(values=c("#3E9DD1", "#E5A859"), guide=FALSE) + scale_y_continuous(breaks=seq(60, 95, 5)) + theme(axis.text.x = element_text(colour = 'black',  size = 12, face = 'bold')) + theme(axis.text.y = element_text(colour='black', size=12, face="bold")) + theme(axis.text.x = element_text(face="bold", color="black", size=12),axis.text.y = element_text(face="bold", color="black",size=12)) +theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

library(readxl)
JMIH_R_Tables <- read_excel("JMIH R Tables.xlsx", 
                            sheet = "T.test GM NGM", col_types = c("numeric", 
                                                                   "numeric"))
View(JMIH_R_Tables)


t.test(TL ~ Male, JMIH_R_Tables)

#T.test between the two potential SD , number of eggs to the nearest nest 