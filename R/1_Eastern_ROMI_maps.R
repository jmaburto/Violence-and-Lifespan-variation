########################### Code for heatmaps
library(devtools)
library(data.table)
library(reshape)
library(latticeExtra)
library(HMDHFDplus)
library(ggplot2)
library(mgcv)
library(xtable)
library(grid)
library(RColorBrewer)
library(ROMIplot)
#get data from HMD
#XYZ <- getHMDcountries()
#us <- "jmaburto@colmex.mx"
#pw <- "kolmogorov"

# now grab all the lifetables and mesh together..
# grab them all
#HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
#  cat(x,"\n")
#  Males        <- readHMDweb(x,"mltper_1x1",username=us,password=pw)
#  Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
#  Males$Sex    <- "m"
#  Females$Sex  <- "f"
#  CTRY         <- rbind(Females, Males)
#  CTRY$PopName <- x
#  CTRY    
#}, us = us, pw = pw))

#HMDL <- data.table(HMDL)
#save(HMDL,file="Data/HMD_Data.RData")

##################### I change datasets
setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")
load("Data/HMD_Deaths.RData")
Deaths <- HMDL
load("Data/HMD_Exp.RData")
Exposures <- Exp_HMDL
Data.D.E <- Deaths
Data.D.E$Female_exp <- Exposures$Female
Data.D.E$Male_exp <- Exposures$Male
gdata::keep(Data.D.E, sure=T)

Data.D.E$f.mx <- Data.D.E$Female/Data.D.E$Female_exp
Data.D.E$m.mx <- Data.D.E$Male/Data.D.E$Male_exp

Eastern_Data   <- subset(Data.D.E,(PopName=="BLR" | PopName=="BGR" | PopName=="CZE" | 
                                     PopName=="HUN"| PopName=="POL"| PopName=="RUS" | 
                                     PopName=="SVK" | PopName=="UKR" | PopName=="SVN" 
                                   | PopName=="EST" | PopName=="LVA" | PopName=="LTU") & Year >= 1960)

Eastern_Data$Country[Eastern_Data$PopName=="BLR"] <- "Belarus"
Eastern_Data$Country[Eastern_Data$PopName=="BGR"] <- "Bulgaria"
Eastern_Data$Country[Eastern_Data$PopName=="CZE"] <- "Czech Republic"
Eastern_Data$Country[Eastern_Data$PopName=="HUN"] <- "Hungary"
Eastern_Data$Country[Eastern_Data$PopName=="POL"] <- "Poland"
Eastern_Data$Country[Eastern_Data$PopName=="RUS"] <- "Russia"
Eastern_Data$Country[Eastern_Data$PopName=="SVK"] <- "Slovakia"
Eastern_Data$Country[Eastern_Data$PopName=="UKR"] <- "Ukraine"
Eastern_Data$Country[Eastern_Data$PopName=="SVN"] <- "Slovenia"
Eastern_Data$Country[Eastern_Data$PopName=="EST"] <- "Estonia"
Eastern_Data$Country[Eastern_Data$PopName=="LVA"] <- "Latvia"
Eastern_Data$Country[Eastern_Data$PopName=="LTU"] <- "Lithuania"
gdata::keep(Eastern_Data,sure=T)
source("R/Func.R")

Ro.Data   <- NULL
countries <- unique(Eastern_Data$Country)

for (sex1 in c("Female","Male")){
  for (country in countries){
    
    country.data       <- subset(Eastern_Data,Country==country)
    country.data       <- subset(country.data, select=c("Year","Age","Female","Male","Total"))
    Dx.matrix          <- create.Lexis.matrix(country.data, Sex = sex1, minage = min(country.data$Age),
                                              maxage = max(country.data$Age),
                                              minyear = min(country.data$Year), maxyear = max(country.data$Year))
    
    country.data.1                                          <- subset(Eastern_Data,Country==country)
    country.data.1$Female_exp[country.data.1$Female_exp==0] <- .000001
    country.data.1$Male_exp[country.data.1$Male_exp==0]     <- .000001
    if (sex1 == "Female") country.data.1$Female             <-country.data.1$Female_exp
    if (sex1 == "Male")   country.data.1$Male               <-country.data.1$Male_exp
    
    country.data.1         <- subset(country.data.1, select=c("Year","Age","Female","Male","Total"))
    Ex.matrix              <- create.Lexis.matrix(country.data.1, Sex = sex1, minage = min(country.data.1$Age)
                                                  , maxage = max(country.data.1$Age),
                                                  minyear = min(country.data.1$Year), 
                                                  maxyear = max(country.data.1$Year))
    
    the.agesDx    <- as.numeric(rownames(Dx.matrix))
    the.yearsDx   <- as.numeric(colnames(Dx.matrix))
    the.agesNx    <- as.numeric(rownames(Ex.matrix))
    the.yearsNx   <- as.numeric(colnames(Ex.matrix))  
    fitDx         <- Mort2Dsmooth(x = the.agesDx, y = the.yearsDx, 
                                  Z = Dx.matrix, offset = log(Ex.matrix))
    
    mx   <- fitDx$fitted.values/Ex.matrix
    mx.2 <- mx[, -1]
    mx.1 <- mx[, -(ncol(mx))]
    ro   <- 100 * -log(mx.2/mx.1)
    
    ro.Data          <- melt(ro,varnames = c("Age","Year"))
    ro.Data$Sex      <- sex1
    ro.Data$Country  <- country
    
    Ro.Data          <- rbind(Ro.Data,ro.Data)
    print(country)
  }
}
Ro.Data$Sex <- as.factor(Ro.Data$Sex)

Ro.graph1 <- levelplot(value ~ Year*Age|Country, data=subset(Ro.Data,Sex=="Male" & Year>=1960 & Age <96),          
                       main="Male-rates of mortality improvement (percent %)",            
                       ylab.right = list("percent %)",cex=1.2),
                       par.settings=my.settings,
                       xlab=list("Year",cex=1.2),
                       ylab=list("Age",cex=1.2),
                       colorkey=T,
                       scales=list(x=list(cex=1.2),
                                   y=list(alternating=1,cex=1.2)),
                       par.strip.text=list(cex=1.2),
                       at=c(do.breaks(c(range(Ro.Data$value)[1],-5.111),20), 
                            do.breaks(c(-5,-.5),20),
                            do.breaks(c(-.499999,.499999),20), 
                            do.breaks(c(.5,10),20),               
                            do.breaks(c(10.00001,range(Ro.Data$value)[2]),20)),                             
                       col.regions=colorRampPalette(c("black","red","white","lightblue","blue")),
                       panel = function(...) { 
                         panel.fill(col = "gray89") 
                         panel.levelplot(...) 
                       } ,
                       layout=c(4,3))
Ro.graph1

pdf(file="Latex/Ro_figMales.pdf",width=10,height=8,pointsize=6)
print(Ro.graph1)
dev.off()


Ro.graph2 <- levelplot(value ~ Year*Age|Country, data=subset(Ro.Data,Sex=="Female" & Year>=1960 & Age <96),          
                       main="Female-rates of mortality improvement (percent %)",            
                       ylab.right = list("percent %)",cex=1.2),
                       par.settings=my.settings,
                       xlab=list("Year",cex=1.2),
                       ylab=list("Age",cex=1.2),
                       colorkey=T,
                       scales=list(x=list(cex=1.2),
                                   y=list(alternating=1,cex=1.2)),
                       par.strip.text=list(cex=1.2),
                       at=c(do.breaks(c(range(Ro.Data$value)[1],-5.111),20), 
                            do.breaks(c(-5,-.5),20),
                            do.breaks(c(-.499999,.499999),20), 
                            do.breaks(c(.5,10),20),               
                            do.breaks(c(10.00001,range(Ro.Data$value)[2]),20)),                             
                       col.regions=colorRampPalette(c("black","red","white","lightblue","blue")),
                       panel = function(...) { 
                         panel.fill(col = "gray89") 
                         panel.levelplot(...) 
                       } ,
                       layout=c(4,3))
Ro.graph2

pdf(file="Latex/Ro_figFemales.pdf",width=10,height=8,pointsize=6)
print(Ro.graph2)
dev.off()


