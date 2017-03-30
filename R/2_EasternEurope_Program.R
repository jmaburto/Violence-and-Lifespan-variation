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
library(locfit)
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

setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")

load("Data/HMD_Data.RData")
source("R/Func.R")

unique(HMDL$PopName)

#### Select countries from eastern Europe
Eastern_HMDL   <- subset(HMDL,(PopName=="BLR" | PopName=="BGR" | PopName=="CZE" | 
                                         PopName=="HUN"| PopName=="POL"| PopName=="RUS" | 
                                         PopName=="SVK" | PopName=="UKR" | PopName=="SVN" 
                                       | PopName=="EST" | PopName=="LVA" | PopName=="LTU"))

Eastern_HMDL$Country[Eastern_HMDL$PopName=="BLR"] <- "Belarus"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="BGR"] <- "Bulgaria"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="CZE"] <- "Czech Republic"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="HUN"] <- "Hungary"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="POL"] <- "Poland"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="RUS"] <- "Russia"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="SVK"] <- "Slovakia"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="UKR"] <- "Ukraine"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="SVN"] <- "Slovenia"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="EST"] <- "Estonia"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="LVA"] <- "Latvia"
Eastern_HMDL$Country[Eastern_HMDL$PopName=="LTU"] <- "Lithuania"

Eastern_HMDL$Sex1[Eastern_HMDL$Sex=="f"] <- "Females"
Eastern_HMDL$Sex1[Eastern_HMDL$Sex=="m"] <- "Males"

###Drop data before 1960
Eastern_HMDL <- subset(Eastern_HMDL, Year>=1960)
#Russia <- subset(Eastern_HMDL, Country=="Russia" & Sex1=="Males" & Age==0)
#Russia$ex[Russia$Year==1987]-Russia$ex[Russia$Year==1994]
#Latvia <- subset(Eastern_HMDL, Country=="Latvia" & Sex1=="Males" & Age==0)
#Latvia$ex[Latvia$Year==1987]-Latvia$ex[Latvia$Year==1994]
#Slovenia <- subset(Eastern_HMDL, Country=="Slovenia" & Sex1=="Males" & Age==0)
#Slovenia$ex[Slovenia$Year==2010]-Russia$ex[Russia$Year==2010]

####Quick plot
F1b <- xyplot(ex ~ Year | Sex1, data=subset(Eastern_HMDL,Age==0 & Sex=="m"), type="o",
              main="Life expectancy",
             group=Country,pch=pch.fig.F1b,col=makeTransparent(col.fig.F1b,160),xlab=list("Year",cex=1.5),
             xlim=c(1959,2016),ylim=c(50,85),ylab=list("Years",cex=1.5),between=list(x=1.5),
             key = list(x=.05,y=.95, title="Country",background="white", 
                        text=list(c("Belarus","Bulgaria","Czech Republic","Estonia","Hungary","Latvia",
                                    "Lithuania","Poland","Russia","Slovakia","Slovenia","Ukraine"))
                        ,cex=1,
                        points=list(pch=pch.fig.F1b,col=col.fig.F1b)),
             par.settings=my.settings,strip=F,
             scales=list(alternating=1,x=list(cex=1.5,at=c(seq(1950,2010,10))),
                         y=list(cex=1.5,at=c(seq(50,85,5)),alternating=1)),
             panel = function(x, y, ...){            
               panel.abline(v=c(seq(1950,2010,10)),col='dark grey',lty=3)
               panel.abline(h=c(seq(50,85,5)),col='dark grey',lty=3)                  
               panel.abline(v=c(1991),col='blue',lty=1)
               panel.arrows(1980,55,1986,55,length=.1,col="black")  
               panel.text(1968,55,"Gorbachev's anti-alcohol campaign",cex=1.1)
               panel.arrows(1997,55,1991,55,length=.1,col="black")  
               panel.text(2005,55,"Soviet Union break up",cex=1.1)
               panel.rect(xleft=1985, xright=1987,ybottom=50, ytop=85,col=makeTransparent("red",90))
               panel.xyplot(x, y, ...)         
             })
F1b

F1c <- xyplot(ex ~ Year | Sex1, data=subset(Eastern_HMDL,Age==0), type="o",
             group=Country,pch=pch.fig.F1b,col=makeTransparent(col.fig.F1b,160),xlab=list("Year",cex=1.5),
             xlim=c(1959,2016),ylim=c(50,85),ylab=list("Years",cex=1.5),between=list(x=1.5),
             key = list(x=.55,y=.95, title="Country",background="white", 
                        text=list(c("Belarus","Bulgaria","Czech Republic","Estonia","Hungary","Latvia",
                                    "Lithuania","Poland","Russia","Slovakia","Slovenia","Ukraine"))
                        ,cex=1,
                        points=list(pch=pch.fig.F1b,col=col.fig.F1b)),
             par.settings=my.settings,
             scales=list(alternating=1,x=list(cex=1.5,at=c(seq(1950,2010,10))),
                         y=list(cex=1.5,at=c(seq(50,85,5)),alternating=1)),
             panel = function(x, y, ...){            
               panel.abline(v=c(seq(1950,2010,10)),col='dark grey',lty=3)
               panel.abline(h=c(seq(50,85,5)),col='dark grey',lty=3)                  
               panel.abline(v=c(1991),col='blue',lty=1)
               panel.arrows(1980,55,1986,55,length=.1,col="black")  
               panel.text(1968,55,"Gorbachev's anti-alcohol campaign",cex=.8)
               panel.arrows(1997,55,1991,55,length=.1,col="black")  
               panel.text(2005,55,"Soviet Union break up",cex=.8)
               panel.rect(xleft=1985, xright=1987,ybottom=50, ytop=85,col=makeTransparent("red",90))
               panel.xyplot(x, y, ...)         
             })
F1c

pdf(file="Latex/F1c.pdf",width=17,height=8,pointsize=4)
print(F1c)
dev.off()



############ Plotting deaths distribution for males in selected years
Eastern_HMDL$dx.p <- Eastern_HMDL$dx/100000

dx1         <- subset(Eastern_HMDL, (Year==1970) & Sex=="m")
dx1.1       <- subset(Eastern_HMDL, (Year==1983) & Sex=="m" & Country== "Slovenia")
dx1         <- rbind(dx1, dx1.1)
dx1$ID      <- 1
dx1$low.ci  <- dx1$dx.p
dx1$up.ci   <- 0

dx2         <- subset(Eastern_HMDL, (Year==1994) & Sex=="m")
dx2$ID      <- 2
dx2$low.ci  <- dx2$dx.p
dx2$up.ci   <- 0
unique(dx2$Country)

dx3         <- subset(Eastern_HMDL, (Year==2009) & Sex=="m")
dx3$ID      <- 3
dx3$low.ci  <- dx3$dx.p
dx3$up.ci   <- 0

F1.dx  <- dx.Fig(Data1=dx1,lcolor="dodgerblue3",fillcol=makeTransparent("dodgerblue3",90),
              upci=dx1$up.ci,lowci=dx1$low.ci,a=dx1$Age,
              main="Distribution of male lifetable-deaths (dx)")

F2.dx  <- dx.Fig(Data1=dx2,lcolor="black",fillcol=makeTransparent("black",90),
              upci=dx2$up.ci,lowci=dx2$low.ci,a=dx2$Age,
              main="Distribution of male lifetable-deaths (dx)")

F3.dx  <- dx.Fig(Data1=dx3,lcolor="firebrick1",fillcol=makeTransparent("firebrick1",95),
              upci=dx3$up.ci,lowci=dx3$low.ci,a=dx3$Age,
              main="Distribution of male lifetable-deaths (dx)")

F4.dx  <- F1.dx+F2.dx+F3.dx
F4.dx

pdf(file="Latex/dx.fig.pdf",width=16,height=10,pointsize=4)
print(F4.dx)
dev.off()


################ Calculate edagger
Eastern_HMDL$fx    <- (Eastern_HMDL$lx/100000)*Eastern_HMDL$qx
Data               <- data.table(Eastern_HMDL)
edagger            <- Data[, e.dagger(fx=fx,ex=ex,ax=ax), by = list(PopName,Sex,Year)]
edagger            <- as.data.frame(edagger)
edagger            <- rename(edagger,c(V1="edagger"))

edagger$Country[edagger$PopName=="BLR"] <- "Belarus"
edagger$Country[edagger$PopName=="BGR"] <- "Bulgaria"
edagger$Country[edagger$PopName=="CZE"] <- "Czech Republic"
edagger$Country[edagger$PopName=="HUN"] <- "Hungary"
edagger$Country[edagger$PopName=="POL"] <- "Poland"
edagger$Country[edagger$PopName=="RUS"] <- "Russia"
edagger$Country[edagger$PopName=="SVK"] <- "Slovakia"
edagger$Country[edagger$PopName=="UKR"] <- "Ukraine"
edagger$Country[edagger$PopName=="SVN"] <- "Slovenia"
edagger$Country[edagger$PopName=="EST"] <- "Estonia"
edagger$Country[edagger$PopName=="LVA"] <- "Latvia"
edagger$Country[edagger$PopName=="LTU"] <- "Lithuania"

edagger$Sex1[edagger$Sex=="f"] <- "Females"
edagger$Sex1[edagger$Sex=="m"] <- "Males"

####Quick plot


F2c <- xyplot(edagger ~ Year | Sex1, data=edagger, type="o",
             group=Country,pch=pch.fig.F1b,col=makeTransparent(col.fig.F1b,160),xlab=list("Year",cex=1.5),
             xlim=c(1959,2016),
             ylim=c(9,20),ylab=list("Years",cex=1.5),between=list(x=1.5),
             #key = list(x=.51,y=.95, title="Country",background="white", 
                      #  text=list(c("Belarus","Bulgaria","Czech Republic","Estonia","Hungary","Latvia",
                         #           "Lithuania","Poland","Russia","Slovakia","Slovenia","Ukraine"))
                     #   ,cex=1,
                   #     points=list(pch=pch.fig.F1b,col=col.fig.F1b)),
             par.settings=my.settings,
             scales=list(alternating=1,x=list(cex=1.5,at=c(seq(1950,2010,10))),
                         y=list(cex=1.5,at=c(seq(10,20,1)),alternating=1)),
             ,panel = function(x, y, ...){            
               panel.abline(v=c(seq(1950,2010,10)),col='dark grey',lty=3)
               panel.abline(h=c(seq(10,20,1)),col='dark grey',lty=3)                  
               panel.abline(v=c(1991),col='blue',lty=1)
               #panel.arrows(1980,20,1986,20,length=.1,col="black")  
               #panel.text(1968,20,"Gorbachev's anti-alcohol campaign",cex=.95)
               #panel.arrows(1997,20,1991,20,length=.1,col="black")  
               #panel.text(2005,20,"Soviet Union break up",cex=.95)
               panel.rect(xleft=1985, xright=1987,ybottom=8, ytop=25,col=makeTransparent("red",90))
               panel.xyplot(x, y, ...)         
             })

F2c

pdf(file="Latex/F2c.pdf",width=17,height=8,pointsize=4)
print(F2c)
dev.off()




F2 <- xyplot(edagger ~ Year | Sex1, data=subset(edagger,Sex=="m"), type="o",
             main="Life disparity",strip=F,
              group=Country,pch=pch.fig.F1b,col=makeTransparent(col.fig.F1b,160),xlab=list("Year",cex=1.5),
             xlim=c(1959,2016),
             ylim=c(9,20),ylab=list("Years",cex=1.5),between=list(x=1.5),
             #key = list(x=.51,y=.95, title="Country",background="white", 
             #  text=list(c("Belarus","Bulgaria","Czech Republic","Estonia","Hungary","Latvia",
             #           "Lithuania","Poland","Russia","Slovakia","Slovenia","Ukraine"))
             #   ,cex=1,
             #     points=list(pch=pch.fig.F1b,col=col.fig.F1b)),
             par.settings=my.settings,
             scales=list(alternating=1,x=list(cex=1.5,at=c(seq(1950,2010,10))),
                         y=list(cex=1.5,at=c(seq(10,20,1)),alternating=1)),
             ,panel = function(x, y, ...){            
               panel.abline(v=c(seq(1950,2010,10)),col='dark grey',lty=3)
               panel.abline(h=c(seq(10,20,1)),col='dark grey',lty=3)                  
               panel.abline(v=c(1991),col='blue',lty=1)
               #panel.arrows(1980,20,1986,20,length=.1,col="black")  
               #panel.text(1968,20,"Gorbachev's anti-alcohol campaign",cex=.95)
               #panel.arrows(1997,20,1991,20,length=.1,col="black")  
               #panel.text(2005,20,"Soviet Union break up",cex=.95)
               panel.rect(xleft=1985, xright=1987,ybottom=8, ytop=25,col=makeTransparent("red",90))
               panel.xyplot(x, y, ...)        
              })

F2

require(gridExtra)
pdf(file="Latex/F1.pdf",width=17,height=8,pointsize=4)
grid.arrange(F1b,F2, ncol=2)
dev.off()



