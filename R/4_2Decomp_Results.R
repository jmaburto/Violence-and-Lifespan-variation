library(latticeExtra)
library(reshape2)
library(plyr)
library(grid)
###########################################################################
####  Load decomposition by single year and single age
###########################################################################
setwd("/Users/josemanuelaf/Desktop/Aburto_vanRaalte_2016/")
load("Data/EEDecomp_results.Rdata")

### Some data manipulation
Data                     <- Decomp.results
Data$Sex1[Data$Sex=="f"] <- "Females"
Data$Sex1[Data$Sex=="m"] <- "Males"
Data$Age                 <- as.numeric(levels(Data$Age))[Data$Age]

################# To group by 5 years age-intervals
#set ages as 0,5,10,...85
Labels.age      <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                     '40-44','45-49','50-54','55-59','60-64','65-69',
                     "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105-109","110+")
Data$Age5       <- (cut(Data$Age+1, breaks=c(seq(0,110,5),Inf),labels=Labels.age))

################# 
#Data aggregated by 5-years age-groups
Data5 <- aggregate(Data$value, by = list(Age=Data$Age5,Measure=Data$measure,Year=Data$Year,
                                         Sex=Data$Sex1,Country=Data$Name),FUN=sum)
gdata::keep(Data5,sure=T)

############### Create variabel with breaks in periods
Period.Label      <- c("Stagnation","Improvements","Deterioration","Divergence","Convergence","Rest")
Data5$Period      <- (cut(Data5$Year+1, breaks=c(1960,1980,1987,1994,2000,2010,Inf),labels=Period.Label))
Data.Period       <- aggregate(Data5$x, by = list(Age=Data5$Age,Measure=Data5$Measure,Period=Data5$Period,
                                                  Sex=Data5$Sex,Country=Data5$Country),FUN=sum)
Data.Period$G                      <- 1
Data.Period$G[Data.Period$x>0]     <- 2
  
myColours <- c("grey", "tomato")
my.settings <- list(
  superpose.polygon=list(col=myColours[], border="transparent"),
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)


############## Ready to play a bit


####################### Results for males
#Stagnation males
Graphic1 <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Males"&Period=="Stagnation"&
                                                   Country!="Slovenia" & (Age!="0-4" & Age!="110+")), 
                                  groups=G,  ylab= "Age group",xlab="Contribution (years)",
                                  main="Stagnation",
                                  stack=TRUE,
                                  between = list(x = .5),
                                  layout = c(4, 3),
                                  xlim=c(-.45,.45),
                                  scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                                  par.settings=my.settings,
                                  key = list(x=.85,y=.90,background="white",title="Lifespan disparity",
                                             text=list(c("Decrease",
                                                         "Increase"))
                                             ,cex=.9,
                                             points=list(pch=19,col=myColours)),
                                  panel=function(x,y,...){                      
                                    panel.abline(v=seq(-.4,.4,.1),lwd=1,lty=3,col="darkgrey",...)
                                    panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                                    panel.barchart(...,border="transparent",x,y)
                                    panel.abline(v=0,lwd=1,lty=1,col="black",...)
                                  })
Graphic1


#Improvements males
Graphic2 <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Males"&Period=="Improvements"
                                                  &(Age!="0-4" & Age!="110+")), 
                    groups=G,  ylab= "Age group",xlab="Contribution (years)",
                    main="Improvements",
                    stack=TRUE,
                    between = list(x = .5),
                    layout = c(4, 3),
                    xlim=c(-.45,.45),
                    scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                    par.settings=my.settings,
                    #key = list(x=.85,y=.90,background="white",title="Lifespan disparity",
                     #          text=list(c("Decrease",
                      #                     "Increase"))
                       #        ,cex=.9,
                        #       points=list(pch=19,col=myColours)),
                    panel=function(x,y,...){                      
                      panel.abline(v=seq(-.4,.4,.1),lwd=1,lty=3,col="darkgrey",...)
                      panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                      panel.barchart(...,border="transparent",x,y)
                      panel.abline(v=0,lwd=1,lty=1,col="black",...)
                    })
Graphic2

#Dterioration males
Graphic3 <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Males"&Period=="Deterioration"
                                                    & (Age!="0-4" & Age!="110+")), 
                    groups=G,  ylab= "Age group",xlab="Contribution (years)",
                    main="Deterioration",
                    stack=TRUE,
                    between = list(x = .5),
                    layout = c(4, 3),
                    xlim=c(-.45,.45),
                    scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                    par.settings=my.settings,
                    #key = list(x=.85,y=.90,background="white",title="Lifespan disparity",
                     #          text=list(c("Decrease",
                      #                     "Increase"))
                       #        ,cex=.9,
                        #       points=list(pch=19,col=myColours)),
                    panel=function(x,y,...){                      
                      panel.abline(v=seq(-.4,.4,.1),lwd=1,lty=3,col="darkgrey",...)
                      panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                      panel.barchart(...,border="transparent",x,y)
                      panel.abline(v=0,lwd=1,lty=1,col="black",...)
                    })
Graphic3


#Divergence males
Graphic4 <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Males"&Period=="Divergence"
                                                  & (Age!="0-4" & Age!="110+")), 
                    groups=G,  ylab= "Age group",xlab="Contribution (years)",
                    main="Divergence",
                    stack=TRUE,
                    between = list(x = .5),
                    layout = c(4, 3),
                    xlim=c(-.45,.45),
                    scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                    par.settings=my.settings,
                    #key = list(x=.85,y=.90,background="white",title="Lifespan disparity",
                    #          text=list(c("Decrease",
                    #                     "Increase"))
                    #        ,cex=.9,
                    #       points=list(pch=19,col=myColours)),
                    panel=function(x,y,...){                      
                      panel.abline(v=seq(-.4,.4,.1),lwd=1,lty=3,col="darkgrey",...)
                      panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                      panel.barchart(...,border="transparent",x,y)
                      panel.abline(v=0,lwd=1,lty=1,col="black",...)
                    })
Graphic4


#Convergence males
Graphic5 <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Males"&Period=="Convergence"
                                                  & (Age!="0-4" & Age!="110+")), 
                    groups=G,  ylab= "Age group",xlab="Contribution (years)",
                    main="Convergence",
                    stack=TRUE,
                    between = list(x = .5),
                    layout = c(4, 3),
                    xlim=c(-.45,.45),
                    scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                    par.settings=my.settings,
                    #key = list(x=.85,y=.90,background="white",title="Lifespan disparity",
                    #          text=list(c("Decrease",
                    #                     "Increase"))
                    #        ,cex=.9,
                    #       points=list(pch=19,col=myColours)),
                    panel=function(x,y,...){                      
                      panel.abline(v=seq(-.4,.4,.1),lwd=1,lty=3,col="darkgrey",...)
                      panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                      panel.barchart(...,border="transparent",x,y)
                      panel.abline(v=0,lwd=1,lty=1,col="black",...)
                    })
Graphic5


pdf(file="Latex/Decomp_males.pdf",width=11,height=9,pointsize=12)
print(Graphic1)
print(Graphic2)
print(Graphic3)
print(Graphic4)
print(Graphic5)
dev.off()

####################### Results for females
#Stagnation males
Graphic1f <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Females"&Period=="Stagnation"&
                                                    Country!="Slovenia" & (Age!="0-4" & Age!="110+")), 
                    groups=G,  ylab= "Age group",xlab="Contribution (years)",
                    main="Stagnation",
                    stack=TRUE,
                    between = list(x = .5),
                    layout = c(4, 3),
                    xlim=c(-.45,.45),
                    scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                    par.settings=my.settings,
                    key = list(x=.85,y=.90,background="white",title="Lifespan disparity",
                               text=list(c("Decrease",
                                           "Increase"))
                               ,cex=.9,
                               points=list(pch=19,col=myColours)),
                    panel=function(x,y,...){                      
                      panel.abline(v=seq(-.4,.4,.1),lwd=1,lty=3,col="darkgrey",...)
                      panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                      panel.barchart(...,border="transparent",x,y)
                      panel.abline(v=0,lwd=1,lty=1,col="black",...)
                    })
Graphic1f


#Improvements males
Graphic2f <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Females"&Period=="Improvements"
                                                  &(Age!="0-4" & Age!="110+")), 
                    groups=G,  ylab= "Age group",xlab="Contribution (years)",
                    stack=TRUE,
                    main="Improvements",
                    between = list(x = .5),
                    layout = c(4, 3),
                    xlim=c(-.45,.45),
                    scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                    par.settings=my.settings,
                    #key = list(x=.85,y=.90,background="white",title="Lifespan disparity",
                    #          text=list(c("Decrease",
                    #                     "Increase"))
                    #        ,cex=.9,
                    #       points=list(pch=19,col=myColours)),
                    panel=function(x,y,...){                      
                      panel.abline(v=seq(-.4,.4,.1),lwd=1,lty=3,col="darkgrey",...)
                      panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                      panel.barchart(...,border="transparent",x,y)
                      panel.abline(v=0,lwd=1,lty=1,col="black",...)
                    })
Graphic2f

#Dterioration males
Graphic3f <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Females"&Period=="Deterioration"
                                                  & (Age!="0-4" & Age!="110+")), 
                    groups=G,  ylab= "Age group",xlab="Contribution (years)",
                    stack=TRUE,
                    main="Deterioration",
                    between = list(x = .5),
                    layout = c(4, 3),
                    xlim=c(-.45,.45),
                    scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                    par.settings=my.settings,
                    #key = list(x=.85,y=.90,background="white",title="Lifespan disparity",
                    #          text=list(c("Decrease",
                    #                     "Increase"))
                    #        ,cex=.9,
                    #       points=list(pch=19,col=myColours)),
                    panel=function(x,y,...){                      
                      panel.abline(v=seq(-.4,.4,.1),lwd=1,lty=3,col="darkgrey",...)
                      panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                      panel.barchart(...,border="transparent",x,y)
                      panel.abline(v=0,lwd=1,lty=1,col="black",...)
                    })
Graphic3f


#Divergence males
Graphic4f <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Females"&Period=="Divergence"
                                                  & (Age!="0-4" & Age!="110+")), 
                    groups=G,  ylab= "Age group",xlab="Contribution (years)",
                    stack=TRUE,
                    main="Divergence",
                    between = list(x = .5),
                    layout = c(4, 3),
                    xlim=c(-.45,.45),
                    scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                    par.settings=my.settings,
                    #key = list(x=.85,y=.90,background="white",title="Lifespan disparity",
                    #          text=list(c("Decrease",
                    #                     "Increase"))
                    #        ,cex=.9,
                    #       points=list(pch=19,col=myColours)),
                    panel=function(x,y,...){                      
                      panel.abline(v=seq(-.4,.4,.1),lwd=1,lty=3,col="darkgrey",...)
                      panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                      panel.barchart(...,border="transparent",x,y)
                      panel.abline(v=0,lwd=1,lty=1,col="black",...)
                    })
Graphic4f


#Convergence males
Graphic5f <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Females"&Period=="Convergence"
                                                  & (Age!="0-4" & Age!="110+")), 
                    groups=G,  ylab= "Age group",xlab="Contribution (years)",
                    stack=TRUE,
                    main="Convergence",
                    between = list(x = .5),
                    layout = c(4, 3),
                    xlim=c(-.45,.45),
                    scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                    par.settings=my.settings,
                    #key = list(x=.85,y=.90,background="white",title="Lifespan disparity",
                    #          text=list(c("Decrease",
                    #                     "Increase"))
                    #        ,cex=.9,
                    #       points=list(pch=19,col=myColours)),
                    panel=function(x,y,...){                      
                      panel.abline(v=seq(-.4,.4,.1),lwd=1,lty=3,col="darkgrey",...)
                      panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                      panel.barchart(...,border="transparent",x,y)
                      panel.abline(v=0,lwd=1,lty=1,col="black",...)
                    })
Graphic5f

pdf(file="Latex/Decomp_females.pdf",width=11,height=9,pointsize=12)
print(Graphic1f)
print(Graphic2f)
print(Graphic3f)
print(Graphic4f)
print(Graphic5f)
dev.off()


#########################
myColours1 <- c("grey", "blue", "red", "green", "magenta")

my.settings1 <- list(
  superpose.polygon=list(col=myColours1[], border="transparent"),
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)
fig.labels <- c("Stagnation 1960-1980","Improvements 1980-1987","Deterioration 1987-1994",
                "Divergence 1994-2000","Convergence 2000-2010")

key <- list( rep=FALSE,
             points=list(col=c(myColours1[1], myColours1[2]), type="p",
                        pch=19),
             text=list(lab=c(fig.labels[1:2])),
             points=list(col=c(myColours1[3], myColours1[4]), type="p",
                        pch=19),
             text=list(lab=c(fig.labels[3:4])),
             points = list(col= myColours1[5], type="p",pch=19),
             text=list(lab=fig.labels[5]))

Graphic.P <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Males"
                                                   & (Age!="0-4" & Age!="110+")&Period!="Rest"), 
                     groups=Period,  ylab= "Age group",xlab="Contribution (years)",
                     stack=TRUE,
                     main="Males",
                     between = list(x = .5),
                     layout = c(4, 3),
                     xlim=c(-.7,.7),
                     scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                     par.settings=my.settings1,
                     key = key,
                     panel=function(x,y,...){                      
                       panel.abline(v=seq(-.6,.6,.1),lwd=1,lty=3,col="darkgrey",...)
                       panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                       panel.barchart(...,border="transparent",x,y)
                       panel.abline(v=0,lwd=1,lty=1,col="black",...)
                     })
Graphic.P

pdf(file="Latex/MalesDecomp_Periods.pdf",width=11,height=9,pointsize=12)
print(Graphic.P)
dev.off()

###########
Graphic.Pf <-barchart(Age ~ x |Country, data=subset(Data.Period,Measure=="ed"&Sex=="Females"
                                                   & (Age!="0-4" & Age!="110+")&Period!="Rest"), 
                     groups=Period,  ylab= "Age group",xlab="Contribution (years)",
                     stack=TRUE,
                     main="Females",
                     between = list(x = .5),
                     layout = c(4, 3),
                     xlim=c(-.7,.7),
                     scales = list(x = list(alternating=1),y=list(alternating=3),cex=.6), 
                     par.settings=my.settings1,
                     key = key,
                     panel=function(x,y,...){                      
                       panel.abline(v=seq(-.6,.6,.1),lwd=1,lty=3,col="darkgrey",...)
                       panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                       panel.barchart(...,border="transparent",x,y)
                       panel.abline(v=0,lwd=1,lty=1,col="black",...)
                     })
Graphic.Pf

pdf(file="Latex/FemalesDecomp_Periods.pdf",width=11,height=9,pointsize=12)
print(Graphic.Pf)
dev.off()

########### Now the group 0-4
Graphic.infant <-barchart(Country ~ x|Sex, data=subset(Data.Period,Measure=="ed"
                                                    & (Age=="0-4")&Period!="Rest"), 
                      groups=Period,  ylab= "Age group",xlab="Contribution (years)",
                      stack=TRUE,
                      main="Contributions of the age group 0-4",
                      between = list(x = .5),
                      #layout = c(4, 3),
                      xlim=c(-3.5,1),
                      scales = list(x = list(alternating=1),y=list(alternating=1),cex=1), 
                      par.settings=my.settings1,
                      key = key,
                      panel=function(x,y,...){                      
                        panel.abline(v=seq(-3.5,1,.5),lwd=1,lty=3,col="darkgrey",...)
                        panel.abline(h=seq(1,12,1),lwd=1,lty=3,col="darkgrey",...)
                        panel.barchart(...,border="transparent",x,y)
                        panel.abline(v=0,lwd=1,lty=1,col="black",...)
                      })
Graphic.infant

pdf(file="Latex/MalesInfant_Periods.pdf",width=9,height=5,pointsize=12)
print(Graphic.infant)
dev.off()