# Results at the national level
rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(latticeExtra)

setwd("C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation")

load("Data/National.decomp.RData")
National.decomp$Age <- rep(0:109,80)

########################
source("R/Functions.R")


#### sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 
#### 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer,
#### 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories

# Group causes as I want
amenable     <- rowSums(National.decomp[,c(4:7,9)])
other        <- rowSums(National.decomp[,c(17:19)])
National.Fig <- cbind(National.decomp[,c(1:3,20)],amenable,National.decomp[,c(8)],National.decomp[,c(10:16)],other)
Names1       <- colnames(National.decomp)
colnames(National.Fig) <- c(Names1[c(1:3,20)], CoD.name.vec2)

National.Fig <- melt(National.Fig, id.vars = 1:4,variable.name = 'Cause',value.name = 'Contribution')

################# To group by 5 years age-intervals
#set ages as 0,5,10,...85
Labels.age      <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                     '40-44','45-49','50-54','55-59','60-64','65-69',
                     "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105-109","110+")
National.Fig$Age5       <- (cut(National.Fig$Age+1, breaks=c(seq(0,110,5),Inf),labels=Labels.age))

################# 
#Data aggregated by 5-years age-groups
National.Fig.5 <- aggregate(National.Fig$Contribution, by = list(Age = National.Fig$Age5,Measure=National.Fig$measure,
                                                                 Year=National.Fig$Year, Sex=National.Fig$Sex,
                                                                 Cause=National.Fig$Cause),FUN=sum)

unique(National.Fig.5$Cause)
############### Create variabel with breaks in periods
Period.Label      <- c("1995-2005","2005-2015")
National.Fig.5$Period      <- (cut(National.Fig.5$Year+1, breaks=c(1990,2005,Inf),labels=Period.Label))
National.Fig.5       <- aggregate(National.Fig.5$x, by = list(Age = National.Fig.5$Age,Measure=National.Fig.5$Measure,
                                                              Period=National.Fig.5$Period, Sex=National.Fig.5$Sex,
                                                              Cause=National.Fig.5$Cause),FUN=sum)

col.diabetes   <- "#e265e7"
col.amenable   <- "#7265e7"
col.homicide   <- brewer.pal(9,"Reds")[c(9)]

base2 <- toupper(c(col.amenable,col.diabetes,brewer.pal(7,"Blues")[c(4)],"#e7657d", "#e7bc65",
                   "#a2e765", "#65e797",'red','yellow','lightgrey'))
#plot(1:7,1:7,col=toupper(base2),pch=16,cex=5)
# determine order by eyeballing colors to causes (HT J. Schoeley)
myColours1 <- base2

my.settings1 <- list(
  superpose.polygon=list(col=myColours1[], border="transparent"),
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)

fig.labels <- CoD.name.vec2

National.e0 <-barchart(Age ~ x|Period , 
                       data=National.Fig.5[National.Fig.5$Measure == 1 & National.Fig.5$Sex ==1 &
                                             National.Fig.5$Age != '0-4' & National.Fig.5$Age != '80-84' &
                                             National.Fig.5$Age != '85-89' & National.Fig.5$Age != '90-94' &
                                             National.Fig.5$Age != '95-99' & National.Fig.5$Age != '100-104' &
                                             National.Fig.5$Age != '105-109',], 
                            groups=Cause,  ylab= 'Age group',xlab='',
                            strip=T,main='Male life expectancy',
                            stack=TRUE,box.ratio=7,
                            between = list(x=.5),
                            #layout = c(4, 3),
                            xlim=c(-.3,.3),
                            scales = list(x = list(alternating=1),y=list(alternating=1),cex=.6), 
                            par.settings=my.settings1,
                       key = list(x=.01,y=.45,background="white",title="Cause",
                                  points=list(col=myColours1,pch=19),text=list(fig.labels)
                                  ,cex=1),
                            panel=function(x,y,...){                      
                              panel.abline(v=seq(-.3,.3,.1),lwd=1,lty=3,col="darkgrey",...)
                              panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                              panel.barchart(...,border="transparent",x,y)
                              panel.abline(v=0,lwd=1,lty=1,col="black",...)
                            })
National.e0

National.edagger <-barchart(Age ~ x|Period , 
                       data=National.Fig.5[National.Fig.5$Measure == 2 & National.Fig.5$Sex ==1 &
                                             National.Fig.5$Age != '0-4' & National.Fig.5$Age != '80-84' &
                                             National.Fig.5$Age != '85-89' & National.Fig.5$Age != '90-94' &
                                             National.Fig.5$Age != '95-99' & National.Fig.5$Age != '100-104' &
                                             National.Fig.5$Age != '105-109',],
                       groups=Cause,  ylab= "Age group",xlab="Contribution",
                       strip=T,main='Male life disparity',
                       stack=TRUE,box.ratio=7,
                       between = list(x=.5),
                       #layout = c(4, 3),
                       xlim=c(-.13,.13),
                       scales = list(x = list(alternating=1),y=list(alternating=1),cex=.6), 
                       par.settings=my.settings1,
                       panel=function(x,y,...){                      
                         panel.abline(v=seq(-.3,.3,.05),lwd=1,lty=3,col="darkgrey",...)
                         panel.abline(h=seq(1,21,2),lwd=1,lty=3,col="darkgrey",...)
                         panel.barchart(...,border="transparent",x,y)
                         panel.abline(v=0,lwd=1,lty=1,col="black",...)
                       })
National.edagger


require(gridExtra)
pdf(file="R/Figures/National_decomp.pdf",width=12,height=12,pointsize=4,useDingbats = F)
grid.arrange(National.e0,National.edagger, nrow=2)
dev.off()