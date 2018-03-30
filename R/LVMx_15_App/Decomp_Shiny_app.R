library(shiny)
library(ggplot2)
library(data.table)
library(RColorBrewer)
setwd("C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation/R/LVMx_15_App/")


rsconnect::setAccountInfo(name='demographs',
                          token='85D46E99E52F997A5B7724A2CB92970D',
                          secret='nSFJ33exT9xU84CNmqwmlIRlXOAfma3HuwEg5owg')

state.ind   <- 'National'
initial.ind <- 2000
final.ind   <- 2010
Data        <- Data.ex

Data.fig   <- Data[Data$year >= initial.ind & Data$year < final.ind & Data$Name == state.ind, ]
Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)] 
#Data.fig        <- Data.fig[Data.fig$Age!= '0-4',]

Total.Age <- Data.fig[,sum(Contribution), by = list(Age,Sex)]
Total.Age$V1 <- round(Total.Age$V1,2)

base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'lightgrey')
Data.fig$Contribution <- round(Data.fig$Contribution,2)
p <- ggplot(Data.fig[Data.fig$Sex=='Males'], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('Decomposition of life expectancy (years)', subtitle = paste0(state.ind,', ', initial.ind,'-',final.ind))+
  #facet_wrap(~Sex)+
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = " ", y = " ",size=16)+
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 16, colour = "black"))+
  geom_hline(yintercept = 0)
p
pdf(file="Cause_e0_decomp_Males.pdf",width=8,height=5,pointsize=6,useDingbats = F)
print(p)
dev.off()


### now for e.dagger

Data        <- Data.ed

Data.fig   <- Data[Data$year >= initial.ind & Data$year < final.ind & Data$Name == state.ind, ]
Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)] 
#Data.fig        <- Data.fig[Data.fig$Age!= '0-4',]

Total.Age <- Data.fig[,sum(Contribution), by = list(Age,Sex)]
Total.Age$V1 <- round(Total.Age$V1,2)

base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'lightgrey')
Data.fig$Contribution <- round(Data.fig$Contribution,2)
q <- ggplot(Data.fig[Data.fig$Sex=='Males'], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('Decomposition of lifespan  expectancy (years)', subtitle = paste0(state.ind,', ', initial.ind,'-',final.ind))+
  #facet_wrap(~Sex)+
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
 # ylim(-.55,1.3 )+
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = " ", y = " ",size=16)+
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 16, colour = "black"))+
  geom_hline(yintercept = 0)
q
pdf(file="Cause_ed_decomp_Males.pdf",width=8,height=5,pointsize=6,useDingbats = F)
print(q)
dev.off()





#### now chihuahua
state.ind   <- 'Chihuahua'
initial.ind <- 2000
final.ind   <- 2010

Data        <- Data.ed

Data.fig   <- Data[Data$year >= initial.ind & Data$year < final.ind & Data$Name == state.ind, ]
Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)] 
#Data.fig        <- Data.fig[Data.fig$Age!= '0-4',]

Total.Age <- Data.fig[,sum(Contribution), by = list(Age,Sex)]
Total.Age$V1 <- round(Total.Age$V1,2)

base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'lightgrey')
Data.fig$Contribution <- round(Data.fig$Contribution,2)
r <- ggplot(Data.fig[Data.fig$Sex=='Males'], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('Decomposition of lifespan  expectancy (years)', subtitle = paste0(state.ind,', ', initial.ind,'-',final.ind))+
  #facet_wrap(~Sex)+
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  ylim(-.55,1.3 )+
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = " ", y = " ",size=16)+
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 16, colour = "black"))+
  geom_hline(yintercept = 0)
r
pdf(file="Cause_ed_decomp_Males_Chihuahua.pdf",width=8,height=5,pointsize=6,useDingbats = F)
print(r)
dev.off()



#### now chihuahua
state.ind   <- 'Guerrero'
initial.ind <- 2005
final.ind   <- 2015

Data        <- Data.ed

Data.fig   <- Data[Data$year >= initial.ind & Data$year < final.ind & Data$Name == state.ind, ]
Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)] 
#Data.fig        <- Data.fig[Data.fig$Age!= '0-4',]

Total.Age <- Data.fig[,sum(Contribution), by = list(Age,Sex)]
Total.Age$V1 <- round(Total.Age$V1,2)

base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'lightgrey')
Data.fig$Contribution <- round(Data.fig$Contribution,2)
s <- ggplot(Data.fig[Data.fig$Sex=='Males'], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('Decomposition of lifespan  expectancy (years)', subtitle = paste0(state.ind,', ', initial.ind,'-',final.ind))+
  #facet_wrap(~Sex)+
  scale_fill_manual('Cause of death', values = base2) + 
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  #ylim(-.55,1.3 )+
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = " ", y = " ",size=16)+
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 16, colour = "black"))+
  geom_hline(yintercept = 0)
s
pdf(file="Cause_ed_decomp_Males_Guerrero.pdf",width=8,height=5,pointsize=6,useDingbats = F)
print(s)
dev.off()




pdf(file="Cause_ed_decomp_Males_states.pdf",width=8,height=5,pointsize=6,useDingbats = F)
for(i in unique(Data.ed$Name)){
#i <- unique(Data.ed$Name)[1]
  
state.ind   <- i
initial.ind <- 2005
final.ind   <- 2015

Data        <- Data.ed

Data.fig   <- Data[Data$year >= initial.ind & Data$year < final.ind & Data$Name == state.ind & Data$Age!= '0-4'& Data$Age!= '5-9', ]
Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)] 
#Data.fig        <- Data.fig[Data.fig$Age!= '0-4',]

base2 <- c(rep('light grey',5),rev(brewer.pal(8,name = 'Spectral'))[8],'light grey','lightgrey')

Data.fig$Contribution <- round(Data.fig$Contribution,2)
s <- ggplot(Data.fig[Data.fig$Sex=='Males'], aes(x = Age, y = Contribution, fill = Cause)) +
  ggtitle('Decomposition of lifespan  variation (years)', subtitle = paste0(state.ind,', ', initial.ind,'-',final.ind))+
  #facet_wrap(~Sex)+
  scale_fill_manual('Cause of death', values = base2,guide=FALSE) + 
  geom_bar(stat = "identity",position = "stack")+
  theme_light()+
  ylim(-.23,.2)+
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=45, hjust=1))+
  labs(x = " ", y = " ",size=16)+
  theme(text = element_text(size=15),
        strip.text.x = element_text(size = 16, colour = "black"))+
  geom_hline(yintercept = 0)
s
print(s)
}

dev.off()


 #source('C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation/R/2_3_Reshape_Results.R')

 #transform datasets to have a faster response in shinny server

 getData.function.g <- function(Data2,state ,initial,final){
    #AMS= #### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 4. Birth, 5. Other AMS,HIV

   Data <- cbind(Data2[,1:6], AMS = rowSums(Data2[,c(7,10,8,9,12,14)]), Diabetes = Data2[,c(11)], IHD = Data2[,c(13)],
                 LungCancer = Data2[,c(16)], Cirrhosis = Data2[,c(17)],Homicide = Data2[,c(18)],TAcc = Data2[,c(19)],
                 Rest = rowSums(Data2[,c(15,20,21)]))

   colnames(Data) <- c('Name','Region','Sex','State','year','age1',1:8)
   Data.melt               <- melt(Data, id.vars = 1:6,variable.name = 'Cause',value.name = 'Contribution')
   levels(Data.melt$Cause) <- c('AMS','Diabetes','IHD','Lung Cancer','Cirrhosis',
                                'Homicide','Traffic accidents','Rest')
   #Data.melt   <- Data.melt[Data.melt$year >= initial & Data.melt$year < final & Data.melt$Name == state, ]

   #Data.melt <- Data.melt[Data.melt$age1 < 100,]
   Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
                              '40-44','45-49','50-54','55-59','60-64','65-69',
                              #                          "70-74","75-79","80-84","85-89","90-94","95+")
                              "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105+")
   Data.melt$Age       <- (cut(Data.melt$age1+1, breaks=c(seq(0,105,5),Inf),labels=Labels.age))
   Data.melt5 <- Data.melt[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,year,Cause,Age)]

   #Data.melt5$Contribution <- round(Data.melt5$Contribution,2)
   Data.melt5
 }

 state.ind   <- state <- 'National'
 initial.ind <- initial <- 2000
 final.ind   <- final   <- 2010
 Data.ex     <- getData.function.g(Data2 = DT.Decomp.ex,state = NULL,initial = NULL,final = NULL)
 Data.ed     <- getData.function.g(Data2 = DT.Decomp.ed,state = NULL,initial = NULL,final = NULL)


 getData2.function <- function(Data = DT.Decomp.ex){
   colnames(Data) <- c('Name','Region','Sex','State','year','age1',1:14,16)
   Data.melt               <- melt(Data, id.vars = 1:6,variable.name = 'Cause',value.name = 'Contribution')
   Data.melt[Data.melt$age1 >= 85,]$Cause <- "16"

   DT.out              <-  Data.melt[, list(Contribution = sum(Contribution)), by = list(Name,Sex,State,year,Cause)]

   DT.out$Contribution <- round(DT.out$Contribution,2)
   DT.out
 }

 Data.ex2 <- getData2.function(Data = DT.Decomp.ex)
 Data.ed2 <- getData2.function(Data = DT.Decomp.ed)




 setwd("C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation/R/LVMx_15_App//")
 # run this line to update results and to update shinny app info
 save(Data.ex,Data.ed,Data.ex2,Data.ed2,DT.LTmx, file = 'Shinny_data.RData')
 # 

runApp()
