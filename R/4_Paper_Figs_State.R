# Results at the national level
rm(list=ls(all=TRUE))
setwd("C:/Users/jmaburto/Documents/GitHub/Violence-and-Lifespan-variation")

#### sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 
#### 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer,
#### 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories
# devtools::install_github('hadley/ggplot2')

#### Grouping
#### AMS: 7+10+8+9+12, Diabetes, IHD, LUNG CANCER, CIRRHOSIS, HOMICIDES, TRAFFIC ACCIDENTS,
#### REST: 14+21+15+20
library(ggthemes)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(data.table)
library(reshape2)


#load('R/LVMx_App/Shinny_data.RData')
source('R/2_3_Reshape_Results.R')
source('R/Functions.R')

# Get some nice colors
#display.brewer.all()
base2 <- c(1:15)
base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'lightgrey')


# Changes in life expectancy by period for males ------------------------------------
final.y <- 2015
Sex     <- 'Males'
Changes.data   <- DT.LTmx[DT.LTmx$year %in% c(1995,2005,final.y) & DT.LTmx$age == 0 & DT.LTmx$state.name!= 'National',]
Changes.data   <- Changes.data[order(state, sex, year),]
Dif.data.state <- Changes.data[, list(Difference=diff(ex)), by = list(state.name,region,sex,state)]
Dif.data.state$Difference.ed <- Changes.data[, list(Difference.ed=diff(e.dagger)), by = list(state.name,region,sex,state)]$Difference.ed
Dif.data.state <- Dif.data.state[,Period := c('1995-2005',paste0(2005,'-',final.y)), by = list(state.name,region,sex,state)]
Dif.data.state <- Dif.data.state[,Ref.order := rep(Difference[2],2), by = list(state.name,region,sex,state)]
Dif.data.state <- Dif.data.state[Dif.data.state$sex == Sex,]
Dif.data.state$region <- factor(Dif.data.state$region,levels = rev(levels(Dif.data.state$region)))

head(Dif.data.state)


Dif.data.state$state.name <- reorder(Dif.data.state$state.name,Dif.data.state$Ref.order)

changes.ex.males <- ggplot(Dif.data.state, aes(Difference, state.name)) +
  ggtitle(bquote('A Changes in state male life expectancy '~(e[0])), subtitle = 'by period')+
  geom_vline(xintercept = 0)+
  geom_point(data = Dif.data.state, aes(Difference, state.name,col=Period, shape=Period),size = 3) +
  facet_grid(region ~., scales = "free", space = "free") +
  theme_light()+
  scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')

changes.ex.males


changes.ed.males <- ggplot(Dif.data.state, aes(Difference.ed, state.name)) +
  ggtitle(bquote('B Changes in state male lifespan variation '~(e^"\u2020")), subtitle = 'by period')+
  geom_vline(xintercept = 0)+
  geom_point(data = Dif.data.state, aes(Difference.ed, state.name,col=Period, shape=Period),size = 3) +
  facet_grid(region ~., scales = "free", space = "free") +
  xlab("Difference") +
  theme_light()+
  scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position ='bottom')

changes.ed.males



require(gridExtra)
pdf(file="R/Figures/changes_males.pdf",width=13,height=7,useDingbats = F)
grid.arrange(changes.ex.males,changes.ed.males,ncol=2)
dev.off()








# Changes in life expectancy by period for females------------------------------------
final.y <- 2015
Sex     <- 'Females'
Changes.data   <- DT.LTmx[DT.LTmx$year %in% c(1995,2005,final.y) & DT.LTmx$age == 0 & DT.LTmx$state.name!= 'National',]
Changes.data   <- Changes.data[order(state, sex, year),]
Dif.data.state <- Changes.data[, list(Difference=diff(ex)), by = list(state.name,region,sex,state)]
Dif.data.state$Difference.ed <- Changes.data[, list(Difference.ed=diff(e.dagger)), by = list(state.name,region,sex,state)]$Difference.ed
Dif.data.state <- Dif.data.state[,Period := c('1995-2005',paste0(2005,'-',final.y)), by = list(state.name,region,sex,state)]
Dif.data.state <- Dif.data.state[,Ref.order := rep(Difference[2],2), by = list(state.name,region,sex,state)]
Dif.data.state <- Dif.data.state[Dif.data.state$sex == Sex,]
Dif.data.state$region <- factor(Dif.data.state$region,levels = rev(levels(Dif.data.state$region)))

head(Dif.data.state)


Dif.data.state$state.name <- reorder(Dif.data.state$state.name,Dif.data.state$Ref.order)

changes.ex.females <- ggplot(Dif.data.state, aes(Difference, state.name)) +
  ggtitle(bquote('A Changes in state male life expectancy '~(e[0])), subtitle = 'by period')+
  geom_vline(xintercept = 0)+
  geom_point(data = Dif.data.state, aes(Difference, state.name,col=Period, shape=Period),size = 3) +
  facet_grid(region ~., scales = "free", space = "free") +
  theme_light()+
  scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')

changes.ex.females


changes.ed.females <- ggplot(Dif.data.state, aes(Difference.ed, state.name)) +
  ggtitle(bquote('B Changes in state male lifespan variation '~(e^"\u2020")), subtitle = 'by period')+
  geom_vline(xintercept = 0)+
  geom_point(data = Dif.data.state, aes(Difference.ed, state.name,col=Period, shape=Period),size = 3) +
  facet_grid(region ~., scales = "free", space = "free") +
  xlab("Difference") +
  theme_light()+
  scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position ='bottom')

changes.ed.females



require(gridExtra)
pdf(file="R/Figures/changes_females.pdf",width=13,height=7,useDingbats = F)
grid.arrange(changes.ex.females,changes.ed.females,ncol=2)
dev.off()





# Figures for cause of death contributions ------------------------------------


#get data 
final   <- 2015


ex.COD_state.1995.2005 <- get.data.function2(Data2 = DT.Decomp.ex,initial =1995,final = 2005)
ex.COD_state.1995.2005$Period <- '1995-2005'
ex.COD_state.2005.2015 <- get.data.function2(Data2 = DT.Decomp.ex,initial =2005,final = final)
ex.COD_state.2005.2015$Period <-  paste0('2005-',final)

COD.state.ex <- rbind(ex.COD_state.1995.2005,ex.COD_state.2005.2015)
COD.state.ex <- COD.state.ex[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Period)]
COD.state.ex <- COD.state.ex[COD.state.ex$Name != 'National',]


##### do figures for males
sex          <- 'Males'
COD.state.ex <- COD.state.ex[COD.state.ex$Sex == sex,]
ref.order    <- COD.state.ex[COD.state.ex$Period == paste0('2005-',final),]
ref.order    <- ref.order[,list(ref.order = rep(Contribution[6],8)), by = list(Name,Region,Sex,State)]
ref.order    <- ref.order[order(Name),]
COD.state.ex <- COD.state.ex[order(Period,Name),]
COD.state.ex$Ref.order <- c(ref.order$ref.order,ref.order$ref.order)
COD.state.ex$Region <- factor(COD.state.ex$Region,levels = rev(levels(COD.state.ex$Region)))

COD.state.ex$Name <- reorder(COD.state.ex$Name,COD.state.ex$Ref.order)


unique(COD.state.ex$Cause)
COD.ex.fig <- COD.state.ex[COD.state.ex$Cause %in% unique(COD.state.ex$Cause)[c(1,2,6,7)], ]

changes.COD.males.ex <- ggplot(COD.ex.fig, aes(Contribution, Name)) +
  ggtitle(bquote('Cause-specific contributions to the change in '~(e[0])), subtitle =bquote('Negative values decrease '~(e[0])~' and positive values increase '~(e[0])) )+
  geom_vline(xintercept = 0)+
  xlim(c(-2,2))+
  geom_point(data = COD.ex.fig, aes(Contribution, Name,col=Period, shape=Period),size = 3) +
  facet_grid(Region ~ Cause, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')

changes.COD.males.ex

pdf(file="R/Figures/COD_ex_males.pdf",width=13,height=7.5,useDingbats = F)
print(changes.COD.males.ex)
dev.off()

### for the appendix


changes.COD.males.ex2 <- ggplot(COD.state.ex, aes(Contribution, Name)) +
  ggtitle(bquote('Cause-specific contributions to the change in '~(e[0])), subtitle =bquote('Negative values decrease '~(e[0])~' and positive values increase '~(e[0])) )+
  geom_vline(xintercept = 0)+
  xlim(c(-2,2))+
  geom_point(data = COD.state.ex, aes(Contribution, Name,col=Period, shape=Period),size = 3) +
  facet_grid(Region ~ Cause, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')

changes.COD.males.ex2

pdf(file="R/Figures/COD_ex_males_appendix.pdf",width=18,height=9,useDingbats = F)
print(changes.COD.males.ex2)
dev.off()


#now for females



#get data 
final   <- 2015


ex.COD_state.1995.2005 <- get.data.function2(Data2 = DT.Decomp.ex,initial =1995,final = 2005)
ex.COD_state.1995.2005$Period <- '1995-2005'
ex.COD_state.2005.2015 <- get.data.function2(Data2 = DT.Decomp.ex,initial =2005,final = final)
ex.COD_state.2005.2015$Period <-  paste0('2005-',final)

COD.state.ex <- rbind(ex.COD_state.1995.2005,ex.COD_state.2005.2015)
COD.state.ex <- COD.state.ex[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Period)]
COD.state.ex <- COD.state.ex[COD.state.ex$Name != 'National',]


##### do figures for males
sex          <- 'Females'
COD.state.ex <- COD.state.ex[COD.state.ex$Sex == sex,]
COD.state.ex <- COD.state.ex[order(Period,Name),]
COD.state.ex$Ref.order <- c(ref.order$ref.order,ref.order$ref.order)
COD.state.ex$Region <- factor(COD.state.ex$Region,levels = rev(levels(COD.state.ex$Region)))

COD.state.ex$Name <- reorder(COD.state.ex$Name,COD.state.ex$Ref.order)

### for the appendix


changes.COD.females.ex2 <- ggplot(COD.state.ex, aes(Contribution, Name)) +
  ggtitle(bquote('Cause-specific contributions to the change in '~(e[0])), subtitle =bquote('Negative values decrease '~(e[0])~' and positive values increase '~(e[0])) )+
  geom_vline(xintercept = 0)+
  xlim(c(-2,2))+
  geom_point(data = COD.state.ex, aes(Contribution, Name,col=Period, shape=Period),size = 3) +
  facet_grid(Region ~ Cause, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')

changes.COD.females.ex2

pdf(file="R/Figures/COD_ex_females_appendix.pdf",width=18,height=9,useDingbats = F)
print(changes.COD.females.ex2)
dev.off()




# Now for lifespan variation ----------------------------------------------


#get data 
final   <- 2015


ed.COD_state.1995.2005 <- get.data.function2(Data2 = DT.Decomp.ed,initial =1995,final = 2005)
ed.COD_state.1995.2005$Period <- '1995-2005'
ed.COD_state.2005.2015 <- get.data.function2(Data2 = DT.Decomp.ed,initial =2005,final = final)
ed.COD_state.2005.2015$Period <-  paste0('2005-',final)

COD.state.ed <- rbind(ed.COD_state.1995.2005,ed.COD_state.2005.2015)
COD.state.ed <- COD.state.ed[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Period)]
COD.state.ed <- COD.state.ed[COD.state.ed$Name != 'National',]


##### do figures for males
sex          <- 'Males'
COD.state.ed <- COD.state.ed[COD.state.ed$Sex == sex,]
COD.state.ed <- COD.state.ed[order(Period,Name),]
COD.state.ed$Ref.order <- c(ref.order$ref.order,ref.order$ref.order)
COD.state.ed$Region <- factor(COD.state.ed$Region,levels = rev(levels(COD.state.ed$Region)))

COD.state.ed$Name <- reorder(COD.state.ed$Name,COD.state.ed$Ref.order)

######
unique(COD.state.ed$Cause)
COD.ex.fig <- COD.state.ed[COD.state.ed$Cause %in% unique(COD.state.ed$Cause)[c(1,2,6,7)], ]

changes.COD.males.ed <- ggplot(COD.ex.fig, aes(Contribution, Name)) +
  ggtitle(bquote('Cause-specific contributions to the change in '~(e^"\u2020")), subtitle = bquote('Negative values decrease '~(e^"\u2020")~' and positive values increase '~(e^"\u2020")) )+
  geom_vline(xintercept = 0)+
  xlim(c(-1.25,1))+
  geom_point(data = COD.ex.fig, aes(Contribution, Name,col=Period, shape=Period),size = 3) +
  facet_grid(Region ~ Cause, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')

changes.COD.males.ed

pdf(file="R/Figures/COD_ed_males.pdf",width=13,height=7.5,useDingbats = F)
print(changes.COD.males.ed)
dev.off()

### for the appendix


changes.COD.males.ed2 <- ggplot(COD.state.ed, aes(Contribution, Name)) +
  ggtitle(bquote('Cause-specific contributions to the change in '~(e^"\u2020")), subtitle = bquote('Negative values decrease '~(e^"\u2020")~' and positive values increase '~(e^"\u2020")) )+
  geom_vline(xintercept = 0)+
  xlim(c(-1.25,1))+
  geom_point(data = COD.state.ed, aes(Contribution, Name,col=Period, shape=Period),size = 3) +
  facet_grid(Region ~ Cause, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')

changes.COD.males.ed2

pdf(file="R/Figures/COD_ed_males_appendix.pdf",width=18,height=9,useDingbats = F)
print(changes.COD.males.ed2)
dev.off()


#now for females



#get data 
final   <- 2015


ed.COD_state.1995.2005 <- get.data.function2(Data2 = DT.Decomp.ed,initial =1995,final = 2005)
ed.COD_state.1995.2005$Period <- '1995-2005'
ed.COD_state.2005.2015 <- get.data.function2(Data2 = DT.Decomp.ed,initial =2005,final = final)
ed.COD_state.2005.2015$Period <-  paste0('2005-',final)

COD.state.ed <- rbind(ed.COD_state.1995.2005,ed.COD_state.2005.2015)
COD.state.ed <- COD.state.ed[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Period)]
COD.state.ed <- COD.state.ed[COD.state.ed$Name != 'National',]


##### do figures for females
sex          <- 'Females'
COD.state.ed <- COD.state.ed[COD.state.ed$Sex == sex,]
COD.state.ed <- COD.state.ed[order(Period,Name),]
COD.state.ed$Ref.order <- c(ref.order$ref.order,ref.order$ref.order)
COD.state.ed$Region <- factor(COD.state.ed$Region,levels = rev(levels(COD.state.ed$Region)))

COD.state.ed$Name <- reorder(COD.state.ed$Name,COD.state.ed$Ref.order)

### for the appendix
COD.ex.fig <- COD.state.ed[COD.state.ed$Cause %in% unique(COD.state.ed$Cause)[c(1,2,3,5,6)], ]

changes.COD.females.ed <- ggplot(COD.ex.fig, aes(Contribution, Name)) +
  ggtitle(bquote('Cause-specific contributions to the change in '~(e^"\u2020")), subtitle = bquote('Negative values decrease '~(e^"\u2020")~' and positive values increase '~(e^"\u2020")) )+
  geom_vline(xintercept = 0)+
  xlim(c(-1.25,1))+
  geom_point(data = COD.ex.fig, aes(Contribution, Name,col=Period, shape=Period),size = 3) +
  facet_grid(Region ~ Cause, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')

changes.COD.females.ed

pdf(file="R/Figures/COD_ed_males.pdf",width=13,height=7.5,useDingbats = F)
print(changes.COD.males.ed)
dev.off()

### for the appendix


changes.COD.females.ed2 <- ggplot(COD.state.ed, aes(Contribution, Name)) +
  ggtitle(bquote('Cause-specific contributions to the change in '~(e^"\u2020")), subtitle = bquote('Negative values decrease '~(e^"\u2020")~' and positive values increase '~(e^"\u2020")) )+
  geom_vline(xintercept = 0)+
  xlim(c(-1.25,1))+
  geom_point(data = COD.state.ed, aes(Contribution, Name,col=Period, shape=Period),size = 3) +
  facet_grid(Region ~ Cause, scales = "free", space = "free") +
  theme_light()+
  scale_color_manual(values=base2[c(1,6)])+
  theme(axis.title.y=element_blank())+
  theme(axis.title.x = element_text(size = 12, angle = 00))+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))+
  theme(strip.text.y = element_text(colour = "black"))+
  theme(legend.position = 'bottom')

changes.COD.females.ed2

pdf(file="R/Figures/COD_ed_females_appendix.pdf",width=18,height=9,useDingbats = F)
print(changes.COD.females.ed2)
dev.off()


