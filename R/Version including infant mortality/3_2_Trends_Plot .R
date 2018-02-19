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

load('R/LVMx_App/Shinny_data.RData')
#source('R/2_3_Reshape_Results.R')
library(ggthemes)
library(ggplot2)
#library(plotly)

state.ind  <- 'Zacatecas'
Data       <- DT.LTmx
Data$group <- 3
Data[Data$state.name == 'National',]$group <- 2
Data[Data$state.name == 'National',]$state <- 33
Data[Data$state.name == state.ind,]$group  <- 1
Data[Data$state.name == state.ind,]$state  <- 34
if (state.ind == 'National') {
  Data$group    <- factor(Data$group, levels = c(1,3), labels = c('National','Rest of states'))
  colors.trends <- c('black','lightgrey')
} else {
  Data$group <- factor(Data$group, levels = 1:3, labels = c(state.ind,'National','Rest of states'))
  colors.trends <- c('black', 'red', 'lightgrey')
  }


p <-ggplot(Data[Data$age==0,], aes(x = year,y = ex,colour=group)) +
  ggtitle('Life expectancy at birth') +
  geom_line(aes(group = state), size= 1.2) +
  facet_wrap(~sex)+
  theme_light()+
  labs(y = "Years")+
  scale_colour_manual('State', values = colors.trends) + 
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))
p
#ggplotly(p)



q <-ggplot(Data[Data$age==0,], aes(x = year,y = e.dagger,colour=(group))) +
  ggtitle('Life expectancy at birth') +
  geom_line(aes(group = state), size= 1.2) +
  facet_wrap(~sex)+
  theme_light()+
  labs(y = "Years")+
  scale_colour_manual('State', values = colors.trends) + 
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))
q


#require(gridExtra)
#pdf(file="R/Figures/National_decomp.pdf",width=12,height=12,pointsize=4,useDingbats = F)
#grid.arrange(National.e0,National.edagger, nrow=2)
#dev.off()