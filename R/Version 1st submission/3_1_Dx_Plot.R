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


# run this line to update results and to update shinny app info
source('R/2_3_Reshape_Results.R')
library(ggthemes)
library(ggplot2)



state.ind <- 'National'
years     <- seq(1995,2015,by = 5)
Data      <- DT.LTmx
age.ind   <- 0
#Data[Data$dx == 0,]$dx <- NA

ggplot(Data[Data$state.name == state.ind & Data$year %in% years & Data$age>=age.ind,], aes(x = age,y = dx,colour = as.character(year))) +
  ggtitle(paste0('Age at death distribution (dx), ', state.ind)) +
  geom_line(aes(group = as.character(year)), size= 1) +
  facet_wrap(~sex)+
  scale_colour_manual('Year', values = c('black', 'orange', 'green', 'red', 'blue')) + 
  theme_light()+
  theme(text = element_text(size=14),
        strip.text.x = element_text(size = 14, colour = "black"))
  




# require(gridExtra)
# pdf(file="R/Figures/National_decomp.pdf",width=12,height=12,pointsize=4,useDingbats = F)
# grid.arrange(National.e0,National.edagger, nrow=2)
# dev.off()