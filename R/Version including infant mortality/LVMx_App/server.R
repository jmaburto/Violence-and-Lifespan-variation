
#setwd( "C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA/R/Decomp_App")
library(ggplot2)
library(DT)
library(plotly)
library(RColorBrewer)
load('Shinny_data.RData')


# getData.function.g <- function(Data2,state ,initial,final){
#   Data <- cbind(Data2[,1:6], AMS = rowSums(Data2[,c(7,10,8,9,12)]), Diabetes = Data2[,c(11)], IHD = Data2[,c(13)],
#                 LungCancer = Data2[,c(16)], Cirrhosis = Data2[,c(17)],Homicide = Data2[,c(18)],TAcc = Data2[,c(19)],
#                 Rest = rowSums(Data2[,c(14,21,15,20)]))
#   
#   
#   colnames(Data) <- c('Name','Region','Sex','State','year','age1',1:8)
#   Data.melt               <- melt(Data, id.vars = 1:6,variable.name = 'Cause',value.name = 'Contribution')
#   levels(Data.melt$Cause) <- c('AMS','Diabetes','IHD','Lung Cancer','Cirrhosis',
#                                'Homicide','Traffic accidents','Rest')
#   Data.melt   <- Data.melt[Data.melt$year >= initial & Data.melt$year < final & Data.melt$Name == state, ]
#   
#   #Data.melt <- Data.melt[Data.melt$age1 < 100,]
#   Labels.age            <- c('0-4', '5-9', '10-14', '15-19', '20-24','25-29','30-34','35-39',
#                              '40-44','45-49','50-54','55-59','60-64','65-69',
#    #                          "70-74","75-79","80-84","85-89","90-94","95+")
#    "70-74","75-79","80-84","85-89","90-94","95-99","100-104","105+")
#   Data.melt$Age       <- (cut(Data.melt$age1+1, breaks=c(seq(0,105,5),Inf),labels=Labels.age))
#   Data.melt5 <- Data.melt[,list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,year,Cause,Age)]
#   
#   Data.fig   <- Data.melt5[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)] 
#   Data.fig$Contribution <- round(Data.fig$Contribution,2)
#   Data.fig
# }

getTable.function <- function(Data = Data.ex2,state = state.ind,initial = initial.ind,final = final.ind){
  
  Data.1 <- Data[Data$year >= initial & Data$year < final & Data$Name == state,]
  
  DT.out              <-  Data.1[, list(Contribution = sum(Contribution)), by = list(Name,Sex,Cause)]
  #DT.out$Contribution <- round(DT.out$Contribution,2)
  total               <- DT.out[,list(Contribution= sum(Contribution)),by = list(Name,Sex)]
  total$Cause         <- '17'
  total               <- total[,c('Name','Sex','Cause','Contribution')]
  DT.out              <- rbind(DT.out,total) 
  levels(DT.out$Cause) <- c('Infect & respiratory','Cancers','Circulatory','Birth conditions',
                            'Diabetes','Other AMS','IHD','HIV','Suicide','Lung Cancer','Cirrhosis',
                            'Homicide','Traffic accidents','Other HD', 'Rest', 'Total difference')
  DT.out
}


shinyServer(
  function(input, output) {
    var1 <- reactive(seq(as.integer(input$initial.ind)+1,2015,1))

    output$vx <- renderUI({
      selectInput('final.ind','Final year',choices = var1(),selected = 2010)
    })
    
    output$dx.plot <- renderPlotly({
      
      state.ind <- input$state.ind
      years     <- seq(2000,2015,by = 5)
      Data      <- DT.LTmx
      
      #Data[Data$dx == 0,]$dx <- NA
      
      p <- ggplot(Data[Data$state.name == state.ind & Data$year %in% years & Data$age<=105,], aes(x = age,y = dx,colour = as.character(year))) +
        ggtitle(paste0('Age at death distribution (dx), ', state.ind)) +
        geom_line(aes(group = as.character(year)), size= 1) +
        facet_wrap(~sex)+
        scale_colour_manual('Year', values = c('orange', 'green', 'red', 'blue')) + 
        theme_light()+
        theme(text = element_text(size=14),
              strip.text.x = element_text(size = 14, colour = "black"))
      
      print(ggplotly(p,width = 1350, height = 400))
      
    })
    
    output$DT.sum.males = renderDataTable({
      
      state.ind <- input$state.ind
      years     <- seq(1995,2015,by = 5)
      Data      <- DT.LTmx
      
      DT.info   <- Data[Data$state.name == state.ind & Data$year %in% years & Data$age==0,]
      Males     <- round(DT.info[DT.info$sex == 'Males',c('year','ex','e.dagger')],2)
      colnames(Males) <- c('Year', 'Life expectancy', 'Life disparity')
      rownames(Males) <- NULL
      datatable(Males, options = list(paging=FALSE,ordering=T, dom = 't'),rownames = F)
    })
    
    output$DT.sum.females = renderDataTable({
      
      state.ind <- input$state.ind
      years     <- seq(1995,2015,by = 5)
      Data      <- DT.LTmx
      
      DT.info   <- Data[Data$state.name == state.ind & Data$year %in% years & Data$age==0,]
      Females     <- round(DT.info[DT.info$sex == 'Females',c('year','ex','e.dagger')],2)
      colnames(Females) <- c('Year', 'Life expectancy', 'Life disparity')
      rownames(Females) <- NULL
      datatable(Females, options = list(paging=FALSE,ordering=T, dom = 't'),rownames = F)
    })
    
    output$e0.trends <- renderPlotly({
      
      state.ind <- input$state.ind
      
      Data       <- DT.LTmx
      Data$ex    <- round(Data$ex,2)
      Data$group <- 3
      Data[Data$state.name == 'National']$group <- 2
      Data[Data$state.name == 'National']$state <- 33
      Data[Data$state.name == state.ind]$group  <- 1
      Data[Data$state.name == state.ind]$state  <- 34
      if (state.ind == 'National') {
        Data$group    <- factor(Data$group, levels = rev(c(1,3)), labels = rev(c('National','Rest of states')))
        colors.trends <- c('red','lightgrey')
      } else {
        Data$group <- factor(Data$group, levels = rev(1:3), labels = rev(c(state.ind,'National','Rest of states')))
        colors.trends <- c('black', 'red', 'lightgrey')
      }
      
      
      p <-ggplot(Data[Data$age==0,], aes(x = year,y = ex,colour=(group))) +
        ggtitle('Life expectancy at birth') +
        geom_line(aes(group = state.name), size= 1) +
        facet_wrap(~sex)+
        theme_light()+
        labs(y = "Years")+
        scale_colour_manual('State', values = rev(colors.trends)) + 
        theme(text = element_text(size=14),
              strip.text.x = element_text(size = 14, colour = "black"))
      #p
      print(ggplotly(p,width = 1350, height = 400))
      
    })
    
    output$ed.trends <- renderPlotly({
      
      state.ind <- input$state.ind
      
      Data       <- DT.LTmx
      Data$e.dagger    <- round(Data$e.dagger,2)
      Data$group <- 3
      Data[Data$state.name == 'National']$group <- 2
      Data[Data$state.name == 'National']$state <- 33
      Data[Data$state.name == state.ind]$group  <- 1
      Data[Data$state.name == state.ind]$state  <- 34
      if (state.ind == 'National') {
        Data$group    <- factor(Data$group, levels = rev(c(1,3)), labels = rev(c('National','Rest of states')))
        colors.trends <- c('red','lightgrey')
      } else {
        Data$group <- factor(Data$group, levels = rev(1:3), labels = rev(c(state.ind,'National','Rest of states')))
        colors.trends <- c('black', 'red', 'lightgrey')
      }
      
      
      q <-ggplot(Data[Data$age==0,], aes(x = year,y = e.dagger,colour=(group))) +
        ggtitle('Life disparity') +
        geom_line(aes(group = state.name), size= 1) +
        facet_wrap(~sex)+
        theme_light()+
        labs(y = "Years")+
        scale_colour_manual('State', values = rev(colors.trends)) + 
        theme(text = element_text(size=14),
              strip.text.x = element_text(size = 14, colour = "black"))
      #p
      print(ggplotly(q,width = 1350, height = 400))
      
    })
    
    output$e0.decomp <- renderPlotly({
      
      state.ind   <- input$state.ind
      initial.ind <- input$initial.ind
      final.ind   <- input$final.ind
      Data        <- Data.ex
      
      Data.fig   <- Data[Data$year >= initial.ind & Data$year < final.ind & Data$Name == state.ind, ]
      Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)] 
      #Data.fig        <- Data.fig[Data.fig$Age!= '0-4',]
      
      Total.Age <- Data.fig[,sum(Contribution), by = list(Age,Sex)]
      Total.Age$V1 <- round(Total.Age$V1,2)

      base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'lightgrey')
      Data.fig$Contribution <- round(Data.fig$Contribution,2)
      p <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
        ggtitle('Decomposition of life expectancy (years)', subtitle = paste0(state.ind,', ', initial.ind,'-',final.ind))+
        facet_wrap(~Sex)+
        scale_fill_manual('Cause of death', values = base2) + 
        geom_bar(stat = "identity",position = "stack")+
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = " ", y = " ",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)
      #  coord_flip()
      rr <- ggplotly(p,width = 1200, height = 380)
      rr$x$layout$xaxis$title = "Age group"
      rr$x$layout$xaxis2$title = "Age group"
      
      print(rr)
      
    })
    
    output$ed.decomp <- renderPlotly({
      
      state.ind   <- input$state.ind
      initial.ind <- input$initial.ind
      final.ind   <- input$final.ind
      Data        <- Data.ed
      
      Data.fig   <- Data[Data$year >= initial.ind & Data$year < final.ind & Data$Name == state.ind, ]
      Data.fig   <- Data.fig[, list(Contribution = sum(Contribution)), by = list(Name,Region,Sex,State,Cause,Age)] 
      #Data.fig        <- Data.fig[Data.fig$Age!= '0-4',]
      
      base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'lightgrey')
      
      Data.fig$Contribution <- round(Data.fig$Contribution,2)
      q <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
        ggtitle('Decomposition of life disparity (years)', subtitle = paste0(state.ind,', ', initial.ind,'-',final.ind))+
        facet_wrap(~Sex)+
        scale_fill_manual('Cause of death', values = base2) + 
        geom_bar(stat = "identity",position = "stack")+
        theme_light()+
        theme(text = element_text(size=10),
              axis.text.x = element_text(angle=45, hjust=1))+
        labs(x = " ", y = " ",size=10)+
        theme(text = element_text(size=10),
              strip.text.x = element_text(size = 12, colour = "black"))+
        geom_hline(yintercept = 0)
      #  coord_flip()
      r <- ggplotly(q,width = 1200, height = 380)
      r$x$layout$xaxis$title = "Age group"
      r$x$layout$xaxis2$title = "Age group"
      
      
      print(r)
      
      
      
    })
    
    output$mytable = renderDataTable({
      
      state.ind   <- input$state.ind
      initial.ind <- input$initial.ind
      final.ind   <- input$final.ind
      
      DT.table.ex <- getTable.function(Data = Data.ex2,state = state.ind,initial = initial.ind,final = final.ind) 
      DT.table.ed <- getTable.function(Data = Data.ed2,state = state.ind,initial = initial.ind,final = final.ind) 
      
      Dmales.ex   <- DT.table.ex[DT.table.ex$Sex == 'Males', c('Cause','Contribution')]
      Dmales.ed   <- DT.table.ed[DT.table.ed$Sex == 'Males', c('Cause','Contribution')]
      DT.males    <- cbind(Dmales.ex,Dmales.ed$Contribution)
      colnames(DT.males) <- c('Cause','Life expectancy','Life disparity')
      DT.males[,2:3] <- round(DT.males[,2:3],2)
      # 
      # Data <- Decomp_results[Decomp_results$Period1==period & Decomp_results$Sex == sx & 
      #                          Decomp_results$Country.name==country & Decomp_results$Sources==source1 &
      #                          Decomp_results$Age < 80,]
      # Data <- data.table(Data)
      # Total.cause2 <- Data[,sum(Contribution), by = list(Cause,Sex)]
      # 
      # cause.name.vec2  <-c('a) (A00-B99) Certain infectious and parasitic diseases', 'b) (C00-D48) Neoplasms',
      #                      'f) (I00-I99) Diseases of the circulatory system','k) (R00-R99) Not elsewhere classified',
      #                      'd) (F01-F99) Mental and behavioural disorders','e) (G00-G98) Diseases of the nervous system',
      #                      'c) (E00-E88) Endocrine, nutritional and metabolic diseases' ,'g) (K00-K92) Diseases of the digestive system',  
      #                      'i) (N00-N98) Diseases of the genitourinary system','j) (P00-P96) Perinatal  & (Q00-Q99) Congenital malformations',
      #                      'h) (J00-J98) Diseases of the respiratory system','l) (V01-Y89) External mortality: accidents and suicide',
      #                      'm) (X85-Y09) Homicide', 'n) Rest of causes')
      # 
      # Total.cause2$Cause <- cause.name.vec2
      # Total.cause2$Contribution <- round(Total.cause2$V1,2)
      # Total.cause2<- data.frame(Total.cause2[,c('Cause','Contribution')])
      # Cause.Total <- data.frame(cbind(Cause='Total',Contribution=sum(Total.cause2$Contribution)))
      # 
      # Total.cause2 <- rbind(Total.cause2,Cause.Total)
      # Total.cause2 <- Total.cause2[with(Total.cause2,order(Cause)),]
      # Total.cause2 <- data.frame(Total.cause2)
      # rownames(Total.cause2) <- NULL
      # Total.cause2$Contribution <- as.numeric(Total.cause2$Contribution)
      datatable(DT.males, options = list(paging=FALSE,ordering=T,dom = 't'),rownames = F,caption = 'Males')
    })
    
    output$mytable2 = renderDataTable({
      
      state.ind   <- input$state.ind
      initial.ind <- input$initial.ind
      final.ind   <- input$final.ind
      
      DT.table.ex <- getTable.function(Data = Data.ex2,state = state.ind,initial = initial.ind,final = final.ind) 
      DT.table.ed <- getTable.function(Data = Data.ed2,state = state.ind,initial = initial.ind,final = final.ind) 
      
      Dfemales.ex   <- DT.table.ex[DT.table.ex$Sex == 'Females', c('Cause','Contribution')]
      Dfemales.ed   <- DT.table.ed[DT.table.ed$Sex == 'Females', c('Cause','Contribution')]
      DT.females    <- cbind(Dfemales.ex,Dfemales.ed$Contribution)
      colnames(DT.females) <- c('Cause','Life expectancy','Life disparity')
      DT.females[,2:3] <- round(DT.females[,2:3],2)
      datatable(DT.females, options = list(paging=FALSE,ordering=T,dom = 't'),rownames = F,caption = 'Females')
    })
    
    output$text4 <- renderText({
      state.ind   <- input$state.ind
      initial.ind <- input$initial.ind
      final.ind   <- input$final.ind
      
      t2 <- paste0('Cause-specific contributions in life expectancy and life disparity. ',
             paste0(state.ind,', ',initial.ind,'-',final.ind,'.'))
      t2
      
    })
      
    output$DT.sum.males2 = renderDataTable({
      
      state.ind <- input$state.ind
      years     <- c(input$initial.ind,input$final.ind)
      Data      <- DT.LTmx
      
      DT.info   <- Data[Data$state.name == state.ind & Data$year %in% years & Data$age==0,]
      Males     <- DT.info[DT.info$sex == 'Males',c('year','ex','e.dagger')]
      Dif       <- Males[2,2:3]-Males[1,2:3]
      Dif       <- cbind(year='Difference',Dif)
      DT.males  <- rbind(Males,Dif)
      colnames(DT.males) <- c('Year', 'Life Exp', 'L. Disparity')
      rownames(DT.males) <- NULL
      DT.males[,2:3] <- round(DT.males[,2:3],2)
      datatable(DT.males, options = list(paging=FALSE,ordering=T, dom = 't'),rownames = F,caption = 'Males')
    })
    
    output$DT.sum.females2 = renderDataTable({
      
      state.ind <- input$state.ind
      years     <- c(input$initial.ind,input$final.ind)
      Data      <- DT.LTmx
      
      DT.info   <- Data[Data$state.name == state.ind & Data$year %in% years & Data$age==0,]
      Females     <- DT.info[DT.info$sex == 'Females',c('year','ex','e.dagger')]
      Dif       <- Females[2,2:3]-Females[1,2:3]
      Dif       <- cbind(year='Difference',Dif)
      DT.females  <- rbind(Females,Dif)
      colnames(DT.females) <- c('Year', 'Life Exp', 'L. Disparity')
      rownames(DT.females) <- NULL
      DT.females[,2:3] <- round(DT.females[,2:3],2)
      datatable(DT.females, options = list(paging=FALSE,ordering=T, dom = 't'),rownames = F,caption = 'Females')
    })
    
    
})
