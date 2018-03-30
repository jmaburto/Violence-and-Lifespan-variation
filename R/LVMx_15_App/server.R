
#setwd( "C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA/R/Decomp_App")
library(ggplot2)
library(DT)
library(plotly)
library(RColorBrewer)
load('Shinny_data.RData')


getTable.function <- function(Data = Data.ex2,state = state.ind,initial = initial.ind,final = final.ind){
  
  Data.1 <- Data[Data$year >= initial & Data$year < final & Data$Name == state,]
  
  DT.out              <-  Data.1[, list(Contribution = sum(Contribution)), by = list(Name,Sex,Cause)]
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

      
      p <- ggplot(Data[Data$state.name == state.ind & Data$year %in% years & Data$age<=105 & Data$age>=15,], aes(x = age,y = dx,colour = as.character(year))) +
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
      
      DT.info   <- Data[Data$state.name == state.ind & Data$year %in% years & Data$age==15,]
      Males     <- round(DT.info[DT.info$sex == 'Males',c('year','ex','e.dagger')],2)
      colnames(Males) <- c('Year', 'Life expectancy', 'Life disparity')
      rownames(Males) <- NULL
      datatable(Males, options = list(paging=FALSE,ordering=T, dom = 't'),rownames = F)
    })
    
    output$DT.sum.females = renderDataTable({
      
      state.ind <- input$state.ind
      years     <- seq(1995,2015,by = 5)
      Data      <- DT.LTmx
      
      DT.info   <- Data[Data$state.name == state.ind & Data$year %in% years & Data$age==15,]
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
      
      
      p <-ggplot(Data[Data$age==15,], aes(x = year,y = ex,colour=(group))) +
        ggtitle('Life expectancy at age 15') +
        geom_line(aes(group = state.name), size= 1) +
        facet_wrap(~sex)+
        theme_light()+
        labs(y = "Years")+
        scale_colour_manual('State', values = rev(colors.trends)) + 
        theme(text = element_text(size=14),
              strip.text.x = element_text(size = 14, colour = "black"))
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
      
      
      q <-ggplot(Data[Data$age==15,], aes(x = year,y = e.dagger,colour=(group))) +
        ggtitle('Life disparity at age 15') +
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
      age.labels <-unique(Data.fig$Age)
      Data.fig        <- Data.fig[Data.fig$Age %in% age.labels[4:20],]
      
      
      Total.Age <- Data.fig[,sum(Contribution), by = list(Age,Sex)]
      Total.Age$V1 <- round(Total.Age$V1,2)

      base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'lightgrey')
      Data.fig$Contribution <- round(Data.fig$Contribution,2)
      p <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
        ggtitle('Decomposition of life expectancy (years)')+
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
      
      age.labels <-unique(Data.fig$Age)
      Data.fig        <- Data.fig[Data.fig$Age %in% age.labels[4:20],]
      
      base2 <- c(rev(brewer.pal(8,name = 'Spectral'))[1:5],rev(brewer.pal(8,name = 'Spectral'))[8],rev(brewer.pal(8,name = 'Spectral'))[7],'lightgrey')
      
      Data.fig$Contribution <- round(Data.fig$Contribution,2)
      q <- ggplot(Data.fig, aes(x = Age, y = Contribution, fill = Cause)) +
        ggtitle('Decomposition of life disparity (years)')+
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
      
      DT.info   <- Data[Data$state.name == state.ind & Data$year %in% years & Data$age==15,]
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
      
      DT.info   <- Data[Data$state.name == state.ind & Data$year %in% years & Data$age==15,]
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
