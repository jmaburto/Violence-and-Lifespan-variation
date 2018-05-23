

library(shiny)
library(data.table)
library(DT)
library(plotly)

#https://jmaburto.shinyapps.io/LVMx_App/

  states <- c("National","Aguascalientes","Baja California","Baja California Sur","Campeche",
               "Coahuila","Colima","Chiapas","Chihuahua","Mexico City","Durango",
               "Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico State","Michoacan",
               "Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro",
               "Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas",
               "Tlaxcala","Veracruz","Yucatan","Zacatecas")
  Initial.year <- 1995:2014
  
  shinyUI(
    fluidPage(
      titlePanel('Lifespan variation in Mexico, 1995-2015'),
      navbarPage(
        # 'Aburto JM & Beltran-Sanchez H. "The impact of violence on lifespan variation in Mexico and 
        # its states, 1995-2015". Biodemography Unit, Institute of Public Health, University of Southern Denmark.',
        'App for the paper Homicides in Mexico increased inequality of lifespans and slowed down life expectancy gains in 2005-2015',
        position = c("fixed-bottom")
        ),
      
      sidebarLayout(
        sidebarPanel(
          selectInput( 'state.ind','State',states, selected = 'National'),
          br(),
          selectInput( 'initial.ind','Initial year',Initial.year, selected = 2000),
          br(),
          uiOutput('vx'),
          br(),
          
            dataTableOutput('DT.sum.males2'),
            dataTableOutput('DT.sum.females2'),
          width = 2
        ),

        
        mainPanel(
        tabsetPanel(tabPanel("Age at death distribution",
                             fluidRow( plotlyOutput("dx.plot"),
                                      column(6,h3('Males'),dataTableOutput("DT.sum.males")),
                                      column(6,h3('Females'),dataTableOutput("DT.sum.females")))
        ),
        tabPanel("Life expectancy and life disparity trends",
                 plotlyOutput('e0.trends'),
                 plotlyOutput('ed.trends')),
        tabPanel("Decomposition results",
                 plotlyOutput("e0.decomp"),
                 plotlyOutput("ed.decomp")),
        tabPanel("Cause-specific summary",
                 fluidRow(h3(textOutput('text4')),
                          column(6, dataTableOutput("mytable")),
                          column(6, dataTableOutput("mytable2"))),
                 p("Note: Values represent only the results for ages below age 85. 
                    Information for ages 85 and above is included in the category 'Rest' since it is likely to 
                    be unaccurate due to comorbidities and coding practices"))
        )
        )
        )
      )
    )
  
  
#  devtools::install_github('hadley/ggplot2')
  