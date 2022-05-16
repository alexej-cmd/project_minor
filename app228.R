library(shiny)

library(dplyr)
library(tidymodels)
library(rpart.plot)
library(tidyverse)
library(sf)
library(leaflet)
aaa <- read_csv("data_cities.csv")

ui <- fluidPage(
  titlePanel("Безопасность городов США"),
  sidebarLayout(
    sidebarPanel(
      #две строчки ввода!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      textInput("city", label = h3("Введите название города"), value = ""),
      textInput("state", label = h3("Введите название штата"), value = ""),
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
      
      
    ),
    mainPanel(
      textOutput("gorod")
    )
  )
)

server <- function(input, output) {
  #нужная нам функция!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  f=function(city, state){
    #слепляет через пробел город и штат!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    location = paste(city, state, sep=' ')
    z = filter(aaa, aaa$city_state==location)
    if (nrow(z)==0){
      population_user=10000
      state_u=state
      as=filter(aaa, state==state_u)
      avgcnt = as$avg_cnt[1]
      otnincome = 1
      otnpop= population_user/as$avg_city[1]
      res=-77.79*otnincome+5.54*otnpop+1.26*avgcnt+94.15
      b=0
      
      if (population_user<5000){
        cp_user=0
      }else if (population_user<15000){
        cp_user=1
      }else if (population_user<50000){
        cp_user=2
      }else if (population_user<300000){
        cp_user=3
      }else{cp_user=4}
      
      
      ass=filter(aaa, cat_pop==cp_user)
      for (i in 1:nrow(ass)){
        if (ass[i,13]>=res){
          b=b+1
        }
      }
      
      
      
      otvet1 = paste('К сожалению, у нас нет данных про этот город.')
      rez=round(b/nrow(ass)*100,1)
      if (rez>50){
        otvet2=(paste('Мы можем только предположить, что город ',   city,   ', штат ',   state,   ', безопаснее ', rez, ' % ', 'городов США схожих размеров',  sep=''))
        otvet3=''
      } else{
        otvet2=(paste('Но мы можем предположить, что ',  (100-rez),  ' % ',  'городов США схожих размеров безопасней города ', city, ', штат ', state, '.', sep=''))
        otvet3 = ('Будьте осторожнее!')
        
      }
      
    }
    else{
      b=-1
      
      if (z[1,6]<5000){
        cp_user=0}
      else if (z[1,6]<15000){
        cp_user=1}
      else if (z[1,6]<50000){
        cp_user=2}
      else if (z[1,6]<300000){
        cp_user=3}
      else{cp_user=4}
      
      
      ass=filter(aaa, cat_pop==1)
      for (i in 1:nrow(ass)){
        if (ass[i,13]>=z[1,13]){
          b=b+1}
      }
      rez=round(b/nrow(ass)*100,1)
      otvet1=''
      if (rez>50){
        otvet2=(paste('По нашим данным, город ', city, ', штат ', state, ' ', 'безопаснее ', rez , ' % ', 'городов США схожих размеров.', sep=''))
        otvet3=''
      } else{
        otvet2 = (paste('По нашим данным, ',  (100-rez),  ' % ',  'городов США схожих размеров безопасней города ', city, ', штат ', state, '.', sep=''))
        otvet3=('Будьте осторожнее!')
      }
      
    }
    otvet = paste(otvet1, otvet2, otvet3)
    otvet
    }
  #функция закончилась!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  #вывод!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  output$gorod <- renderText(
    {f(input$city,input$state)} 
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

