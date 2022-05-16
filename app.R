


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dplyr)
library(leaflet)
library(shiny)

library(dplyr)
library(tidymodels)
library(rpart.plot)
library(tidyverse)
library(sf)
library(leaflet)
aaa <- read_csv("data_cities.csv")





library(leaflet)
df1=read.csv('data1.csv')
df2=read.csv('data2.csv')
df3=read.csv('data3.csv')
df4=read.csv('data4.csv')
df5=read.csv('data5.csv')
df6=read.csv('data6.csv')
df7=read.csv('data7.csv')
datax=df3 <- rbind(df1, df2, df3,df4,df5,df6,df7)
data <- datax[,-1] 
usa = data %>% select(state,latitude,longitude,n_killed, n_injured)
usa$city=data$city_or_county
usa$state=as.factor(usa$state)
usa$city=as.factor(usa$city)

usa2=usa %>% filter(n_injured>0 & n_killed>0)






data_states = read.csv("data_states (1).csv")
data_render = data_states %>% select(-"X", -"mean_n_killed", -"mean_n_killed_dif", -"max_n_killed_dif", -"mean_n_injured",-"mean_n_injured_dif" ,-"max_n_injured_dif", -"population", -"mean_female_percentage", -"mean_child_percentage")

choices = c("Alabama" = "Alabama", "Alaska" = "Alaska", "Arizona" = "Arizona", "Arkansas" = "Arkansas",
            "California" = "California", "Colorado" = "Colorado", "Connecticut" = "Connecticut", "Delaware" = "Delaware",
            "Florida" = "Florida", "Georgia" = "Georgia", "Hawaii" = "Hawaii", "Idaho" = "Idaho", "Illinois" = "Illinois",
            "Indiana" = "Indiana", "Iowa" = "Iowa", "Kansas" = "Kansas", "Kentucky" ="Kentucky", "Louisiana" = "Louisiana",
            "Maine" = "Maine", "Maryland" = "Maryland", "Massachusetts" = "Massachusetts", "Michigan" = "Michigan",
            "Minnesota" = "Minnesota", "Mississippi" = "Mississippi", "Missouri" = "Missouri", "Montana" = "Montana",
            "Nebraska" = "Nebraska", "Nevada" = "Nevada", "New Hampshire" = "New Hampshire", "New Jersey" = "New Jersey",
            "New Mexico" = "New Mexico", "New York" = "New York", "North Carolina" = "North Carolina",
            "North Dakota" = "North Dakota", "Ohio" = "Ohio", "Oklahoma" = "Oklahoma", "Oregon" = "Oregon",
            "Pennsylvania" = "Pennsylvania", "Rhode Island" = "Rhode Island", "South Carolina" = "South Carolina",
            "South Dakota" = "South Dakota", "Tennessee" = "Tennessee", "Texas" = "Texas" , "Utah" = "Utah",
            "Vermont" = "Vermont", "Virginia" = "Virginia", "Washington" = "Washington", "West Virginia" = "West Virginia",
            "Wisconsin" = "Wisconsin", "Wyoming" = "Wyoming")


header = dashboardHeader(title = "Gun violence in the US 2013-2018", titleWidth=500)

sidebar = dashboardSidebar()


body = dashboardBody(tabsetPanel(
                     tabPanel("Безопасность городов США" , 
                              textInput("city1", label = h3("Введите название города"), value = ""),
                              textInput("state1", label = h3("Введите название штата"), value = ""),
                              hr(),
                              fluidRow(column(3, verbatimTextOutput("value"))),
                              textOutput("gorod")
                              ),
                    
                     tabPanel("Map",  leaflet(data=usa2) %>% setView(lng = -77.21822, lat = 38.537066, zoom = 4) %>%
                                addProviderTiles(providers$Stamen.Toner) %>% 
                                addCircles(~longitude, ~latitude, popup=paste("City: ", usa2$city, "<br>",
                                                                              "State: ",  usa2$state),
                                           label=paste("Number killed: ", usa2$n_killed, "<br>",
                                                       "Number injured: ",  usa2$n_injured),color="red")),
                     tabPanel("Bar chart",
                              selectInput("state", h3("Select state"), choices),
                              fluidRow(
                                valueBoxOutput("killedper100k"),
                                valueBoxOutput("numberper100k"),
                                valueBoxOutput("death_perc"),
                                valueBoxOutput("HFR"),
                                valueBoxOutput("female"),
                                valueBoxOutput("child"),
                                DTOutput('tbl')
                              ))))



ui = dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {

output$killedper100k <- renderValueBox({
  valueBox(round(data_states[data_states$state == input$state,]$total_killed_per100000), 
           "Total number killed per 100 000", icon = icon("fire"), color = "red")
})

output$death_perc <- renderValueBox({
  valueBox(round(data_states[data_states$state == input$state,]$death_perc), 
           "Death percentage in incident", icon = icon("fire"), color = "red")
})

output$HFR <- renderValueBox({
  valueBox(round(data_states[data_states$state == input$state,]$HFR * 100), 
           "Gun ownership percentage in 2016", icon = icon("fire"), color = "red")
})

output$female <- renderValueBox({
  valueBox(round(data_states[data_states$state == input$state,]$mean_female_percentage), 
           "Female percentage involvment", icon = icon("fire"), color = "red")
})

output$child <- renderValueBox({
  valueBox(round(data_states[data_states$state == input$state,]$mean_child_percentage,digits = 2), 
           "Child percentage involvment", icon = icon("fire"), color = "red")
})

output$numberper100k <- renderValueBox({
  valueBox(round(data_states[data_states$state == input$state,]$number_of_incidents_per100000), 
           "Total number of incidents per 100 000", icon = icon("fire"), color = "red")
})


  
output$tbl = renderDT(
  data_render
)




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
  {f(input$city1,input$state1)} 
)



}

# Run the application 
shinyApp(ui = ui, server = server)
