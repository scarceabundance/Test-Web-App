library(shiny)
library(shinydashboard)
library(dplyr)
library(magrittr)
library(ggplot2)
library(leaflet)
library(readxl)
library(RgoogleMaps)
library(ggmap)
library(wesanderson)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


##### Loading the data #####

OPD_90_CRIME <- 
  read_excel("C:/Users/AGabrielClay/Desktop/R Studio Graphics/MACRO mental health research/OPD Data/CrimeWatch_Maps_Past_90-Days.xlsx", 
             sheet = "CrimeWatch_Maps_Past_90-Days")
View(OPD_90_CRIME)

Non_Violent_Crime <- filter(OPD_90_CRIME, CRIMETYPE == "OTHER" | CRIMETYPE == "CURFEW & LOITERING" | 
                              CRIMETYPE == "DRUNKENNESS" | CRIMETYPE == "BRANDISHING" |
                              CRIMETYPE == "NARCOTICS" | CRIMETYPE == "DISORDERLY CONDUCT")
# using filter (dplyr package) function to create new tbl_df containing only the desired non-violent categories 

View(Non_Violent_Crime)

##### GRADIENT; Non-Violent crime by police-beat #####

##### CODING DA DASHBOARD ##### 

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Test-MACRO Dashboard")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'),
             href = "https://urbanstrategies.org/")
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    title = "Geospatial Crime Map"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("spatialcrime", height = "300px")
  )
  ,box(
    title = "Non-Violence Crime Gradient (by Police Beat)"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("policebeatcount", height = "300px")
  )
)

frow3 <- fluidRow(
  box(
    title = "Non-Violent Crime count by OPD Beat"
    ,status = "warning"
    ,solidheader = TRUE
    ,collapsible = TRUE
    ,plotOutput("policebeatgradient", height = "300px")
  )
  ,box(
    title = "Non Violent Crime denisty heat map"
    ,status = "warning"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("densityheatmap", height = "300px")
  )
)
# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Test dashboard title', header, sidebar, body, skin='blue')

# create the server functions for the dashboard  
server <- function(input, output) { 
  #some data manipulation to derive the values of KPI boxes
  nviolent.count <- count(Non_Violent_Crime, vars = "Description")
  
  # these ar the three KPI summaries to display summary statistics of the dataset 
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(crime.type$value, format="d", big.mark=',')
      ,paste('Top Frequency:',crime.type$CRIMETYPE)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(nviolent.count, format="d", big.mark=',')
      ,'Number of Non-Violent Offenses'
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(beat.count$value, format="d", big.mark=',')
      ,paste('Busiest Beat:',beat.count$POLICEBEAT)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")   
  })
  
  
  #creating the plotOutput content
  output$spatialcrime <- renderPlot({
    oaklandMap +
      geom_point(aes(x = lon, y = lat, colour =CRIMETYPE, size =CRIMETYPE),data = Non_Violent_Crime)
  })
  output$policebeatcount <- renderPlot({
    ggplot(data = Non_Violent_Crime) + 
      geom_bar(mapping = aes(x = CRIMETYPE, fill = POLICEBEAT), position = "fill")+ 
      coord_flip() +
      labs(title = "Police-Beat gradient by Non-Violent Crime Type (over a 3 month period)",
           caption = "Data: Oakland Police Department (OPD)")
  })
  output$policebeatgradient <- renderPlot({
    ggplot(data = Non_Violent_Crime) + 
      geom_bar(mapping = aes(x = CRIMETYPE, fill = POLICEBEAT), position = "dodge")+
      labs(title = "Count/ Frequency of Non-Violent Crime Type by OPD Beat (over a 3 month period)",
           caption = "Data: Oakland Police Department (OPD)")
  })
  output$densityheatmap <- renderPlot({
    oaklandMap +
      stat_density2d(aes(x=lon, y = lat, fill = ..level..,alpha=10), bins = 5, geom = "polygon", 
                     data = OPD_90_CRIME) +
      scale_fill_gradient(low = "green", high = "red")+
      ggtitle("Map of Non-Violent Crime Density in Oakland")
  })
}

shinyApp(ui, server)
