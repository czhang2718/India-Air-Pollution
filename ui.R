library(leaflet)
library(shiny)

shinyUI(fluidPage(
    
    navbarPage("Air Pollution in India", id="nav", position="fixed-top",
    
        tabPanel("Map", div(class="outer",
                 tags$head(
                     includeCSS("proto.styles.css")
                 ),
                 leafletOutput("map", width="100%", height="100%"),
                 absolutePanel(id = "panel", class = "panel panel-defualt", fixed = TRUE, draggable = FALSE, top = 70,
                               left = 50, right = "auto", bottom = "auto", width = 350, 
                               
                               h2("Interactive Map"),
                               h4("(1987-2015)"),
                               
                               selectInput("byPop", "Color by", c("Pollutant", "Population")),
                               selectInput("pollutant", "Pollutant", vars),
                               # textInput("location", "City"),
                               
                               h3("Graph"),
                               
                               
                               selectInput("city", "City", c(cities, ""), selected=""),
                               
                               checkboxInput("fut", "Include predictions", FALSE),
                               plotOutput("allTime", height=200)
                               # plotOutput("cityTime", height=200)
                               
                 ),
                 
                 absolutePanel(id = "slider", top = 10, right = 20,
                               sliderInput("period", "Date",
                                           min = as.Date("1986-12-31", tz=""),
                                           max = as.Date("2015-1-30", tz=""),
                                           step = 30,
                                           value = as.Date("1986-12-31", tz="")
                                           ,
                                           animate=
                                               animationOptions(interval = 100, loop = TRUE, playButton=NULL))
                 ))
             ),
        tabPanel("Data", class="outer", id="graphs",
                 # #setBackgroundImage(src="https://github.com/czhang2718/India-Air-Pollution/blob/master/india_badair.jpg?raw=true"),
                 # setBackgroundColor("white"),
                 
                 div(id="text", div(id="title", p("Data Sources")), div(id="des", p("Data to create the interactive map 
                                                                       is from user Shruti Bhargava on Kaggle. The dataset is a cleaned version of Historical Daily Ambient Air Quality Data from 
                                                                       the Ministry of Environment and Forests and Central Pollution Control Board of India under the National Data Sharing and Accessibility Policy (NDSAP). It 
                                                                       includes information about the amount of suspended particulate matter, respiratory suspended particulate amtte,r 
                                                                       nitrogen dioxide, sulfur dioxide, along with the state, location, date, agency, and area type of recording.")),
                 selectInput("var", "Attribute", axis)),
                 plotlyOutput("plot")
                 
                 
                 )
        )
    )
)
