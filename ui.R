library(leaflet)
library(shiny)

shinyUI(fluidPage(

    tags$head(
        class="outer",
        includeCSS("proto.styles.css")
    ),
    absolutePanel(id = "panel", class = "panel panel-defualt", fixed = TRUE, draggable = FALSE, top = 70,
                  left = 50, right = "auto", bottom = "auto", width = 350, 
                  h2("Air Pollution in India"),
                  h4("(1987-2015)"),
                  
                  selectInput("byPop", "Color by", c("Pollutant", "Population")),
                  selectInput("pollutant", "Pollutant", vars),
                  # textInput("location", "City"),
                  
                  h3("Graph"),
                  
                  
                  selectInput("city", "City", c(cities, ""), selected=""),
                  
                  
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
                    animationOptions(interval = 100, loop = TRUE, playButton=NULL)
                )
    ),

    leafletOutput("map")
))

