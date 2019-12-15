library(rsconnect)
library(shiny)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(leaflet)
library(ggplot2)
shinyServer(function(input, output) {
    
    set.seed(100)

    output$map <- renderLeaflet({
        leaflet() %>%
            # map option #2
            # addProviderTiles(providers$OpenStreetMap) %>%

            # option 3
            # addTiles(
            #     urlTemplate="//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            #     attribution= "<a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> Basemap  <a href='http://www.tcu.edu'>Texas Christian University</a>"
            # ) %>%
            addProviderTiles(providers$CartoDB.Positron)%>%
            setView(lat = 20.5937, lng = 78.9629, zoom = 5)
    })


    datInBounds <- reactive({
        if (is.null(input$map_bounds))
            return(polldat[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        print("hi")
        from<- input$period-180
        till<- input$period+180
        print(from)
        subset(polldat_avg,
               lat >= latRng[1] & lat <= latRng[2] &
                lng >= lngRng[1] & lng <= lngRng[2] &
               as.Date(ISOdate(year, 1, 1))  >= from & 
            as.Date(ISOdate(year, 1, 1))  <= till )
    })


    
    
  
    output$allTime <- renderPlot({
        if(input$city!=""){
            if(input$pollutant=="all"){
                poll_index = grep("avg", colnames(polldat_avg))
            }
            else{
                poll_index = grep(input$pollutant, colnames(polldat_avg))
            }
            
            city_ind = grep(as.character(input$city), polldat_avg$city)
            data = polldat_avg[city_ind,]
            ggplot()+
                geom_boxplot(data=data,mapping=aes(date, data[, poll_index]))+
                   labs(x="Date",
                   y=paste("Amount of ", input$pollutant, " (ppm)"),
                   title="Pollution Levels over Time")+
            theme(axis.text.x=element_text(angle = 90, hjust = 0))
        }
        
    })

    
    observe({
        dat = datInBounds()
        if(input$pollutant == "all"){
            colorBy = "all"
            if(input$byPop == "Population") colorData <- dat$avg
            else colorData <- c(dat$so2, dat$no2, dat$spm)
        }
        else{
            colorBy = input$pollutant
            colorData <- dat[[colorBy]]
        }
        
        # pal <- colorNumeric("inferno", c(0, 30), na.color="#808080", alpha = FALSE)
        if(colorBy=="spm" || colorBy=="all") rad = 100*colorData
        else rad = 1000*colorData
        
        if(input$pollutant=="so2") col = "orange"
        if(input$pollutant=="no2") col= "#27ba4c"
        if(input$pollutant=="spm") col="purple"
        
        if(input$byPop=="Population") pal = colorBin("plasma", polldat_avg$pop, 
                                                     bins=c(0, 500000, 1000000, 1500000, 2000000, 
                                                            2500000, 18410000))
        #plasma
        
        
        
        
        
        if(!input$pollutant=="all" & input$byPop=="Pollutant"){
            leafletProxy('map', data=dat) %>%
                clearShapes() %>%
                clearControls() %>%
                addCircles(~lng, ~lat, radius= rad, stroke=FALSE,
                           fillColor=col, fillOpacity=0.3)  %>%
                addLegend("bottomright", colors=c("orange", "green", "purple"),  
                          labels=c("Sulfur Dioxide", "Nitrogen Dioxide", "Suspended Particle Matter")
                          , title="Type of Pollutant")
                

        }
        
        
        
        
        else if(!input$pollutant=="all" & input$byPop=="Population"){
            leafletProxy('map', data=dat) %>%
                clearShapes() %>%
                clearControls() %>%
                addCircles(~lng, ~lat, radius= rad, stroke=FALSE,
                       fillColor=pal(polldat_avg$pop), fillOpacity=0.3) %>%
                addLegend("bottomright", values = polldat_avg$pop, pal=pal, 
                          labels=c("0-500,000", "500,000-1,000,000", "1,000,000-1,500,000",
                                   "1,500,000-2,000,000", "2,000,000-2,500,000", ">=2,500,000"),
                          title="Population")
        }
        
        
        
        else if(input$pollutant =="all" & input$byPop == "Pollutant"){
            leafletProxy('map', data=dat) %>%
                clearShapes() %>%
                clearControls() %>%
                
                #fill color is pal(polldat_avg$radius)
                
                addCircles(~lng, ~lat, radius= 1000*dat$no2, stroke=FALSE,
                           fillColor="green", fillOpacity=0.2) %>%
                addCircles(~lng, ~lat, radius= 100*dat$spm, stroke=FALSE,
                           fillColor="purple", fillOpacity=0.2) %>%
                addCircles(~lng, ~lat, radius= 1000*dat$so2, stroke=FALSE,
                       fillColor="orange", fillOpacity=0.15) %>%
               
                addLegend("bottomright", colors=c("orange", "green", "purple"), 
                          labels=c("Sulfur Dioxide", "Nitrous Oxide", "Suspended Particle Matter"),
                          title="Type of Pollutant")
        }
        
        else if(input$pollutant=="all" & input$byPop =="Population"){
            leafletProxy('map', data=dat) %>%
                clearShapes() %>%
                clearControls() %>%
                addCircles(~lng, ~lat, radius= 1.5*rad, stroke=FALSE,
                           fillColor=pal(polldat_avg$pop), fillOpacity=0.3) %>%
                addLegend("bottomright", values = polldat_avg$pop, pal=pal,
                          title="Population")
        }
        
            

    })
    
    showPopup <- function(lat, lng) {
        print(datInBounds())
        print(lat)
        selected =datInBounds()[datInBounds()$lat==lat,]
        print(selected)
        content <- as.character(tagList(
            tags$h4(selected$city),
            paste("Latitude: ", lat), 
            tags$br(),
            paste("Longitude: ", lng), 
                        
                        
            tags$br() 
        ))
        leafletProxy("map") %>% addPopups(lng, lat, content)
    }
    
    # showZipcodePopup <- function(zipcode, lat, lng) {
    #     selectedZip <- allzips[allzips$zipcode == zipcode,]
    #     content <- as.character(tagList(
    #         tags$h4("Score:", as.integer(selectedZip$centile)),
    #         tags$strong(HTML(sprintf("%s, %s %s",
    #                                  selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
    #         ))), tags$br(),
    #         sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
    #         sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
    #         sprintf("Adult population: %s", selectedZip$adultpop)
    #     ))
    #     leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
    # }
    # 
    
    observe({
        leafletProxy("map") %>% clearPopups()
        event <- input$map_shape_click
        if (is.null(event))
            return()

        isolate({
            showPopup(event$lat, event$lng)
        })
    })
    
    
    observe({
        if (input$city == "")
            return()
        isolate({
            map <- leafletProxy("map")
            map %>% clearPopups()
            dist <- 1
            lat <- geocities$lat[geocities$city==input$city]
            lng <- geocities$long[geocities$city==input$city]
            showPopup(lat, lng)
            map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
        })
    })
})
