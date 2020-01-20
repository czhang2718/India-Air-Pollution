library(rsconnect)
library(shiny)
library(RColorBrewer)
library(scales)
library(dplyr)
library(leaflet)
library(ggplot2)
shinyServer(function(input, output) {
    
    set.seed(100)

    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron)%>%
            setView(lat = 20.5937, lng = 78.9629, zoom = 5)
    })


    datInBounds <- reactive({
        if (is.null(input$map_bounds))
            return(polldat[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        from<- input$period-181
        till<- input$period+181
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
            if(input$fut==TRUE){
              data = polldat_avg[city_ind,]
            }
            else{
              data = polldat_avg[as.numeric(polldat_avg$year)<=30,][city_ind,]
            }
            
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
        
        if(colorBy=="spm" || colorBy=="all") rad = 80*colorData
        else rad = 1000*colorData
        
        if(input$pollutant=="so2") col = "orange"
        if(input$pollutant=="no2") col= "#27ba4c"
        if(input$pollutant=="spm") col="purple"
        
        if(input$byPop=="Population") pal = colorBin("plasma", polldat_avg$pop, 
                                                     bins=c(0, 500000, 1000000, 1500000, 2000000, 
                                                            2500000, 18410000))
  
        
        if(!input$pollutant=="all" & input$byPop=="Pollutant"){
            isolate({
                leafletProxy('map', data=dat) %>%
                    clearShapes() %>%
                    clearControls() %>%
                    addCircles(~lng, ~lat, radius= rad, stroke=FALSE,
                               fillColor=col, fillOpacity=0.3)  %>%
                    addLegend("bottomright", colors=c("orange", "green", "purple"),  
                              labels=c("Sulfur Dioxide", "Nitrogen Dioxide", "Suspended Particle Matter")
                              , title="Type of Pollutant")
            })

        }
        
        
        else if(!input$pollutant=="all" & input$byPop=="Population"){
            leafletProxy('map', data=dat) %>%
                clearShapes() %>%
                # clearPopups() %>%
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
    
    showPopup <- function(lat, lng, city, map) {
        print(datInBounds())
        print(lat)
        
        # print(selected)
        if(map & !input$pollutant =="all"){
            selected =datInBounds()[datInBounds()$lat==lat,]
            poll_index = grep(input$pollutant, colnames(polldat_avg))
            content <- as.character(tagList(
                tags$h4(city),
                paste("Latitude: ", lat), 
                tags$br(),
                paste("Longitude: ", lng), 
                tags$br(),
                paste("Amount of", input$pollutant, ": ", selected[poll_index], " ppm"),
                
                tags$br() 
            ))
        }
        
        
        else if (map & input$pollutant =="all"){
            selected =datInBounds()[datInBounds()$lat==lat,]
            poll_index = grep(input$pollutant, colnames(polldat_avg))
            content <- as.character(tagList(
                tags$h4(city),
                paste("Latitude: ", lat), 
                tags$br(),
                paste("Longitude: ", lng), 
                tags$br(),
                paste("Amount of SO2: ", selected$so2, " ppm"),
                
                tags$br() 
            ))
        }
        
        
        else{
            content <- as.character(tagList(
                tags$h4(city),
                paste("Latitude: ", lat), 
                tags$br(),
                paste("Longitude: ", lng), 
                
                
                tags$br() 
            ))
        }
       
        leafletProxy("map") %>% addPopups(lng, lat, content)
    }

    observe({
        leafletProxy("map") %>% clearPopups()
        event <- input$map_shape_click
        if (is.null(event))
            return()
        print("not null")
        
        isolate({
            lat = event$lat
            lng = event$lng
            cit = datInBounds()$city[datInBounds()$lat==lat]
            showPopup(lat, lng, cit, TRUE)
        })
    })
    
    
    observe({
        map <- leafletProxy("map")
        if (input$city == "")
            return()
        isolate({
            
            dist <- 1
            lat <- geocities$lat[geocities$city==input$city]
            lng <- geocities$long[geocities$city==input$city]
            
            map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
            showPopup(lat, lng, input$city, FALSE)
        })
    })
    
    
    output$plot <- renderPlotly({
      
      sub = dat[[input$var]]
      typekey = {}
      typeval = {}
      
      for(i in 1:length(summary(sub))){
        factor = as.character(unique(sub))[i]
        print(factor)
        if(is.na(factor)) next
        typekey = c(typekey, factor)
        typeval = c(typeval, length(dat[as.character(sub)==factor, 1]))
      }
      
      
      plot_ly(
        x = typekey,
        y = typeval,
        name = "Pollution by Type",
        type = "bar"
      )
    })
})
