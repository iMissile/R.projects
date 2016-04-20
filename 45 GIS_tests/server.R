# чистый образец взят отсюда: http://rstudio-pubs-static.s3.amazonaws.com/23390_18f799a4d9ea46c392460b252fed6f15.html
library(shiny)
library(ggplot2)
library(ggmap)

shinyServer(function(input, output) {
  output$map<-renderPlot({
    
    #getMap<-get_map(input$address, zoom=input$zoom)
    #ggmap(getMap, extent="panel")  
    
    
    us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
    map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
    ggmap(map)
  
    downtown <- subset(crime,
                       -95.39681 <= lon & lon <= -95.34188 &
                         29.73631 <= lat & lat <=  29.78400
    )
    
    qmplot(lon, lat, data = downtown, maptype = "toner-background", color = I("red"))
    
    
    robberies <- subset(downtown, offense == "robbery")
    
    qmplot(lon, lat, data = downtown, geom = "blank", zoom = 15, maptype = "toner-background", darken = .7) +
      stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
      scale_fill_gradient2("Robbery\nPropensity", low = "white", mid = "yellow", high = "red", midpoint = 1500)
      
  })
})