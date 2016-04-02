shinyServer(function(input, output) {
  
  #Create object to use for map legend
  maplabels <- c("Homicide","Rape","Robbery","Aggravated Assault","Burglary","Larceny","Auto Theft","Assault","Sex Offense","DUI")
  cl <- c("01","02","03","04","05","06","07","08","17","28")
  labeldf <- data.frame(maplabels, cl)
  
  #Load Outline of Montgomery County
  moco <- readOGR(dsn="./poly", "poly")
  
  #Download and clean crime data
  crime <- reactive({
    #Translate text entries in the dropdown menu to strings that can be used to filter dataset
    dataIn <- switch(input$class,
                     "All Crime" = c("01","02","03","04","05","06","07","08","17","28"),
                     "Homicide" = "01",
                     "Rape" = "02",
                     "Robbery" = "03",
                     "Aggravated Assault" = "04",
                     "Burglary" = "05",
                     "Larceny" = "06",
                     "Auto Theft" = "07",
                     "Assault" = "08",
                     "Sex Offense" = "17",
                     "DUI" = "28") 
    
    #Write API Query. For more information see: https://dev.socrata.com/docs/queries/index.html
    #Query selects based on date range set by user and removes several high-frequency, less interesting crime types like minor drug offenses
    #The query was made more specific to limit the total number of records that had to be downloaded in order to get the subset of interest
    url <- paste0("https://data.montgomerycountymd.gov/resource/crime.json?$where=incident_type!=%272938%27%20AND%20incident_type!=%272942%27%20AND%20incident_type!=%272941%27%20AND%20incident_type!=%271834%27%20AND%20incident_type!=%271031%27%20AND%20start_date%20>%20%27",input$date[1],"%27%20AND%20start_date%20<%20%27",input$date[2],"%27&$limit=50000")
    #Retrieve data
    crime <- fromJSON(url)
    #Remove records that do not have a geotag
    crime <- crime[!is.na(crime$longitude), ]
    
    #Crime classifications are hierarchical and many of the specific codes are not useful (e.g., 0522 - BURG NO FORCE - RES/DAY)
    #Create a 2-digit classification at a higher level of aggregation (e.g., 05 - Burglary)
    crime$cl <- substr(crime$incident_type,1,2)
    
    #Subset the data based on the type of crime selected in the Crime dropdown
    crime <- subset(crime, cl %in% dataIn)
    
    #Merge the labels for the 2-digit crime classifications
    crime <- join(crime,labeldf, by='cl',type='left',match='all')
    crime$maplabels <- as.factor(crime$maplabels)
    
    #Convert latitude and longitude to numeric variables, the preferred for mat for Leaflet
    crime$longitude <- as.numeric(crime$longitude)
    crime$latitude <- as.numeric(crime$latitude)
    
    #Convert start_date to variable format recognizable by R
    crime$start_date <- as.POSIXct(crime$start_date, "%Y-%m-%dT%H:%M:%S", tz="EST")
    
    #Create variables for use in charts
    crime$dayofweek <- format(crime$start_date, "%a")    #By Weekday
    crime$hourofday <- format(crime$start_date, "%H")    #By Hour
    
    #Create global variable showing the number of rows (i.e., recorded crimes) in the dataset for reactive text
    numrow <<- nrow(crime)
    
    #Return downloaded and cleaned dataset to variable crime() for use in other functions
    return(crime)
  })
  
  output$map <- renderLeaflet({
    #Heatmap (Kernel Density Estimate) Calculation
    x <- crime()[ ,c("longitude","latitude")]
    x <- x[complete.cases(x), ]
    est <- bkde2D(x, 
                  bandwidth=c(input$band, input$band),
                  gridsize=c(1200, 1200))
    est$fhat[est$fhat < input$thresh] <- NA

    loc_density_raster <- raster(
      list(x = est$x1, y = est$x2, z = est$fhat)
    )
    projection(loc_density_raster) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    
    #Create Legend for Crime Points Map
    legdf <- data.frame(unique(crime()[, "cl"]), unique(crime()[, "maplabels"]))
    legdf <- arrange(legdf,unique.crime......cl...)
    
    #Create popup that appears when a crime location is clicked on
    popup <-  with(crime(),paste(sep = "",
                                 "<b><h4>",narrative,"</h4></b>",
                                 "<b>Address: </b> ",location,"<br/>",
                                 "<b>Date/Time: </b> ",start_date,"<br/>",
                                 "<b>Place: </b> ",place,"<br/>"))
    
    #Set up color scheme for crime points
    col <- colorFactor(brewer.pal(nrow(legdf), "Paired"), domain= legdf$unique.crime......cl...)
    
    #Set up color scheme for heatmap
    color_pal <- colorNumeric(
      palette = rev(brewer.pal(9, input$color)), domain = values(loc_density_raster), 
      na.color = "transparent"
    )
    
    #There are two map options; crime points and a heatmap
    if(input$maptype == 1) {
          leaf <- leaflet(crime()) %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(lng = mean(crime()[, "longitude"]) - .05, lat = mean(crime()[, "latitude"]) + .037, zoom = 11) %>%
            addPolygons(data = moco, weight = 2, color = "black", fillOpacity = 0) %>%
            addCircles(lng = crime()$longitude, lat = crime()$latitude, popup= popup, 
                       weight = 8, radius=8, color= col(crime()$cl), stroke = TRUE, fillOpacity = .6)
          
          if(input$class =="All Crime") {
            leaf <- leaf %>%
              addLegend("bottomright", colors= brewer.pal(nrow(legdf), "Paired"), 
                        labels= legdf$unique.crime......maplabels...,opacity = 0.5)
          }    
          
    }else if(input$maptype == 2) {
      leaf <- leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = mean(x[, "longitude"]) - .05, lat = mean(x[, "latitude"]) + .037, zoom = 11) %>%
        addPolygons(data = moco, weight = 2, color = "black", fillOpacity = 0) %>%
        addRasterImage(x = loc_density_raster, colors=color_pal, opacity = input$opacity, project = FALSE) #%>%
        #addLegend("bottomright",title=input$class, pal = color_pal, values = values(loc_density_raster))
    }
    

    #Add Layers If Selected
    if(input$school == TRUE){
      schools <- reactive({
        url <- "https://data.montgomerycountymd.gov/resource/772q-4wm8.json"
        #Retrieve data
        school <- fromJSON(url)
        school$longitude <- as.numeric(school$longitude)
        school$latitude <- as.numeric(school$latitude)
        return(school)
      })
      
      popupsc <-  with(schools(),paste(sep = "",
                                       "<b><h4>",school_name,"</h4></b>",
                                       "<b>Type: </b> ",category,"<br/>"))
 
      schoolIcon <- makeIcon(iconUrl= "http://www.clker.com/cliparts/8/3/A/B/B/e/black-mortarboard-md.png",
                             iconWidth = 20, iconHeight = 12)
      
      leaf <- leaf %>%
        addMarkers(lng = schools()$longitude, lat = schools()$latitude, popup= popupsc,
                   icon = schoolIcon)
    }
    
    if(input$hospital == TRUE){
      hospitals <- reactive({
        url <- "https://data.montgomerycountymd.gov/resource/63iv-suxf.json?in_county=In-County"
        #Retrieve data
        hospital <- fromJSON(url)
        hospital$longitude <- as.numeric(hospital$longitude)
        hospital$latitude <- as.numeric(hospital$latitude)
        return(hospital)
      })
      
      popuphosp <-  with(hospitals(),paste(sep = "",
                                           "<b><h4>",name,"</h4></b>"))
      hospIcon <- makeIcon(iconUrl= "http://www.ipharmd.net/images/red_cross_red_round.png",
                             iconWidth = 20, iconHeight = 20)
      leaf <- leaf %>%
        addMarkers(lng = hospitals()$longitude, lat = hospitals()$latitude, popup= popuphosp,
                   icon = hospIcon)
    }
    
    if(input$liquor == TRUE){
      liquor <- reactive({
        url <- "https://data.montgomerycountymd.gov/resource/6bdh-is2m.json?channel_type=Off%20Premise"
        #Retrieve data
        liquor <- fromJSON(url)
        liquor <- liquor[liquor$location$coordinates != "NULL", ]
        
        latlon <- data.frame(matrix(ncol=2,nrow=1))
        names(latlon) <- c("longitude","latitude")
        for(i in 1:nrow(liquor)){
          lon <- as.numeric(liquor$location$coordinates[[i]][1])
          lat <- as.numeric(liquor$location$coordinates[[i]][2])
          temp <- data.frame(cbind(lon,lat))
          names(temp) <- c("longitude","latitude")
          latlon <- rbind(latlon,temp)
        }
        latlon <- latlon[2:nrow(latlon), ]
        
        liquor <- cbind(liquor, latlon)
        return(liquor)
      })
      
      popupliquor <-  with(liquor(),paste(sep = "",
                                           "<b><h4>",licensee_name,"</h4></b>",
                                          "<b>Address: </b> ",location_address,"<br/>"))
      liquorIcon <- makeIcon(iconUrl= "https://cdn3.iconfinder.com/data/icons/badger-s-christmas/300/wine-bottle-512.png",
                           iconWidth = 17, iconHeight = 17)
      leaf <- leaf %>%
        addMarkers(lng = liquor()$longitude, lat = liquor()$latitude, popup= popupliquor,
                   icon = liquorIcon)
    }
    
    leaf
  })
  
  #Create plot for crimes by hour of the day
  output$hourofday <- renderPlot ({
    ggplot(data=as.data.frame(prop.table(table(crime()[, "hourofday"])))) +
      geom_bar(stat="identity",aes(x=Var1, y=Freq), fill="lightyellow3", color='black', size=.3) +
      geom_hline(yintercept = .0416, linetype=3) +
      scale_x_discrete(limits=c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')) +
      scale_y_continuous(expand = c(0,0), labels=percent) +
      labs(
        title = "Percentage of Crimes by Hour of the Day",
        x = "Hour of the Day",
        y = "Count"
      ) +
      theme(plot.title = element_blank(), #element_text(size = 14, face="bold", color = "black"),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(color = "black", size=0.75),
            axis.line.x = element_line(color = "black", size=0.75),
            axis.text = element_text(size = 12, color="black"),
            axis.text.x = element_text(angle = 90, vjust=0.5,hjust=0.5),
            axis.title = element_text(size = 14, face="bold", color = "black"),
            axis.title.y = element_blank(),
            axis.ticks = element_line(color="black"),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(size = 14, color= "black"),
            legend.title = element_blank())
    
  }, bg="transparent")
  
  #Create plot for crimes by day of the week
  output$dayofweek <- renderPlot({
    ggplot(data=as.data.frame(prop.table(table(crime()[, "dayofweek"])))) +
      geom_bar(stat="identity",aes(x=Var1, y=Freq), fill="darkolivegreen3", color='black', size=.3) +
      geom_hline(yintercept = .1428, linetype=3) +
      scale_x_discrete(limits=c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')) +
      scale_y_continuous(expand = c(0,0), labels = percent) +
      labs(
        title = paste0("Percentage of ", input$class,"s by..."),
        x = "Day of Week",
        y = "Percent"
      ) +
      theme(plot.title = element_text(size = 18, face="bold", color = "black"),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(color = "black", size=0.75),
            axis.line.x = element_line(color = "black", size=0.75),
            axis.text = element_text(size = 12, color="black"),
            axis.title = element_text(size = 14, face="bold", color = "black"),
            axis.title.y = element_blank(),
            axis.ticks = element_line(color="black"),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(size = 14, color= "black"),
            legend.title = element_blank())
  }, bg="transparent")
  
  #Change text for "XXXX Crimes Displayed" whenever the date or crime type changes
  observeEvent(input$class, {
    output$txt <- renderText(paste0(numrow[1], " Crimes Displayed"))
  })   
  observeEvent(input$date, {
    output$txt <- renderText(paste0(numrow[1], " Crimes Displayed"))
  })
  #Render Text for Banner
  observeEvent(input$class, {
    output$heading <- renderText(input$class)
  })
})