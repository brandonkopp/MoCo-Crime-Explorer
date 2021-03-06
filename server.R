shinyServer(function(input, output) {
  
  #Create object to use for map legend
  nibrs <- read.xlsx("nibrs.xlsx",sheetIndex =1)
  
  #Load Outline of Montgomery County
  moco <- readOGR(dsn="./poly", "poly")
  moco@proj4string@projargs <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  
  #Download and clean crime data
  crime <- reactive({
    #Translate text entries in the dropdown menu to strings that can be used to filter dataset
    dataIn <- switch(input$class,
                     "All Crime" = nibrs$cl,
                     "All Crime (except Petty & Financial)" =  nibrs$cl[nibrs$AllCrimeExcept == 1],
                     "Violent Crime" =  nibrs$cl[nibrs$Violent == 1],
                     "Financial Crimes (e.g., Forgery)" =  nibrs$cl[nibrs$Financial == 1],
                     "Petty Offenses" =  nibrs$cl[nibrs$Petty == 1],
                     "Homicide" = "09",
                     "Sex Offenses" = "11",
                     "Robbery" = "12",
                     "Assault" = "13",
                     "Burglary" = "22",
                     "Larceny" = "23",
                     "Auto Theft" = "24",
                     "Arson" = "20",
                     "DUI" = "90D") 
    
    #Write API Query. For more information see: https://dev.socrata.com/docs/queries/index.html
    #Query selects based on date range set by user and removes several high-frequency, less interesting crime types like minor drug offenses
    #The query was made more specific to limit the total number of records that had to be downloaded in order to get the subset of interest
    url <- paste0("https://data.montgomerycountymd.gov/resource/yc8a-5df8.json?$where=nibrs_code!=%2790Z%27%20AND%20nibrs_code!=%2790I%27%20AND%20nibrs_code!=%27370%27%20AND%20nibrs_code!=%2740A%27%20AND%20nibrs_code!=%2740B%27%20AND%20nibrs_code!=%2736A%27%20AND%20nibrs_code!=%2736B%27%20AND%20start_date%20>%20%27",input$date[1],"%27%20AND%20start_date%20<%20%27",input$date[2],"%27&$limit=50000")
    #Retrieve data
    crime <- fromJSON(url)
    #Remove records that do not have a geotag
    crime$longitude <- as.numeric(crime$longitude)
    crime$latitude <- as.numeric(crime$latitude)
    crime <- crime[!is.na(crime$longitude), ]
    crime <- crime[ ,names(crime) != "geolocation"]
    
    #Remove miscoded geotags (ones that are far outside the county)
    crime <- crime[(crime$longitude < -76.6) & (crime$longitude > -77.8), ]
    crime <- crime[(crime$latitude > 38.7) & (crime$latitude < 39.5), ]
  
    #Crime classifications are hierarchical and many of the specific codes are not useful (e.g., 0522 - BURG NO FORCE - RES/DAY)
    #Create a 2-digit classification at a higher level of aggregation (e.g., 05 - Burglary)
    crime$cl <- substr(crime$nibrs_code,1,2)
    crime$cl[crime$nibrs_code == "90D"] <- "90D"
    
    #Subset the data based on the type of crime selected in the Crime dropdown
    crime <- subset(crime, cl %in% dataIn)
    
    #Merge the labels for the 2-digit crime classifications
    nibrs$cl <- as.character(nibrs$cl)
    crime <- left_join(crime,nibrs, by="cl")
    crime$maplabels <- as.factor(crime$maplabels)
    if(input$class %in% c("Petty Offenses", "Assault", "Larceny", "Sex Offenses")){
      crime$cl <- crime$nibrs_code
      crime$maplabels <- crime$crimename2
    }
    
    #Convert start_date to variable format recognizable by R
    crime$start_date <- as.POSIXct(crime$start_date, "%Y-%m-%dT%H:%M:%S", tz="America/New_York")
    crime$newdate <- as.Date(format(crime$start_date, "%Y-%m-%d"))
    
    #Create variables for use in charts
    crime$dayofweek <- format(crime$start_date, "%a")    #By Weekday
    crime$hourofday <- format(crime$start_date, "%H")    #By Hour
    
    #Create global variable showing the number of rows (i.e., recorded crimes) in the dataset for reactive text
    numrow <<- nrow(crime)
    
    #Add Grid Info
    grid <- raster(extent(moco))
    res(grid) <- input$box/1600/67
    proj4string(grid)<-proj4string(moco)
    gridpolygon <- rasterToPolygons(grid)
    moco.grid <- intersect(moco, gridpolygon)
    moco.grid@data$sector <- 1:nrow(moco.grid@data)
    new <- crime
    xy <- new[ ,c("longitude","latitude")]
    new <- SpatialPointsDataFrame(coords=xy, data = new)
    new@proj4string@projargs <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    res <- over(new, moco.grid)
    crime$sector <- res$sector
    
    #Return downloaded and cleaned dataset to variable crime() for use in other functions
    return(crime)
  })
  
  mocodat <- reactive({
    # Create an empty raster.
    grid <- raster(extent(moco))
    # Choose its resolution. I will create boxes that are roughly 1000 sq m.
    res(grid) <- input$box/1600/67
    # Make the grid have the same coordinate reference system (CRS) as the shapefile.
    proj4string(grid)<-proj4string(moco)
    # Transform this raster into a polygon and create a grid without an overall shape.
    gridpolygon <- rasterToPolygons(grid)
    # Intersect the grid with Montgomery County.
    moco.grid <- intersect(moco, gridpolygon)
    #Create ID Number for Each Box In Grid
    moco.grid@data$sector <- 1:nrow(moco.grid@data)

    x <- as.data.frame(table(crime()$sector))
    moco.grid@data <- moco.grid@data[ ,1:22]
    moco.grid@data <- data.frame(moco.grid@data, x[match(moco.grid@data[,"sector"], x$Var1),])
    
    moco.grid@data$Freq[moco.grid@data$Freq < input$threshold] <- NA
    return(moco.grid)
  })
  
  output$map <- renderLeaflet({
    #Heatmap (Kernel Density Estimate) Calculation
    x <- crime()[ ,c("longitude","latitude")]
    x <- x[complete.cases(x), ]
    est <- bkde2D(x, 
                  bandwidth=c(input$band, input$band),
                  gridsize=c(1800, 1800))
    est$fhat[est$fhat < input$thresh] <- NA

    loc_density_raster <- raster(
      list(x = est$x1, y = est$x2, z = est$fhat)
    )
    projection(loc_density_raster) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    
    #Create Legend for Crime Points Map
    if(input$class %in% c("Petty Offenses", "Assault", "Larceny", "Sex Offenses")){
      legdf <- data.frame(unique(crime()[, c("cl","maplabels")]))
      legdf <- legdf[complete.cases(legdf), ]
      legdf <- arrange(legdf,cl)
    }else{
      legdf <- data.frame(unique(crime()[, "cl"]), unique(crime()[, "maplabels"]))
      legdf <- arrange(legdf,unique.crime......cl...)
    }
    
    names(legdf) <- c("cl","maplabels")
    
    #Create popup that appears when a crime location is clicked on
    popup <-  with(crime(),paste(sep = "",
                                 "<b><h4>",crimename3,"</h4></b>",
                                 "<table style='width:250px'>",
                                 "<tr><td><b>Address:</b></td><td>",location,"</td></tr>",
                                 "<tr><td><b>Date/Time:</b></td><td>",start_date,"</td></tr>",
                                 "<tr><td><b>Place:</b></td><td>",place,"</td></tr>",
                                 "</table>"))
    
    #Set up color scheme for crime points
    col <- colorFactor(rev(brewer.pal(nrow(legdf), "Paired")), domain= legdf$cl)

    #There are two map options; crime points and a heatmap
    if(input$maptype == 1) {
          leaf <- leaflet(crime()) %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(-77.177915, 39.133585, zoom=11) %>% 
            #setView(lng = mean(crime()[, "longitude"]) - .05, lat = mean(crime()[, "latitude"]) + .037, zoom = 11) %>%
            addPolygons(data = moco, weight = 2, color = "black", fillOpacity = 0) %>%
            addCircleMarkers(lng = crime()$longitude, lat = crime()$latitude, popup= popup, 
                             fillColor = col(crime()$cl), fill=TRUE, fillOpacity=0.6,
                       weight = 0.5, radius=5, color= "black" , stroke = TRUE)
          
          if(input$class %in% c("All Crime (except Petty & Financial)","Violent Crime",
                                "Financial Crimes (e.g., Forgery)",
                                "Petty Offenses", "Assault", "Larceny", "Sex Offenses")) {
            leaf <- leaf %>%
              addLegend("bottomright", colors= rev(brewer.pal(nrow(legdf), "Paired")), 
                        labels= legdf$maplabels,opacity = 0.5)
          }    
          
    }else if(input$maptype == 2) {
      #Set up color scheme for heatmap
      color_pal <- colorNumeric(
        palette = rev(brewer.pal(9, input$color)), domain = values(loc_density_raster), 
        na.color = "transparent"
      )
      
      leaf <- leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-77.177915, 39.133585, zoom=11) %>% 
        #setView(lng = mean(x[, "longitude"]) - .05, lat = mean(x[, "latitude"]) + .037, zoom = 11) %>%
        addPolygons(data = moco, weight = 2, color = "black", fillOpacity = 0) %>%
        addRasterImage(x = loc_density_raster, colors=color_pal, opacity = input$opacity, project = FALSE)
    }else if(input$maptype == 3){
      
      pal <- colorNumeric(rev(brewer.pal(6, "RdBu")), domain = mocodat()@data$Freq, na.color = "transparent")
      
      popup <- with(mocodat()@data,paste0("<h4><strong>Sector: ",sector,"</strong></h4>",
                      "<strong>",input$class,": ",Freq,"</strong><br>"))
      
      leaf <- leaflet(mocodat()) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(-77.177915, 39.133585, zoom=11) %>% 
        #setView(lng = mean(x[, "longitude"]) - .05, lat = mean(x[, "latitude"]) + .037, zoom = 11) %>%
        addPolygons(data = moco, weight = 2, color = "black", fillOpacity = 0) %>%
        addPolygons(data = mocodat(), weight = .3 ,color = "gray", popup=popup,layerId = mocodat()@data$sector,
                    fillColor = ~pal(mocodat()@data$Freq), fillOpacity = 0.5) %>%
        addLegend("bottomright",title=paste0("Total ", input$class), pal = pal, 
                  values = mocodat()@data$Freq,bins=5)
    }else if(input$maptype == 4) {
      leaf <- leaflet(crime()) %>%
        addProviderTiles("CartoDB.DarkMatter") %>%
        addProviderTiles("Stamen.TonerLines",options = providerTileOptions(opacity = 0.35)) %>%
        addProviderTiles("Stamen.TonerLabels",options = providerTileOptions(opacity = 0.35)) %>%
        setView(-77.177915, 39.133585, zoom=11) %>% 
        #setView(lng = mean(crime()[, "longitude"]) - .05, lat = mean(crime()[, "latitude"]) + .037, zoom = 11) %>%
        addPolygons(data = moco, weight = 3, color = "white", fillOpacity = 0) %>%
        addCircleMarkers(lng = crime()$longitude, lat = crime()$latitude, popup= popup, fill=TRUE,
                         weight = 4, radius=4, color= col(crime()$cl), stroke = TRUE, opacity=0.6,
                         fillColor="white",fillOpacity = 0.9)
      
      if(input$class %in% c("All Crime (except Petty & Financial)","Violent Crime",
                            "Financial Crimes (e.g., Forgery)",
                            "Petty Offenses", "Assault", "Larceny", "Sex Offenses")) {
        leaf <- leaf %>%
          addLegend("bottomright", colors= rev(brewer.pal(nrow(legdf), "Paired")), 
                    labels= legdf$maplabels,opacity = 0.7)
      }
    }
    

    #Add Layers If Selected
    if(input$police == TRUE){
      pstation <- reactive({
        url <- "https://data.montgomerycountymd.gov/resource/caxq-tx48.json"
        #Retrieve data
        stations <- fromJSON(url)
        stations$longitude <- as.numeric(stations$longitude)
        stations$latitude <- as.numeric(stations$latitude)
        return(stations)
      })
      
      popupps <-  with(pstation(),paste(sep = "",
                                       "<b><h4>",name,"</h4></b>",
                                       "<b>Address: </b> ",address,"<br/>",
                                       "<b>Phone: </b> ",phone,"<br/>"))
      
      policeIcon <- makeIcon(iconUrl= "http://www.freeiconspng.com/uploads/security-police-icon-24.png",
                             iconWidth = 50, iconHeight = 50)
      
      leaf <- leaf %>%
        addMarkers(lng = pstation()$longitude, lat = pstation()$latitude, popup= popupps,
                   icon = policeIcon)
    }
    
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
 
      schoolIcon <- makeIcon(iconUrl= "http://www.freeiconspng.com/uploads/school-lessons-icon-png-47.png",
                             iconWidth = 20, iconHeight = 20)
      
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
    
  if(input$bar == TRUE){
    liquor <- reactive({
      url <- "https://data.montgomerycountymd.gov/resource/6bdh-is2m.json?channel_type=On%20Premise"
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
    liquorIcon <- makeIcon(iconUrl= "https://icons8.com/iconizer/files/DOT_pictograms/orig/drink_bar_cocktails.png",
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
        title = paste0("Percentage of ", input$class," by..."),
        x = "Day of Week",
        y = "Percent"
      ) +
      theme(plot.title = element_text(size = ifelse(nchar(input$class)>25,12,
                                                    ifelse(nchar(input$class)>17,14,18)),
                                      face="bold", color = "black"),
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
  

  output$crimefreq <- renderPlot({
   event <- input$map_shape_click
    if (is.null(event))
      return()
    
    clickplot <- crime()[crime()$sector == event$id & !is.na(crime()$sector), ]
    
    if(nrow(clickplot)==0) return() 
    
    clickplot <- data.frame(table(clickplot$newdate))
    clickplot$Var1 <- as.Date(clickplot$Var1, "%Y-%m-%d")
    fill <- data.frame(seq(as.Date(input$date[1]), as.Date(input$date[2]), "days"))
    names(fill) <- "Var1"
    clickplot <- merge(fill,clickplot, by="Var1",all.x = TRUE)
    clickplot$Freq[is.na(clickplot$Freq)] <- 0
    clickplot <- clickplot[1:(nrow(clickplot)-3), ]
    
    ggplot(clickplot, aes(x=Var1,y=Freq)) +
      geom_bar(stat="identity",fill="lightsteelblue3",color='black', size=.2) +
      geom_smooth(color="indianred3",se=FALSE) +
      theme(plot.title = element_text(size = ifelse(nchar(input$class)>25,12,
                                                    ifelse(nchar(input$class)>17,14,18)),
                                      face="bold", color = "black"),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_line(color = "black", size=0.75),
            axis.line.x = element_line(color = "black", size=0.75),
            axis.text = element_text(size = 12, color="black"),
            axis.title = element_blank(),
            axis.ticks = element_line(color="black"),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(size = 14, color= "black"),
            legend.title = element_blank())
  }, bg="transparent")
  
})