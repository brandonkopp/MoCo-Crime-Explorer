require(shiny)
require(curl)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(jsonlite)
library(ggplot2)
library(scales)
library(KernSmooth)
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(xlsx)

#Create object for "Crime" dropdown menu
labels <- c("All Crime","All Crime (except Petty & Financial)","Violent Crime",
            "Financial Crimes (e.g., Forgery)","Petty Offenses","-------- Individual Crimes ---------",
            "Homicide","Sex Offenses","Robbery","Assault","Burglary","Larceny","Auto Theft",
            "Arson", "DUI")

#SHINY UI
shinyUI(navbarPage("MoCo Crime Explorer",id="nav",
  tabPanel("Interactive map",                 
   div(class="outer",
       tags$head(
         includeCSS("styles.css")
       ),
       
      #Display Map
      leafletOutput("map", width = "100%", height = "100%"),
      #Banner Across Map With Crime Label
      tags$div(id="master",style="z-index:500;",
               textOutput("heading")
      ),
      #Left Panel
      absolutePanel(top = 60, left = 50, class = "panel panel-default", 
                    bottom = "auto", height="auto", width=290,fixed = TRUE,
                    style = "z-index:500;opacity: 0.80;padding: 10px; border-bottom: 1px solid #CCC; background: #e5f2ff;",
                    h3(textOutput("txt")),
                    radioButtons("maptype","Map Type:",
                                 choices = list("Points" = 1, "Heatmap" = 2,"Grid" = 3,"Firefly"=4),selected = 1),
                    dateRangeInput("date", "Date Range:",
                                   start  = Sys.Date()-30,
                                   end    = Sys.Date(),
                                   min    = "2013-07-01",
                                   max    = date(),
                                   separator = " - "),
                    selectInput("class", "Crime:", choices = list(
                      Crime_Types = c("All Crime","All Crime (except Petty & Financial)","Violent Crime",
                                                     "Financial Crimes (e.g., Forgery)","Petty Offenses"),
                      Individual_Crimes = c("Homicide","Sex Offenses","Robbery","Assault","Burglary","Larceny",
                                            "Auto Theft","Arson", "DUI")),
                                selected = "All Crime (except Petty & Financial)"),
                    tags$b("____________________________________"),tags$br(),
                    h4("Add Layers"),"Works Best With the Heatmap",
                    checkboxInput("school", "Schools"),
                    checkboxInput("police", "Police Stations"),
                    checkboxInput("hospital","Hospitals"),
                    checkboxInput("liquor","Liquor Stores"),
                    checkboxInput("bar","Bar/Restaurant")
      ),
      #Right Graph Panel
      conditionalPanel(condition = "input.maptype == '1' | input.maptype == '4'",
                    absolutePanel(top = 19, right = 10, width=320, class = "panel panel-default", 
                                bottom = "auto", height="auto", fixed = FALSE, draggable = TRUE,
                                style = "z-index:500;opacity: 0.80;padding: 2px; border-bottom: 1px solid #CCC; background: #e5f2ff;",
                                plotOutput("dayofweek", height = 200),
                                plotOutput("hourofday", height = 200, width=320)
                                )
      ),
      #Right Heatmap Adjust Panel
      conditionalPanel(condition = "input.maptype == '2'",
                   absolutePanel(top = 19, right = 10, width=275, class = "panel panel-default", 
                                bottom = "auto", height="auto", fixed = FALSE, draggable = TRUE,
                                style = "z-index:500;opacity: 0.80;padding: 10px; border-bottom: 1px solid #CCC; background: #e7adb7;",
                                h4("Adjust the Heatmap"),
                                sliderInput("band","Bandwidth:",min=0.002,max=0.020,step = 0.002,value=0.004 ),
                                sliderInput("thresh","Threshold:",min=5,max=50,step=5,value=30),
                                tags$b("____________________________________"),
                                h4("Style the Heatmap"),
                                selectInput("color", label = "Color Scheme:",
                                            choices = c("RdBu","RdGy","RdYlGn","RdYlBu"), selected = "RdBu"),
                               sliderInput("opacity","Opacity:",min=.2,max=1,step=.1,value=.5)
                               )
      ),
      #Right Polygon Adjust Panel
      conditionalPanel(condition = "input.maptype == '3'",
                       absolutePanel(top = 19, right = 10, width=275, class = "panel panel-default", 
                                     bottom = "auto", height="auto", fixed = FALSE, draggable = TRUE,
                                     style = "z-index:500;opacity: 0.80;padding: 10px; border-bottom: 1px solid #CCC; background: #b8e186;",
                                     h4("Adjust the Grid Map"),
                                     sliderInput("box","Grid Size (in meters):",min=500,max=3500,step=500,value=1500),
                                     sliderInput("threshold","Threshold:",min=0,max=20,step=2,value=0),
                                     tags$b("____________________________________"),
                                     h4("Crime Frequency"),
                                     plotOutput("crimefreq",height = 175)
                                     # selectInput("polcolor", label = "Color Scheme:",
                                     #             choices = c("RdBu","RdGy","RdYlGn","RdYlBu"), selected = "RdBu"),
                                     # sliderInput("polopacity","Opacity:",min=.2,max=1,step=.1,value=.5)
                       )
      )
   ),
   #Bottom Left Citations
   tags$div(id="cite",
            'Source: ', tags$a(href="https://data.montgomerycountymd.gov/developers/docs/crime","dataMontgomery API"),".", tags$br(),
            tags$i("This page is a ", tags$a(href="http://www.brandonkopp.com","brandonkopp.com"), " creation.")
   )
),
tabPanel("Documentation",
         fluidRow(
           column(1,
                  ''
           ),
           column(10,
                  h2("Overview"),
                  tags$p("Ever since I started experimenting with R, I've been attracted to free and open datasets. Since I live in Montgomery County, MD, just outside of Washington, DC, discovering the ",
                         tags$a(href='"https://data.montgomerycountymd.gov', "county's open data portal"), " has been so much fun. This application uses four of many datasets provided through this site."),
                  tags$p("The ", tags$b("Montgomery County, MD Crime Explorer"), " application allows the user to visualize current crime reports.  The reports are drawn from ",
                  tags$a(href="https://data.montgomerycountymd.gov/Public-Safety/Crime/icn6-v9z3","dataMontgomery API"), " each time the user logins in or changes options on the page."),
                  tags$p("This application allows the user three different ways to visualize crime data:"),
                  tags$ol(
                    tags$li(tags$b("Points Map - "), "Each report is plotted on a map by its latitude and longitude. Clicking on the points provides detail about the crime committed."),
                    tags$li(tags$b("Heatmap - "), "Kernel density estimation is used to summarize the co-occurrence of crime within a certain geographic area. The parameters and aesthetics of the heatmap can be tuned using on-screen controls."),
                    tags$li(tags$b("Grid Map - "), "A color gradient shows how many crimes have been committed in a particular area. You can adjust the size of the areas using a slider."),
                    tags$li(tags$b("Plots - "), "Several charts are provided showing the distribution of crimes across time.")
                    ),
                  h2("How To Use This Application"),
                  tags$p("You can select a ", tags$b("date range"), " using the date pickers in the upper left-hand corner of the screen."),
                  tags$p("You can also select the ", tags$b("type of crime"), " using the dropdown box."),
                  tags$p("Once you have selected the subset of crimes you are intersted in, you can choose between the ", tags$b("points map"), " and ", tags$b("heatmap"), " using the radio buttons."),
                  tags$p("When you select 'Heatmap' as a map type, the plots on the right-hand side of the screen will be replaced by a new menu that allows you to ", tags$b("make adjustments to the heatmap.")),
                  tags$p("When you select 'Grid' as a map type, another menu appears that allows you to ", tags$b("make adjustments to the grid map.")),
                  tags$p("If you would like to see crime in relation to community features like schools and hospitals or businesses like liquor stores, you can ", tags$b("add layers"), " by clicking one of the checkboxes."),
                  tags$img(src="layout.png", align = "center", width="950"),
                  h2("Understanding the Heatmap"),
                  tags$p("The heatmap is actually a ", tags$a(href='https://en.wikipedia.org/wiki/Kernel_density_estimation', "kernel density plot"), " overlaid on the map.",
                         "The kernel density plot shows clusters of points (or the density of the points) using a color gradient."),
                  tags$p("You can adjust two parameters of the heatmap (see Figure 2 for an illustration): "),
                  tags$ol(
                    tags$li(tags$b("Bandwidth - "), "The bandwidth is the space between color gradients. A higher bandwidth allows for a less strict definition of which points are considered 'close together.' This results in more spread out and diffuse blobs of color."),
                    tags$li(tags$b("Threshold - "), "The threshold sets the bottom limit of the color gradient. Any areas with values below the threshold will be cut off. This can be useful in limiting the colors to only the most dense crime areas.")
                  ),
                  tags$img(src="kernel2.png", align = "center", width="950"),
                  h2("Find Out More."),
                  tags$ul(
                    tags$li("See the full code for this application on ", tags$a(href="https://github.com/brandonkopp/MoCo-Crime-Explorer", "Github")),
                    tags$li("See my summary of this application on ", tags$a(href="http://brandonkopp.github.io/MoCoCrimePresentation/#/", "Github.io")),
                    tags$li("Check out my writeups on other R data analyses at ", tags$a(href='http://rpubs.com/brandonkopp', "RPubs")),
                    tags$li("See all of the other places I exist on the Internets at my website, ", tags$a(href="http://www.brandonkopp.com", "brandonkopp.com"))
                  ),
                  h2("Thank You For Viewing!")
                  )
           )
         )
)
)