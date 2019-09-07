# MoCo Crime Explorer
This repository contains code for an R Shiny application that accesses crime data through an API and displays it on a Leaflet map.

The live application can be viewed on <a href='https://brandonkopp.shinyapps.io/MoCo-Crime-Explorer/'>Shinyapps.io</a>.

<img src='https://github.com/brandonkopp/MoCo-Crime-Explorer/blob/master/www/Overall.png' width="700">

## UPDATE (2019-09-07)
There were a couple crimes posted to the API with bad coordinates (like in the southern hemisphere bad) and that was causing problems with how I chose a starting point and with how the heatmap worked. So I added filters to screen out points that are far outside the borders of Montgomery County and set the map view to always be the center of the county.

## UPDATE (2018-06-28)
Montgomery County changed the format of the data passed through the API which broke the application for a bit. I have updated the code to compensate for this. They also changed the coding scheme for crimes.  Finally, I added the "firefly" map option which displays brightly colored points of crime on a black Leaflet map.