library(leaflet)
library(shiny)
library(tidyverse)

shinyUI(fluidPage(
  navbarPage("Access to Hospital Care", id="Nav",
             tabPanel("Map",
                      
                      leafletOutput("mymap", width="100%", height="700px"),
                      absolutePanel(id = "controls", class = "panel panel-default", 
                                    fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 200, height = "auto",
                        h3("Options"),
                        checkboxInput("hospitals", "View Hospitals (Yellow)",value=TRUE),
                        checkboxInput("under", "View Underserved (Red)",value=FALSE),
                        checkboxInput("over", "View Overserved (Blue)",value=FALSE),
                        checkboxInput("range", "View In Range (Green)",value=FALSE),
                        checkboxInput("no_hospitals", "Number of Hospitals (Orange)", value=FALSE),
                        sliderInput("min","Min. Hospitals (Orange)", 0,max(county_summary$hospitals),0, animate=animationOptions(interval=3000)),
                        sliderInput("max","Max. Hospitals (Orange)", 0,max(county_summary$hospitals),0)
                      )
               
             )
             )
))