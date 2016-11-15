library(shiny)
library(leaflet)
library(dplyr)
library(readr)

load("counties.RData")

county_summary <- read_csv("./data/county_summary.csv") %>%
  left_join(all_files_index, by=c("state","county")) %>%
  filter(is.na(file_name)==FALSE) 

line_data <- read_csv("./data/line_data.csv")

hospital_list <- read_csv("./data/hospital_list.csv") %>%
  filter(is.na(longitude)==FALSE, is.na(latitude)==FALSE) %>%
  group_by(zip,longitude,latitude) %>%
  summarise(count=n()) %>%
  mutate(content=paste(sep="","<b>Zip Code: ",zip,"</b>", 
                       "<br/>Hospitals: ", count))    
   

ui <- fluidPage(
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
                                    uiOutput("min_max")
                                    #sliderInput("min","Min. Hospitals (Orange)", 0,83,0, animate=animationOptions(interval=3000)),
                                    #sliderInput("max","Max. Hospitals (Orange)", 0,83,0)
                      )
                      
             )
  )
)


server <- function(input, output) {
   output$min_max <- renderUI({
     sliderInput("range", "Min. & Max Number of Hospitals (Orange)", 0, 83, c(0,0))
   })
   
    output$mymap <- renderLeaflet({
      
      no_hopitals <- county_summary %>%
        filter(hospitals>=min(input$range) , hospitals<=max(input$range)) %>%
        filter(index>=ifelse(input$no_hospitals==TRUE,0,99999))
      no_hopitals_files <- all_files[no_hopitals$index]
      no_hopitals_files <-(rbind(no_hopitals_files)) 
      
      over <- county_summary %>%
        filter(result==ifelse(input$over==TRUE, "Over",""))
      over_files <- all_files[over$index]
      over_files <-(rbind(over_files))

      range <- county_summary %>%
        filter(result==ifelse(input$range==TRUE, "In Range",""))
      range_files <- all_files[range$index]
      range_files <-(rbind(range_files))

      under <- county_summary %>%
        filter(result==ifelse(input$under==TRUE, "Under",""))
      under_files <- all_files[under$index]
      under_files <-(rbind(under_files))


      leaflet(hospital_list) %>%
        addProviderTiles("CartoDB.Positron")  %>%
        addGeoJSON(geojson=under_files, fillColor = "red", weight=1,
                 fillOpacity = 0.5, fill=TRUE, color="gray") %>%
        addGeoJSON(geojson=over_files, fillColor = "blue", weight=1,
                 fillOpacity = 0.5, fill=TRUE, color="gray") %>%
        addGeoJSON(geojson=range_files, fillColor = "green", weight=1,
                 fillOpacity = 0.3, fill=TRUE, color="gray") %>%
        addGeoJSON(geojson=no_hopitals_files, fill=FALSE,weight=3, color="orange") %>%        
        setView(-93.65, 38.0285, zoom = 5) %>%
          addCircleMarkers(lng=~longitude,
                         lat=~latitude, popup=~content,
                         radius=~ifelse(input$hospitals==TRUE,count,0), fillColor="#FFFF00", color="gray",
                         fillOpacity = 0.8,weight=1)})
}


shinyApp(ui=ui, server=server)
  

