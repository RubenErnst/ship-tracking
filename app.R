library(shiny)
library(dplyr)
library(leaflet)
library(htmltools)
library(RSQLite)

AISdb <- DBI::dbConnect(RSQLite::SQLite(), "~/AIS_MAIN_DB.sqlite")

AIS_Name_idx <- as.character(as.vector(dbGetQuery(AISdb, "SELECT name FROM ais_namecalltype;"))[,1])
AIS_CallID_idx <- as.character(as.vector(dbGetQuery(AISdb, "SELECT callsign FROM ais_namecalltype;"))[,1])
AIS_Type_idx <- as.character(as.vector(dbGetQuery(AISdb, "SELECT DISTINCT ship_type FROM ais_namecalltype ORDER BY ship_type;"))[,1])

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  # Output: Plot map including requested vessels ----
  leafletOutput("mapPlot", height = "100%", width = "100%"),
  # Sidebar panel for inputs ----
  absolutePanel(top = 150, left = 10,
    selectizeInput(
      "vName", "Search by Vessel Name",
      choices = AIS_Name_idx,
      options = list(
      placeholder = "Select",
      onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    selectizeInput(
      "vCall", "Search by Vessel Call ID",
      choices = AIS_CallID_idx,
      options = list(
        placeholder = "Select",
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    selectizeInput(
      "vType", "Search by Vessel Type",
      choices = AIS_Type_idx,
      options = list(
        placeholder = "Select",
        onInitialize = I('function() { this.setValue(""); }')
      ),
      multiple = TRUE
    ),
    verbatimTextOutput('out1'),
    style = "display: inline-block, vertical-align: center;",
    actionButton("reset_filters", "Reset Filters", width = "100%"),
    conditionalPanel(condition = "input.vName != ''", actionButton("show_path", "Show Path", width = "100%", style = "background-color: RGB(24,129,174); color: #fff; margin-top: 5px;")),
    style = "background-color: rgba(255, 255, 255, 0.7); padding: 20px; border-radius: 10px; box-shadow: 0 4px 20px 0 rgba(0, 0, 0, 0.4), actionButton.show_path{margin-top: 20px};"
  )
)

# Data pre-processing ----
filter_names <- c("input$vName", "input$vCall", "input$vType")
filters <- c("AIS_Name_idx %in% input$vName", "AIS_CallID_idx %in% input$vCall","AIS_Type_idx %in% input$vType")
checknull <- NULL

# Define server logic ----
server <- function(input, output, session) {
  shipIcon <- makeIcon("ship.png", "ship.png", 18, 18)
  # Generate a plot of the requested vessels on the map ----
  output$mapPlot <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(6.8, 55.7, zoom = 5)
  })
  
  observe({
    #If all inputs are empty, show all vessels
    if (input$vName == "" && input$vCall == "" && is.null(input$vType)){
      df <- dbGetQuery(AISdb, "SELECT * FROM ais_namecalltype;")
      #If Name input has a value
    } else if (input$vName != "") {
      df <- dbGetQuery(AISdb, paste0("SELECT * FROM ais_namecalltype WHERE name = '", input$vName, "';"))
      updateSelectizeInput(session, "vCall", choices = AIS_CallID_idx, selected = df$callsign, server = TRUE)
      updateSelectizeInput(session, "vType", choices = AIS_Type_idx, selected = df$ship_type, server = TRUE)
      #If Callsign input has value
    } else if (input$vCall != ""){
      df <- dbGetQuery(AISdb, paste0("SELECT * FROM ais_namecalltype WHERE callsign = '", input$vCall, "';"))
      updateSelectizeInput(session, "vName", choices = AIS_Name_idx, selected = df$name, server = TRUE)
      updateSelectizeInput(session, "vType", choices = AIS_Type_idx, selected = df$ship_type, server = TRUE)
      #If Type input has value
    } else if (!is.null(input$vType)){
      if (length(input$vType) > 1){
        numquer <<- ""
        for (i in 2:length(input$vType)){
          numQuery <<- paste0(numquer, paste0(" OR ship_type = '", input$vType[i],"'"))
        }
      }
      else{
        numQuery <<- ""
      }
      df <- dbGetQuery(AISdb, paste0("SELECT * FROM ais_namecalltype WHERE ship_type = '", input$vType[1], "'", numQuery, ";"))
      updateSelectizeInput(session, "vName", choices = df$name, server = TRUE)
      updateSelectizeInput(session, "vCall", choices = df$callsign, server = TRUE)
    }
    
    leafletProxy(mapId = "mapPlot", data = df) %>%
      clearMarkerClusters() %>%
      clearShapes() %>%
      clearMarkers() %>%
      addMarkers(icon = shipIcon,
                 lng = ~initLon,
                 lat = ~initLat,
                 popup = ~paste0("<b style = 'color: RGB(24,129,174);'>", htmlEscape(name), "</b></br><b>Callsign: </b>", htmlEscape(callsign), "</br><b>Vessel Type: </b>", htmlEscape(ship_type)),
                 popupOptions = (closeButton = FALSE),
                 clusterOptions = markerClusterOptions(disableClusteringAtZoom = 10)
      )
  })
  
  #Show the Path if button is clicked
  observeEvent(input$show_path, {
    df_path <- transform(dbGetQuery(AISdb, paste0("SELECT time_stamp, latitude, longitude FROM ais_20180601_new WHERE name = '", input$vName, "' AND latitude <> '91.000000' AND rowID % 5 = 0;")), latitude = as.double(latitude), longitude = as.double(longitude))
    leafletProxy(mapId = "mapPlot", data = df_path) %>%
      addPolylines(lat = ~latitude, lng = ~longitude, color = "red") %>%
      addLabelOnlyMarkers(lat = ~latitude, lng = ~longitude, label = ~time_stamp)
  })
    
    observeEvent(input$reset_filters, {
      updateSelectizeInput(session,'vName', choices=AIS_Name_idx)
      updateSelectizeInput(session,'vCall', choices=AIS_CallID_idx)
      updateSelectizeInput(session,'vType', choices=AIS_Type_idx)
      leafletProxy(mapId = "mapPlot") %>%
        setView(6.8, 55.7, zoom = 5)
    })
}

shinyApp(ui, server)
