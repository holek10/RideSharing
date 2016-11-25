# Load Required Packages ----
library(shiny)
library(RJSONIO)
library(RCurl)

# Local data source:
tempData <- readRDS("data.rds") 

## Helper Functions to Get and Process Geo Data ----

# Distance between location points (lat / lng)
calculateDistance <- function(latLng, waypoints, method = "euclidean") {
  # calculate difference between two locations (home and all other stored route waypoints)
  waypointsDim <- dim(waypoints)[1]
  difference <- matrix(NA, nrow = waypointsDim, ncol = 2)
  for (i in c(1:waypointsDim) ) {
    difference[i,] <- latLng - waypoints[i,]
  }
  # select method for calculating the distance (default: Euclidean distance)
  if (method == "euclidean") minDistance <- min(sqrt(rowSums(difference^2)))
  else if (method == "manhattan") minDistance <- min(rowSums(abs(difference)))
  else if (method == "quadrance") minDistance <- min(rowSums(difference^2))
  else if (method == "differeces") minDistance <- min(difference) 
  else stop(sprint("Unknown method: %s", method))
  return(minDistance)
}

# Construct URL for fetching geocode data
constructGeocodeURL <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

# Get latitude and longitude for given address
gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- constructGeocodeURL(address)
  doc <- getURL(u)
  x <- fromJSON(doc, simplifyDataFrame = F)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location[1]
    lng <- x$results[[1]]$geometry$location[2]
    return(c(lat, lng))
  } else {
    return(c(NA,NA))
    # return(stop("Wrong address"))
  }
}

## Helper Functions for UI ---- 

# Define mandatory fields to fill out
fieldsMandatory <- c("homeAddress", "name", "team")  
oneFieldMandatory <- c("homeAddress")

# JS code to control Action buttons
disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

enableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',false)"
                                             ,sep="")))
}

resetActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').val('0').change();"
                                             ,sep="")))
}

# Wrapper for inputs to extend original HTML <input> tag with 'disable' option
textInputNormal <- function(inputId, label, value = "", placeholder = "", ...)
{
  div(class="form-group shiny-input-container",
      tags$label(label, 'for' = inputId),
      tags$input(id = inputId, type = "text", value = value, class = "form-control", placeholder = placeholder, ...)
      )
}

# Wrapper for inputs to extend original HTML <input> tag and place them side-by-side
textInputRow<-function (inputId, label, value = "", placeholder = "") 
{
  div(style="display:inline-block; width: 48%;", class="form-group shiny-input-container",
      tags$label(label, 'for' = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="form-control", placeholder = placeholder))
}


## Create UI Component (navbar page) ----
ui <- navbarPage("Ridesharing with your friends",
        tabPanel("Interactive map",
          tags$head(
            # Include our custom CSS
            includeCSS("styles.css")
          ),
          # Create right hand side semi-transparent panel
          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
            width = 350, height = "auto", 
            textInputNormal(inputId = "hqAddress", label = "Company HQ address:", value = "Powstancow Slaskich 7a, Wroclaw", placeholder ="", disabled ="disabled" ),
            textInputNormal(inputId = 'homeAddress', label = "Your home address:", value= "", placeholder = "Street name, City" ),
            actionButton(inputId = 'showRouteBtn', label = 'Show route' ,class="btn-info", style="display:inline-block;" ),
            tags$div(id = "routeErrorMessage", style = "display:inline-block;", class="shiny-html-output"),
            br(),br(),
            tags$small("Click and drag your route to better reflect the true way to work. The more waypoints the better."),
            hr(),
            sliderInput("time", "What time you leave home and work ?",  min = as.POSIXct("2016-02-01 05:00"),
                        max = as.POSIXct("2016-02-01 20:00"),
                        value = c(as.POSIXct("2016-02-01 07:30"), as.POSIXct("2016-02-01 16:00")),
                        timeFormat = "%H:%M", step=NULL),
            textInputRow(inputId="name", label="Your details", value = '', placeholder = 'Your name'),
            textInputRow(inputId="team", label='', value = '', placeholder = 'Your team'),
            actionButton(inputId = 'saveRouteBtn' , label = 'Save route', class="btn-success", value = "5", 'data-value' = "9"),
            hr(),
            tags$label("Check your ridesharing friend", 'for' = "showNearestBtn"),
            actionButton(inputId = "showNearestBtn", label = 'Show nearest routes', class = "btn-warning", width = "100%"),
            br(),br(),
            tags$small("Once your route is saved the system automatically calculates the 3 nearest routes to yours using ",
            a("Euclidean distance", href="https://en.wikipedia.org/wiki/Euclidean_distance", target="_blank"),"metric. 
            Select route to see who might give you a lift at your way to/from work :-)" )
          ),

          # Load necessary Leaflet/Routing Machine JS and CSS 
          tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.0.1/leaflet.css")),
          tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.0.1/leaflet.js")),
          tags$head(tags$link(rel = "stylesheet",  href = "http://www.liedman.net/leaflet-routing-machine/dist/leaflet-routing-machine.css")),
          tags$head(tags$script(src="http://www.liedman.net/leaflet-routing-machine/dist/leaflet-routing-machine.js")),
          
          # initiate Leaflet map with default settings
          tags$div(id = "map"),
          includeScript("map.js"),
          tags$style(".leaflet {height: 100%; width: 100%;} "),
          
          # Show route when usser clicks the button
          uiOutput("routeMap") ,
          
          # Show output when user saves the route
          uiOutput("routeSave"),
          
          # Show nearest route 
          uiOutput("showNearestRoute"),
        
          # Custom handler to pass values from router back to server
          tags$script("
            document.getElementById('showRouteBtn').onclick = function() {
              document.getElementById('saveRouteBtn').value = '0';
            };
            document.getElementById('saveRouteBtn').onclick = function() {
              var routeArray = new Array();
              routeArray = routeControl.getWaypoints();  
                Shiny.onInputChange('mydata', routeArray);
            };
            Shiny.addCustomMessageHandler('jsCode',function(message) {
              console.log(message)
              eval(message.code);
            });
          ")
        ),
        # Data explorer
        tabPanel("Data explorer",
          column(10, dataTableOutput("table"), offset = 1)
        )
      )     
    
## Define Server Logic  ----
server <- function(input, output, session) {
 
  # Observer to control whether user filled out mandatory fields
  # Action button is disabled by default
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)

    if(mandatoryFilled == F) {
      disableActionButton("saveRouteBtn",session)
    } else {
      enableActionButton("saveRouteBtn", session)
      }

    oneMandatoryFilled <-vapply(oneFieldMandatory,
                                 function(x) {
                                   !is.null(input[[x]]) && input[[x]] != ""
                                 },
                                 logical(1))
    #oneMandatoryFilled <- all(mandatoryFilled)
    if(oneMandatoryFilled == F) {
      disableActionButton("showRouteBtn",session)
    } else {
      enableActionButton("showRouteBtn", session)
    }
    
    if (input$saveRouteBtn == 0) {
      disableActionButton("showNearestBtn", session)
    } else {
      enableActionButton("showNearestBtn", session)
    }
  })
  
  # Show modal on application load
  observe({
    
    showModal(
      modalDialog(
        title = "Ridesharing with friends from work",
       br(),"Getting to/from work has just been made easier!",br(),
       em("Economy of sharing"), "calls for new ways of communitng. This small app aims at matching a driver and a passanger, both being friends from work.", 
       br(),br(),
       "Would you like to see who can give you a lift?",br(),
       "Simply provide few basic information such as:",
       tags$ul(tags$li("your home address"), tags$li("usual time you leave home and work"), tags$li("your name (or nickname)"), tags$li("team you work in")),br(),
       "You will see whether any of your friends take similar route to yours.",
       footer = tags$button(type="button", class="btn btn-default btn-info"  ,'data-dismiss'="modal", "Got it!"),
        size = "m",
        easyClose = F
      )
    )
  })
  
  # Error message in case wrong address is entered
  output$routeErrorMessage <- renderUI({
    if (input$showRouteBtn == 0) return()
    company_address <- isolate(input$homeAddress)
    if ( all(is.na(gGeoCode(company_address))) == T ) {
      tags$p(style = "color: red; font-weight: bold;", " Wrong address. Try again")
    } 
  })
  
  # Reactive values created from data upload
  values <- reactiveValues(id = tempData$id, 
                           name = tempData$name, 
                           team = tempData$team, 
                           address = tempData$address, 
                           lat = tempData$lat, 
                           lng = tempData$lng,
                           waypoints = tempData$waypoints,
                           timeLeaveHome = tempData$timeLeaveHome,
                           timeLeaveWork = tempData$timeLeaveWork
                           )

  # Clear content of Name and Team input, and reset action button (experimental)
  observeEvent( input$showRouteBtn, {
    updateTextInput(session, "name", value = '')
    updateTextInput(session, "team", value = '')
    resetActionButton("saveRouteBtn", session)
  })
  
  # Define logic after pressing 'Save route'
  observeEvent( input$saveRouteBtn, {
    # return routing information obtained from Routing Machine, extract waypoints and 'home' coordinates 
    routingInfo <- as.list(isolate(input$mydata))
    latitude <- as.numeric(routingInfo[names(routingInfo) == "latLng.lat"])
    longitude <- as.numeric(routingInfo[names(routingInfo) == "latLng.lng"])          
    homeLatitue <- latitude[length(latitude)]
    homeLongitude <- longitude[length(longitude)]
    # increment number each time new route is saved
    values$id <- c(values$id , max(values$id) + 1)
    # store name $ team
    values$name <- c(values$name , input$name)
    values$team <- c(values$team, input$team)
    # store home address
    values$address <- c(values$address, input$homeAddress)
    #store latitue for home address
    values$lat <- c(values$lat , homeLatitue)
    # store longitude for home address
    values$lng <- c(values$lng , homeLongitude)
    # store waypoints for the route (used later to plot the similar route)
    values$waypoints <- c(values$waypoints, list(cbind(latitude, longitude)))
    # store time leaving home and leaving work
    values$timeLeaveHome <- c(values$timeLeaveHome, strftime(input$time[1], format = "%H:%M"))
    values$timeLeaveWork <- c(values$timeLeaveWork, strftime(input$time[2], format = "%H:%M"))
    #save output to RDS file
    saveRDS(reactiveValuesToList(values), "data.rds")
  })
 
  # Display route for entered address
  output$routeMap <- renderUI({
    # Take dependency on action button
    if (input$showRouteBtn == 0) return()
    # Get inital coordinates (home and work waypoints)
    work_address <- isolate(gGeoCode(input$hqAddress))
    home_address <- isolate(gGeoCode(input$homeAddress))
    mywaypoints = list(as.numeric(work_address), as.numeric(home_address))
    #mywaypoints = list(c(40.74119, -73.9925), c(40.73573, -73.99302))
    # Add routing machine script
    tags$body(tags$script(HTML(sprintf("
      var mywaypoints = %s
      var routeControl = L.Routing.control({
        waypoints: [
        L.latLng.apply(null, mywaypoints[0]),
        L.latLng.apply(null, mywaypoints[1])
        ],
        show: false,
        router: new L.Routing.Mapbox('pk.eyJ1IjoiaG9sZWsiLCJhIjoiY2l0dG81ajB1MDAwNDJvcW14bWxsdWN3diJ9.Pe6D3DGCxZfZV7ZQp_-CTA')
        }).addTo(map)
      // Display error message if needed
      ", RJSONIO::toJSON(mywaypoints)
    ))))
  })
  
  # Display nearest routes
  output$showNearestRoute <- renderUI({
    # Take dependency on action button
    if(input$showNearestBtn == 0) return()
    # Get reactive values
    routeValues <- isolate(values)
    # Get number of stored routes 
    routeNumber <- max(routeValues$id)
    # Take user home address coordinates
    currentLatLng <- c(routeValues$lat[routeNumber], routeValues$lng[routeNumber])
    # Take all stored waypoints excluding HQ coordinates (stored at position #1)
    waypointList <- routeValues$waypoints[-1]
    # Calculate distance
    ids <- 1:c(routeNumber-1)
    distance <- lapply(ids, function(x) calculateDistance(latLng = currentLatLng, waypoints = waypointList[[x]], method = "euclidean"))
    # Get indices of top 3 nearest routes
    sortedIndex <- sort(structure(unlist(distance), names = c(ids + 1) ) ) # add +1 for previously removed #1 index (i.e. HQ address coordinates)
    finalIndex <- as.numeric(names(sortedIndex[sortedIndex != 0][1:3])) 
    # Prepare waypoints and names to be ploted on map
    indexWaypoints <- lapply(routeValues$waypoints[finalIndex], round, 5)  # round to 5 decimal places
    indexNames <- routeValues$name[finalIndex]
    indexTeams <- routeValues$team[finalIndex]
    indexLeaveHome <- routeValues$timeLeaveHome[finalIndex]
    indexLeaveWork <- routeValues$timeLeaveWork[finalIndex]
    # Prepare top 3 route coordinates, names and colors
    count <- c(1:3)
    waypointString <- lapply(1:3, function(x) paste(gsub("\\s*\\c\\(|)$+|\\s", "", paste("L.latLng(", do.call(c, apply(indexWaypoints[[x]], 1, list)) , ")")), collapse = ", "))
    routeName <- lapply(1:3, function(x) paste("This is route of <b>",indexNames[x],"</b> from team <b>",indexTeams[x],"</b>, who usually leaves home around ",indexLeaveHome[x]," and works until ",indexLeaveWork[x],".<br>Reach out to that person to get more details."))
    colors <- c("blue", "green", "orange")
    # Prepare script to load 
    lapply(count, function(x) tags$body(tags$script(HTML(sprintf("
      var routeControl%s = L.Routing.control({
        waypoints: [ %s  
        ],
        createMarker: function() { return null; },
        routeLine: function(r) {
          var line = L.Routing.line(r, { 
          addWaypoints: false ,
          styles: [
          {color: '%s', opacity: 0.7, weight: 3}
          ]
          });
          line.eachLayer(function(l) {
          l.bindPopup(' %s ');
           l.on('mouseover', function(e) {l.openPopup(); });
           l.on('mouseout', function (e) {l.closePopup(); });
          });
          return line;
          },
      show: false,
       router: new L.Routing.Mapbox('pk.eyJ1IjoiaG9sZWsiLCJhIjoiY2l0dG81ajB1MDAwNDJvcW14bWxsdWN3diJ9.Pe6D3DGCxZfZV7ZQp_-CTA')
      }).addTo(map)
      routeControl%s.hide()", count[x], waypointString[[x]], colors[x], routeName[x], count[x] )))))
  })

  # DataTable with stored values
  output$table <- renderDataTable({
    # Get reactive values
    tableValues <- isolate(values)
    data.frame(id = tableValues$id, 
               address = tableValues$address,
               lat = tableValues$lat, 
               lng = tableValues$lng,
               timeLeavingHome = tableValues$timeLeaveHome,
               timeLeavingWork = tableValues$timeLeaveWork,
               name = tableValues$name, 
               team = tableValues$team
                )
    } ,  options = list( pageLength = 10)
  )
}

# Run The Application ----
shinyApp(ui, server)
