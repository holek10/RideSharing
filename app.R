library(shiny)
library(RJSONIO)
library(RCurl)


tempData <- readRDS("data.rds") 

calculateDistance <- function(latLng, waypoints, method = "euclidean") {
  #waypoints <- waypoints[-1,]
  # calculate difference between 
  waypointsDim <- dim(waypoints)[1]
  difference <- matrix(NA, nrow = waypointsDim, ncol = 2)
  for (i in c(1:waypointsDim) ) {
    difference[i,] <- latLng - waypoints[i,]
  }
  #minDistance <- min(rowSums(tempMatrix))
  
  # select method for calculating the distance (default: Euclidean distance)
  if (method == "euclidean") minDistance <- min(sqrt(rowSums(difference^2)))
  else if (method == "manhattan") minDistance <- min(rowSums(abs(difference)))
  else if (method == "quadrance") minDistance <- min(rowSums(difference^2))
  else if (method == "differeces") minDistance <- min(difference) 
  else stop(sprint("Unknown method: %s", method))
  return(minDistance)
}

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
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

fieldsMandatory <- c("homeAddress", "name", "team")  
oneFieldMandatory <- c("homeAddress")

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

textInputNormal <- function(inputId, label, value = "", placeholder = "", ...)
{
  div(class="form-group shiny-input-container",
      tags$label(label, 'for' = inputId),
      tags$input(id = inputId, type = "text", value = value, class = "form-control", placeholder = placeholder, ...)
      )
}


textInputRow<-function (inputId, label, value = "", placeholder = "") 
{
  div(style="display:inline-block; width: 48%;", class="form-group shiny-input-container",
      tags$label(label, 'for' = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="form-control", placeholder = placeholder))
}

# css <- "
# .shiny-output-error { visibility: hidden; }
# .shiny-output-error:before {
# visibility: visible;
# content: ' Wrong address.  '; }
# }
# "



ui <- navbarPage("Ridesharing with your colleagues",
        tabPanel("Interactive map", #shinyjs::useShinyjs(),
          tags$head(
            # Include our custom CSS
            includeCSS("styles.css")
          ),
          # tags$head(
          #   tags$style(HTML("
          #                   .shiny-output-error-validation {
          #                   color: green;
          #                   }
          #                   "))
          #   ),
          #tags$style(type="text/css", css),
          
          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
            width = 350, height = "auto", 
            #textInput(inputId = 'hqAddress', label = "Unit4 HQ address:", value= 'Powstancow Slaskich 7a, Wroclaw',placeholder = "Company's address"),
            textInputNormal(inputId = "hqAddress", label = "Unit4 HQ address:", value = "Powstancow Slaskich 7a, Wroclaw", placeholder ="", disabled ="disabled" ),
            # tags$div(class="form-group shiny-input-container",
            # tags$label("for" ="hqAddress", "Unit4 HQ address"),
            # tags$input(id="hqAddress", type = "text", disabled = "distabled", class="form-control", value="Powstancow Slaskich 7a, Wroclaw")
            # ),
          #  <input id="hqAddress" type="text" class="form-control" value="Powstancow Slaskich 7a, Wroclaw" placeholder="Company&#39;s address"/>
            textInputNormal(inputId = 'homeAddress', label = "Your home address:", value= "", placeholder = "Street name, City" ),
            actionButton(inputId = 'showRouteBtn', label = 'Show route' ,class="btn-info", style="display:inline-block;" ),
            tags$div(id = "routeErrorMessage", style = "display:inline-block;", class="shiny-html-output"),
            br(),br(),
            #uiOutput("routeErrorMessage", container = tags$div, style = "display:inline-block;"),
            tags$small("Click and drag your route to better reflect the true way to work. The more waypoints the better."),
            #actionButton(inputId = 'show_waypoints', label = 'Show waypoints' ),
            hr(),
            #textInput(inputId = 'name', label = "Save the route under you name" , value = '', placeholder = 'Your name' ),
            sliderInput("time", "What time you leave home and work ?",  min = as.POSIXct("2016-02-01 05:00"),
                        max = as.POSIXct("2016-02-01 20:00"),
                        value = c(as.POSIXct("2016-02-01 07:30"), as.POSIXct("2016-02-01 16:00")),
                        timeFormat = "%H:%M", step=NULL),
            textInputRow(inputId="name", label="Your details", value = '', placeholder = 'Your name'),
            textInputRow(inputId="team", label='', value = '', placeholder = 'Your team'),
            # div(style="display:inline-block; width: 48%;",
            #     sliderInput("leavingHome", "Leaving home",  min = as.POSIXct("2016-02-01 05:00"),
            #                 max = as.POSIXct("2016-02-01 10:00"),
            #                 value = c(as.POSIXct("2016-02-01 07:30") ),
            #                 timeFormat = "%H:%M", step=NULL)
            # ),
            # div(style="display:inline-block; width: 48%;",
            #     sliderInput("leavingWork", "Leaving work",  min = as.POSIXct("2016-02-01 14:00"),
            #                 max = as.POSIXct("2016-02-01 19:00"),
            #                 value = c(as.POSIXct("2016-02-01 16:00")),
            #                 timeFormat = "%H:%M", step=NULL)
            # ),
            actionButton(inputId = 'saveRouteBtn' , label = 'Save route', class="btn-success", value = "5", 'data-value' = "9"),
            hr(),
            tags$label("Check your ridesharing colleague", 'for' = "showNearestBtn"),
            actionButton(inputId = "showNearestBtn", label = 'Show nearest routes', class = "btn-warning", width = "100%"),
            br(),br(),
            # sliderInput("slider_hours", "Hours:", min=0, max=23, value=0, step = 1),
            # sliderInput("slider_mins", "Mins:",min = 0, max = 59, value = 0, step = 1),
            # tags$input( id="ui_time", type="time",  name="input", required="required", value="13:59",
            #             placeholder="HH:mm:ss", min="08:00", max="17:00"),
            tags$small("Once your route is saved the system automatically calculates the 3 nearest routes to yours using ",
            a("Euclidean distance", href="https://en.wikipedia.org/wiki/Euclidean_distance", target="_blank"),"metric. 
            Select route to see who might give you a lift at your way to/from work :-)" )
            #htmlOutput("choose_route"),
            #, verbatimTextOutput("results")
            #,verbatimTextOutput("test2")
          ),
          
          # 
          # tags$head(tags$script(HTML('
          # Shiny.addCustomMessageHandler("jsCode",function(message) {
          #                        console.log(message)
          #                        eval(message.code);
          #                        });
          # '))),
          
          # show base map and style to full screen
          #showOutput("baseMap", "leaflet"),
          #tags$style(".leaflet {height: 100%; width: 100%;} "),

          # Load necessary Leaflet/Routing Machine JS and CSS 
          tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.0.1/leaflet.css")),
          tags$body(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.0.1/leaflet.js")),
          tags$head(tags$link(rel = "stylesheet",  href = "http://www.liedman.net/leaflet-routing-machine/dist/leaflet-routing-machine.css")),
          tags$body(tags$script(src="http://www.liedman.net/leaflet-routing-machine/dist/leaflet-routing-machine.js")),
          
          # iniate Leaflet map with default settings
          tags$div(id = "map"),
          includeScript("map.js"),
          tags$style(".leaflet {height: 100%; width: 100%;} "),
          
          # Show route when usser clicks the button
          uiOutput("routeMap") ,
          
          # Show output when user saves the route
          uiOutput("routeSave"),
          
          # Show nearest route 
          uiOutput("showNearestRoute"),
          
          
          # Testing ...
          #uiOutput('clearmap'),
          #verbatimTextOutput("temp"),
          #verbatimTextOutput("temp2"),
           
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
 // 

$('#showRouteBtn').click(function(){
                   
                      document.getElementById('saveRouteBtn').value = '87';
});      
  //     $document.on('click','#showRouteBtn',function() {
  // $('#saveRouteBtn').button( 'option', 'value', '0' )
  //                    });
                      ")
      ),
      tabPanel("Data explorer",
        column(10, dataTableOutput("table"), offset = 1)
       
        )
       # ,tabPanel( 
       #   verbatimTextOutput("temp")
       #           )
      
    )     
    

server <- function(input, output, session) {
 
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
    # enable/disable the submit button
    # updateActionButton(session, "save_route", disabled="")
    #                    label = "New label",
    #                    icon = icon("calendar"))
    #shinyjs::toggleState(id = "show_route", condition = mandatoryFilled)
  })
  

  # # 
  #  output$temp <- renderPrint({
  #    input$showNearestBtn
  #  })
  
  # addressValidate <- reactive({
  #   
  #   company_address <- input$hqAddress
  #   validate(
  #     need(all(is.na(gGeoCode(company_address))) == T, "Please...")
  #   )
  # })
  # 
  observe({
    
    showModal(
      modalDialog(
        title = "Ridesharing with colleagues from work",
       br(),"Getting to/from work has just been made easier!",br(),
       em("Economy of sharing"), "calls for new ways of communitng. This small app aims at matching a driver and a passanger, both being colleagues from work.", 
       br(),br(),
       "Would you like to see who can give you a lift?",br(),
       "Simply provide few basic information such as:",
       tags$ul(tags$li("your home address"), tags$li("usual time you leave home and work"), tags$li("your name (or nickname)"), tags$li("team you work in")),br(),
       "You will see whether any of your colleaguges take similar route to yours.",
       footer = tags$button(type="button", class="btn btn-default btn-info"  ,'data-dismiss'="modal", "Got it!"),
        size = "m",
        easyClose = F
      )
    )
    
  })
  
  output$routeErrorMessage <- renderUI({

    if (input$showRouteBtn == 0) return()
    company_address <- isolate(input$homeAddress)
    if ( all(is.na(gGeoCode(company_address))) == T ) {
      tags$p(style = "color: red; font-weight: bold;", " Wrong address. Try again")
    } 
    
  })
  
 
  coordinates <- reactive({
    
    company_address <- input$hqAddress
    as.numeric(gGeoCode(company_address))
    
  })
  
  # create reactive values and assign from 'temp'
  # values <- reactive({
  #   
  #   values <- tempData
  #   return(values)
  #   
  # })
    
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
  #values<- reactiveValues()
  
 # observe(
     # isolate({
        
        # values$id <- tempData$id,
        # values$name <- tempData$name,
        # values$address <- tempData$address,
        # values$lat <- tempData$lat,
        # values$lng <- tempData$lng,
        # values$waypoints <- tempData$waypoints
        
    #  })
   #   )
 
 #   temp <- readRDS("C:/Projects/Own projects/data.rds") 
  #
   
 # # })
 #  
 #  output$temp2 <- renderPrint({as.list(input$mydata) #reactiveValuesToList(values)
 #    })

  # values$id <- 1
  # values$name <- "Unit4 HQ"
  # values$address <- isolate(input$hqAddress)
  # values$lat <- isolate(coordinates()[1])
  # values$lng <- isolate(coordinates()[2])
  # values$waypoints <- isolate(list(as.numeric(coordinates())))
  observeEvent( input$showRouteBtn, {
    resetActionButton("saveRouteBtn", session)
  }
  )
  
  observeEvent( input$showRouteBtn, {

    updateTextInput(session, "name", value = '')
    updateTextInput(session, "team", value = '')

    # if(input$saveRouteBtn > 0){
    #   # if((input$saveRouteBtn-input$ActionButtonMemory)>0){
    #   attr(input, "readonly") <- FALSE
    #   input$saveRouteBtn <- 0 # Equalize
    #   # DO YOUR THING HERE
    # }
    
    
  })
  
  #attr(input, "readonly") <- FALSE
  #input$ActionButtonMemory <- 0
  # observe({
  #   if(input$saveRouteBtn > 0){
  #    # if((input$saveRouteBtn-input$ActionButtonMemory)>0){
  #     attr(input, "readonly") <- FALSE
  #       input$saveRouteBtn <- 0 # Equalize
  #       # DO YOUR THING HERE
  #   }
  #   
  # })
  
  output$temp <- renderPrint({
    #input$saveRouteBtn 
    class(input$time[1])
    })
 #  routeSave <- reactive({
 # output$routeSave <- renderUI({
#  observe({
 # input$saveRouteBtn
 
   observeEvent( input$saveRouteBtn, {
     
   # values <- isolate(values)
    # return routing information obtained from Routing Machine, extract waypoints and 'home' coordinates 
    routingInfo <- as.list(isolate(input$mydata))
    latitude <- as.numeric(routingInfo[names(routingInfo) == "latLng.lat"])
    longitude <- as.numeric(routingInfo[names(routingInfo) == "latLng.lng"])          
    homeLatitue <- latitude[length(latitude)]
    homeLongitude <- longitude[length(longitude)]
    #data.frame(lat = as.numeric(unlist(x[names(x) == "latLng.lat"])), lng = as.numeric(unlist(x[names(x) == "latLng.lng"])))
    #k <- cbind(as.numeric(x[names(x) == "latLng.lat"]),as.numeric(x[names(x) == "latLng.lng"]))
    #do.call(c, apply(k, 1, list))
    # add and store relevant information for further use (id, name, home address)
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
    #saveRDS(values, "data.rds")
    saveRDS(reactiveValuesToList(values), "data.rds")
    })
  
 # observeEvent( input$showRouteBtn, { showNotification(ui = "Route has been plotted",  type = "message")})
  
  # output$baseMap <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = -93.85, lat = 37.45, zoom = 4)
  #   
  # })
  # 
  
  
  # output$baseMap <- renderMap({
  # 
  # 
  #   #input$show_route
  #   baseMap = Leaflet$new()
  #   baseMap$setView(c(coordinates()), 13)
  #   baseMap$marker(c(coordinates()), bindPopup = "Unit4 headquarter  ('Globis')")
  #   #baseMap$tileLayer(provider = 'Stamen.Watercolor')
  #   #baseMap$tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png')
  #   #baseMap$tileLayer('http://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png')
  #   #baseMap$tileLayer('http://korona.geog.uni-heidelberg.de/tiles/roads/x={x}&y={y}&z={z}')
  #   baseMap$tileLayer('https://api.mapbox.com/styles/v1/mapbox/streets-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiaG9sZWsiLCJhIjoiY2l0dG81ajB1MDAwNDJvcW14bWxsdWN3diJ9.Pe6D3DGCxZfZV7ZQp_-CTA')   
  #   #baseMap$tileLayer('//mt{s}.googleapis.com/vt?x={x}&y={y}&z={z}')
  #   #baseMap$fullScreen(TRUE)
  #   #baseMap$set(width = "1000", height = "600")
  #   #baseMap$enablePopover(TRUE)
  #   baseMap
  # })
  
  
#   output$clearmap <- renderUI({
#     
#     input$showRouteBtn
#     tags$body(tags$script(HTML(
#       "
#       var routeControl
#       //var routeArray
#       if (routeControl != null){
#       //routeArray = routeControl.getWaypoints().length;  
#       //map.removeControl(routeControl);
#       //map.routeControl.setWaypoints([]);
#       routeControl.spliceWaypoints(0, 2);
#   
#       //routeControl.getPlan().setWaypoints([]);
#       }
# ")))
#   })
  
  output$routeMap <- renderUI({


  #routeMap <- reactive({
 # observeEvent( 
  if (  input$showRouteBtn == 0 ) return()
  
  # if (all(is.na(gGeoCode(input$homeAddress))) == T  ) return()
   
     
   
    work_address <- isolate(coordinates())
    home_address <- isolate(gGeoCode(input$homeAddress))
    mywaypoints = list(as.numeric(work_address), as.numeric(home_address))
    #mywaypoints = list(c(40.74119, -73.9925), c(40.73573, -73.99302))
    tags$body(tags$script(HTML(sprintf(
      "
// var routeControl 
//var routeArray
//if (routeControl != null){
//routeArray = routeControl.getWaypoints().length;  
//map.removeControl(routeControl);
//map.routeControl.setWaypoints([]);
//routeControl.spliceWaypoints(0, 2);
//routeControl.setWaypoints([]);
//routeControl.getPlan().setWaypoints([]);
//}
      
var mywaypoints = %s
      var routeControl = L.Routing.control({
      
      waypoints: [
      L.latLng.apply(null, mywaypoints[0]),
      L.latLng.apply(null, mywaypoints[1])
      ],
      show: false,
      router: L.Routing.mapbox('pk.eyJ1IjoiaG9sZWsiLCJhIjoiY2l0dG81ajB1MDAwNDJvcW14bWxsdWN3diJ9.Pe6D3DGCxZfZV7ZQp_-CTA')
      }).addTo(map)

 // L.Routing.errorControl(routeControl).addTo(map);
      //routeControl.hide();
//routeControl.spliceWaypoints(0, 2);
   // routeError.hide();

      ", RJSONIO::toJSON(mywaypoints)
    ))))
    
  
    
})
  
  
  # output$test2 <- renderPrint({
  #   
  #   n <- as.numeric(isolate(input$route_choice))
  #  
  #   if (length(n) == 0) { mywaypoints <- NULL
  #   } else {
  #       mywaypoints <- values$waypoints[[n]]
  #       mywaypoints <- do.call(c, apply(mywaypoints, 1, list))
  #   }
  #   #class(mywaypoints)
  #   mywaypoints
  # })
  
#   output$routeSaved <- renderUI({
#     
#    # observeEvent( 
#     input$plot_route
#     #work_address <- isolate(coordinates())
#     #home_address <- isolate(gGeoCode(input$adres))
#     route_number <- as.numeric(isolate(input$route_choice))
#     
#     if (length(route_number) ==0) { mywaypoints <- NULL
#     } else {
#       mywaypoints <- values$waypoints[[route_number]]
#       mywaypoints <- do.call(c, apply(mywaypoints, 1, list))
#     }
#     #mywaypoints = list(as.numeric(work_address), as.numeric(home_address))
#     #mywaypoints = list(c(40.74119, -73.9925), c(40.73573, -73.99302))
#     #waypoints_count <- length(mywaypoints)
#     #script_part <- sprintf(rep(' L.latLng.apply(null, mywaypoints[%s]),' , waypoints_count), c(0:(waypoints_count-1)))
#     #gsub("\\s*\\c\\(|)$+|\\s", "", xxx)
#     
#     waypoints_string <- paste(gsub("\\s*\\c\\(|)$+|\\s", "", paste("L.latLng(", mywaypoints, ")")), collapse = ", ")
#     
#     route_name <- paste("This is route of: ",values$name[[route_number]]) 
#     #route_name <- "sth"
#     #mywaypoints = list(c(40.74119, -73.9925), c(40.73573, -73.99302), c(40.71573, -73.98302))
#     tags$body(tags$script(HTML(paste("
#    //   if (routeControl2 != null){
# //map.removeControl(routeControl2);}
# 
# var routeControl2 = L.Routing.control({
#       waypoints: [", waypoints_string, "
#       ],
#      
#       
#     //  lineOptions : {
#     //    addWaypoints: false,
#     //  styles: [
#     //  {color: 'blue', opacity: 0.8, weight: 3}
#     //  ]
#    //   }, 
#       createMarker: function() { return null; },
#       routeLine: function(r) {
# 	    var line = L.Routing.line(r, { 
#         addWaypoints: false ,
#         styles: [
#           {color: 'blue', opacity: 0.8, weight: 3}
#         ]
#       });
#       line.eachLayer(function(l) {
#         l.bindPopup('",route_name,"');
#       //  l.on('mouseover', function(e) {l.openPopup(); });
#      //   l.on('mouseout', function (e) {l.closePopup(); });
#       });
#       return line;
#       }
# 
#       }).addTo(map)
#       
#       routeControl2.hide()"))))
#   
# })

  
   output$showNearestRoute <- renderUI({
     
    if( input$showNearestBtn == 0 ) return()
   #  input$showNearestBtn
    
  #observeEvent( input$showNearestBtn, {  
    routeValues <- isolate(values)
    #routeValues <- isolate(values())
    # get number of stored routes 
    routeNumber <- max(routeValues$id)
    # take user home address coordinates
    currentLatLng <- c(routeValues$lat[routeNumber], routeValues$lng[routeNumber])
    # take all stored waypoints excluding HQ coordinates (stored at position #1)
    waypointList <- routeValues$waypoints[-1]
    # calculate distance
    ids <- 1:c(routeNumber-1)
    distance <- lapply(ids, function(x) calculateDistance(latLng = currentLatLng, waypoints = waypointList[[x]], method = "euclidean"))
    #distance <- calculateDistance(latLng = currentLatLng, waypoints = waypointList[[x]], method = "euclidean")
    
    # get indices of top 3 nearest routes
    sortedIndex <- sort(structure(unlist(distance), names = c(ids + 1) ) ) # add +1 for previously removed #1 index (i.e. HQ address coordinates)
    finalIndex <- as.numeric(names(sortedIndex[sortedIndex != 0][1:3])) 
    
    # prepare waypoints and names to be ploted on map
    indexWaypoints <- lapply(routeValues$waypoints[finalIndex], round, 5)  # round to 5 decimal places
    indexNames <- routeValues$name[finalIndex]
    indexTeams <- routeValues$team[finalIndex]
  
    # prepare top 3 route coordinates, names and colors
    count <- c(1:3)
    waypointString <- lapply(1:3, function(x) paste(gsub("\\s*\\c\\(|)$+|\\s", "", paste("L.latLng(", do.call(c, apply(indexWaypoints[[x]], 1, list)) , ")")), collapse = ", "))
    routeName <- lapply(1:3, function(x) paste("This is route of <b>",indexNames[x],"</b> from team <b>",indexTeams[x],"</b>. <br>Reach out to that person and get more details."))
    colors <- c("blue", "green", "orange")
    
    # prepare script to load 
    lapply(count, function(x) tags$body(tags$script(HTML(sprintf("
        
    // var routeControl2 
    // if (routeControl2 != null){routeControl2.setWaypoints([]);}
      
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
      router: L.Routing.mapbox('pk.eyJ1IjoiaG9sZWsiLCJhIjoiY2l0dG81ajB1MDAwNDJvcW14bWxsdWN3diJ9.Pe6D3DGCxZfZV7ZQp_-CTA')
      }).addTo(map)
      routeControl%s.hide()", count[x], waypointString[[x]], colors[x], routeName[x], count[x] )))))
    
  })
  
 #  outputOptions(output, "showNearestRoute", suspendWhenHidden = TRUE)
   
  # output$results = renderPrint({
  #   x <<- as.list(input$mydata)
  #   #data.frame(lat = as.numeric(unlist(x[names(x) == "latLng.lat"])), lng = as.numeric(unlist(x[names(x) == "latLng.lng"])))
  #   k <- cbind(as.numeric(x[names(x) == "latLng.lat"]),as.numeric(x[names(x) == "latLng.lng"]))
  #   do.call(c, apply(k, 1, list))
  # })
  

  output$table <- renderDataTable({
    
    tableValues <- values
    #tableValues <- isolate(values()),
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




#lapply(1:11, function(x) calculateDistance(latLng = latLng, waypoint_list[[x]]))

shinyApp(ui, server)