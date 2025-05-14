# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(leaflet)
library(airportr)
library(shinyWidgets)
library(geosphere)
library(sp)
library(shinyjs)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),
  tags$head(
    tags$style(HTML(
      # "body {background: #ADD8E6}",
      "#selected_year {
                    font-size: 18px;
                    padding-bottom: 20px;
                  }",
      "#vis2_welcometext {
        font-size: 18px;
        padding-bottom: 20px;
        }",
      "#main-title {
        font-size:20px;
      }",
      "body {margin-left: -15px; margin-right: -15px}"
      ))

  ),
  navbarPage(
    title = div("Airline Delay Webapp", id = "main-title"),
    # tabsetPanel(
      tabPanel("Vis 1",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("year", "Select year",
                               choices = NULL,
                               selected = ""),
                   selectInput("month", "Select month",
                               choices = NULL,
                               selected = ""),
                 ),
                 mainPanel(
                   textOutput("selected_year"),
                   fluidRow(
                     column(width = 12, plotOutput("selected_plot")),
                     column(width = 6, plotOutput("selected_plot_arr")),
                     column(width = 6, plotOutput("selected_plot_dep"))
                   )
                 )
               )
      ),

    tabPanel("Vis 2",
             sidebarLayout(
               sidebarPanel(
                 selectInput("origin", "Select origin",
                             choices = NULL,
                             selected = ""),
                 selectInput("destination", "Select destination",
                             choices = NULL,
                             selected = ""),
                 actionButton("submit_button", "Enter!", disabled = TRUE)
               ),
               mainPanel(
                 textOutput("vis2_welcometext"),
                 #display map on screen
                 leafletOutput("locations") #locations is name of map
               )
             )
    ),

    tabPanel("Vis 3", "This is the page for the 3rd visualisation")
  )
)

# Define server function
server <- function(input, output) {

  month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  data <- read.csv("./unbinned_delay_count.csv")
  data$Month <- factor(data$Month, levels = month_order)
  binned_data <- read.csv("./binned_delay_count.csv")
  observe({
    updateSelectInput(inputId="year", choices=c("",unique(data$Year)), selected="")
  })

  output$selected_year <- renderText({
    if (input$year != "") {
      paste0("Data of flight delays in Year ", input$year)
    } else {
      paste0("Please select a year using the dropdown on the left panel")
    }
  })


  filtered_data_year <- reactive({
    subset(data, Year == input$year)
  })

  observe({
    updateSelectInput(inputId="month", choices=c("",month_order[unique(filtered_data_year()$Month)]))
  })

  output$selected_plot <- renderPlot({
    if (input$year == "") {
      return(NULL)
    } else {
      ggplot(filtered_data_year(), aes(x = Month, y = Arr_Delay_Count)) +
        geom_bar(stat="identity", fill = "skyblue", color = "black") +
        labs(title = "Monthly Aggregated Delay Data",
             x = "Month",
             y = "Delay Count")
    }
  })

  filtered_data_binned <- reactive({
    subset(binned_data, Year == input$year & Month == input$month)
  })

  filtered_data_binned_arr <- reactive({
    filtered_data_binned() %>% select(starts_with("Arr_")) %>% gather(key = "arrBin", value = "value")
  })

  filtered_data_binned_dep <- reactive({
    filtered_data_binned() %>% select(starts_with("Dep_")) %>% gather(key = "depBin", value = "value")
  })


  observeEvent(input$year, {
    # Render the plots as null
    output$selected_plot_arr <- renderPlot(NULL)
    output$selected_plot_dep <- renderPlot(NULL)
  })

  monthPlot_bins <- c("60-74","75-89","90-104","105-119","120-134","135-149","150-164","165-179","Above 180")

  # Create the ordered factor
  monthPlot_bins <- factor(monthPlot_bins, levels = monthPlot_bins)
  observeEvent(input$month, {
    output$selected_plot_arr <- renderPlot({
      if (input$month == "") {
        return(NULL)
      } else {
        ggplot(filtered_data_binned_arr(), aes(x = monthPlot_bins, y = value)) +
          geom_bar(stat="identity", fill = "green", color = "black") +
          labs(title = paste0("Arrival Flights Delay Data (", input$month,")"),
               x = "Delay Time (minutes)",
               y = "Delay Count")
      }
    })

    output$selected_plot_dep <- renderPlot({
      if (input$month == "") {
        return(NULL)
      } else {
        ggplot(filtered_data_binned_dep(), aes(x = monthPlot_bins, y = value)) +
          geom_bar(stat="identity", fill = "purple", color = "black") +
          labs(title = paste0("Departure Flights Delay Data (", input$month,")"),
               x = "Delay Time (minutes)",
               y = "Delay Count")
      }
    })
  })


  ##vis2:
    list_unique_origins <- c("LAX", "SFO", "ATL")
    list_unique_dests <- c("LAX", "SFO", "ATL")

    origin <- reactive(input$origin)
    destination <- reactive(input$destination)
    
    reactive_text <- eventReactive(input$submit_button, {
      paste("You have selected", input$origin, "-->", input$destination)
    })
    output$vis2_welcometext <- renderText({
      if (input$origin == "" || input$destination == "") {
        paste0("select input from left")
      } else if (input$submit_button == 0) {
        paste0("Please press the enter button")
      } else {
        reactive_text()
      }
    })
    
    reactive_leaflet <- eventReactive(input$submit_button, {
      cascade <- read.csv("./cascade.csv")
      tempOrigin <- lapply(cascade$ORIGIN, airport_location)
      cascade$originLat <- sapply(tempOrigin, function(x) x$Latitude)
      cascade$originLng <- sapply(tempOrigin, function(x) x$Longitude)

      tempDest <- lapply(cascade$DEST, airport_location)
      cascade$destLat <- sapply(tempDest, function(x) x$Latitude)
      cascade$destLng <- sapply(tempDest, function(x) x$Longitude)
      cascade$markerColour <- c("blue")
      cascade$markerColour[1] = "red"

      cascade$markerColour2 <- c("green")
      markerColour2 <- cascade$markerColour2

      markerColour <- cascade$markerColour

      icons <- awesomeIcons(icon = 'ios-close',
                            library = 'ion',
                            markerColor =  markerColour)
      icons2 <- awesomeIcons(icon = 'ios-close',
                             library = 'ion',
                             markerColor =  markerColour2)

      greenSubset <- cascade %>% filter(cascade$DEST != "LAX" & cascade$DEST != "SFO")
      polyLinesSubset <- cascade %>% filter(cascade$DEST != "LAX")

      
      #setup an empty map
      first_row_data <- gcIntermediate(c(cascade$originLng[1],cascade$originLat[1]), c(cascade$destLng[1],cascade$destLat[1]),
                                       n=100,
                                       addStartEnd=TRUE,
                                       sp=TRUE)
      locations <- leaflet(data=cascade)
      map <- addTiles(locations)
      tempMap <- addAwesomeMarkers(data = cascade, lat = ~originLat, lng = ~originLng,
                                   map = map, popup = ~ORIGIN, icon = icons) %>%
        # addPolylines(lat = ~originLat, lng = ~originLng, color = "red", opacity = 0.5, weight = ~delayed_arr, label = paste("num delayed flights:", cascade$delayed_arr)) %>%
        addPolylines(data = first_row_data, color = "red", opacity = 0.5, weight = cascade$delayed_arr[1], label = paste("num delayed flights:", cascade$delayed_arr[1])) %>%
        addAwesomeMarkers(data = greenSubset, lat = ~destLat, lng = ~destLng, popup = ~DEST, icon = icons2)
        # addPopups(lat = ~destLat, lng = ~destLng, popup =~DEST )

      polyLinesSubset <- polyLinesSubset %>% mutate(id = row.names(.))
      flights_lines <- apply(polyLinesSubset,1,function(x){
        points <- data.frame(lng=as.numeric(c(x["originLng"],
                                              x["destLng"])),
                             lat=as.numeric(c(x["originLat"],
                                              x["destLat"])),stringsAsFactors = F)
        coordinates(points) <- c("lng","lat")
        Lines(Line(points),ID=x["id"])
      })

      flights_lines <- SpatialLinesDataFrame(SpatialLines(flights_lines), polyLinesSubset)


      tempMap %>%
        addPolylines(data=flights_lines, opacity = 0.5, color = "blue")#, weight = ~delayed_dep)

    })
  
    output$locations <- renderLeaflet({
      if(input$submit_button == 0) {
        NULL
      } else {
        reactive_leaflet()
      }
    })
    
    observe({
      updateSelectInput(inputId="origin", choices=c("",sort(unique(list_unique_origins))), selected="")
      updateSelectInput(inputId="destination", choices=c("",sort(unique(list_unique_dests))), selected="")
    })

    observeEvent(input$origin, {
      selected_origin <- input$origin
      destination_choices <- list_unique_dests
      if (selected_origin != "") {
        destination_choices <- list_unique_dests[list_unique_dests != selected_origin]
      }
      updateSelectInput(inputId = "destination", choices = destination_choices, selected = input$destination)
    })

    observeEvent(input$destination, {
      selected_destination <- input$destination
      origin_choices <- list_unique_origins
      if (selected_destination != "") {
        origin_choices <- list_unique_origins[list_unique_origins != selected_destination]
      }
      updateSelectInput(inputId = "origin", choices = origin_choices, selected = input$origin)
    })

    observeEvent(c(input$origin, input$destination), {
      if (input$origin != "" & input$destination != "") {
        shinyjs::enable("submit_button")
      } else {
        shinyjs::disable("submit_button")
      }
    })

} # server



# Create Shiny object
shinyApp(ui = ui, server = server)
