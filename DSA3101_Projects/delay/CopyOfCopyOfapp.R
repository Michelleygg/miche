library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(igraph)
library(leaflet)
library(readr)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(sp)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
print(getwd())

unbinned_data <- read.csv("./unbinned_delay_count.csv") %>% filter(Year>=1989)
unbinned_data$Year <- as.character(unbinned_data$Year)
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                 "Oct", "Nov", "Dec")
unbinned_data$Month <- factor(unbinned_data$Month, levels=month_order)
binned_data <- read.csv("./binned_delay_count.csv") %>% filter(Year>=1989)
binned_data$Year <- as.character(binned_data$Year)

url1 <- "http://backend_cascade:5000/query?origin=" # + origin
url2 <- "&dest=" # + destination
url3 <- "&year=" # + year
list_unique_origins <- c("ATL", "ORD", "DFW", "LAX", "PHX", "DEN", "IAH", "LAS", "DTW", "STL")
list_unique_dests <-  c("ATL", "ORD", "DFW", "LAX", "PHX", "DEN", "IAH", "LAS", "DTW", "STL")
list_year2 <- as.character(seq(1989, 2012))

get_text <- function(x, y){
  if (x=="All" & y=="All"){
    paste0("Data of flight delays from 1989 to 2012")
  }else if(x=="All" & y!="All"){
    paste0("Data of flight delays in ", y, " from 1989 to 2012")
  }else if(x!="All" & y=="All"){
    paste0("Data of flight delays in ", x)
  }else{
    paste0("Data of flight delays in ", y, " of ", x)
  }
}

filtered_unbinned <- function(data, x, y){
  if (x=="All" & y=="All"){
    data %>% group_by(Year) %>% 
      summarise_at(vars("Dep.Delay.Count", "Arr.Delay.Count"), mean) %>%
      ungroup()
  }else if (x!="All" & y=="All"){
    subset(data, Year==x)
  }else if(x=="All" & y!="All"){
    subset(data, Month==y)
  }else{
    subset(data, Year==x & Month==y)
  }
}

filtered_binned <- function(data, x, y){
  if (x!="All" & y!="All"){
    subset(data, Year==x & Month==y)
  }else{
    data
  }
}

Delayed <- read_csv("pairwise_flights.csv", show_col_types=FALSE)
airports <- read_csv("airports.csv", show_col_types=FALSE) %>%
  filter(IATA %in% list_unique_origins)

Delayed <- na.omit(Delayed)
Delayed$MONTH <- month_order[Delayed$MONTH]

flights <- function(year, origin, dest){
  df <- Delayed %>% filter(ORIGIN==origin, YEAR==year)
  ls <- append(distinct(df, DEST)$DEST, origin)
  df <- airports %>% filter(IATA %in% ls) %>%
      mutate(INFO=paste0(AIRPORT, " | ", CITY, ", ", STATE), 
             COLOR=ifelse(IATA==origin|IATA==dest,"orange","green"))
}

origin_dest <- function(year, origin, dest){
  df <- Delayed %>% filter(ORIGIN==origin, YEAR==year) %>%
      group_by(ORIGIN, DEST) %>%
      summarise_at(vars("delayed_dep", "delayed_arr", "total_flights"), sum) %>%
      ungroup()
  return(df)
}

edges <- function(year, origin, dest){
  df <- origin_dest(year, origin, dest)
  dir <- data.frame("from"=as.vector(df$ORIGIN), "to"=as.vector(df$DEST))
  vert <- flights(year, origin, dest) %>% select(IATA, LATITUDE, LONGITUDE)
  g <- graph.data.frame(dir, directed=FALSE, vertices=vert)
  gg <- get.data.frame(g, "both")
  vert <- gg$vertices
  coordinates(vert) <- ~LONGITUDE+LATITUDE
  edges <- gg$edges
  edges <- lapply(1:nrow(edges), function(i) {
    as(rbind(vert[vert$name==edges[i, "from"], ], 
             vert[vert$name==edges[i, "to"], ]), 
       "SpatialLines")
  })
  for (i in seq_along(edges)) {
    edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
  }
  edges <- do.call(rbind, edges)
}

writeup_summary <- "<li>Our app visualises air travel delays from flights across 
the years 1989 - 1990, 2000 - 2001 and 2006 - 2007.</li><li>Visualisations 1, 2 
and 3 are meant to help ease visualisations of flight delays over such years and 
would help to give different insights.</li>"
writeup_motivation <- "<li>There were several important events that took place 
from 1987 to 2012 which had a significant impact on the aviation industry of the 
USA. Some of the most notable ones are: </li><div style='margin-top:9px;'><ol>
<div id='motivation'><em><li>Gulf War (1990-1991):</li></em><ul><li> The Gulf 
War led to a surge in air travel demand as military personnel and their families 
traveled to and from the region. Airlines increased their capacity to meet the 
demand, leading to a significant increase in profits for the industry.
</ul></li></div>
<div id='motivation'><em><li>September 11 attacks (2001):</li></em><ul><li> 
The terrorist attacks on September 11, 2001, had a profound impact on the 
aviation industry, leading to increased security measures and changes in the 
way airlines operate. The attacks resulted in a significant decline in air 
travel demand, leading to financial losses for the industry.</ul></li></div>
<div id='motivation'><em><li>Global Financial Crisis (2007-2008):
</li></em><ul><li> The global financial crisis had a significant impact on the 
aviation industry, leading to a decline in air travel demand and financial 
losses for airlines. Many airlines were forced to cut costs, reduce capacity, 
and lay off employees to stay afloat.</ul></li></div></div></ol>"

vis1_writeup <- "<ul><li>This visualisation mainly shows monthly aggregated data 
for a chose year. To go further in the analysis, a month within the year can 
be chosen to break the visualisations down into departure delay and arrival 
delay.</li><li>Inputs: <ul><li>Year</li><li>Month</li></ul></li></ul>"
vis2_writeup <- "<ul><li>vis2 writeup</li></ul>"
vis3_writeup <- "<ul><li>vis3 writeup</li></ul>"

ui <- fluidPage(
  useShinyjs(),
  theme=shinytheme("cerulean"),
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
    title=div(id="main-title", "Airline Delay Webapp"),
      tabPanel("Home",
               HTML(paste('<div style="padding: 10px 20px"><h1>Welcome to the Airline Delay Webapp!</h1>
             <h3> Introduction: </h3>
              <div  style="display: flex; flex-direction: column; 
                  align-items: left; font-size:16px;">', writeup_summary,
                  '</div>', '<h3> Motivation of Selected Timeframe: </h3>', 
                  writeup_motivation, '<h3> Visualisations Explained: </h3>', 
                  '<ol>
              <li style="font-size:15px;" id="motivation"> 
              Visualisation 1: <div style="font-size: 15px;">', 
              vis1_writeup, '</div> </li>
              <li style="font-size:15px;" id="motivation"> 
              Visualisation 2: <div style="font-size: 15px;">', 
              vis2_writeup, '</div> </li>
              <li style="font-size:15px;" id="motivation"> 
              Visualisation 3: <div style="font-size: 15px;">', 
              vis3_writeup, '</div> </li>
              </ol>', '</div>', sep = '')
               )
      ),
      tabPanel("Period",
        sidebarLayout(
          sidebarPanel(
            selectInput("Year1", "Select year", multiple=FALSE,
                        choices=NULL,
                        selected="All"),
            selectInput("Month1", "Select month", multiple=FALSE,
                        choices=NULL,
                        selected="All")
          ),
          mainPanel(
            textOutput("selected_year"),
            fluidRow(
              column(width=12, plotOutput("selected_plot")),
              column(width=6, plotOutput("selected_plot_arr")),
              column(width=6, plotOutput("selected_plot_dep"))
            )
          )
        )
      ),
    
      tabPanel("Flight Map",
        sidebarLayout(
          sidebarPanel(
            selectInput("Year2", label="Select year:", 
                        multiple=FALSE,
                        choices=NULL, selected=""),
            selectInput("Origin2", label="Select destination:", 
                       multiple=FALSE, 
                       choices=NULL, selected=""),
            selectInput("Dest2", label="Select destination:", 
                       multiple=FALSE, 
                       choices=NULL, selected=""),
            actionButton("submit_button", "Enter!", disabled = TRUE, 
                         icon = icon("fas fa-plane", lib="font-awesome", 
                                     style="color:black;"))
          ),
          mainPanel(
            textOutput("vis2_welcometext"),
            leafletOutput(outputId="leafletMap"),
            DT::DTOutput("delay_info"),
            plotOutput("delay_line")
          )
        )
      ),
      tabPanel("Delay Factors", "This is the page for the 3rd visualisation")
  )
)


server <- function(input, output) {
  
  #Vis 1
  observe({
    updateSelectInput(inputId="Year1", 
                      choices=c("All",unique(binned_data$Year)), 
                      selected="All")
  })
  
  output$selected_year <- renderText({
    get_text(input$Year1, input$Month1)
  })
  
  filtered_data_year <- reactive({
    filtered_unbinned(unbinned_data, input$Year1, input$Month1)
  })
  
  observe({
    updateSelectInput(inputId="Month1", 
                      choices=c("All",
                                month_order[unique(
                                  filtered_unbinned(
                                    unbinned_data, input$Year1, "All")$Month)]),
                      selected="All")
  })
  
  output$selected_plot <- renderPlot({
    if (input$Year1=="All"){
      ggplot(data=filtered_data_year(), aes(x=Year, y=Arr.Delay.Count)) +
        geom_bar(stat="identity", fill="skyblue", color="black") +
        labs(title="Yearly Aggregated Delay Data",
             x="Year", y="Arrival Delay Count")
    }else{
      ggplot(data=filtered_data_year(), aes(x=Month, y=Arr.Delay.Count)) +
        geom_bar(stat="identity", fill="skyblue", color="black") +
        labs(title="Monthly Aggregated Delay Data",
          x="Month", y="Arrival Delay Count")
    }
  })
  
  filtered_data_binned <- reactive({
    filtered_binned(binned_data, input$Year1, input$Month1)
  })
  
  filtered_data_binned_arr <- reactive({
    filtered_data_binned() %>% select(starts_with("Arr")) %>% 
      gather(key="arrBin", value="value")
  })
  
  filtered_data_binned_dep <- reactive({
    filtered_data_binned() %>% select(starts_with("Dep")) %>% 
      gather(key="depBin", value="value")
  })
  
  monthPlot_bins <- c("60-74","75-89","90-104","105-119","120-134","135-149",
                      "150-164","165-179","Above 180")
  
  monthPlot_bins <- factor(monthPlot_bins, levels=monthPlot_bins)
  output$selected_plot_arr <- renderPlot({
    if (input$Year1!="All" & input$Month1!="All"){
      ggplot(filtered_data_binned_arr(), aes(x=monthPlot_bins, y=value)) +
        geom_bar(stat="identity", fill="green", color="black") +
        labs(title=paste0("Arrival Flights Delay Data (", input$Month1,")"),
             x="Arrival Delay Time (minutes)", y="Arrival Delay Count")
    }else{
      ggplot() +
        annotate("text",x=1,y=1,size=4,
                 label="Graph needs both Year and Month inputs. Please select Year and Month.") +
        theme_void()
    }
  })
    
  output$selected_plot_dep <- renderPlot({
    if (input$Year1!="All" & input$Month1!="All"){
      ggplot(filtered_data_binned_dep(), aes(x=monthPlot_bins, y=value)) +
        geom_bar(stat="identity", fill="purple", color="black") +
        labs(title=paste0("Departure Flights Delay Data (", input$Month1,")"),
             x="Delay Time (minutes)",
             y="Delay Count")
    }else{
      ggplot() +
        annotate("text",x=1,y=1,size=4,
                 label="Graph needs both Year and Month inputs. Please select Year and Month.") +
        theme_void()
    }
  })
  
  #Vis 2
  
  data <- reactive({
    flights(input$Year2, input$Origin2, input$Dest2)
  })
  
  reactive_text <- eventReactive(input$submit_button, {
    cascade_data <- data()
    if (nrow(cascade_data)==0) {
      HTML("<div> You have selected", input$Origin2, "-->", input$Dest2, "in Year", input$Year2,
           "<br></br>", "However, the chosen inputs has 0 flights recorded. </div>")
    } else {
      paste("You have selected", input$Origin2, "-->", input$Dest2, "in Year", input$Year2)  
    }
  })

  output$vis2_welcometext <- renderUI({
    if (input$Year2=="" || input$Origin2=="" || input$Dest2=="") {
      paste0("select input from left")
    } else if (input$submit_button==0) {
      paste0("Please press the enter button")
    } else {
      reactive_text()
    }
  })
  
  # observeEvent(input$submit_button, {
  #   response <- GET(paste0(url1, input$origin, url2, input$destination, url3, input$year2))
  #   content <- content(response, as ="text")
  #   json_content <- fromJSON(content)
  #   df <- as.data.frame(json_content)
  #   dat(df)
  # })
  # 
  usaLat <- 36.5588659
  usaLon <- -107.6660877
  usaZoom <- 3
  
  Icons <- iconList(
    orange=makeIcon(paste0(getwd(),"/2.png"), 
                    iconWidth=24, iconHeight =32),
    
    green=makeIcon(paste0(getwd(),"/3.png"), 
                   iconWidth=24, iconHeight =32))
  
  data <- reactive({
    flights(input$Year2, input$Origin2, input$Dest2)
  })
  
  reactive_leaflet <- eventReactive(input$submit_button, {
    if(nrow(data())==0) {
      #Base map
      leaflet(airports) %>%
        setView(lat=usaLat, lng=usaLon, zoom=usaZoom) %>%
        addTiles() %>%
        addMarkers(~LONGITUDE, ~LATITUDE, popup=~INFO, label=~INFO, icon=~Icons['green']) %>%
        addProviderTiles(providers$Esri.WorldStreetMap)
    }else{
      leaflet(data()) %>%
        setView(lat=usaLat, lng=usaLon, zoom=usaZoom) %>%
        addTiles() %>%
        addMarkers(~LONGITUDE, ~LATITUDE, popup=~INFO, label=~INFO, 
                   icon=~Icons[COLOR]) %>%
        addProviderTiles(providers$Esri.WorldStreetMap) %>%
        addPolylines(data=edges(input$Year2, input$Origin2, input$Dest2),
                     weight=3)
      }
  })
  
  output$leafletMap <- renderLeaflet({
    reactive_leaflet()
  })
    
  observe({
    updateSelectInput(inputId="Year2", choices=c("",list_year2), selected="")
    updateSelectInput(inputId="Origin2", choices=c("",sort(unique(list_unique_origins))), selected="")
    updateSelectInput(inputId="Dest2", choices=c("",sort(unique(list_unique_dests))), selected="")
  })
  
  observeEvent(input$Origin2, {
    selected_origin <- input$Origin2
    destination_choices <- list_unique_dests
    if (selected_origin!="") {
      destination_choices <- list_unique_dests[list_unique_dests != selected_origin]
    }
    updateSelectInput(inputId="Dest2", choices=destination_choices, selected=input$Dest2)
  })
  
  observeEvent(input$Dest2, {
    selected_destination <- input$Dest2
    origin_choices <- list_unique_origins
    if (selected_destination!="") {
      origin_choices <- list_unique_origins[list_unique_origins != selected_destination]
    }
    updateSelectInput(inputId="Origin2", choices=origin_choices, selected=input$Origin2)
  })
  
  
  observeEvent(c(input$Origin2, input$Dest2, input$Year2), {
    if (input$Origin2!="" & input$Dest2!="" & input$Year2!="") {
      shinyjs::enable("submit_button")
    } else {
      shinyjs::enable("submit_button")
    }
  })
  
  output$delay_line <- renderPlot({
    data <- origin_dest(input$Year2, input$Origin2, input$Dest2)
    data <- data %>% select(DEST, delayed_dep, delayed_arr, total_flights) %>%
              gather(var, val , -DEST)
    ggplot(data, aes(x=var, y=val)) + 
      geom_line(aes(color=as.factor(DEST), group=as.factor(DEST)))
  })
  
  output$delay_info <- DT::renderDT({
    origin_dest(input$Year2, input$Origin2, input$Dest2) %>%
      DT::datatable()
  })
}

shinyApp(ui=ui, server=server)