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
library(jsonlite)
library(httr)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

unbinned_data <- read.csv("./unbinned_delay_count.csv") %>% filter(Year>=1989)
unbinned_data$Year <- as.character(unbinned_data$Year)
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                 "Oct", "Nov", "Dec")
unbinned_data$Month <- factor(unbinned_data$Month, levels=month_order)
binned_data <- read.csv("./binned_delay_count.csv") %>% filter(Year>=1989)
binned_data$Year <- as.character(binned_data$Year)

get_text <- function(x, y){
  if (x=="All" & y=="All"){
    t <- paste0("Data of flight delays from 1989 to 2012")
  }else if(x=="All" & y!="All"){
    t <- paste0("Data of flight delays in ", y, " from 1989 to 2012")
  }else if(x!="All" & y=="All"){
    t <- paste0("Data of flight delays in ", x)
  }else{
    t <- paste0("Data of flight delays in ", y, " of ", x)
  }
  print(t)
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
airports <- read_csv("airports.csv", show_col_types=FALSE)

Delayed <- na.omit(Delayed)
Delayed <- Delayed %>%
  merge(airports, by.x="ORIGIN", by.y="IATA") %>%
  merge(airports, by.x="DEST", by.y="IATA")
Delayed$MONTH <- month_order[Delayed$MONTH]

flights <- function(city, year, month, opt){
  if (opt=="Origin"){
    df <- Delayed %>% filter(CITY.x==city, YEAR==year, MONTH==month)
    ls <- append(distinct(df, CITY.y)$CITY.y, city)
    df <- airports %>% filter(CITY %in% ls) %>%
      mutate(INFO=paste0(AIRPORT, " | ", CITY, ", ", STATE), 
             COLOR=ifelse(CITY==city,"orange","green"))
  }else{
    df <- Delayed %>% filter(CITY.y==city, YEAR==year, MONTH==month)
    ls <- append(distinct(df, CITY.x)$CITY.x, city)
    df <- airports %>% filter(CITY %in% ls) %>%
      mutate(INFO=paste0(AIRPORT, " | ", CITY, ", ", STATE), 
             COLOR=ifelse(CITY==city,"green","orange"))
  }
}

edges <- function(city, year, month, opt){
  df <- origin_dest(city, year, month, opt)
  dir <- data.frame("from"=as.vector(df$ORIGIN), "to"=as.vector(df$DEST))
  vert <- flights(city, year, month, opt) %>% select(IATA, LATITUDE,LONGITUDE)
  g <- graph.data.frame(dir, directed=FALSE, vertices=vert)
  gg <- get.data.frame(g, "both")
  vert <- gg$vertices
  coordinates(vert) <- ~LONGITUDE+LATITUDE
  edges <- gg$edges
  edges <- lapply(1:nrow(edges), function(i) {
    as(rbind(vert[vert$name == edges[i, "from"], ], 
             vert[vert$name == edges[i, "to"], ]), 
       "SpatialLines")
  })
  for (i in seq_along(edges)) {
    edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
  }
  edges <- do.call(rbind, edges)
}

origin_dest <- function(city, year, month, opt){
  if (opt=="Origin"){
    df <- Delayed %>% filter(CITY.x==city, YEAR==year, MONTH==month) %>%
      group_by(ORIGIN, DEST) %>%
      summarise_at(vars("delayed_dep", "delayed_arr", "total_flights"), sum) %>%
      ungroup()
  }else{
    df <- Delayed %>% filter(CITY.y==city, YEAR==year, MONTH==month) %>%
      group_by(ORIGIN, DEST) %>%
      summarise_at(vars("delayed_dep", "delayed_arr", "total_flights"), sum) %>%
      ungroup()
  }
  return(df)
}

usaLat <- 36.5588659
usaLon <- -107.6660877
usaZoom <- 3

Icons <- iconList(
  orange=makeIcon(paste0(getwd(),"/2.png"), 
                  iconWidth=24, iconHeight =32),
  
  green=makeIcon(paste0(getwd(),"/3.png"), 
                 iconWidth=24, iconHeight =32))

url4 <- "http://backend_cascade:1000/coefficients?mode="
list_year3 <- as.character(c(1989, 1990, 2000, 2001, 2006, 2007))

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

vis1_writeup <- "<ul><li>This visualisation shows periodic aggregated 
data for a chosen month or year. For further analysis, within specified month 
and year, the visualisation breaks down to show monthly departure delay and 
arrival delay.</li><li>Inputs: <ul><li>Year</li><li>Month</li></ul></li></ul>"
vis2_writeup <- "<ul><li>This visualistion has a geographical map which locates
the destination cities for a chosen origin city, and vice versa. The user 
may explore the percentage of delayed arrival and departure flights for the 
airports within a set month and year with the barchart and dataframe attached 
below.</li><li>Inputs: <ul><li>Year</li><li>Month</li><li>Origin/Destination
</li><li>City</li></ul></li></ul>"
vis3_writeup <- "<ul><li>This visualisation generates a heat map for delay 
factors including distance, precipitation, temperature, season and day of the 
week. This helps to highlight the cause of delay for flights in a selected year 
and flight direction (arrival/departure).</li><li>Inputs: <ul><li>Regression 
mode</li><li>Flight direction</li><li>Year</li></ul></li></ul>"

vis1_instruction <- "<ul><li>Select year = 'All' and month = 'All' for yearly 
aggregated data. This shows the trend of arrival delay from 1989 to 2012.</li>
</ul><ul><li>Select month = 'All' and a specified year for monthly aggregated 
data. This shows the trend of arrival delay from Jan to Dec within the chosen 
year.</li></ul><ul><li>Select year = 'All' and a specified month for yearly 
aggregated data. This shows the trend of arrival delay from 1989 to 2012 for a 
chosen month.</li></ul><ul><li>Select specified year and month for a break 
down of delayed arrival in the bottom left panel and delayed departure in the 
bottom right panel within the chosen month of the year.</li></ul>"

vis2_instruction <- "<ul><li>Select specified year, month, origin/destination 
and city to view flight map.</li></ul><ul><li>If there are no flights within the 
chosen parameters, no data will be shown. Please reselect the parameters.</li>
</ul><ul><li>Orange plane represents origin city and green plane represents 
destination city.</li></ul><ul><li>Clicking on the plane logo shown on the map 
shows the airport, city and state.</li></ul><ul><li>You may scroll down to view 
the exact number of total and delayed flights in the dataframe below.</li></ul>"

vis3_instruction <- "<ul><li>Select regression mode, year and flight direction.
</li></ul><ul><li>The variables are independent factors that we model to find 
their relationship with flight delay.</li></ul>ul><li>Selecting the linear 
regression model 'lm' shows a barplot, while selecting the decision tree model 
'dt' shows a tree plot where left of each split is true, right is false.</li>
</ul>
<div style='font-size: 10px;'><div>
<ul><li>distance: distance of route</div> </li></ul>
<ul><li>prcp_{origin/dest}: precipitation (mm) in the state of the origin/destination airport</li></ul>
<ul><li>snow_{origin/dest}: snowfall (mm) in the state of the origin/destination airport</li></ul>
<ul><li>snwd_{origin/dest}: snow depth (mm) in the state of the origin/destination airport</li></ul>
<ul><li>tmax_{origin/dest}: maximum temperature (°C) in the state of the origin/destination airport</li></ul>
<ul><li>tmin_{origin/dest}:minimum temperature (°C) in the state of the origin/destination airport</li></ul>
<ul><li>season_*: autumn, spring, summer and winter</li></ul>
<ul><li>day_of_week_*: 1 to 7 represents Monday to Sunday</li></ul>
<ul><li>crs_arr_bin_00-06: departure time 0000 to before 0600</li></ul>
<ul><li>crs_arr_bin_06-12: departure time 0600 to before 1200</li></ul>
<ul><li>crs_arr_bin_12-18: departure time 1200 to before 1800</li></ul>
<ul><li>crs_arr_bin_18-00: departure time 1800 onwards</li></ul></div>'
"

vis1_instruction <- 
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
            HTML(paste('<h3> How to use the app: </h3>', vis1_instruction)),
            br(),
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
                       choices=sort(unique(Delayed$YEAR)), selected="2007"),
            selectInput("Month2", label="Select month:", 
                       multiple=FALSE, 
                       choices=month_order, selected="Jan"),
            selectInput("Choice2", label="Select origin/destination:", 
                       multiple=FALSE, 
                       choices=sort(c("Origin","Dest")), selected="Origin"),
            selectInput("City2", label="Select city:", 
                       multiple=FALSE, 
                       choices=sort(airports$CITY), selected="Atlanta")
          ),
          mainPanel(
            HTML(paste('<h3> How to use the app: </h3>', vis2_instruction)),
            br(),
            leafletOutput(outputId="leafletMap"),
            br(),
            br(),
            plotOutput("delay_bar"),
            br(),
            br(),
            DT::DTOutput("delay_info")
          )
        )
      ),
      tabPanel("Delay Factors",
        sidebarLayout(
          sidebarPanel(
            radioButtons("Mode3", "Select mode of regression",
                        choices=c("Decision tree" = "dt",
                                  "Linear regression" = "lm"),
                        selected="Linear regression"),
            radioButtons("Direction3", "Select direction",
                        choices=c("Arrival" = "arr",
                                  "Departure" = "dep"),
                        selected="Arrival"),
            selectInput("Year3", "Select year", multiple=FALSE,
                        choices=list_year3,
                        selected="2007"),
            actionButton("submit_button2", "Enter!", disabled = TRUE, 
                         icon = icon("fas fa-plane", 
                                     lib="font-awesome", style="color:black;"))
        ),
        mainPanel(
          HTML(paste('<h3> How to use the app: </h3>', vis3_instruction)),
          br(),
          textOutput("vis3_welcometext")
            # fluidRow(
            #   column(width=12, plotOutput("ml_plot1")),
            #   column(width=6, plotOutput("ml_plot2")),
            #   column(width=6, plotOutput("ml_plot3"))
        )
      )
    )
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
                                month_order),
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
    flights(input$City2, input$Year2, input$Month2, input$Choice2)
  })
  
  output$leafletMap <- renderLeaflet({
    leaflet(data()) %>%
      setView(lat=usaLat, lng=usaLon, zoom=usaZoom) %>%
      addTiles() %>%
      addMarkers(~LONGITUDE, ~LATITUDE, popup=~INFO, label=~INFO, 
                 icon=~Icons[COLOR]) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addPolylines(data=edges(input$City2, input$Year2, 
                              input$Month2, input$Choice2),
                   weight=3)
  })
  
  output$delay_bar <- renderPlot({
    data <- origin_dest(input$City2, input$Year2, input$Month2, input$Choice2) %>% 
      mutate('Delayed_departure'=delayed_dep/total_flights*100, 
             'Delayed_arrival'=delayed_arr/total_flights*100)
    if (input$Choice2=="Origin"){
      data <- data %>% select(DEST, Delayed_arrival, Delayed_departure) %>%
        gather(variable, percentage , -DEST)
      ggplot(data, aes(DEST, percentage, fill = variable)) +
        geom_bar(stat="identity", position = "dodge") +
        labs(title=paste("Percentage of delayed arrival vs departure from",
                         input$City2, "as origin city."))
    }else{
      data <- data %>% select(ORIGIN, Delayed_arrival, Delayed_departure) %>%
        gather(variable, percentage , -ORIGIN)
      ggplot(data, aes(ORIGIN, percentage, fill = variable)) +
        geom_bar(stat="identity", position = "dodge") +
        labs(title=paste("Percentage of delayed arrival vs departure from",
                         input$City2, "as desination city."))
    }
  })
  
  output$delay_info <- DT::renderDT({
    origin_dest(input$City2, input$Year2, input$Month2, input$Choice2) %>%
      DT::datatable()
  })
  
  #Vis 3
  
  dat2 <- reactiveVal(NULL)
  
  reactive_text2 <- eventReactive(input$submit_button2, {
    ml_data <- dat2()
    if (nrow(ml_data) == 0) {
      HTML("<div> You have selected the", input$Mode3, "model for", input$Direction3, "flights in Year", input$Year3,
           "<br></br>", "However, the chosen inputs has 0 flights recorded. </div>")
    } else {
      paste("You have selected the", input$Mode3, "model for", input$Direction3, "flights in Year", input$Year3)  
    }
  })
  
  output$vis3_welcometext <- renderUI({
    reactive_text2()
  })
  
  observeEvent(input$submit_button2, {
    if (input$Mode3=="lm"){
      response <- GET(paste0(url4, "dt_", input$Direction3, "_", input$Year3, "_T"))
      df <- fromJSON(content(response, "text"), simplifyDataFrame = TRUE)
    }else{
      response <- GET(paste0(url4, "dt_", input$Direction3, "_", input$Year3))
      df <- fromJSON(content(response, "text"), simplifyDataFrame = TRUE)
    }
    dat2(df)
  })
  
  observeEvent(c(input$origin, input$destination, input$year2), {
    if (input$origin != "" & input$destination != "" & input$year2 != "") {
      shinyjs::enable("submit_button")
    } else {
      shinyjs::disable("submit_button")
    }
  })
}

shinyApp(ui=ui, server=server)