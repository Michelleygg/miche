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

setwd("C:/Users/admin/Dropbox/Apps/GoodNotes 5/Files/Sem 4/DSA3101/delay")

unbinned_data <- read.csv("./unbinned_delay_count.csv") %>% filter(Year>=1989)
unbinned_data$Year <- as.character(unbinned_data$Year)
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                 "Oct", "Nov", "Dec")
unbinned_data$Month <- factor(unbinned_data$Month, levels=month_order)
binned_data <- read.csv("./binned_delay_count.csv") %>% filter(Year>=1989)
binned_data$Year <- as.character(binned_data$Year)

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
  orange=makeIcon("C:/Users/admin/Dropbox/Apps/GoodNotes 5/Files/Sem 4/DSA3101/delay/2.png", 
                    iconWidth=24, iconHeight =32),
  
  green=makeIcon("C:/Users/admin/Dropbox/Apps/GoodNotes 5/Files/Sem 4/DSA3101/delay/3.png", 
                   iconWidth=24, iconHeight =32))

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
    title=div("Airline Delay Webapp", id="main-title"),
      tabPanel("Home",
             HTML(paste('<div style="padding: 10px 20px"><h1>Welcome to the 
             Airline Delay Webapp!</h1>
             <h3> Introduction: </h3>
              <div  style="display: flex; flex-direction: column; 
                      align-items: left; font-size:15px;">', writeup_summary,
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
              </ol>', '<h3> How to use app: </h3>', '</div>', sep = '')
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
                 label="No data. Please select Year and Month.") +
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
                 label="No data. Please select Year and Month.") +
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
  
  output$delay_line <- renderPlot({
    data <- origin_dest(input$City2, input$Year2, input$Month2, input$Choice2)
    if (input$Choice2=="Origin"){
      data <- data %>% select(DEST, delayed_dep, delayed_arr, total_flights) %>%
              gather(var, val , -DEST)
      ggplot(data, aes(x=var, y=val)) + 
        geom_line(aes(color=as.factor(DEST), group=as.factor(DEST)))
    }else{
      data <- data %>% select(ORIGIN, delayed_dep, delayed_arr, total_flights) %>%
        gather(var, val , -ORIGIN)
      ggplot(data, aes(x=var, y=val)) + 
        geom_bar(aes(color=as.factor(ORIGIN), group=as.factor(ORIGIN)))
    }
  })
  
  output$delay_info <- DT::renderDT({
    origin_dest(input$City2, input$Year2, input$Month2, input$Choice2) %>%
      DT::datatable()
  })
}

shinyApp(ui=ui, server=server)