# Load libraries
library(shiny)
library(shinythemes)
library(bslib)
library(fontawesome)
library(leaflet)
library(DT)
library(tidyverse)
library(plotly)
library(sf)
library(rnaturalearth)
library(randomForest)
library(kableExtra)

# Load datasets
ereefs.data <- read.csv("app.data/ereefs.data.csv")
cots.data <- read.csv("app.data/cots.data.csv")
model.data <- read.csv("app.data/modelling_data.csv") %>%
  as.data.frame() %>%
  mutate(ttl_cots = as.factor(ttl_cots))

# Load Australia map
australia <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")

# Variable name mapping
variable_dict <- c(
  "din" = "Dissolved Inorganic Nitrogen (µmol/L)",
  "dip" = "Dissolved Inorganic Phosphorus (µmol/L)",
  "dic" = "Dissolved Inorganic Carbon (µmol/L)",
  "chlorophyll" = "Chlorophyll (µg/L)"
)

# Random Forest model
rf <- randomForest(ttl_cots ~ dic + logdip + logdin + sqrtchlorophyll + dead_coral,
                   data = model.data, ntree = 100)

# UI
ui <- navbarPage(
  title = "Runoff & Reef",
  theme = bs_theme(version = 5, bootswatch = 'flatly'),
  
  tabPanel("Home",
           fluidPage(
             titlePanel("Runoff & Reef: Investigating Drivers of COTS Outbreaks on the Great Barrier Reef"),
             fluidRow(
               column(12,
                      tags$h5("What causes Crown of Thorns Starfish (COTS) outbreaks?"),
                      tags$p("This dashboard explores how agriculture farming and industrial runoff, particularly nitrogen, phosphorus and carbon based fertilisers, may be contributing to damaging outbreaks of COTS across the Great Barrier Reef."),
                      tags$p("Our focus is on local case studies in Queensland catchments (like Tully and Burdekin), water quality, and reef health intersections."),
                      tags$hr(),
                      tags$h4("Research Motivation"),
                      fluidRow(
                        column(6,
                               tags$strong("The Problem:"),
                               tags$ul(
                                 tags$li("Coral reefs are under threat from repeated COTS outbreaks."),
                                 tags$li("Nutrient runoff may be fueling plankton blooms that feed COTS larvae."),
                                 tags$li("Industrial and agricultural activity, especially farming, is suspected to play a major role."),
                                 tags$li("We focus on within reef effects.")
                               )
                        ),
                        column(6,
                               tags$strong("Why It Matters:"),
                               tags$ul(
                                 tags$li("To inform policy and nutrient load management."),
                                 tags$li("Improve effectiveness of water quality improvement plans."),
                                 tags$li("Prevent outbreaks rather than just reacting to them.")
                               )
                        )
                      ),
                      tags$hr(),
                      tags$h4("Our Research Questions"),
                      tags$ul(
                        tags$li("How do nitrogen and other pollutants’ presence relate to COTS population outbreaks?"),
                        tags$li("Can we detect time lagged patterns between high nutrient levels and future outbreaks?"),
                        tags$li("Can we identify early warning indicators to prevent COTS outbreaks?")
                      ),
                      tags$hr(),
                      tags$h4("How to Use This App"),
                      tags$ol(
                        tags$li("Step 1: Pick which trends to include (e.g., nitrogen, phosphorus, carbon, etc.)"),
                        tags$li("Step 2: Interact with predictive models"),
                        tags$li("Step 3: View implications and policy insights")
                      ),
                      tags$hr(),
                      tags$h4("Key Stakeholders"),
                      tags$p("This project supports evidence based decision making for:"),
                      tags$ul(
                        tags$li(a("Great Barrier Reef Marine Park Authority", href = "https://www.gbrmpa.gov.au", target = "_blank"))
                      ),
                      tags$hr(),
                      tags$p(HTML("Data sources: <a href='https://apps.aims.gov.au/metadata/view/5bb9a340-4ade-11dc-8f56-00008a07204e' target='_blank'>AIMS</a>, <a href='https://thredds.ereefs.aims.gov.au/thredds/catalog/catalog.html' target='_blank'>eReefs</a>")),
                      tags$p("DATA3888/MARS3888 | For educational purposes only. Not an official predictive tool.")
               )
             )
           )
  ),
  
  tabPanel("Map Explorer",
           sidebarLayout(
             sidebarPanel(
               sliderInput('year', 'Select Year', min = 2010, max = 2019, value = 2013, sep = ""),
               radioButtons(
                 'variable', 'Environmental Variable',
                 choices = setNames(names(variable_dict), variable_dict),
                 selected = "dip"
               ),
               checkboxInput("show_cots", "Display COTS Sightings", TRUE)
             ),
             mainPanel(
               plotlyOutput("visualisation", height = "600px")
             )
           )
  ),
  
  tabPanel("Threat Level Predictor",
           sidebarLayout(
             sidebarPanel(
               h5("Set environmental levels below:"),
               sliderInput("din", "DIN (\u00b5mol/L)", 0, 200, 5),
               sliderInput("dip", "DIP (\u00b5mol/L)", 0, 30, 2),
               sliderInput("dic", "DIC (\u00b5mol/L)", 22360, 23776, 23000),
               sliderInput("chloro", "Chlorophyll (\u00b5g/L)", 0, 4, 0.9),
               sliderInput("dead_coral", "Dead Coral Cover (%)", 0, 70, 25)
             ),
             mainPanel(
               h4("Predicted Probability of COTS Threat Level"),
               uiOutput("predict_table")
             )
           )
  ),
  
  tabPanel("How to Use",
           fluidPage(
             titlePanel("Using the Runoff & Reef Dashboard"),
             fluidRow(
               column(12,
                      tags$p("This dashboard is designed to help visualise environmental drivers and predict Crown of Thorns Starfish (COTS) outbreaks. Use the tabs above to explore data and generate predictions."),
                      tags$ul(
                        tags$li("Explore variable maps in the 'Map Explorer' tab"),
                        tags$li("Use sliders in the 'Threat Level Predictor' tab to simulate different runoff conditions"),
                        tags$li("Review insights to inform reef conservation efforts")
                      ),
                      tags$br(),
                      tags$p("Hover over map markers for detailed info. Use the checkboxes and sliders to customise the view.")
               )
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive map
  plot <- reactive({
    plot_sf <- ereefs.data %>%
      filter(year == input$year) %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326)
    
    cots_year <- cots.data %>%
      filter(year == input$year) %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326)
    
    australia_buffered <- st_buffer(australia, dist = 0.15)
    plot_sf <- plot_sf[!st_intersects(plot_sf, australia_buffered, sparse = FALSE)[, 1], ]
    
    plot_coords <- st_coordinates(plot_sf)
    plot_sf$lon <- plot_coords[, 1]
    plot_sf$lat <- plot_coords[, 2]
    cots_coords <- st_coordinates(cots_year)
    cots_year$lon <- cots_coords[, 1]
    cots_year$lat <- cots_coords[, 2]
    
    var_name <- variable_dict[[input$variable]]
    plot_sf$color_value <- plot_sf[[input$variable]]
    
    p <- plot_ly(type = "scattergeo", mode = "markers") %>%
      add_trace(
        data = plot_sf,
        lat = ~lat, lon = ~lon,
        marker = list(
          size = 6,
          color = ~color_value,
          colorscale = 'YlGnBu',
          showscale = TRUE,
          colorbar = list(title = var_name, x = 0.92, y = 0.7, len = 0.35),
          opacity = 0.8
        ),
        text = ~paste0(var_name, ": ", round(color_value, 2),
                       "<br>Lat: ", round(lat, 2), ", Lon: ", round(lon, 2)),
        hoverinfo = "text",
        name = "Water Quality",
        showlegend = FALSE
      )
    
    if (input$show_cots) {
      p <- p %>%
        add_trace(
          data = cots_year,
          lat = ~lat, lon = ~lon,
          marker = list(
            size = ~mean_cots_per_tow * 2,
            color = ~mean_cots_per_tow,
            colorscale = 'Viridis',
            showscale = TRUE,
            colorbar = list(title = "COTS (ind./tow)", x = 0.92, y = 0.3, len = 0.3),
            opacity = 0.6
          ),
          text = ~paste0("COTS: ", round(mean_cots_per_tow, 2),
                         "<br>Lat: ", round(lat, 2), ", Lon: ", round(lon, 2)),
          hoverinfo = "text",
          name = "COTS",
          showlegend = FALSE
        )
    }
    
    p %>% layout(
      title = list(text = paste("Interactive Map -", var_name), x = 0),
      geo = list(
        scope = "world",
        showland = TRUE,
        landcolor = "rgb(240,240,240)",
        countrycolor = "rgb(200,200,200)",
        lataxis = list(range = c(-24, -10)),
        lonaxis = list(range = c(144, 156)),
        resolution = 50
      )
    )
  })
  
  # Render plot
  output$visualisation <- renderPlotly({ plot() })
  
  # Prediction logic
  model <- reactive({
    new_data <- data.frame(
      dic = input$dic,
      logdip = log(input$dip + 1),
      logdin = log(input$din + 1),
      sqrtchlorophyll = sqrt(input$chloro),
      dead_coral = input$dead_coral
    )
    
    probs <- predict(rf, newdata = new_data, type = 'prob')
    probs <- round(probs, 3)
    colnames(probs) <- paste0(colnames(probs), " Risk")
    as.data.frame(probs)
  })
  
  output$predict_table <- renderUI({
    probs <- model()
    HTML(
      kable(probs, format = "html", digits = 3) %>%
        kable_styling("striped", full_width = FALSE, bootstrap_options = "hover")
    )
  })
}

# Run App
shinyApp(ui, server)
