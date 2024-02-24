
#library(shiny)
library(ggplot2)
library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(purrr)


# rsconnect::setAccountInfo(name='eagles-select-2013',
#                           token='8A581FA65BB0B84581D41071B9534CEE',
#                           secret='kl8mZxUHzqRUToiDikpZTxMXzq8TdI6UoKoJ6FCf')

#### Define UI ####
ui <- fluidPage(
  titlePanel("Softball Pitch Charting"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pitchType", "Pitch Type", 
                  choices = c("Fastball", "Changeup", "2-Seam")),
      selectInput("pitchZone", "Pitch Zone",
                  choices = c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5",
                              "Zone 6", "Zone 7", "Zone 8", "Zone 9", "Inside", "Outside", "High", "Low")),
      selectInput("pitchResult", "Pitch Result", 
                  choices = c("Strike", "Ball")),
      checkboxInput("wasHit", "Was the pitch hit?", FALSE),
      conditionalPanel(
        condition = "input.wasHit == true",
        selectInput("hitType", "Type of Hit", 
                    choices = c("Ground Ball", "Fly Ball", "Line Drive")),
        selectInput("hitLocation", "Location",
                    choices = c("RIF", "LIF", "CIF", "RF", "CF", "LF")),
        checkboxInput("hitResult", "True hit?", FALSE)
      ),
      actionButton("submit", "Submit Pitch")
    ),
    mainPanel(
      plotOutput("pitchChart")
    )
  )
)

#### Server ####
# Define server logic
server <- function(input, output) {
  # Initialize an empty data frame to store pitch data
  pitchesData <- reactiveVal(data.frame(
    pitchType = character(),
    zone = character(),
    pitchResult = character(),
    wasHit = logical(),
    hitType = character(),
    hitLocation = character(),  # Added hitLocation to store this new input
    stringsAsFactors = FALSE
  ))
  
  # Define zone mappings as an example (update with actual mappings)
  zoneMappings <- list(
    #strike zone
    "Zone 1" = list(xmin = 4, xmax = 5, ymin = 4, ymax = 5),
    "Zone 2" = list(xmin = 4, xmax = 5, ymin = 3, ymax = 4),
    "Zone 3" = list(xmin = 4, xmax = 5, ymin = 2, ymax = 3),
    
    "Zone 4" = list(xmin = 3, xmax = 4, ymin = 2, ymax = 3),
    "Zone 5" = list(xmin = 3, xmax = 4, ymin = 3, ymax = 4),
    "Zone 6" = list(xmin = 3, xmax = 4, ymin = 4, ymax = 5),
    
    "Zone 7" = list(xmin = 2, xmax = 3, ymin = 4, ymax = 5),
    "Zone 8" = list(xmin = 2, xmax = 3, ymin = 3, ymax = 4),
    "Zone 9" = list(xmin = 2, xmax = 3, ymin = 2, ymax = 3),
    
    #balls
    "Inside" = list(xmin = 5, xmax = 6, ymin = 1, ymax = 6),
    "Outside"= list(xmin = 1, xmax = 2, ymin = 1, ymax = 6),
    "Low"    = list(xmin = 1, xmax = 6, ymin = 1, ymax = 2),
    "High"   = list(xmin = 1, xmax = 6, ymin = 5, ymax =6)
  )
  
  # Observe when the 'Submit Pitch' button is clicked
  observeEvent(input$submit, {
    # Append the new data to the existing data frame with all information
    newEntry <- data.frame(
      pitchType = input$pitchType,
      zone = input$pitchZone,
      pitchResult = input$pitchResult,
      wasHit = input$wasHit,
      hitType = ifelse(input$wasHit, input$hitType, NA),
      hitLocation = ifelse(input$wasHit, input$hitLocation, NA),  # Capture the hit location
      stringsAsFactors = FALSE
    )
    
    currentData <- pitchesData()
    updatedData <- rbind(currentData, newEntry)
    pitchesData(updatedData)
  })
  
  
  # Display the data frame in the UI for verification
  output$pitchData <- renderTable({
    pitchesData()
  })
  
  # Render pitch chart
  output$pitchChart <- renderPlot({
    data <- pitchesData() %>%
      mutate(outcome = case_when(
        pitchResult == "Strike" ~ "Strike",
        pitchResult == "Ball" ~ "Ball",
        TRUE ~ "Other" # Captures any other outcomes; adjust as necessary
      )) %>%
      group_by(zone, outcome) %>%
      summarize(count = n(), .groups = 'drop') %>%
      mutate(
        x = map_dbl(zone, ~mean(c(zoneMappings[[.]]$xmin, zoneMappings[[.]]$xmax))),
        # Adjust y for vertical separation within the zone
        y = case_when(
          outcome == "Strike" ~ map_dbl(zone, ~mean(c(zoneMappings[[.]]$ymin, zoneMappings[[.]]$ymax))) + 0.15,
          outcome == "Ball" ~ map_dbl(zone, ~mean(c(zoneMappings[[.]]$ymin, zoneMappings[[.]]$ymax))) - 0.15,
          TRUE ~ map_dbl(zone, ~mean(c(zoneMappings[[.]]$ymin, zoneMappings[[.]]$ymax)))
        )
      ) %>%
      drop_na()
    
    ggplot(data) +
      scale_x_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) +
      scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) +
      labs(title = "", x = "", y = "") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      scale_color_manual(values = c("Strike" = "darkgreen", "Ball" = "red")) +
      geom_rect(data = map_df(names(zoneMappings), ~zoneMappings[[.]]), 
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                color = "black", fill = "white") +
      NULL +
      geom_text(aes(x = x, y = y, label = count, color = outcome), size = 5)
  })
}


# Run the application with defined UI and server
#deployApp()
app<-shinyApp(ui = ui, server = server)
app



