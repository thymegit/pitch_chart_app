
#library(shiny)
library(ggplot2)
library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(purrr)
library(tidyr)
library(shinyjs)

install.packages("shinyjs")
# rsconnect::setAccountInfo(name='eagles-select-2013',
#                           token='8A581FA65BB0B84581D41071B9534CEE',
#                           secret='kl8mZxUHzqRUToiDikpZTxMXzq8TdI6UoKoJ6FCf')

#### Define UI ####
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs for enhanced JS functionalities
  
  tags$head(
    tags$script(HTML("
      $(document).on('shiny:connected', function(event) {
      Shiny.setInputValue('appConnected', true); // Signal that the app is connected
        setInterval(function() {
          Shiny.setInputValue('keep_alive', new Date());
        }, 10000); // sends a message every 10 seconds
      });
    "))
  ),
  tags$head(
    tags$style(HTML("
      .grid-container {
        display: grid;
        grid-template-columns: repeat(3, 1fr); /* 3 columns */
        padding: 10px;
        gap: 10px;
      }
      .grid-item {
        padding: 10px;
        text-align: center;
      }
      .grid-label {
        text-align: center;
        margin-bottom: 5px;
      }
    "))
  ),
  titlePanel("Softball Pitch Charting"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("pitchType", "Pitch Type", 
                   choices = c("Fastball", "Changeup", "2-Seam"),
                   inline = TRUE),
      # Custom grid layout for Pitch Zone
      div(class = 'grid-container',
          div(class = 'grid-item', 
              checkboxInput("pitchZoneInside", "Inside")),
          div(class = 'grid-item', 
              checkboxInput("pitchZoneHigh", "High")),
          div(class = 'grid-item', 
              checkboxInput("pitchZoneOutside", "Outside")),
          div(class = 'grid-item', 
              checkboxInput("pitchZone7", "7")),
          div(class = 'grid-item', 
              checkboxInput("pitchZone6", "6")),
          div(class = 'grid-item', 
              checkboxInput("pitchZone1", "1")),
          div(class = 'grid-item', 
              checkboxInput("pitchZone8", "8")),
          div(class = 'grid-item', 
              checkboxInput("pitchZone5", "5")),
          div(class = 'grid-item', 
              checkboxInput("pitchZone2", "2")),
          div(class = 'grid-item', 
              checkboxInput("pitchZone9", "9")),
          div(class = 'grid-item', 
              checkboxInput("pitchZone4", "4")),
          div(class = 'grid-item', 
              checkboxInput("pitchZone3", "3")),
          div(class = 'grid-item', 
              checkboxInput("pitchZoneLow", "Low"))
      ),
      radioButtons("pitchResult", "Pitch Result", 
                   choices = c("Strike", "Ball"),
                   inline = TRUE),
      checkboxInput("wasHit", "Was the pitch hit?", FALSE),
      conditionalPanel(
        condition = "input.wasHit == true",
        radioButtons("hitType", "Type of Hit", 
                     choices = c("Ground Ball", "Fly Ball", "Line Drive")),
        radioButtons("hitLocation", "Location",
                     choices = c("RIF", "LIF", "CIF", "RF", "CF", "LF")),
        checkboxInput("hitResult", "True hit?", FALSE)
      ),
      actionButton("submit", "Submit Pitch"),
      downloadButton("downloadPlot", "Download Chart")
    ),
    mainPanel(
      plotOutput("pitchChart")
    )
  )
)


#### Server ####
# Define server logic
server <- function(input, output, session) {
  # Define currentYear function if not already defined
  currentYear <- function() {
    as.numeric(format(Sys.Date(), "%Y"))
  }
  
  # Store player info in reactive values
  playerInfo <- reactiveValues(birthYear = NULL, number = NULL)
  
  playerInfoModal <- function() {
    modalDialog(
      title = "Enter Player Information",
      numericInput("playerBirthYear", "Player's Birth Year", value = 1990, min = 1900, max = currentYear(), step = 1),
      numericInput("playerNumber", "Player's Number", value = 1, min = 1, max = 99, step = 1),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("savePlayerInfo", "Save", class = "btn-primary")
      ),
      easyClose = FALSE,
      fade = TRUE,
      size = "m"
    )
  }
  
  # Display modal on connection
  observeEvent(TRUE, {  # Changed to TRUE for immediate execution
    showModal(playerInfoModal())
  }, ignoreNULL = FALSE, once = TRUE)
  
  # Capture player info from modal and store it
  observeEvent(input$savePlayerInfo, {
    playerInfo$birthYear <- input$playerBirthYear
    playerInfo$number <- input$playerNumber
    
    removeModal()
  })
  
  # Initialize an empty data frame to store pitch data
  pitchesData <- reactiveVal(data.frame(
    pitchType = character(),
    zone = character(),
    pitchResult = character(),
    wasHit = logical(),
    hitType = character(),
    hitLocation = character(),
    playerBirthYear = integer(),
    playerNumber = integer(),
    stringsAsFactors = FALSE
  ))
  
  
  # Define zone mappings as an example (update with actual mappings)
  zoneMappings <- list(
    #strike zone
    "1" = list(xmin = 4, xmax = 5, ymin = 4, ymax = 5),
    "2" = list(xmin = 4, xmax = 5, ymin = 3, ymax = 4),
    "3" = list(xmin = 4, xmax = 5, ymin = 2, ymax = 3),
    
    "4" = list(xmin = 3, xmax = 4, ymin = 2, ymax = 3),
    "5" = list(xmin = 3, xmax = 4, ymin = 3, ymax = 4),
    "6" = list(xmin = 3, xmax = 4, ymin = 4, ymax = 5),
    
    "7" = list(xmin = 2, xmax = 3, ymin = 4, ymax = 5),
    "8" = list(xmin = 2, xmax = 3, ymin = 3, ymax = 4),
    "9" = list(xmin = 2, xmax = 3, ymin = 2, ymax = 3),
    
    #balls
    "Inside" = list(xmin = 5, xmax = 6, ymin = 1, ymax = 6),
    "Outside"= list(xmin = 1, xmax = 2, ymin = 1, ymax = 6),
    "Low"    = list(xmin = 1, xmax = 6, ymin = 1, ymax = 2),
    "High"   = list(xmin = 1, xmax = 6, ymin = 5, ymax =6)
  )
  
  # Observe when the 'Submit Pitch' button is clicked
  observeEvent(input$submit, {
    # Create a named list of all checkbox inputs for pitch zones
    zonesInputList <- list(
      High = input$pitchZoneHigh,
      Inside = input$pitchZoneInside,
      Outside = input$pitchZoneOutside,
      "1" = input$pitchZone1,
      "2" = input$pitchZone2,
      "3" = input$pitchZone3,
      "4" = input$pitchZone4,
      "5" = input$pitchZone5,
      "6" = input$pitchZone6,
      "7" = input$pitchZone7,
      "8" = input$pitchZone8,
      "9" = input$pitchZone9,
      Low = input$pitchZoneLow
    )
    
    # Determine if any zone is selected
    selectedZone <- names(zonesInputList)[unlist(zonesInputList)]
    
    # If no zone is selected, default to "Missing"
    if(length(selectedZone) == 0) {
      selectedZone <- "Missing"
    } else {
      # If for some reason multiple are selected, this will concatenate them
      selectedZone <- paste(selectedZone, collapse=", ")
    }
    
    # Proceed to create the new entry with either the selected zone(s) or "Missing"
    newEntry <- data.frame(
      pitchType = input$pitchType,
      zone = selectedZone,
      pitchResult = input$pitchResult,
      wasHit = input$wasHit,
      hitType = ifelse(input$wasHit, input$hitType, NA),
      hitLocation = ifelse(input$wasHit, input$hitLocation, NA),
      playerBirthYear = playerInfo$birthYear,
      playerNumber = playerInfo$number,
      stringsAsFactors = FALSE
    )
    
    pitchesData(rbind(pitchesData(), newEntry))
  })
  
  
  # Render pitch cha
  plotData <- reactive({
    # Ensure there's data to plot
    if(nrow(pitchesData()) == 0) return()
    
    # Extract the most recent player info
    latestEntry <- tail(pitchesData(), 1)
    playerBirthYear <- latestEntry$playerBirthYear
    playerNumber <- latestEntry$playerNumber
    
    # Create a dynamic title incorporating player info
    plotTitle <- paste("Player Info - Birth Year:", playerBirthYear, 
                       "Number:", playerNumber, 
                       "Date:", format(Sys.Date(), "%Y-%m-%d"))
    
    
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
      labs(title = plotTitle, x = "", y = "") +
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
  
  output$pitchChart <- renderPlot({
    plotData()
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("pitch-chart", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Save the plot to the specified file path
 
      ggsave(file, plot = plotData(), device = "png", width = 7, height = 5, dpi = 300,bg = "lightgrey")
    }
  )
  
  
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  
  #reset input pitch zone clickbox
  observeEvent(input$submit, {
    
    # Reset pitch zone checkboxes
    updateCheckboxInput(session, "pitchZoneHigh", value = FALSE)
    updateCheckboxInput(session, "pitchZoneInside", value = FALSE)
    updateCheckboxInput(session, "pitchZoneOutside", value = FALSE)
    updateCheckboxInput(session, "pitchZone1", value = FALSE)
    updateCheckboxInput(session, "pitchZone2", value = FALSE)
    updateCheckboxInput(session, "pitchZone3", value = FALSE)
    updateCheckboxInput(session, "pitchZone4", value = FALSE)
    updateCheckboxInput(session, "pitchZone5", value = FALSE)
    updateCheckboxInput(session, "pitchZone6", value = FALSE)
    updateCheckboxInput(session, "pitchZone7", value = FALSE)
    updateCheckboxInput(session, "pitchZone8", value = FALSE)
    updateCheckboxInput(session, "pitchZone9", value = FALSE)
    updateCheckboxInput(session, "pitchZoneLow", value = FALSE)
  })
  
  
  
}

# Run the application with defined UI and server
#deployApp()
app<-shinyApp(ui = ui, server = server)
app





# Run the application with defined UI and server
#deployApp()
app<-shinyApp(ui = ui, server = server)
app



