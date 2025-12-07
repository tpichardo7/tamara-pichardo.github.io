library(shiny)
library(tidyverse)
library(DT)
library(readxl)

rutgers_2024 = read_xlsx("./data/rutgers_2024.xlsx")

rutgers_2024 = rutgers_2024 |> 
  mutate(
    hitting_pct = (kill - attack_error) / attack_attempt,
    setting_pct = (assist - set_error) / set_attempt,
    serve_pct = (ace - serve_error) / serve_attempt,
    total_blocks = block_solo + block_assist
  )

rutgers_2024 = rutgers_2024 |> 
  mutate(
    type = as.factor(type),
    location = as.factor(location),
    position = as.factor(position)
  )


# UI
ui <- fluidPage(
  titlePanel("Volleyball Scouting Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("player", "Select Player(s)", 
                     choices = unique(rutgers_2024$player),
                     selected = NULL,
                     multiple = TRUE),
      selectizeInput("match", "Select Match(s)", 
                     choices = unique(rutgers_2024$match),
                     selected = NULL,
                     multiple = TRUE),
      selectInput("metric", "Choose Metric to Plot", 
                  choices = c("Hitting %" = "hitting_pct",
                              "Setting %" = "setting_pct",
                              "Serving %" = "serve_pct"),
                  selected = "hitting_pct"),
      selectInput("position", "Select Position", 
                  choices = c("All", unique(rutgers_2024$position)))
    ),
    mainPanel(
      DTOutput("stats_table"),
      plotOutput("hitting_plot"),
      DTOutput("summary_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- rutgers_2024 |> filter(player %in% input$player)
    
    if (!is.null(input$match) && length(input$match) > 0) {
      data <- data |> filter(match %in% input$match)
    }
    
    if (input$position != "All") {
      data <- data |> filter(position == input$position)
    }
    
    data
  })
  
  output$stats_table <- renderDT({
    filtered_data() |> 
      select(player, position, match, kill, attack_error, attack_attempt, hitting_pct)
  })
  
  output$hitting_plot <- renderPlot({
    plot_data <- filtered_data()
    if (nrow(plot_data) == 0) return(NULL)
    
    ggplot(plot_data, aes_string(x = "match", y = input$metric, fill = "player")) +
      geom_col(position = "dodge") +
      labs(title = paste(input$metric, "by Match"), x = "Match", y = input$metric) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$summary_table <- renderDT({
    filtered_data() |> 
      group_by(player, position) |> 
      summarise(
        total_attack_attempts = sum(attack_attempt, na.rm = TRUE),
        total_kills = sum(kill, na.rm = TRUE),
        total_errors = sum(attack_error, na.rm = TRUE),
        overall_hitting_pct = (sum(kill, na.rm = TRUE) - 
                                 sum(attack_error, na.rm = TRUE)) 
        / sum(attack_attempt, na.rm = TRUE),
        .groups = "drop"
      )
  })
}

shinyApp(ui, server)
