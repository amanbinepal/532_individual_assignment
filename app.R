# Used DSCI_532_vis-2_book/code/lecture02/R/app.R and group app.py as reference

library(shiny)
library(bslib)
library(dplyr)
library(plotly)

# Load data
df <- read.csv("data/raw/supply_chain_data.csv")

# UI
ui <- page_fillable(
  title = "Supply Chain Dashboard",
  layout_sidebar(
    sidebar = sidebar(
      checkboxGroupInput(
        inputId = "input_product_type",
        label = "Product Category",
        choices = sort(unique(df$Product.type)),
        selected = sort(unique(df$Product.type))
      ),
      open = "desktop"
    ),
    layout_columns(
      value_box(
        title = "Avg Manufacturing Cost",
        value = textOutput("value_cost_unit")
      ),
      value_box(
        title = "Inspection Pass Rate",
        value = textOutput("value_pass_rate")
      ),
      
      fill = FALSE
    ),
    card(
      card_header("Customer Demographics"),
      plotlyOutput("plot_customer_demo"),
      full_screen = TRUE
    )
  )
)

# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    df %>%
      filter(
        Product.type %in% input$input_product_type
      )
  })
  
  output$value_cost_unit <- renderText({
    paste0("$", sprintf("%.2f", mean(filtered_data()$Manufacturing.costs)))
  })
  
  output$value_pass_rate <- renderText({
    rate <- mean(filtered_data()$Inspection.results == "Pass") * 100
    paste0(sprintf("%.1f", rate), "%")
  })
  
  output$plot_customer_demo <- renderPlotly({
    plot_ly(
      data = filtered_data() %>% count(Customer.demographics),
      x = ~n,
      y = ~Customer.demographics,
      type = "bar",
      orientation = "h"
    ) %>%
      layout(
        xaxis = list(title = "Total Customers"),
        yaxis = list(title = "Demographics")
      )
  })

}

# Create app
shinyApp(ui = ui, server = server)