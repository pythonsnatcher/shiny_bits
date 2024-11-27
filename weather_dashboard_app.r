library(shiny)
library(plotly)
library(dplyr)

# Load the CSV file into a data frame
csv_path <- "/Users/snatch./PycharmProjects/Shiny/csvs/maybe.csv"
df <- tryCatch({
  read.csv(csv_path, sep = "|")  # Adjust delimiter as needed
}, error = function(e) {
  data.frame(Error = as.character(e))  # Return error message if CSV can't be loaded
})

# Check and convert 'report_date' if it's numeric (e.g., Excel date format)
# Assuming 'report_date' is numeric and represents days since 1900-01-01
if (is.numeric(df$report_date)) {
  df$report_date <- as.Date(df$report_date, origin = "1900-01-01")
} else {
  # If the date is already in proper format, no need for conversion
  df$report_date <- as.Date(df$report_date)
}

# Define the UI for the Shiny app
ui <- fluidPage(
  # Custom CSS for linear gradient background and added space
  tags$head(
    tags$style(
      HTML("
        body {
          background: linear-gradient(270deg, #66b3ff, #ffffff); /* Lighter Blue to White gradient */

          background-size: 400% 400%;
          -webkit-animation: gradientAnimation 30s ease infinite;
          -moz-animation: gradientAnimation 30s ease infinite;
          animation: gradientAnimation 30s ease infinite;
          padding: 20px; /* Padding for body */
        }
        .container-fluid {
          margin-bottom: 30px; /* Margin between sections */
        }
        .well {
          margin-bottom: 20px; /* Margin between elements */
        }

        @keyframes gradientAnimation {
          0% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
          100% { background-position: 0% 50%; }
        }
        @-webkit-keyframes gradientAnimation {
          0% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
          100% { background-position: 0% 50%; }
        }
        @-moz-keyframes gradientAnimation {
          0% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
          100% { background-position: 0% 50%; }
        }
      ")
    )
  ),

  h2("English Weather Stats", style = "margin-bottom: 40px;"),  # Adding space below the title

  sidebarLayout(
    sidebarPanel(
      h4("Filter Options"),
      selectInput(
        inputId = "location_filter",
        label = "Select Location:",
        choices = c("All", unique(df$location_name)),
        selected = "All"
      ),
      actionButton("apply_filter", "Apply Filter", class = "btn btn-primary"),

      # Slider to select date range
      sliderInput(
        inputId = "date_range",
        label = "Select Date Range:",
        min = min(df$report_date),
        max = max(df$report_date),
        value = c(min(df$report_date), max(df$report_date)),
        timeFormat = "%Y-%m-%d"
      ),
      width = 3  # Reduce sidebar width
    ),

    mainPanel(
      # Grid Layout: Using fluidRow and column for structuring the layout
      fluidRow(
        column(12,
               h4("CSV Viewer"),
               div(
                 uiOutput("csv_scroller"),
                 style = "height: 300px; overflow-y: scroll; overflow-x: auto; border: 1px solid #ccc; padding: 10px; background-color: white; width: 100%; box-sizing: border-box; text-align: left;"
               ),
               br()  # Add space after CSV viewer
        )
      ),

      fluidRow(
        column(12,
               h4("Temperature vs. Time Plot"),
               plotlyOutput("temperature_plot")
        ),
        br()  # Add space after temperature plot
      ),

      fluidRow(
        column(9,
               h4("Weather Condition Count"),
               plotlyOutput("weather_condition_histogram")
        ),
        column(3,
               h4("UV Index Level Count"),
               plotlyOutput("uv_bar_chart")
        ),
        br()  # Add space after weather and UV charts
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {

  # Reactive expression to filter data based on the selected location and date range
  get_filtered_data <- reactive({
    filtered_data <- df

    if (input$location_filter != "All") {
      filtered_data <- filtered_data %>% filter(location_name == input$location_filter)
    }

    # Filter by date range
    filtered_data <- filtered_data %>%
      filter(report_date >= input$date_range[1] & report_date <= input$date_range[2])

    return(filtered_data)
  })

  # Display the CSV in a scrollable table
  output$csv_scroller <- renderUI({
    df_to_display <- get_filtered_data()
    tableOutput("csv_table")
  })

  # Render the table
  output$csv_table <- renderTable({
    df_to_display <- get_filtered_data()

    # Format the 'report_date' column to display in the correct format
    df_to_display$report_date <- format(df_to_display$report_date, "%Y-%m-%d")

    # Return the table to be displayed
    df_to_display
  })


  # Render the temperature vs. time plot
  output$temperature_plot <- renderPlotly({
    filtered_data <- get_filtered_data()

    fig <- plot_ly(
      data = filtered_data,
      x = ~report_date,
      y = ~current_temperature,
      type = "scatter",
      mode = "lines",
      name = "Temperature"
    ) %>%
      layout(
        title = "Temperature vs Time",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Temperature (°C)"),
        margin = list(l = 20, r = 20, t = 40, b = 20)
      )

    fig
  })

  # Render the weather condition histogram
  output$weather_condition_histogram <- renderPlotly({
    filtered_data <- get_filtered_data()

    # Create a count of weather conditions and sort in descending order
    weather_counts <- filtered_data %>%
      count(weather_condition) %>%
      arrange(desc(n)) %>%
      filter(n >= 3)  # Filter out counts less than 1

    # Explicitly set the factor levels of weather_condition based on the count order
    weather_counts$weather_condition <- factor(weather_counts$weather_condition, levels = weather_counts$weather_condition)

    # Create the histogram plot
    fig <- plot_ly(
      data = weather_counts,
      x = ~weather_condition,
      y = ~n,
      type = "bar",
      name = "Weather Condition Count",
      marker = list(color = 'rgba(55, 128, 191, 0.7)', line = list(color = 'rgba(0,0,0,0.1)', width = 1))
    ) %>%
      layout(
        title = "Weather Condition Count",
        xaxis = list(title = "Weather Condition", tickangle = 45),
        yaxis = list(title = "Count"),
        margin = list(l = 40, r = 40, t = 40, b = 80)
      )

    fig
  })

  # Render the UV Index Level Count as a stacked bar chart
  output$uv_bar_chart <- renderPlotly({
    filtered_data <- get_filtered_data()

    # Count the occurrences of each UV index level
    uv_counts <- filtered_data %>%
      count(uv_index_level) %>%
      arrange(desc(n))  # Sort by count in descending order

    # Create the stacked bar chart for UV index levels (Using barmode = 'stack')
    fig <- plot_ly(
      data = uv_counts,
      x = ~uv_index_level,
      y = ~n,
      type = "bar",
      name = "UV Index Level Count",
      marker = list(color = c('rgba(255, 159, 64, 0.7)', 'rgba(255, 99, 132, 0.7)', 'rgba(54, 162, 235, 0.7)')),
      barmode = 'stack'  # This ensures the bars are stacked on top of each other
    ) %>%
      layout(
        title = "UV Index Level Count",
        xaxis = list(title = "UV Index Level"),
        yaxis = list(title = "Count"),
        margin = list(l = 40, r = 40, t = 40, b = 80)
      )

    fig
  })
}

# Create and run the Shiny app
shinyApp(ui = ui, server = server)
