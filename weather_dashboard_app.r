options(repos = c(CRAN = "https://cran.rstudio.com"))
install.packages("shinyjs")
library(shiny)
library(plotly)
library(dplyr)
library(leaflet)
library(shinyjs)  # Added shinyjs for modal popup

# Load the CSV file into a data frame
csv_path <- "data/maybe.csv"
df <- tryCatch({
  read.csv(csv_path, sep = "|")  # Adjust delimiter as needed
}, error = function(e) {
  data.frame(Error = as.character(e))  # Return error message if CSV can't be loaded
})

# Check and convert 'report_date' if it's numeric (e.g., Excel date format)
if (is.numeric(df$report_date)) {
  df$report_date <- as.Date(df$report_date, origin = "1900-01-01")
} else {
  df$report_date <- as.Date(df$report_date)
}

# Add latitude and longitude to the data frame
location_coords <- data.frame(
  location_name = c("London", "Birmingham", "Manchester", "Nottingham", "Leeds", "Liverpool", "Bristol", "Newcastle upon Tyne", "Southampton", "Brighton"),
  latitude = c(51.5074, 52.4862, 53.4839, 52.9548, 53.8008, 53.4084, 51.4545, 54.9783, 50.9097, 50.8225),
  longitude = c(-0.1278, -1.8904, -2.2446, -1.1581, -1.5491, -2.9916, -2.5879, -1.6174, -1.4044, -0.1372)
)

df <- left_join(df, location_coords, by = "location_name")

# Define the UI for the Shiny app
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs for modal
  tags$head(
    tags$style(HTML("
        body {
          background: linear-gradient(270deg, #66b3ff, #ffffff);
          background-size: 400% 400%;
          -webkit-animation: gradientAnimation 30s ease infinite;
          animation: gradientAnimation 30s ease infinite;
          padding: 20px;
        }
        .container-fluid {
          margin-bottom: 30px;
        }
        .well {
          margin-bottom: 20px;
        }
        @keyframes gradientAnimation {
          0% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
          100% { background-position: 0% 50%; }
        }
        /* Reduce size of Leaflet control buttons (+/-) */
        .leaflet-control-zoom-in,
        .leaflet-control-zoom-out {
          width: 20px;
          height: 20px;
          font-size: 14px;
        }
        /* Customize plotly legends */
        .plotly .legend {
          font-size: 16px;
          font-weight: bold;
        }
        /* Customize buttons */
        .btn-primary {
          background-color: #007bff;
          border-color: #007bff;
        }
        .btn-primary:hover {
          background-color: #0056b3;
          border-color: #0056b3;
        }
      "))
  ),

  h2("English Weather Stats", style = "margin-bottom: 40px; text-align: center; color: #333; font-weight: 600;"),

  sidebarLayout(
    sidebarPanel(
      h4("Filter Options"),
      selectInput("location_filter", "Select Location:", choices = c("All", unique(df$location_name)), selected = "All"),
      sliderInput("date_range", "Select Date Range:", min = min(df$report_date), max = max(df$report_date),
                  value = c(min(df$report_date), max(df$report_date)), timeFormat = "%Y-%m-%d"),
      actionButton("apply_filter", "Reset Filter", class = "btn btn-primary", style = "margin-top: 20px;"),

      h4("Location Map", style = "margin-top: 30px;"),
      leafletOutput("map", height = "300px"),
      width = 3
    ),

    mainPanel(
      fluidRow(column(12, h4("CSV Viewer"), div(uiOutput("csv_scroller"),
                                                style = "height: 300px; overflow-y: scroll; overflow-x: auto; border: 1px solid #ccc; padding: 10px; background-color: white; width: 100%; box-sizing: border-box; text-align: left;"), br())),
      fluidRow(column(12, h4("Temperature vs. Time Plot"), plotlyOutput("temperature_plot")), br()),
      fluidRow(column(9, h4("Weather Condition Count"), plotlyOutput("weather_condition_histogram")),
               column(3, h4("UV Index Level Count"), plotlyOutput("uv_bar_chart")), br())
    )
  )
)

# Define the server logic
server <- function(input, output, session) {

  get_filtered_data <- reactive({
    filtered_data <- df
    if (input$location_filter != "All") {
      filtered_data <- filtered_data %>% filter(location_name == input$location_filter)
    }
    filtered_data <- filtered_data %>%
      filter(report_date >= input$date_range[1] & report_date <= input$date_range[2])
    return(filtered_data)
  })

  output$csv_scroller <- renderUI({
    df_to_display <- get_filtered_data()
    tableOutput("csv_table")
  })

  output$csv_table <- renderTable({
    df_to_display <- get_filtered_data()
    df_to_display$report_date <- format(df_to_display$report_date, "%Y-%m-%d")
    df_to_display %>% select(-latitude, -longitude)  # Hide coordinates in CSV output
  })

  output$temperature_plot <- renderPlotly({
    filtered_data <- get_filtered_data()
    plot_ly(data = filtered_data, x = ~report_date, y = ~current_temperature, type = "scatter", mode = "lines+markers", name = "Temperature",
            line = list(shape = "spline", smoothing = 0.3)) %>%
      layout(
        title = "Temperature vs Time",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Temperature (°C)"),
        hovermode = "closest",
        plot_bgcolor = "#f2f2f2",
        transition = list(duration = 500)  # Smooth transition
      )
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

    # Create the histogram plot with animation
    fig <- plot_ly(
      data = weather_counts,
      x = ~weather_condition,
      y = ~n,
      type = "bar",
      name = "Weather Condition Count",
      marker = list(color = 'rgba(55, 128, 191, 0.7)', line = list(color = 'rgba(0,0,0,0.1)', width = 1)),
      animation_opts = list(frame = list(duration = 1000, redraw = TRUE), fromcurrent = TRUE)
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

  output$map <- renderLeaflet({
    filtered_data <- get_filtered_data()
    leaflet(data = filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(~longitude, ~latitude, popup = ~location_name, radius = 8, color = "blue", opacity = 0.7, fillOpacity = 0.6)
  })

  observeEvent(input$apply_filter, {
    updateSliderInput(session, "date_range", value = c(min(df$report_date), max(df$report_date)))
    updateSelectInput(session, "location_filter", selected = "All")
  })

  # Show the modal with instructions on the first run
  observe({
    showModal(modalDialog(
      title = "Welcome to the Weather App",
      "This app provides interactive plots of weather data. You can filter by location and date.
      Use the Plotly graphs' interactive features like zoom, hover, and click-drag to explore the data further.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

shinyApp(ui = ui, server = server)
