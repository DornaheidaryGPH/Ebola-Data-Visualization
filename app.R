# Libraries used in this app:
# shiny     -> builds the interactive web app (UI + server)
# ggplot2   -> creates the plots
# dplyr     -> data wrangling (filter, mutate, summarize)
# tidyr     -> reshaping data and drop_na()

# Auto-install required packages (only installs what's missing)
packages <- c("shiny", "ggplot2", "dplyr", "tidyr")

missing <- packages[!packages %in% rownames(installed.packages())]
if (length(missing) > 0) {
  install.packages(missing, dependencies = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))

# Load the dataset directly from the CSV
df_raw <- read.csv("country_timeseries.csv", stringsAsFactors = FALSE)

# Prepare a clean, simple dataset for the app
df <- df_raw %>%
  select(-Day) %>% # Drop the Day column as Date is enough
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  # Reshape so 'Cases' and 'Deaths' are in one column, and 'Countries' in another
  pivot_longer(
    cols = -Date,
    names_to = c("Indicator", "Country"),
    names_sep = "_",
    values_to = "Count"
  ) %>%
  tidyr::drop_na() # Remove missing values to keep charts clean

ui <- fluidPage(
  titlePanel("Simple Ebola Outbreak Tracker (2014-2015)"),
  
  sidebarLayout(
    sidebarPanel(
      # Filter 1: Country
      selectInput("country", "1. Country", 
                  choices = c("All (Top 5)", "Liberia", "SierraLeone", "Guinea", "Nigeria", "Mali"), 
                  selected = "All (Top 5)"),
      
      # Filter 2: Time Period
      selectInput("time_period", "2. Outbreak Stage", 
                  choices = c("All Time", 
                              "Early outbreak (Mar - Jun 2014)", 
                              "Peak transmission (Aug - Dec 2014)", 
                              "Decline (2015)"), 
                  selected = "All Time"),
      
      # Filter 3: Indicator (Cases or Deaths)
      radioButtons("indicator", "3. Case Type", 
                   choices = c("Cases", "Deaths"), 
                   selected = "Cases")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Trend Over Time", plotOutput("linePlot", height = 360)),
        tabPanel("Total by Country", plotOutput("barPlot", height = 360))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive function to filter data based on user selections
  filtered_data <- reactive({
    d <- df
    
    # 1. Filter by Case Type (Cases or Deaths)
    d <- d %>% filter(Indicator == input$indicator)
    
    # 2. Filter by Country
    if (input$country != "All (Top 5)") {
      d <- d %>% filter(Country == input$country)
    } else {
      # If "All" is selected, only show the 5 requested countries
      d <- d %>% filter(Country %in% c("Liberia", "SierraLeone", "Guinea", "Nigeria", "Mali"))
    }
    
    # 3. Filter by Time Period using simple dates
    if (input$time_period == "Early outbreak (Mar - Jun 2014)") {
      d <- d %>% filter(Date >= as.Date("2014-03-01") & Date <= as.Date("2014-06-30"))
    } else if (input$time_period == "Peak transmission (Aug - Dec 2014)") {
      d <- d %>% filter(Date >= as.Date("2014-08-01") & Date <= as.Date("2014-12-31"))
    } else if (input$time_period == "Decline (2015)") {
      d <- d %>% filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2015-12-31"))
    }
    
    d
  })
  
  # Output 1: Line Plot (Trend)
  output$linePlot <- renderPlot({
    d <- filtered_data()
    
    # Show a message if no data matches the filters
    if (nrow(d) == 0) {
      plot.new()
      text(0.5, 0.5, "No data for the selected filters", cex = 1.2)
      return()
    }
    
    ggplot(d, aes(x = Date, y = Count, color = Country)) +
      geom_line(size = 1) +
      geom_point() +
      theme_minimal() +
      labs(x = "Date", y = input$indicator)
  })
  
  # Output 2: Bar Plot (Totals)
  output$barPlot <- renderPlot({
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      plot.new()
      text(0.5, 0.5, "No data for the selected filters", cex = 1.2)
      return()
    }
    
    # Get the highest count recorded per country for the bar chart
    d_summary <- d %>%
      group_by(Country) %>%
      summarize(Total = max(Count, na.rm = TRUE))
    
    ggplot(d_summary, aes(x = Country, y = Total, fill = Country)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Country", y = paste("Total", input$indicator))
  })
}

shinyApp(ui, server)