

library(httr)
library(jsonlite)


url <- "https://air-quality-api.open-meteo.com/v1/air-quality?latitude=22&longitude=79&hourly=pm10,pm2_5,carbon_monoxide,nitrogen_dioxide,sulphur_dioxide,ozone,aerosol_optical_depth,dust,uv_index"


api_call <- httr::GET(url)

api_call$status_code

api_call$content

api_char <-base::rawToChar(api_call$content)
api_char
api_JSON <-jsonlite::fromJSON(api_char,flatten=TRUE)
#api_JSON

data<-api_JSON$hourly

df <- as.data.frame(data)
df
#data$ozone
df$tt <- sub("^.*T", "", df$time)
df$dd <- sub("T.*$", "", df$time)
df

######################################

library(shiny)
library(tidyverse)
library(elo)
library(plotly)
library(shinydashboard)
library(DT)

library(reshape2)
library(ggplot2)

library(httr)
library(jsonlite)


ui <- dashboardPage(
  dashboardHeader(title = "Air Quality Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset Info",
               tabName = "head_tab",
               icon = icon("dashboard")),
      menuItem("Visualization part-1",
               tabName = "weight_class_tab",
               icon = icon("dashboard")),
      
      menuItem("Visualization part-2",
               tabName = "fighter_tab",
               icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "head_tab",
        fluidRow(
          box(
            title = "About Air Quality API",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            "The Air Quality API provide pollutants and pollen forecast in 11 km resolution.
             The API endpoint  accepts a geographical coordinate, a list of weather variables and responds with a 
            JSON hourly air quality forecast for 5 days. Time always starts at 0:00 today."
          )
        ),
       
        
        fluidRow(
          box(
            style = "width: 100%;", 
            dataTableOutput("pageNo"),
            width=12,
          )
        )
        
      ),
      tabItem(tabName = "weight_class_tab",
              
              box(plotlyOutput("histoPlot")),
              box(plotlyOutput("TimePlot")),
              box(plotlyOutput("Heatmap")),
              
              fluidRow(
                fluidRow(
                  box(uiOutput("date1")),
                  box(valueBoxOutput("Info", width = 10)) # Adjust width as needed
                )
              )
              
      ),
      
      tabItem(tabName = "fighter_tab",
              
              
              box(plotlyOutput("density1")),
              box(plotlyOutput("BoxPlot")),
              box(plotlyOutput("Scatter_plot")),
              fluidRow(box(uiOutput("x_bar")), 
                       (box(uiOutput("y_bar"))), 
                       box(uiOutput("x_limit"))),
              
      )
    )
  )
  
)


server <- function(input, output) {
  helper <- reactive({
    load("advanced_dashboard_helper.Rdata")
  })
  
  output$date1 <- renderUI({
    selectInput(inputId = "date",
                label = "Date:",
                choices = unique(df$dd),
                selected = unique(df$dd)[1])
    
    
  })
  output$bin1 <- renderUI({
    
    sliderInput(inputId = "bins",
                label = " bin:",
                min = 1,
                max = 100,
                value = 5)
    
  })
  
  output$x_limit <- renderUI({
    sliderInput(
      inputId = "xlimit",
      label = "X-axis limit:",
      min = 0,
      max = 400,
      value = 300
    )
  })
  
  output$y_bar <- renderUI({
    selectInput(
      inputId = "y",
      label = "Y-axis:",
      choices = c( "pm10", "pm2_5", "carbon_monoxide", "nitrogen_dioxide", "sulphur_dioxide","ozone",
                   "aerosol_optical_depth","dust","uv_index"),
      selected = "pm10")
  })
  
  
  output$x_bar <- renderUI({
    selectInput(
      inputId = "x",
      label = "x-axis:",
      choices = c( "pm10", "pm2_5", "carbon_monoxide", "nitrogen_dioxide", "sulphur_dioxide","ozone",
                   "aerosol_optical_depth","dust","uv_index"),
      selected = "pm2_5")
    
  })
  output$ylimit <- renderUI({
    sliderInput(
      inputId = "ylimit",
      label = "y-axis limit:",
      min = 0,
      max = 150,
      value = 100
    )
  })
  
  
  output$Info<-renderValueBox({

    valueBox(
      "Air Quality of India", subtitle = HTML(paste("Latitude: ", api_JSON$latitude, "<br>",
                                                     "Longitude: ", api_JSON$longitude, "<br>",
                                                     "Elevation: ", api_JSON$elevation)),
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow",
    )
      
    
    
  })
  
  
  output$pageNo <- renderDataTable({
    datatable(df, options = list(
      searching = FALSE,
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20)
    ))
    
  })
  
  # Plot code 
  output$histoPlot <- renderPlotly({
    variable <- input$x
    date <- input$date
    df_plot <- df[, !(names(df) %in% c("time", "tt", "dd"))]
    df_filtered <- df[df$dd == date, ]
    
    p <- plot_ly()
    for (col in colnames(df_plot)) {
      p <- add_trace(p, x = df$dd, y = df[[col]], type = "bar",
                     mode = "lines",
                     name = col,
                     showlegend = TRUE)
    }
    
    p <- layout(p, xaxis = list(title = "Time"), yaxis = list(title = "Value"))
   p
  })
  
  # Density Plot
  output$density1<- renderPlotly({
    ggplot(data = df, aes_string(x = input$x)) +
      geom_density(fill = "white") + 
      labs(x = paste( input$x), y = "Density") +  
      theme_minimal() +  
      theme(plot.background = element_rect(fill = "white"))  # Set white background
  })
  
  ## Heatmap
  output$Heatmap<- renderPlotly({
    variable <- input$x
    date <- input$date
    
    df_plot <- df[, !(names(df) %in% c("time", "tt", "dd"))]
    df_new <- subset(df, select = -c(time, tt, dd))
    data <- cor(df[sapply(df,is.numeric)])
    
    data1 <- melt(data)
    ggplot(data1,aes(x = Var1, y = Var2, fill = value))+
      geom_tile()+scale_fill_gradient(high = "orange", low = "white")+
      geom_tile() +
      labs(
           x = "Variable 1",
           y = "Variable 2")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  ## Lineplot
  output$TimePlot <- renderPlotly({
    variable <- input$x
    date <- input$date
    df_plot <- df[, !(names(df) %in% c("time", "tt", "dd"))]
    # Filter the data
    df_filtered <- df[df$dd == date, ]
    
    p <- plot_ly()
    
    for (col in colnames(df_plot)) {
      p <- add_trace(p, x = df_filtered$tt, y = df_filtered[[col]], type = "scatter", mode = "lines", name = col)
    }
    
    p <- layout(p, xaxis = list(title = input$date), yaxis = list(title = "Value"))
    p
    ggplotly(p) %>%
      layout(dragmode = "zoom",  
             autosize = TRUE)    
  })
  
  # Scatter plot
  output$Scatter_plot <-renderPlotly({
    
    p <- ggplot(data = df, aes_string(x = input$x, y = input$y)) +
      geom_point() +  
      coord_cartesian(xlim = c(0, input$xlimit), ylim = c(0, input$xlimit)) +  # Set axis limits
      theme_bw() 
    p <- p + theme(panel.background = element_rect(fill = "white"))
  })
  
  ## Box Plot
  output$BoxPlot <- renderPlotly({
  
    p <- ggplot(df, aes(x = as.factor(dd), y = !!sym(input$x), fill = as.factor(dd))) +
      geom_boxplot() +
      labs(x = "Class", y = input$x) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p) %>%
      layout(xaxis = list(range = c(0, 0)),  
             dragmode = "zoom", 
             dragmode = "pan",  
             autosize = TRUE)    
  })
}

shinyApp(ui = ui, server = server)