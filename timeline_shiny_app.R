library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
#install.packages("vistime")
library(vistime)
library(shinythemes)
library(DT)

#read in data
timeline <- read.csv('timeline_data.csv')
timeline$Start <- as.Date(timeline$Start, format = "%m/%d/%Y")
timeline$End <- as.Date(timeline$End, format = "%m/%d/%Y")

#ui settings
ui <- fluidPage(theme =shinytheme('cosmo'),
  titlePanel("Interactive Timeline"),
  mainPanel(
           h4("Hover over event on timeline to see details"),
           plotlyOutput("plot1"),
           htmlOutput("text3"),
           dataTableOutput("table")
           
)
)

##server functions
server <- function(input, output) {
  output$plot1 <- renderPlotly({
    p <- vistime(timeline, start = "Start", end = "End", groups = "Group", events = "Event", optimize_y = TRUE, linewidth = 40, background_lines = 1, colors = "Color", tooltips = 'Tooltips')
    pp <- plotly_build(p)
    pp$x$layout$xaxis$tickfont <- list(size = 14)
    pp$x$layout$yaxis$tickfont <- list(size = 18)
    for(i in seq_along(pp$x$data)){
      if(pp$x$data[[i]]$mode == "text") pp$x$data[[i]]$textfont$size <- 16
    }
    pp$x$layout[["yaxis"]]$tickfont <- list(size = 14)
    pp %>% ggplotly(source = "A") %>% event_register("plotly_hover")
  })
  
  # data to be used in tables and other outputs.
  table_data <- reactive({
      d <- event_data("plotly_hover", source = "A")
      if (!is.null(d)) {
        t <- d$x
        if (d$y > 1){
          y <- 'Work'
        } else { y<- 'Education'}
        
        df <- filter(timeline, End > t, Start < t, Group == y)
        df
        }
      })
  
  
  #main table
  output$table <- renderDataTable({
    df <- table_data()
    cols_to_display <- c('Timeline_description','Details') #filter table for just desired columns
    df <- df[,cols_to_display]
    if (length(df)>0){
      colnames(df)[1] <- 'Timeline Event Description'
    }
    datatable(df, rownames = FALSE, options = list(dom = 't'))
  })
  
  ##header showing selected Event
  output$text3 <- renderUI({
    d <- event_data("plotly_hover", source = "A")
    if (!is.null(d)){
      df <- table_data()
      s <- df$Event
      HTML(paste0('<p style="font-weight: bold;">','Selected Event: ',s,"</p>"))
    }
  })
  
}
  
shinyApp(ui = ui, server = server)
