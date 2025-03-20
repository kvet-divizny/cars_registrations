#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(plotly)
library(sf)
library(leaflet)
library(bslib)
library(shinyWidgets)

colors <- c("#50A2A7", "#E9B44C", "#9B2915", "#2f5f62")

cars <- read_csv("data/cars.gz", col_types = "ccccfDDiffiicfcffcfccDf")

mans <- unique(cars$znacka)


# Define UI for application that draws a histogram
ui <- page_sidebar(
        sidebar = sidebar(
            pickerInput(
                inputId = "manufacturer",
                label = "Multiple", 
                choices = mans,
                multiple = TRUE,
                options = pickerOptions(
                    container = "body",
                    liveSearch = TRUE), 
                width = "100%"
            )),
    layout_column_wrap(
        width = 1/2,
        card(
            plotlyOutput("monthly")
        ),
        card(
            plotlyOutput("monthlyPerc")
        ),
        card(
            plotlyOutput("usedProp")
        )
    )
        )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    theme_set(theme_light())
    
    manufacturer <- reactive({
        if (is.null(input$manufacturer)) {
            mans
        } else {
            input$manufacturer
        }
    })

    
    cars_filt <- reactive({
        if (is.null(input$manufacturer)) {
            cars
        } else {
        cars %>% 
            filter(znacka %in% input$manufacturer)
        }
    })
    
    cars_monthly <- reactive({
        
        cars %>% 
            group_by(month) %>% 
            summarise(
                car_count = sum(znacka %in% manufacturer(), na.rm = TRUE),
                car_perc = round(car_count / n(), digits = 6)
            )
        
    })
    
    output$monthly <- renderPlotly({
        p <- cars_monthly() %>%
            ggplot(aes(x = month, y = car_count)) +
            geom_area(fill = colors[1]) +
            labs(x = "", y = "New registrations")

        ggplotly(p)
    })
    
    output$monthlyPerc <- renderPlotly({
        p <- cars_monthly() %>% 
            ggplot(aes(x = month, y = car_perc)) +
            geom_area(fill = colors[1]) +
            scale_y_continuous(labels = scales::percent)
        
        ggplotly(p)
    })
    
    output$usedProp <- renderPlotly({
        p <- cars_filt() %>% 
            ggplot(aes(x = month, fill = stav)) +
            geom_bar(position = "fill") +
            scale_fill_manual(values = colors[2:1]) +
            scale_y_continuous(labels = scales::percent)
        
        ggplotly(p) 
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
