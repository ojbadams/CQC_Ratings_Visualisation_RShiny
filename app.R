#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(rgdal)
library(leaflet)
library(rgeos)

cqc_data <- read.csv("./Data_Files/cqc_long_lat.csv")
funding_data <- read.csv("./Data_Files/final_pop_fund_data.csv")

possible_categories <- levels(cqcData$Location.Primary.Inspection.Category)
possible_categories <- possible_categories[-which(possible_categories=="GP Practices")] #nolint

list_of_regions <- levels(cqcData$Location.Region)
list_of_regions <- list_of_regions[-which(list_of_regions == "(pseudo) Wales")]

shape_data <-
            readOGR(
                dsn=path.expand("./Shp_Files/Regions_(December_2019)_Boundaries_EN_BFE.shp") # nolint
            )
shape_data <-
            spTransform(
                shape_data, CRS("+proj=longlat +datum=WGS84 +no_defs")
            )

pal <- colorFactor(
                    c("green",
                    "red",
                    "black",
                    "lime green",
                    "orange"
                    ),
                    domain = levels(cqcData$Latest.Rating)
                )

north_regions <- c("North West", "North East", "Yorkshire and The Humber")
south_regions <- c("East of England", "South West", "London")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("North Vs South"),
    fluidRow(
        column(3,
                radioButtons(
                                inputId = "healthType",
                                label = "Private, Public or Both",
                                choices = c("Private", "Public", "Both"),
                                inline = T
                                ),
                selectInput(
                                inputId = "type",
                                label = "Institute",
                                choices = possible_categories
                                ),
                selectInput(
                                inputId = "region1",
                                label = "Region1",
                                choices = c("North", list_of_regions)
                            ),
                selectInput(
                                inputId = "region2",
                                label = "Region2",
                                choices = c("South", list_of_regions)
                            )
                ),
        column(5,
                leafletOutput("CQCMap")
        ),
        column(4,
            #   plotOutput(inputId = "timeSeriesPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = "darkgray", border = "white")
    })
    output$CQCMap <- renderLeaflet({
        cqcData <- cqcData[which(cqcData$Location.Primary.Inspection.Category == input$type), ] #nolint
        cqcData <- cqcData[which(cqcData$Location.Region == input$region1 | cqcData$Location.Region == input$region2), ] #nolint

        leaflet(height = "300px", width = "300px", options = leafletOptions(preferCanvas = T)) %>%  #nolint
             addTiles() %>%
             addPolygons(data = shape_data,
                         col = "black",
                         weight = 3,
                         fillColor = ~reg_color(fundingData$X2019.20),
                         fillOpacity = 0.5,
                         label = ~rgn19nm) %>%
             addCircleMarkers(
                 data = cqcData[c("lat", "long")],
                 color = ~pal(cqcData$Latest.Rating),
                 fillColor = ~pal(cqcData$Latest.Rating),
                 radius = 2.5
                 )
    })

    output$comparePlot <- renderPlot({
        # by_region <- cqcData %>% 
        #             count(Location.Region, Latest.Rating, sort = T) %>%
        #             arrange(Latest.Rating)

        by_region <- by_region[which(by_region$Location.Region == input$region1
                                | by_region$Location.Region == input$region2), ]

        barplot(by_region$n, col = by_region$Latest.Rating)

    })
}

shinyApp(ui = ui, server = server)