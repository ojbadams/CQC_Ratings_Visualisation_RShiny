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

cqcData = read.csv("/home/ojba/Documents/University_Work/DALT7016/rshiny_NHS_data/cqc_long_lat.csv")
fundingData = read.csv("/home/ojba/Documents/University_Work/DALT7016/rshiny_NHS_data/final_pop_fund_data.csv")

possible_categories = levels(cqcData$Location.Primary.Inspection.Category)
possible_categories = possible_categories[-which(possible_categories=="GP Practices")]

list_of_regions = levels(cqcData$Location.Region)
list_of_regions = list_of_regions[-which(list_of_regions == "(pseudo) Wales")]

shapeData = readOGR(dsn = path.expand("/home/ojba/Documents/University_Work/DALT7016/rshiny_NHS_data/Regions_(December_2019)_Boundaries_EN_BFE.shp"))
shapeData = spTransform(shapeData, CRS("+proj=longlat +datum=WGS84 +no_defs"))
#shapeData = gSimplify(shapeData, tol = 0.01, topologyPreserve = F)

pal = colorFactor(c("green", "red", "black", "lime green", "orange"), domain = levels(cqcData$Latest.Rating))

#reg_color = colorNumeric(palette = "magma", domain = range(6977592000:26585776000, by = 1000000000000))

North_Regions = c("North West", "North East", "Yorkshire and The Humber")
South_Regions = c("East of England", "South West", "London")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("North Vs South"),

    
    fluidRow(
        column(3, 
               radioButtons(inputId = "healthType", label = "Private, Public or Both",
                            choices = c("Private", "Public", "Both"), inline = T),
               selectInput(inputId = "type", label = "Institute", choices = possible_categories),
               selectInput(inputId = "region1", label = "Region1", choices = c("North", list_of_regions)),
               selectInput(inputId = "region2", label = "Region2", choices = c("South", list_of_regions))
                
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
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    #https://rstudio-pubs-static.s3.amazonaws.com/359811_ef55c6bd08ce40a8b82b57274a96bbb0.html#4_create_maps
    output$CQCMap = renderLeaflet({
        cqcData = cqcData[which(cqcData$Location.Primary.Inspection.Category == input$type), ]
        cqcData = cqcData[which(cqcData$Location.Region == input$region1 | cqcData$Location.Region == input$region2), ]
        
        leaflet(height = "300px", width = "300px", options = leafletOptions(preferCanvas = T)) %>% 
             addTiles() %>%
             addPolygons(data = shapeData,
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
    
#    output$timeSeriesPlot({
        #Placeholder
        
#    })
    

    output$comparePlot = renderPlot({
        by_region = cqcData %>% count(Location.Region, Latest.Rating, sort = T) %>% arrange(Latest.Rating)
        by_region = by_region[which(by_region$Location.Region == input$region1 | by_region$Location.Region == input$region2), ]
        
        barplot(by_region$n, col = by_region$Latest.Rating)
        #ggplot(data = by_region, aes(fill = Latest.Rating, x = Location.Region, y = n), width = 1000) + geom_bar( stat = "identity", width = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


#addMarkers(data = cqcData[c("lat", "long")], clusterOptions = markerClusterOptions())
#addAwesomeMarkers(data = cqcData[c("lat", "long")], clusterOptions = markerClusterOptions())