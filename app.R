library(shiny)
library(ggplot2)
library(rsconnect)
df <- read.csv("Car_sales.csv")

ui <-shinyUI(fluidPage(
  titlePanel("Car Sales vs Various Variables"),
  sidebarLayout(
    sidebarPanel(
                 radioButtons("radio","Car sales vs:",
                              c("Price (in thousand dollars)" ="dhorse",
                                "Fuel Efficiency (in mi/gal)" = "dfuel")),
                 sliderInput(inputId = "bins",
                             label = "Number of steps:",
                             min = 1,
                             max = 100,
                             value = c(1,30),
                             step = 1)),
    
    mainPanel(plotOutput("sales"))
  )
))

shinyServer(server <- function(input, output){
  
  observe({
    if (input$radio == "dhorse"){
      dat1 <- reactive({
        plot1 <- df[df$Price_in_thousands %in% seq(from=min(input$bins),to=max(input$bins),by=0.0001),]
        plot1
      })
      
      output$sales <-renderPlot({
        ggplot(dat1(), aes(x=Sales_in_thousands, y=Price_in_thousands))+geom_point(colour='red') + xlab("Sales in thousdands") + ylab("Price (in thousand dollars)")
      })
    }
    else if (input$radio == "dfuel"){
      dat2 <- reactive({
        plot2 <- df[df$Fuel_efficiency %in% seq(from=min(input$bins),to=max(input$bins),by=1),]
        plot2
      })
      
      output$sales <-renderPlot({
        ggplot(dat2(), aes(x=Sales_in_thousands, y=Fuel_efficiency))+geom_point(colour='blue') + xlab("Sales in thousdands") + ylab("Fuel Efficiency (in mi/gal)")
      })
    }
  })
  
  
  
  
  
  

  })
shinyApp(ui, server)