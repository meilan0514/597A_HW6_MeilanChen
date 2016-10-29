#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    titlePanel("Central Limit Theory (CLT)"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Visualize the CLT through simulation."),
            
            selectInput("dist", 
                        label = "Choose a distribution",
                        choices = c("Binomial","Normal", 
                                    "Poisson",
                                    "Uniform"),
                        selected = "Uniform"),
            
            sliderInput("m",
                        "Repetition",
                        min = 1,
                        max = 30,
                        value = 5),
            
            sliderInput("n", 
                        label = "Sample size:",
                        min = 1, max = 10000, value = 200),
            
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
      
      

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )))




# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # n <- seq(1, 10000, length.out = input$n + 1)
      # m <- seq(1, 100, length.out = input$m + 1)
      n <- input$n
      m <- input$m
      
      x <- matrix(NA, n, m)
      
      for(j in 1:m){
          x[,j] <- switch(input$dist,
                          "Binomial" = rbinom(n, m, .5),
                          "Poisson" = rpois(n, 0.5),
                          "Uniform" = runif(n, 0, 1),
                          "Normal" = rnorm(n, 0.5, 1))
          xbar <- apply(x,1,mean)
      }
      
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(xbar, breaks = bins, col = 'darkgray', border = 'white')
      lines(density(xbar))

   })
}

# Run the application 
shinyApp(ui = ui, server = server)




