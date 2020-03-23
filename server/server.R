library(shiny)


# define server logic
server <- function(input, output) {
  
  observeEvent(input$Add,{
    output$groceryList <- input$recipe
  }
                )
}
