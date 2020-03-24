library(shiny)
source("./data/data.R")

# define server logic
server <- function(input, output) {
  values <- reactiveValues(dataset = NULL)
  
  grocery_df <- siny::reactiveValues()
  
  grocery_df$df <- data.frame("title" = character(),
                              "rating" = numeric(),
                              stringsAsFactors = F)
  
  observeEvent(input$recipe, {
    values$dataset <- recipe_dataset
    values$colnames <- colnames(recipe_dataset)
    
    output$add <- renderUI({
      actionButton(
        inputId = "Add",
        label = "Add",
        icon = icon("cart-plus")
      )
    })
  })
  observeEvent(input$Add, {
    isolate(grocery_df$df[nrow(grocery_df$df) + 1, ] <-
              c(input$recipe,
                as.numeric(recipe_dataset$rating[recipe_dataset$title == input$recipe])))
  })
}
