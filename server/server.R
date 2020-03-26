library(shiny)
source("./data/data.R")
library(reshape2)
source("./server/utilities.R")
# define server logic
server <- function(input, output) {
  values <-
    reactiveValues(dataset = NULL,
                   molten_dataset  = NULL)
  
  grocery_df <- shiny::reactiveValues()
  
  # change this
  grocery_df$df <- data.frame("ingredient" = character(),
                              stringsAsFactors = F)
  
  observeEvent(input$recipe, {
    values$dataset <- molten_filtered_recipe_dataset2
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
    ingredients <-
      as.vector(get_ingredients(molten_filtered_recipe_dataset2, input$recipe))
    for (i in 1:length(ingredients)) {
      isolate(grocery_df$df[nrow(grocery_df$df) + 1,] <-
                ingredients[i])
    }
    
    print(ingredients)
    output$groceryListUI <- renderUI({
      box(
        title = "Grocery List",
        solidHeader = T,
        width = 6,
        collapsible = T,
        h5(input$recipe),
        div(DT::DTOutput("grocery_df"), style = "font-size: 70%;")
      )
    })
  })
  
  
  output$grocery_df <- DT::renderDataTable(grocery_df$df,
                                           # colnames = c("Quantity", "Units", "Ingredient"),
                                           rownames = F)
}
