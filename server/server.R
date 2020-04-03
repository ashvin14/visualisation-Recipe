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
  
  recipe_df <- shiny::reactiveValues()
  
  # change this
  grocery_df$df <- data.frame("ingredient" = character(),
                              stringsAsFactors = F)
  
  recipe_df$df <- data.frame("Recipes.." = character(),
                          stringsAsFactors = F)
  
  observeEvent(input$recipe, {
 
    output$add <- renderUI({
      actionButton(
        inputId = "Add",
        label = "Add",
        icon = icon("cart-plus")
      )
    })
    
    values$deletedRows <- NULL
    values$deletedRowIndices = list()
  })
  observeEvent(input$Add, {
    ingredients <-
      as.vector(get_ingredients(molten_filtered_recipe_dataset2, input$recipe))
    recipe_df$df[nrow(recipe_df$df)+1,] <- input$recipe
    grocery_df$df <- as.data.frame(grocery_df$df, stringsAsFactors = F)
    for (i in 1:length(ingredients)) {
      grocery_df$df[nrow(grocery_df$df) + 1,] <-
                ingredients[i]
    }

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
    
    output$RecipeListUI <- renderUI({
      box(
        title = "Recipes...",
        solidHeader = T,
        width = 6,
        collapsible = T,
        div(DT::DTOutput("recipe_df"), style = "font-size: 70%;")
      )
    })
    
  })
  
  output$recipe_df <- DT::renderDataTable({
    recipe_df$df}
  )
  
  output$grocery_df <- DT::renderDataTable(
    deleteButtonColumn(grocery_df$df, 'delete_button'))
  
  observeEvent(input$deletePressed, {
    rowNum <- parseDeleteEvent(input$deletePressed)
    grocery_df$df <- data.frame(grocery_df$df, stringsAsFactors =F)
    dataRow <- grocery_df$df[rowNum, ]
    values$deletedRows <- rbind(dataRow, values$deletedRows)
    values$deletedRowIndices <- append(values$deletedRowIndices, rowNum, after = 0)
    grocery_df$df <- grocery_df$df[-(rowNum), ]
  })
  
}

