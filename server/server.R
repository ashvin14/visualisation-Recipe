library(shiny)
source("./data/data.R")
library(reshape2)
source("./server/utilities.R")
# define server logic
library(stringr)

server <- function(input, output) {
  values <-
    reactiveValues(dataset = NULL,
                   molten_dataset  = NULL)
  
  grocery_df <- shiny::reactiveValues()
  
  recipe_df <- shiny::reactiveValues()
  
  grocery_data <- shiny::reactiveValues()
  
  # change this
  # grocery_df$df <- data.frame("ingredient" = character(),"Portion Size" = integer(),
  #                             stringsAsFactors = F)
  
  recipe_df$df <- data.frame("Recipes.." = character(),
                             stringsAsFactors = F )
  
  grocery_data$df <- data.frame("Recipes.." = character(),"Portion Size" = integer(),
                             stringsAsFactors = F )
  
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
      as.vector(get_ingredients(input$recipe))
    
    recipe_df$df[nrow(recipe_df$df)+1,] <- input$recipe
    
    #grocery_df$df <- as.data.frame(grocery_df$df, stringsAsFactors = F)
    grocery_data$df <- as.data.frame(grocery_data$df, "Portion Size" = integer(),
                                     stringsAsFactors = F)
    
    for (i in 1:length(ingredients)) {
      
      if(!(any(grocery_data$df$ingredient == ingredients[i]))){
        grocery_data$df[nrow(grocery_data$df) + 1,] <-
          c(ingredients[i],1)}
      else 
      { 
        k <- grocery_data$df[grocery_data$df$ingredient == ingredients[i],] 
        #print(k)
        grocery_data$df$Portion.Size[grocery_data$df$ingredient == ingredients[i]] <- as.integer(k$Portion.Size) + 1
        #print(grocery_data$df)
      }
    }
    
    
    output$groceryListUI <- renderUI({
      box(
        title = "Grocery List",
        solidHeader = T,
        width = 6,
        collapsible = T,
        h5(input$recipe),
        div(DT::DTOutput("grocery_data"), style = "font-size: 70%;")
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
    recipe_df$df},rownames = FALSE
  )
  
  output$grocery_data <- DT::renderDataTable(
    deleteButtonColumn(grocery_data$df, 'delete_button'))
  
  observeEvent(input$deletePressed, {
    rowNum <- parseDeleteEvent(input$deletePressed)
    grocery_data$df <- data.frame(grocery_data$df, stringsAsFactors =F)
    dataRow <- grocery_data$df[rowNum, ]
    values$deletedRows <- rbind(dataRow, values$deletedRows)
    values$deletedRowIndices <- append(values$deletedRowIndices, rowNum, after = 0)
    grocery_data$df <- grocery_data$df[-(rowNum), ]
  })
  
}