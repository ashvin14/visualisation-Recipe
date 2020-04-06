




library(shiny)
source('./server/utilities.R')
source("./data/data.R")
library(reshape2)


source('./server/utilities.R')

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
  
  recipe_df$df <- data.frame("Recipes" = character(),
                             stringsAsFactors = F)
  
  grocery_data$df <-
    data.frame(
      "Ingredients" = character(),
      "Weight" = integer(),
      stringsAsFactors = F
    )
  
  
  
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
    
    recipe_df$df[nrow(recipe_df$df) + 1,] <- input$recipe
    
    #grocery_df$df <- as.data.frame(grocery_df$df, stringsAsFactors = F)
    grocery_data$df <-
      as.data.frame(grocery_data$df,
                    "Weight" = integer(),
                    stringsAsFactors = F)
    Weights <- as.vector(get_Weight(input$recipe))
    
    for (i in 1:length(ingredients)) {
      #grocery_data$df$ingredient[i] <- grocery_data$df$ingredient[[1]][i]
      print('xyz-------------')
      print(grocery_data$df$Ingredients)
      print('abc-------------')
      # print(ingredients[[i]])
      if (!(any(grocery_data$df$Ingredients == ingredients[i]))) {
        print('abc-------------')
        grocery_data$df[nrow(grocery_data$df) + 1,] <-
          c(ingredients[i], round(as.double(Weights[i]), 2))
        print(grocery_data$df[nrow(grocery_data$df) + 1,])
        
      }
      
      else
      {
        k <-
          grocery_data$df[grocery_data$df$Ingredients == ingredients[i],]
        #print(k)
        weight <-
          as.double(grocery_data$df$Weight[grocery_data$df$Ingredients == ingredients[i]]) + as.double(Weights[i])
        grocery_data$df$Weight[grocery_data$df$Ingredients == ingredients[i]] <-
          round(weight, 2)
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
    
    output$instructionUI <- renderUI({
      selectInput("InstructionRecipe",
                  label = "Instructions",
                  choices = recipe_df$df$Recipes)
    })
    
    #box(recipe_df$df$Recipes[i],cola)
    
    
  })
  
  observeEvent(input$InstructionRecipe, {
    instructions <- get_instructions(recipe_df$df)
    output$instructionSteps <- renderUI({
      for (instructions in instructions[input$InstructionRecipe]) {
        return(lapply(1:length(instructions), function(i) {
          box(
            title = paste("Step ", i),
            width = NULL,
            solidHeader = TRUE,
            status = "warning",
            renderText(instructions[i])
          )
        }))
        
      }
    })
  })
  
  
  
  output$recipe_df <- DT::renderDataTable({
    recipe_df$df
  }, rownames = FALSE)
  
  output$grocery_data <-
    DT::renderDataTable(deleteButtonColumn(grocery_data$df, 'delete_button'))
  
  observeEvent(input$deletePressed, {
    rowNum <- parseDeleteEvent(input$deletePressed)
    grocery_data$df <-
      data.frame(grocery_data$df, stringsAsFactors = F)
    dataRow <- grocery_data$df[rowNum, ]
    values$deletedRows <- rbind(dataRow, values$deletedRows)
    values$deletedRowIndices <-
      append(values$deletedRowIndices, rowNum, after = 0)
    grocery_data$df <- grocery_data$df[-(rowNum), ]
  })
  
  
  
  output$quantity <- renderUI({
    numericInput(
      'number',
      'Enter the number of servings',
      value = 1,
      min = 0,
      max = 100,
      step = 1
    )
    
  })
  
  df <-
    data.frame('Nutrition.Name' = character(), 'Value' = integer())
  
  observeEvent(input$Add, {
    values$number <- input$number
    for (i in recipe_df$df$Recipes) {
      de <- nutri_table(recipe_data, i, values$number)
      df <- rbind(df, de)
    }
    output$table2 <- DT::renderDataTable({
      shiny::validate(need(df, ''))
      df <-
        df %>% group_by(Nutrition.Name) %>% summarize(Value = sum(Value))
      u <- c('KJ(cal)', 'gram', 'gram', 'gram', 'mg', 'gram')
      values$col1 <- data.frame(df, units = u)
      values$col1
    })
  })
  
  
  output$calories <- renderValueBox ({
    validate(need(values$col1, ''))
    dat <-
      values$col1 %>% filter(Nutrition.Name == 'Energy') %>% select(Value)
    if (nrow(dat) > 0) {
      valueBox(
        paste0(dat$Value, 'Kcal'),
        'Energy',
        icon = icon('fire'),
        color = 'orange',
        width = NULL
      )
    }
    
    else{
      valueBox(
        'Add Recipe',
        'Energy',
        icon = icon('fire'),
        color = 'orange',
        width = NULL
      )
    }
    
  })
  output$Protein <- renderValueBox({
    validate(need(values$col1, ''))
    df1 <-
      values$col1 %>% filter(Nutrition.Name == 'Protein') %>% select(Value)
    if (nrow(df1) > 0) {
      valueBox(
        paste0(df1$Value, 'Grams'),
        'Protein',
        icon = icon('child'),
        color = 'green',
        width = NULL
      )
    }
    else{
      valueBox(
        'Add Recipe',
        'Protein',
        icon = icon('child'),
        color = 'green',
        width = NULL
      )
    }
  })
  
}
