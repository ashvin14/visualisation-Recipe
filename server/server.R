







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
    
    recipe_df$df[nrow(recipe_df$df) + 1, ] <- input$recipe
    
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
        grocery_data$df[nrow(grocery_data$df) + 1, ] <-
          c(ingredients[i], round(as.double(Weights[i]), 2))

        print(grocery_data$df[nrow(grocery_data$df) + 1, ])
        

      }
      
      else
      {
        k <-
          grocery_data$df[grocery_data$df$Ingredients == ingredients[i], ]
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
    
    df <-
      data.frame('Nutrition.Name' = character(), 'Value' = integer())
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
    
    
    output$centralPlot <- renderUI({
      labels = list()
      
      for(i in 1:length(values$col1)){
        labels[i] <- paste(values$col1$`Nutrition.Name`[i],"(",values$col1$units[i],")")
      }
      trace1 <- list(
        hole = 0.9,
        type = "pie",
        labels = labels,
        values = values$col1$Value,
        showlegend = TRUE
      )
      aggregation_of_ingredients <- grocery_data$df %>% group_by(Ingredients) %>% count()
      print(aggregation_of_ingredients)
      trace2 <- list(
        pie = 0.4,
        type = "pie",
        labels = aggregation_of_ingredients$Ingredients,
        values = aggregation_of_ingredients$n,
        showlegend = T
      )
      layout <- list(
        xaxis = list(
                     domain = c(0.33, 0.67)),
        yaxis = list(domain = c(0.33, 0.67))
      )
      p <- plot_ly()
      p <-
        add_trace(
          p,
          hole = trace1$hole,
          type = trace1$type,
          labels = trace1$labels,
          values = trace1$values,
          showlegend = trace1$showlegend
        )
      # p <- add_trace(
      #   p,
      #   hole = trace2$hole,
      #   type = trace2$type,
      #   labels = trace2$labels,
      #   y = trace2$values,
      #   showlegend = trace2$showlegend
      # )
      p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis)
      box(p)
    })
    #box(recipe_df$df$Recipes[i],cola)
    
    
  })
  
  observeEvent(input$InstructionRecipe, {
    instructions <- get_instructions(recipe_df$df)
    output$instructionSteps <- renderUI({
      i <- 1
      steps <- ""
      #3for (instructions in instructions[input$InstructionRecipe]){
      #3  for (i in 1:length(instructions)){
      #3    steps = paste(steps,"Step", i, ':', instructions[i])
      #3    print(steps)
      #  }
        return(
          box(
            title = input$InstructionRecipe,
            width = 6,
            solidHeader = TRUE,
            status = "success",
            renderUI(
              for (instructions in instructions[input$InstructionRecipe]){
                return(lapply(1:length(instructions),function(i){
                    p(paste0("Step ",i, ": ",instructions[i]))
                }))
              }
            )
          )
        )
      #3s}
      
    })
    # output$instructionSteps <- renderUI({
    #   for (instructions in instructions[input$InstructionRecipe]) {
    #     return(lapply(1:length(instructions), function(i) {
    #       box(
    #         title = paste("Step ", i),
    #         width = NULL,
    #         solidHeader = TRUE,
    #         status = "warning",
    #         renderText(instructions[i])
    #         
    #         
    #       )
    #     }))
    #     
    #   }
    # })
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
    dataRow <- grocery_data$df[rowNum,]
    values$deletedRows <- rbind(dataRow, values$deletedRows)
    values$deletedRowIndices <-
      append(values$deletedRowIndices, rowNum, after = 0)
    grocery_data$df <- grocery_data$df[-(rowNum),]
  })
  
  
  
 
  

  df <-
    data.frame('Nutrition.Name' = character(), 'Value' = integer())
  
  observeEvent(input$Add, {
    values$number <- input$number
    for (i in recipe_df$df$Recipes) {
      print("Dimple1.................................................")
      de <- nutri_table(recipe_data, i, values$number)
      df <- rbind(df, de)
      print("Dimple2..............................................")
      print(df)
    }
    # output$table2 <- DT::renderDataTable({
    #   #shiny::validate(need(df, ''))
    #   df <-
    #     df %>% group_by(Nutrition.Name) %>% summarize(Value = sum(Value))
    #   u <- c('KJ(cal)', 'gram', 'gram', 'gram', 'mg', 'gram')
    #   values$col1 <- data.frame(df, units = u)
    #   print("Dimple3...............................................")
    #   print(values$col1)
    # })
    
    output$calories <- renderValueBox ({
      df <-
        df %>% group_by(Nutrition.Name) %>% summarize(Value = sum(Value))
      u <- c('KJ(cal)', 'gram', 'gram', 'gram', 'mg', 'gram')
      values$col1 <- data.frame(df, units = u)
      print("Dimple3...............................................")
      #validate(need(values$col1, ''))
      dat <-
        values$col1 %>% filter(Nutrition.Name == 'Energy') %>% select(Value)
      if (nrow(dat) > 0) {
        valueBox(
          paste(dat$Value, 'Kcal'),
          'Energy',
          icon = icon('fire'),
          color = 'red',
          width = NULL
        )
      }
      
      else{
        valueBox(
          'Add Recipe',
          'Energy',
          icon = icon('fire'),
          color = 'red',
          width = NULL
        )
      }
      
    })
    output$Protein <- renderValueBox({
      #validate(need(values$col1, ''))
      df1 <-
        values$col1 %>% filter(Nutrition.Name == 'Protein') %>% select(Value)
      if (nrow(df1) > 0) {
        valueBox(
          paste(df1$Value, 'gm'),
          'Protein',
          icon = icon('child'),
          color = 'maroon',
          width = NULL
        )
      }
      else{
        valueBox(
          'Add Recipe',
          'Protein',
          icon = icon('child'),
          color = 'maroon',
          width = NULL
        )
      }
    })
    output$Fat <- renderValueBox({
      #validate(need(values$col1, ''))
      df1 <-
        values$col1 %>% filter(Nutrition.Name == 'Fat') %>% select(Value)
      if (nrow(df1) > 0) {
        valueBox(
          paste(df1$Value, 'gm'),
          'Fat',
          icon = icon('drumstick-bite'),
          color = 'yellow',
          width = NULL
        )
      }
      else{
        valueBox(
          'Add Recipe',
          'Fat',
          icon = icon('baby'),
          color = 'yellow',
          width = NULL
        )
      }
    })
    output$Sodium <- renderValueBox({
      #validate(need(values$col1, ''))
      df1 <-
        values$col1 %>% filter(Nutrition.Name == 'Sodium') %>% select(Value)
      if (nrow(df1) > 0) {
        valueBox(
          paste(df1$Value, 'mg'),
          'Sodium',
          icon = icon('mortar-pestle'),
          color = 'blue',
          width = NULL
        )
      }
      else{
        valueBox(
          'Add Recipe',
          'Sodium',
          icon = icon('baby'),
          color = 'blue',
          width = NULL
        )
      }
    })
    output$Saturated_Fat <- renderValueBox({
      #validate(need(values$col1, ''))
      df1 <-
        values$col1 %>% filter(Nutrition.Name == 'Saturated fat') %>% select(Value)
      if (nrow(df1) > 0) {
        valueBox(
          paste(df1$Value, 'gm'),
          'Saturated Fat',
          icon = icon('beer'),
          color = 'orange',
          width = NULL
        )
      }
      else{
        valueBox(
          'Add Recipe',
          'Saturated Fat',
          icon = icon('pizza-slice'),
          color = 'orange',
          width = NULL
        )
      }
    })
    output$Sugar <- renderValueBox({
      #validate(need(values$col1, ''))
      df1 <-
        values$col1 %>% filter(Nutrition.Name == 'Sugar') %>% select(Value)
      if (nrow(df1) > 0) {
        valueBox(
          paste(df1$Value, 'gm'),
          'Sugar',
          icon = icon('stroopwafel'),
          color = 'lime',
          width = NULL
        )
      }
      else{
        valueBox(
          'Add Recipe',
          'Sugar',
          icon = icon('cookie'),
          color = 'lime',
          width = NULL
        )
      }
    })
  })

  # observeEvent(input$Add, {
  #   
  # })
  # 
  
  
  
}
