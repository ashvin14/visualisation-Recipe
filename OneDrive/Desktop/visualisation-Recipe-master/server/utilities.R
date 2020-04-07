


library(tidyverse)
library(jsonlite)
# get_ingredients <- function(molten_dataframe, recipe_title) {
#   result <- molten_dataframe[molten_dataframe$title == recipe_title,]
#   return(result$ingredients)
# }


get_ingredients <- function(recipe_title) {
  ingredients <- list()
  result <- recipe_data[recipe_data$title == recipe_title,]
  print(result$ingredient_list)
  ingredients_list <- strsplit(result$ingredient_list, ",")
  n <- length(ingredients_list[[1]])
  for (i in 1:n) {
    ingredients[i] <- ingredients_list[[1]][i]
  }
  return(ingredients)
}

get_Weight <- function(recipe_title) {
  Weight <- list()
  result <- recipe_data[recipe_data$title == recipe_title,]
  print('xyz..........')
  print(result$weight_per_ingr)
  print('abc..........')
  Weight_list <- strsplit(result$weight_per_ingr, ",")
  j <- length(Weight_list[[1]])
  for (i in 1:j) {
    Weight[i] <- as.double(Weight_list[[1]][i])
  }
  return(Weight)
}


deleteButtonColumn <- function(df, id, ...) {
  # function to create one action button as string
  f <- function(i) {
    as.character(
      actionButton(
        paste(id, i, sep = "_"),
        label = NULL,
        icon = icon('trash'),
        onclick = 'Shiny.setInputValue(\"deletePressed\",  this.id, {priority: "event"})'
      )
    )
  }
  
  df <- data.frame(df)
  names(df) <- c("Ingredients", "Weight in Grams")
  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))
  
  # Return a data table
  DT::datatable(
    cbind(df, Delete = deleteCol),
    # Need to disable escaping for html as string to work
    escape = FALSE,
    rownames = FALSE,
    options = list(# Disable sorting for the delete column
      columnDefs = list(list(
        targets = 1, sortable = FALSE
      )))
  )
}

# get_constituents <- function(molten_dataframe, recipe_titles) {
#
#   result <- recipe_dataset[recipe_dataset$title %in% recipe_titles$Recipes,]
#
#   consitituents <- tibble(
#     "title" = recipe_titles$Recipes,
#     "calories" = result$calories,
#     "protein" = result$protein,
#     "fat" = result$fat,
#     "sodium" = result$sodium
#   )
#   print(consitituents)
#   return(consitituents)
# }


parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (!is.na(res))
    res
}


nutri_table <- function(df, t, number) {
  new <-
    df %>% filter(title == c(t)) %>% dplyr::select(Fat, Energy, Protein, `Saturated fat`, Sodium, Sugar)
  print("printing new..")
  print(new)
  tran <-
    pivot_longer(
      new,
      cols = c(1:6),
      names_to = c('Nutrition.Name'),
      values_to = 'Value'
    )
  trans <- as.data.frame(tran)
  print(trans)
  if ((!is.null(number) & number != 0)) {
    trans[, 2] <- trans[, 2] * (number)
  } else if (number == 0) {
    trans <- NULL
  }
  return (trans)
}

get_instructions <- function(recipe_list) {
  instructions_list <- list()
  for (recipe_title in recipe_list$Recipes) {
    print(recipe_data %>% filter(title == recipe_title) %>% dplyr::select(instructions))
    temp_df <-
      recipe_data  %>% filter(title == recipe_title) %>% dplyr::select(instructions)
    instructions_list[recipe_title] <- fromJSON(gsub("\'", "\"", temp_df$instructions[1]))
     
  }
  return(instructions_list)
}
