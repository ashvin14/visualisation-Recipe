library(tidyverse)
library(jsonlite)

get_ingredients <- function(recipe_title) {
  ingredients <- list()
  result <- recipe_data[recipe_data$title == recipe_title,]
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
      )), pageLength = 7)
  )
}

parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (!is.na(res))
    res
}


nutri_table <- function(df, t, number) {
  new <-
    df %>% filter(title == c(t)) %>% dplyr::select(Fat, Energy, Protein, `Saturated fat`, Sodium, Sugar)
  tran <-
    pivot_longer(
      new,
      cols = c(1:6),
      names_to = c('Nutrition.Name'),
      values_to = 'Value'
    )
  trans <- as.data.frame(tran)
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
    temp_df <-
      recipe_data  %>% filter(title == recipe_title) %>% dplyr::select(instructions)
      instructions_list[recipe_title] <-fromJSON(gsub("'(?![a-z'])|(?<=\\{|\\s)'", '"', temp_df$instructions[1], perl = T))
     
  }
  return(instructions_list)
}

get_nutr_per_100gm <- function(recipe_list) {
  nutr_list <- list()
  for (recipe_title in recipe_list$Recipes){
    temp_df <- 
      recipe_data %>% filter(title == recipe_title) %>% dplyr:select(nutr_per_100gm)
    nutr_list[recipe_title] <- fromJSON(gsub("\'", "\"", temp_df$nutr_per_100gm))
  }
}
pie_chart_table<-function(data) {
  pie<-data%>%dplyr::select(title,Fat,Energy,Protein,Sugar,`Saturated fat`,Sodium,Energy_100,Fat_100,Proteins_100,Salt_100,`Saturated fat_100`,Sugar_100)
  pie<-melt(pie,id=c('title','Energy_100','Fat_100','Proteins_100','Salt_100','Saturated fat_100','Sugar_100'))
  pie<-pie%>%rename('nutrition.name'=variable,'quantity'=value)
  pie<-melt(pie,id=c('title','nutrition.name','quantity'))
}