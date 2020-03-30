library(tidyverse)
get_ingredients <- function(molten_dataframe, recipe_title) {
  result <- molten_dataframe[molten_dataframe$title == recipe_title, ]
  return(result$ingredients)
}


get_constituents <- function(molten_dataframe, recipe_title) {
  result <- molten_dataframe[molten_dataframe$title == recipe_title,]
  constituents <- tibble(
    "calories" = result$calories,
    "protein" = result$protein,
    "fat" = result$fat,
    "sodium" = result$sodium
  )
  return(constituents)
}