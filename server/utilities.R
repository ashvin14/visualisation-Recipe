library(tidyverse)
get_ingredients <- function(molten_dataframe, recipe_title) {
  result <- molten_dataframe[molten_dataframe$title == recipe_title,]
  return(result$ingredients)
}

