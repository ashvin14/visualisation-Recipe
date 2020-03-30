library(tidyverse)
get_ingredients <- function(molten_dataframe, recipe_title) {
  result <- molten_dataframe[molten_dataframe$title == recipe_title,]
  return(result$ingredients)
}


deleteButtonColumn <- function(df, id, ...) {
  # function to create one action button as string
  f <- function(i) {
    as.character(actionButton(paste(id, i, sep="_"), label = NULL, icon = icon('trash'),
                              onclick = 'Shiny.setInputValue(\"deletePressed\",  this.id, {priority: "event"})'))
  }

  df <- data.frame(df)
  names(df) <- "Ingredients"
  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))

  # Return a data table
  DT::datatable(cbind(df, Delete = deleteCol),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(list(targets = 1, sortable = FALSE))
                ))
}


parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}




