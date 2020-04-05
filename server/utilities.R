library(tidyverse)
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
  for(i in 1:n){
    ingredients[i] <- ingredients_list[[1]][i]
  }
  return(ingredients)
}


deleteButtonColumn <- function(df, id, ...) {
  # function to create one action button as string
  f <- function(i) {
    as.character(actionButton(paste(id, i, sep="_"), label = NULL, icon = icon('trash'),
                              onclick = 'Shiny.setInputValue(\"deletePressed\",  this.id, {priority: "event"})'))
  }
  
  df <- data.frame(df)
  names(df) <- c("Ingredients", "No of Portions")
  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))
  
  # Return a data table
  DT::datatable(cbind(df, Delete = deleteCol),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                rownames = FALSE,
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(list(targets = 1, sortable = FALSE))
                ))
}


parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}

nutrition<- function(df,t,number) {
  new<-df%>%filter(title==c(t))%>%select(Fat,Energy,Protein,`Saturated fat`,Sodium,Sugar)
  u<-c('gram','KJ(cal)','gram','gram','mg','gram')
  tran<-pivot_longer(new,cols=c(1:6),names_to ='Nutrition Name',values_to='Value')
  trans<-as.data.frame(tran,unit=u)
  if ((!is.null(number) & number!=0)){
    trans[,2]<-trans[,2]*(number)
  } else if (number==0){
    trans<-NULL
  }
  return (trans)
}

