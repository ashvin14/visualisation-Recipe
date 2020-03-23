library(shiny)
library(shinydashboard)


# define UI logic
ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "Recipe"),
  dashboardSidebar(
    selectizeInput(inputId = "recipe", 
                   label = "Recipe", 
                   choices = recipe_dataset$title,
                   selected = NULL, 
                   multiple = FALSE,
                   options = list(
                     placeholder = 'Type to search ',
                     onInitialize = I('function() { this.setValue(""); }'))),
    
      #inputId = "add",
      #label = "Add to List",
      #icon = icon("cart-plus"),
      #width = "100%"
    #)
    actionButton("Add", 
                 "Add",
                 icon = icon("cart-plus"))
    
  ),
  
  dashboardBody(
    fluidPage(fluidRow(
      column(6, )
    ))
  ),
  
  #skin = "green"
)
 