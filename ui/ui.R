library(shiny)
library(shinydashboard)
source("./data/data.R")

# define UI logic
ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "Recipe"),
  dashboardSidebar(
    selectizeInput(
      inputId = "recipe",
      label = "Recipe",
      choices = recipe_dataset$title,
      selected = NULL,
      multiple = FALSE,
      options = list(
        placeholder = 'Type to search...',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    # actionButton("Add",
    #              "Add",
    #              icon = icon("cart-plus"))
    uiOutput("add")
    
  ),
  
  dashboardBody(fluidPage(fluidRow(
    box(
      title = "Grocery List",
      solidHeader = T,
      width = 4,
      collapsible = T,
      div(DT::DTOutput("grocery_df"), style = "font-size: 70%;")
    )
  ))),
  
  skin = "green"
)
