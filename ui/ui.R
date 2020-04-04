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
      choices = recipe_data$title,
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
    uiOutput("add"),
    uiOutput('undoUI')
  ),
  
  dashboardBody(fluidPage(fluidRow(
    uiOutput("RecipeListUI"),
    uiOutput("groceryListUI")

  ),
  fluidRow(
    uiOutput("constituents_bar_graph")
  ))),
  
  skin = "green"
)
