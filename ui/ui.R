library(shiny)
library(shinydashboard)
source("./data/data.R")
library(dashboardthemes)

# define UI logic
ui <- dashboardPage(
  # Application title
  dashboardHeader(title = shinyDashboardLogo(
    theme = "blue_gradient")),
  dashboardSidebar(
    width = 400,
    tags$head(
      tags$style(HTML("

      .selectize-input {
        height: 600px;
        width: 300px;
        font-size: 13pt;
        padding-top: 5px;
      }

    "))
    ),
    
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
    uiOutput('quantity'),
    uiOutput("add"),
    uiOutput('undoUI')
    
  ),
  dashboardBody(shinyDashboardThemes(
    theme = "purple_gradient"),tags$head(tags$style(HTML(
    '{margin:5px;}'
  ))),
  fluidRow(width =5,(valueBoxOutput(
    'calories'
  )),
  (valueBoxOutput(
    'Protein'
  ))

  ),
  fluidPage(fluidRow(
    uiOutput("RecipeListUI"),
    uiOutput("groceryListUI")
    
  )),
  conditionalPanel(condition = 'input.table2.length >0',
  fluidRow(column(width=5,style='padding-top:100px',box(
    title='Nutrition Table',
    solidHeader = T,
    collapsible = T,
    width=NULL,
    div(DT::DTOutput('table2'))
<<<<<<< Updated upstream
  ))))),
=======
  )))))
>>>>>>> Stashed changes
  
)
