library(shiny)
library(shinydashboard)
source("./data/data.R")
library(dashboardthemes)

# define UI logic
ui <- dashboardPage(
  dashboardHeader(title = "Rapid Groceries", titleWidth = 320),
  dashboardSidebar(
    width = 320,
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
    conditionalPanel('input.recipe != ""',
                     uiOutput('quantity'),
                     uiOutput("add"))
  ),
  
  dashboardBody(
    theme_blue_gradient
    #theme_grey_light
    ,tags$head(tags$style(HTML(
      '{margin:5px;}'
    ))),
    
    fluidPage(
      fluidRow(width = 5,
               valueBoxOutput('calories'),
               valueBoxOutput('Protein'),
               valueBoxOutput('Sodium'),
               valueBoxOutput('Fat'),
               valueBoxOutput('Saturated_Fat'),
               valueBoxOutput('Sugar')
      ),tags$hr(),
      fluidRow(
        column(width = 6, uiOutput("RecipeListUI")),
        column(width = 6, plotly::plotlyOutput('bubble_chart'))
        ),tags$hr(),
      fluidRow(
        column(width =6, fluidRow(uiOutput("groceryListUI"))),
        column(width =6, plotly::plotlyOutput("centralPlot", height = "450px", width = "450px"))
      ),tags$hr(),
      fluidRow(
        column(width = 12,
               uiOutput("instructionUI"),
               uiOutput("instructionSteps"))
      ),tags$hr(),
      fluidRow(
        uiOutput('pie_chart_choices')
      ),
      fluidRow(
        width = 10,
        plotly::plotlyOutput('pie_chart', width = "1000px"),
      ),tags$hr(),
      fluidRow(
        uiOutput("MachineLearningUI")
      )

        
    ) #end fluidPage
  )
)
