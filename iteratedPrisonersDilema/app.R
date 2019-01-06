library(shiny)
library(plotly)
source('iteratedPrisonersDilema.R')

strategies <- list(optimist,
                   pessimist,
                   forgiving,
                   vengeful,
                   random_,
                   tit_4_tat,
                   pre_tit_4_tat)
names(strategies) <- lapply(strategies, "[[", 'name')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Iterated Prisoner's Dilema"),
   
   
   # Sidebar with a slider input for number of bins
   sidebarLayout(sidebarPanel(
     fluidRow(
       column(
         6 ,
         selectInput(
           "my_strategy",
           "Select your strategy:",
           names(strategies),
           selected = 'random'
         ),
         radioButtons(
           "my_start",
           "Select your first move",
           c("random",
             "defect",
             "cooperate"),
           selected = "random"
         )
       ),
       column(
         6 ,
         selectInput(
           "their_strategy",
           "Select opponent strategy:",
           names(strategies),
           selected = 'random'
         ),
         radioButtons(
           "their_start",
           "Select opponent first move",
           c("random",
             "defect",
             "cooperate"),
           selected = "random"
         )
       )
     ),
     sliderInput(
       "iterations",
       "Iterations",
       min = 2,
       max = 100,
       value = 10
     ),
     actionButton("RUN", "Run")
   ),
   
   # Show a plot of the generated distribution
   mainPanel(
     tabsetPanel(
       tabPanel('Outcomes by Round',  tableOutput('outcomes')),
       tabPanel('Turns by Turn', tableOutput('turns')),
       tabPanel('Overall Scores', tableOutput('scores'))
     )
   )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$RUN, {
    f1 <- strategies[[input$my_strategy]]
    f2 <- strategies[[input$their_strategy]]
    iterations <- as.numeric(input$iterations)
    start_move <- sample(c(0, 1), 2)
    if (input$my_start != 'random') {
      start_move[1] <- ifelse(input$my_start == 'defect', 1, 0)
    }
    if (input$their_start != 'random') {
      start_move[2] <- ifelse(input$their_start == 'defect', 1, 0)
    }
    result <-
      pd$game(f1,
              f2,
              rounds = iterations,
              random = F,
              start = start_move)
    output$outcomes <- renderTable(t(result$outcomes),
                                   striped = T,
                                   bordered = T,
                                   rownames = T, 
                                   digits = 0, 
                                   align = 'c')
    output$turns <- renderTable(t(result$turns),
                                striped = T,
                                bordered = T,
                                rownames = T, 
                                digits = 0, 
                                align = 'c')
    output$scores <- renderTable(t(result$scores),
                                 striped = T,
                                 bordered = T,
                                 rownames = T, 
                                 digits = 0, 
                                 align = 'c')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

