library(shiny)

intro <- tabPanel(
  "Introduction",
  titlePanel("Introduction")
)


page_one <- tabPanel(
  "Page 1" # Page 1 content here, change title as needed
)

page_two <- tabPanel(
  "Page 2" # Page 3 content here, change title as needed
)

page_three <- tabPanel(
  "Page 3" # Page 3 content here, change title as needed
)

summary <- tabPanel(
  "Summary",
  titlePanel("Summary")
)

ui <- navbarPage(
  "Relationships between Education and Socioeconomic Factors",
  intro,
  page_one,
  page_two,
  page_three,
  summary
)

server <- function(input, output){
  # server elements for page 1
  
  
  # server elements for page 2
  
  
  # server elements for page 3
}

shinyApp(ui = ui, server = server)
