library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)


# dataset
education_df <- read.csv("states_all.csv")



intro <- tabPanel(
  "Introduction",
  img("", src="https://th.bing.com/th/id/OIP.oEEcPnzQJgzlxI1DpoTduAHaBa?pid=ImgDet&rs=1"),
  h2("Introduction"),
  p("In this project, we are interested in investigating the relationship between education and socioeconomic factors in the United States. We 
    are interested in this topic because there are many disparities in education quality and outcomes between different communities throughout 
    the US. We think that it may be valuable to use data to draw conclusions about the factors that that could cause or contribute
    to these disparities."),
  
  h3("Questions"),
  p("What socioeconomic factors most strongly influence educational attainment?"),
  p("How much do certain socioeconomic factors affect education?"),
  p("What aspects of educational achievement do socioeconomic factors affect?"),
  
  h3("Data"),
  p("The dataset that we are using comes from ", a("Kaggle", href="https://www.kaggle.com/noriuk/us-education-datasets-unification-project"), 
  " and was posted by user Roy Garrard. It includes data covering state revenue, academic achievement, and enrollment, which is also broken down 
  further into race and gender. Titled U.S. Education Datasets: Unification Project, the data is aggregated from multiple .gov sites, such as 
  The Nation's Report Card, The US Census Bureau, and the National Center for Education Statistics.")
)
# Irene's Page (?)
page_one <- tabPanel(
  "Enrollment K-12", # enrollment of grades from K-12 by year
  
  sidebarPanel(
    #change the year to change the graph - set the min and max to the earliest and most recent years. set the value to the most recent?
    sliderInput("slideryear", label = h3("Year"), min = min(education_df$YEAR), max = max(education_df$YEAR), value = max(education_df$YEAR), sep = "",
                round = TRUE)
    
  ),
  mainPanel(
    #the funny graph goes here 
    plotOutput("enroll_chart")
    
  )
  
)

# Cody's Page
page_two <- tabPanel(
  "Math Score",
  h1("Highest Math Score in each State"),
  sidebarLayout(
    sidebarPanel(sliderInput(
      inputId = "mathSlider",
      label = "Select a year",
      min = min(education_df$YEAR),
      max = max(education_df$YEAR),
      value = min(education_df$YEAR),
      sep = "",
      round = TRUE
    )),
    mainPanel(
      plotOutput("map")
    )
  )
)

# Jaspreet's Page
page_three <- tabPanel(
  "Net Income",
  h1("Net Income"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "Blue",
        label = "Years",
        choices = list("1900" = "yas",
                       "2000" = "mah")
      )
    ),
    mainPanel(
      plotOutput("viz")
    )
  )
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
  education_df <- mutate(
    education_df,
    polyname = tolower(STATE)
  )
  map_df <- map_data("state")
  map_df <- mutate(
    map_df,
    polyname = region
  )
  merged_data = left_join(x = education_df, y = map_df, by = "polyname")
  
  
  
  output$map <- renderPlot({
    
  })
  
  # server elements for page 3
}

shinyApp(ui = ui, server = server)
