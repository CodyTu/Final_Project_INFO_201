library(shiny)
library(dplyr)
library(tidyverse)
library(plotly)
library(maps)


# dataset
education_df <- read.csv("states_all.csv")


# map cleaning
map_score_df <- education_df %>%
  rename("Average 4th Grade Math Score" = AVG_MATH_4_SCORE,
         "Average 8th Grade Math Score" = AVG_MATH_8_SCORE,
         "Average 4th Grade Reading Score"= AVG_READING_4_SCORE,
         "Average 8th Grade Reading Score" = AVG_READING_8_SCORE)

# enrollment cleaning 
enroll_df <- education_df %>%
  select(YEAR, STATE, GRADES_KG_G, GRADES_4_G, GRADES_8_G, GRADES_12_G)%>%
  group_by(YEAR)%>%
  summarise(GRADES_KG_G = sum(GRADES_KG_G),
            GRADES_4_G = sum(GRADES_4_G),
            GRADES_8_G = sum(GRADES_8_G),
            GRADES_12_G = sum(GRADES_12_G)
  ) %>%
  na.omit() %>%
  rename(Kindergarten = GRADES_KG_G,
         Fourth_Grade = GRADES_4_G,
         Eighth_Grade = GRADES_8_G,
         Twelfth_Grade = GRADES_12_G)


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
  "Enrollment", # enrollment of grades from K-12 by year
  h1("Enrollment by Grade over the years"),
  sidebarPanel(
   
    selectInput(inputId = "grade", 
                label = "Select a Grade Year", 
                choices = colnames(enroll_df)[2:5], 
                selected = "Twelfth_Grade")
    
  ),
  mainPanel(
   
    plotOutput("enroll_chart"),
  )
  
)

# Cody's Page
page_two <- tabPanel(
  "Average Score",
  h1("Average Math or Reading Score in each State of 4th or 8th Grade"),
  sidebarLayout(
    sidebarPanel(selectInput(
      inputId = "scoreSelect",
      label = "Select a grade",
      choices = colnames(map_score_df)[22:25]
    )),
    mainPanel(
      plotlyOutput("map")
  )
))

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

  output$value <- renderPrint({ input$select })
  
  output$enroll_chart <- renderPlot({
    ggplot(data=enroll_df) + 
      
      geom_path(aes(x=YEAR, y=.data[[as.character(input$grade)]], color = "Students"), size = 0.5) + 
      xlim(min(education_df$YEAR), max(education_df$YEAR))+ ylim(0, 4000000)+
      
      labs(title="title", x="Year", y="Number of Students") 
    
  })
  
  # server elements for page 2
  output$map <- renderPlotly({
    states <- read.csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>% 
      mutate(STATE = toupper(State))
    
    map_education_df <- inner_join(x = map_score_df, y = states, by = "STATE") %>%
      select(YEAR, Abbreviation, avg_score = input$scoreSelect) %>%
      mutate(hover = paste0(Abbreviation, ",  Score: ", avg_score))
    
    education_graph <- plot_geo(
      map_education_df,
      locationmode = "USA-states",
      frame = ~YEAR
    ) %>%
      add_trace(
        locations = ~Abbreviation,
        z = ~avg_score,
        zmin = min(map_education_df$avg_math, na.rm = TRUE),
        zmax = max(map_education_df$avg_math, na.rm = TRUE),
        color = ~avg_score,
        colorscale = "Electric",
        text = ~hover,
        hoverinfo = "text"
      ) %>%
      layout(geo = list(scope = "usa"),
             title = "Average Score Selected in the USA"
             ) %>%
      colorbar(title = "Average Score")
  })
  
  # server elements for page 3
}

shinyApp(ui = ui, server = server)
