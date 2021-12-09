library(shiny)
library(dplyr)
library(tidyverse)
library(plotly)
library(maps)
#source("work.R")


# dataset
education_df <- read.csv("https://raw.githubusercontent.com/CodyTu/Final_Project_INFO_201/main/states_all.csv")


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
  h1("Introduction"),
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
            



net_df <- education_df %>%
  select(YEAR, STATE,  TOTAL_REVENUE, TOTAL_EXPENDITURE)%>%
  group_by(YEAR)%>%
  summarise(TOTAL_REVENUE = sum(TOTAL_REVENUE),
            TOTAL_EXPENDITURE = sum(TOTAL_EXPENDITURE)
        
  ) %>%
  na.omit() %>%
  rename(Total_Revenue = TOTAL_REVENUE,
         Total_Expenditure = TOTAL_EXPENDITURE) %>%
  mutate(Net_Income = Total_Revenue - Total_Expenditure)
         

# Irene's Page (?)
page_one <- tabPanel(
  "Enrollment", # enrollment of grades from K-12 by year
  h1("Enrollment by Grade Over Time"),
  sidebarPanel(
    p("This chart shows how many students have been enrolled in public schools in the US, displaying trends for each of the grades over time. "),
    
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
    
    sidebarPanel(
      p("This chart maps out the average value of either reading scores or math scores of NAEP exams based on a user selection of 4th or 8th graders. 
        The slider can also be used to select a year to be viewed."),
      
      selectInput(
      inputId = "scoreSelect",
      label = "Select a grade",
      choices = colnames(map_score_df)[22:25]
    )),
    mainPanel(
      plotlyOutput("map")
  )
))

# Jaspreet's Page
page_three <- tabPanel(
  "Net Income",
  h1("Schools Related Net Income Information by Year"),
  sidebarLayout(
    sidebarPanel(
      p("The page shows a histogram bar chart that has the calculated net income from the total revenue and total expenditure also grouped by year, 
        showing changes in net income by recorded year. "),
      
      sliderInput("bins",
                  "Years",
                  min = 1,
                  max = 12,
                  value = 10
      ),
      #radioButtons(
        #inputId = "Income",
        #label = "Income Information by Year",
        #choices = list("Total_Revenue" = revplot,
                       #"Total_Expenditure" = explot,
                       #"Net_Income" = netplot)
      #)
      hr(),
      helpText(" 1 = 1993"),
      helpText(" 2 = 1995"),
      helpText(" 3 = 1997"),
      helpText(" 4 = 1999"),
      helpText(" 5 = 2001"),
      helpText(" 6 = 2004"),
      helpText(" 7 = 2006"),
      helpText(" 8 = 2008"),
      helpText(" 9 = 2010"),
      helpText(" 10 = 2012"),
      helpText(" 11 = 2014"),
      helpText(" 12 = 2016"),
      
      
    ),
    mainPanel(
      plotOutput("viz"),
      plotOutput("bar")
    )
  )
)


summary <- tabPanel(
  "Summary",
  h1("Summary"),
  p("In our analysis of the relationship between socioeconomic factors/funding and education statistics and outcomes, there are three main 
    takeaways, one from each visualization."),
  h3("Enrollment in American schools has increased steadily over the past few decades"),
  p("In the charts showing the enrollment trends for students in kindergarten, fourth grade, eighth grade, and twelfth grade, all of them show
    upward trends, pointing at an increase in enrollment in public schools. This emphasizes the significance of our investigation: as the number
    of students in public schools increases over time, so does the urgency of understanding the relationships between funding and outcomes in our
    public school system."),
  h3("Northern states have higher standardized testing scores compared to the rest of the country"),
  p("More northern states, especially those in the northeast, have higher averages of both reading and math scores than in other regions
    of the country, such as the South which had the lowest averages. These disparities can be connected to differences in funding and 
    socioeconomic factors between states, which may have complex roots that can be further studied."),
  h3("Education funding has improved in recent years"),
  p("In recent years, education revenue has changed from being lower than expenditure to more closely matching or exceeding expenditure. This
    shows that funding for education has improved, as fewer states are underfunded. In the 1990s, a majority of states were spending much more than 
    their revenue, while in more recent decades spending and revenue have become more balanced.")
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
      
      labs(x="Year", y="Number of Students") 
    
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
  output$bar <- renderPlot({
    ggplot(data = net_df, aes(y= Net_Income, x=YEAR)) + 
    geom_bar (stat = "sum", fill=rgb(0.1, 0.1, 0.9, 0.6)) + 
    xlab("Years") +
    ylab("Net Income") +
    ggtitle("Change in Net Income by Year") +
    theme_bw(base_size = 16) +
    theme(legend.position = "none")
  })
  
  output$viz <- renderPlot({
    
    #revplot <- ggplot(data = net_df, aes(y= Total_Revenue, x=YEAR)) + 
      #geom_bar (stat = "sum", fill=rgb(0, 0.9, 0, 0.6)) + 
      #xlab("Years") +
      #ylab("Total Revenue") +
      #ggtitle("Total Revenue by Year") +
      #theme_bw(base_size = 16)
    
    
   #ggplot(data = net_df, aes(y= Total_Expenditure, x=YEAR)) + 
      #geom_bar (stat = "sum", fill=rgb(0.9, 0, 0, 0.6)) + 
      #xlab("Years") +
      #ylab("Total Expense")  +
      #ggtitle("Total Expense by Year") +
      #theme_bw(base_size = 16)
    
    #ggplot(data = net_df, aes(y= Net_Income, x=YEAR)) + 
      #geom_bar (stat = "sum", fill=rgb(0.1, 0.1, 0.9, 0.6)) + 
      #xlab("Years") +
      #ylab("Net Income") +
      #ggtitle("Net Income by Year") +
      #theme_bw(base_size = 16)
    
    ggplot(data = net_df, aes(y= Net_Income)) + 
      geom_histogram (bins = input$bins , 
                fill=rgb(0.1, 0.1, 0.9, 0.6)) + 
      xlab("Years") +
      ylab("Net Income") +
      ggtitle("Change in Net Income by Year") +
      theme_bw(base_size = 16)
    
  })
    
}




shinyApp(ui = ui, server = server)
