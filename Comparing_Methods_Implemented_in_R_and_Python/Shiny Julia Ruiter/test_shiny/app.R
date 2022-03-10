########## TESTING

library(tidyverse)         # for plotting
library(shiny)             # for creating a shiny app
library(shinydashboard)    # for sleek interface

#movies_df <- read.csv("movies.csv")

# ui ---------------------------------------------------------------------------

ui <- dashboardPage(
  
  # title ----
  dashboardHeader(title = "movies.csv"),
  
  
  # sidebar ----
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
                menuItem("Data Table", tabName = "page1"),   #---select not working
                #conditionalPanel(
                  #checkboxGroupInput("col_select", "Select Columns to view:",
                              #choices = c('director_facebook_likes', 
                                          #'facenumber_in_poster', 
                                          #'aspect_ratio', 
                                          #'imdb_score', 
                                          #'duration', 
                                          #'num_critic_for_reviews', 
                                          #'actor_3_facebook_likes', 
                                          #'actor_2_facebook_likes', 
                                          #'actor_1_facebook_likes', 
                                          #'title_year', 
                                          #'num_user_for_reviews', 
                                          #'cast_total_facebook_likes', 
                                          #'movie_facebook_likes', 
                                          #'num_voted_users', 
                                          #'budget', 
                                          #'gross',
                                          #'genres', 
                                          #'plot_keywords', 
                                          #'movie_title', 
                                          #'actor_1_name', 
                                          #'color', 
                                          #'language', 
                                          #'movie_imdb_link', 
                                          #'director_name', 
                                          #'actor_2_name', 
                                          #'content_rating', 
                                          #'country', 
                                          #'actor_3_name')
                  #)
                #),
                
                menuItem("Distributions (click title to view)", tabName = "page2"),
                conditionalPanel(
                  'input.sidebarid == "page2"',
                  selectInput("dist_select", "Select Distribution to view:",
                              choices = c('director_facebook_likes', 
                                          'facenumber_in_poster', 
                                          'aspect_ratio', 
                                          'imdb_score', 
                                          'duration', 
                                          'num_critic_for_reviews', 
                                          'actor_3_facebook_likes', 
                                          'actor_2_facebook_likes', 
                                          'actor_1_facebook_likes', 
                                          'title_year', 
                                          'num_user_for_reviews', 
                                          'cast_total_facebook_likes', 
                                          'movie_facebook_likes', 
                                          'num_voted_users', 
                                          'budget', 
                                          'gross'),
                              multiple = FALSE,
                              selectize = TRUE,
                              selected = c('director_facebook_likes', 
                                           'facenumber_in_poster', 
                                           'aspect_ratio', 
                                           'imdb_score', 
                                           'duration', 
                                           'num_critic_for_reviews', 
                                           'actor_3_facebook_likes', 
                                           'actor_2_facebook_likes', 
                                           'actor_1_facebook_likes', 
                                           'title_year', 
                                           'num_user_for_reviews', 
                                           'cast_total_facebook_likes', 
                                           'movie_facebook_likes', 
                                           'num_voted_users', 
                                           'budget', 
                                           'gross') 
                  )
                ),
                
                menuItem("Histograms (click title to view)", tabName = "page3"),
                conditionalPanel(
                  'input.sidebarid == "page3"',
                  selectInput("cat_select", "Select Histogram to view:",
                              choices = c('genres', 
                                          'plot_keywords', 
                                          'movie_title', 
                                          'actor_1_name', 
                                          'color', 
                                          'language', 
                                          'movie_imdb_link', 
                                          'director_name', 
                                          'actor_2_name', 
                                          'content_rating', 
                                          'country', 
                                          'actor_3_name'),
                              multiple = FALSE,
                              selectize = TRUE,
                              selected = c('genres', 
                                           'plot_keywords', 
                                           'movie_title', 
                                           'actor_1_name', 
                                           'color', 
                                           'language', 
                                           'movie_imdb_link', 
                                           'director_name', 
                                           'actor_2_name', 
                                           'content_rating', 
                                           'country', 
                                           'actor_3_name') 
                    )
                  )
    )
  ),
  
  # body ----
  dashboardBody(
    tabItems(
      # page 1 ----
      tabItem(tabName = "page1", 
              "Data may take a while to load",
              #"Please select the columns to display (max 12)",
              br(), br(),
              tableOutput("info_table")),
      
      # page 2 ----
      tabItem(tabName = "page2", 
              "Please select the attribute to see distributions for",
              br(), br(),
              plotOutput("dist_plot")),
      
      # page 3 ----
      tabItem(tabName = "page3", 
              "Please select the attribute to see histograms for",
              br(), br(),
              plotOutput("cat_plot"))
    )
  )
)

# server -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  movies_df <- read.csv("movies.csv")
  
  
  #### TABLE 
  
  
  output$info_table <- renderTable(read.csv("movies.csv"))
  
# select columns not working  
#  observeEvent(input$col_select,{
#    movies0_df <- read.csv("movies.csv")
#    cols <- as.numeric(input$col_select)
#    if(length(input$col_select) == 1){
#      df <- data.frame(movies0_df[,cols])
#      names(df) <- names(movies0_df)[cols]
#      output$info_table = renderDataTable(df)
#    }else{
#      output$info_table = renderDataTable(movies0_df[,cols])
#    }
#  })
  
  
  #### HISTOGRAM
  
  reactivedf <- reactive({ 
    movies_df <- read.csv("movies.csv")  
    col <- input$cat_select
    filtereddf <- movies_df[col]
    filtereddf['selected'] <- filtereddf[col]
    filtereddf
  })
  
  
  output$cat_plot <- renderPlot({
    reactivedf() %>%
      ggplot(aes(selected)) +
      geom_bar()
  }) 
  
 #### DISTRIBUTION
  
  reactivedf2 <- reactive({ 
    movies_df <- read.csv("movies.csv")  
    col2 <- input$dist_select
    filtereddf2 <- movies_df[col2]
    filtereddf2['selected'] <- filtereddf2[col2]
    filtereddf2
  })
  
  
  output$dist_plot <- renderPlot({
    reactivedf2() %>%
      ggplot(aes(selected)) +
      geom_density()
  })  


}

# shiny app --------------------------------------------------------------------

shinyApp(ui, server)