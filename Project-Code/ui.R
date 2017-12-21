###################################Libraries#######################################
###########################SHiny Library for the GUI###############################
###################################################################################
library(shiny)

#Storing the genres from the dataset into the Genre to display the options for the user 
genres <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film.Noir", "Horror", "Musical", "Mystery","Romance",
            "Sci.Fi", "Thriller", "War", "Western"
                )

#Shiny UI for the application to display the available movies and the genres
shinyUI(fluidPage(
  
  theme = shinythemes::shinytheme("superhero"),
  
  titlePanel("Movie Recommendation System:"),
  fluidRow(
    
  
    navbarPage("       ",
               
               
      tabPanel("Rating-Distribution",
               titlePanel("Rating-Distribution"),
               
               plotOutput("phonePlot")
      ),
      tabPanel("Movies-Distribution",
               titlePanel("Movies-Distribution by year"),
               
               
               plotOutput("phonePlot2")
      ),
      
      
      tabPanel("Genre-Distribution",
               titlePanel("Genre-Distribution"),
               
               selectInput("reg", "Genre:", 
                           choices=colnames(movies2[4:21])),
               
               plotOutput("phonePlot3")
      ),

      tabPanel("Movie-Recommender",
               
               titlePanel("Select Movie Genres:"),
               #Selecting panels for the user to select the genres and the movies of from the genre
               column(4, 
                      wellPanel(
                        selectInput("GenreInput", "Genre-1",
                                    genres),
                        uiOutput("mov1")
                      )),
               
               #Selecting panels for the user to select the genres and the movies of from the genre
               column(4, 
                      wellPanel(
                        selectInput("GenreInput2", "Genre-2",
                                    genres),
                        uiOutput("mov2"))
                      
               ),
               
               #Selecting panels for the user to select the genres and the movies of from the genre
               column(4,
                      wellPanel(
                        selectInput("GenreInput3", "Genre-3",
                                    genres),
                        uiOutput("mov3")
                      )),
               fluidRow(
                 wellPanel(
                   
                   #Display the recoomended movies for the user  
                   column(12,titlePanel("Suggested Movies"),
                          tableOutput("table")
                   ))
               )
               )
            )
    
    )
))
