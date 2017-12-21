#######################################libraries############################################
################   Shiny Library for the web application user interface    #################
###################  Porxy library to convert the matrices   ##############################
###############  Recommender library to perform recommender operations   ##################
####################################    Reshape2    ########################################

library(shiny)
library(proxy)
library(recommenderlab)
library(reshape2)
library(shinythemes)
library(ggplot2)

#objects to store the data read from the .csv datasets
movies <- read.csv("movies.csv", header = TRUE, stringsAsFactors=FALSE)
ratings1 <- read.csv("ratings.csv", header = TRUE)
search <- read.csv("movies.csv", stringsAsFactors=FALSE)
ratings <- read.csv("ratings.csv", header = TRUE)
search <- search[-which((search$movieId %in% ratings$movieId) == FALSE),]
movies2 <- movies[-which((movies$movieId %in% ratings1$movieId) == FALSE),]



#Recommedor function used to obtain the User Based Collabrative filter function 
mRecommender <- function(userInput,userInput2,userInput3) {
  
  col_num1 <- which(movies2[,2] == userInput)
  col_num2 <- which(movies2[,2] == userInput2)
  col_num3 <- which(movies2[,2] == userInput3)
  
  #Based on the total number of Movies in the Movies.csv dataset
  mSelected <- matrix(NA,10325)
  mSelected[col_num1] <- 5 
  mSelected[col_num2] <- 4 
  mSelected[col_num3] <- 3 
  mSelected <- t(mSelected)
  
  #dcast function is used to obtain the matrix with rows=userid and col=movieID   
  rMatrix <- dcast(ratings1, userId~movieId, value.var = "rating", na.rm=FALSE)
  rMatrix <- rMatrix[,-1]
  colnames(mSelected) <- colnames(rMatrix)
  
  #combines the userSelect and rMatrix data by the rows
  rMatrix2 <- rbind(mSelected,rMatrix)
  rMatrix2 <- as.matrix(rMatrix2)
  
  #Convert the rMatrix2 into the sparsing matrix by using realRatingMatrix
  rMatrix2 <- as(rMatrix2, "realRatingMatrix")
  
  #Recommender function with UBCF method is used to perform the UCBF operation with cosine functionality 
  recommender_model <- Recommender(rMatrix2, method = "UBCF",param=list(method="Cosine",nn=30))
  
  #recommender lab function to preditc the similarities 
  Output <- predict(recommender_model, rMatrix2[1], n=10)
  result_list <- as(Output, "list")
  notFound <- data.frame(matrix(NA,1))
  
  
  
  #10 suggested movies are stored into the result 
  result <- data.frame(matrix(NA,10))
  #When there are no recommendations by the recommender function
  if (as.character(result_list[1])=='character(0)'){
    notFound[1,1] <- "Sorry, Select different movies you like."
    colnames(notFound) <- "No Recommendations found "
    return(notFound) 
  } else {
    
    #to display the 10 movies from the recommended lists
    for (i in c(1:10)){
      result[i,1] <- as.character(subset(movies, 
                                               movies$movieId == as.integer(result_list[[1]][i]))$title)
    }
    colnames(result) <- "Based on User Ratings"
    return(result)
  }
}


formatInput <- function(v,a,d){
c(v,a,d)
}

#ShinyServer logic to display the output from the recommender function
shinyServer(function(input, output) {
  
  output$phonePlot <- renderPlot({
    
    # Render a barplot
    qplot(ratings1[,3], geom="histogram",main="Ratings-distribution",
          ylab="Number of movies ",
          xlab="Ratings",col = I('red'),fill=I('black'))
    
  })
  output$phonePlot2 <- renderPlot({
    # Render a barplot
    hist(movies[,3], 
         main=input$region,
         ylab="movies ",
         xlab="year",col = c('red','green'))
  })
  
  output$phonePlot3 <- renderPlot({
    # Render a barplot
    hist(movies[,input$reg], 
         main=input$region,
         ylab="movies ",
         xlab="genre=1",col = c('red','green'))
  })
  
  # generates calls to UI functions 
  #and make the results appear in a predetermined place in the UI.
  output$mov1 <- renderUI({
    if (is.null(input$GenreInput))
      return()
    
    #Selection of the Genre input 
    switch(input$GenreInput,
           
           #the movies you like in the input section for the first column
           #Sorting the movies according to the genres to select from the Second row in the first column
           "Action" = selectInput("mInput1", "Movie you like ", 
                                  choices = sort(subset(search, Action == 1)$title),
                                  selected = sort(subset(search, Action == 1)$title)[1]),
           "Adventure" = selectInput("mInput1", "Movie you like ",
                                     choices = sort(subset(search, Adventure == 1)$title),
                                     selected = sort(subset(search, Adventure == 1)$title)[1]),
           "Animation" =  selectInput("mInput1", "Movie you like ",
                                      choices = sort(subset(search, Animation == 1)$title),
                                      selected = sort(subset(search, Animation == 1)$title)[1]),
           "Children" =  selectInput("mInput1", "Movie you like ",
                                     choices = sort(subset(search, Children == 1)$title),
                                     selected = sort(subset(search, Children == 1)$title)[1]),
           "Comedy" =  selectInput("mInput1", "Movie you like ",
                                   choices = sort(subset(search, Comedy == 1)$title),
                                   selected = sort(subset(search, Comedy == 1)$title)[1]),
           "Crime" =  selectInput("mInput1", "Movie you like ",
                                  choices = sort(subset(search, Crime == 1)$title),
                                  selected = sort(subset(search, Crime == 1)$title)[1]),
           "Documentary" =  selectInput("mInput1", "Movie you like ",
                                        choices = sort(subset(search, Documentary == 1)$title),
                                        selected = sort(subset(search, Documentary == 1)$title)[1]),
           "Drama" =  selectInput("mInput1", "Movie you like ",
                                  choices = sort(subset(search, Drama == 1)$title),
                                  selected = sort(subset(search, Drama == 1)$title)[1]),
           "Fantasy" =  selectInput("mInput1", "Movie you like ",
                                    choices = sort(subset(search, Fantasy == 1)$title),
                                    selected = sort(subset(search, Fantasy == 1)$title)[1]),
           "Film.Noir" =  selectInput("mInput1", "Movie you like ",
                                      choices = sort(subset(search, Film.Noir == 1)$title),
                                      selected = sort(subset(search, Film.Noir == 1)$title)[1]),
           "Horror" =  selectInput("mInput1", "Movie you like ",
                                   choices = sort(subset(search, Horror == 1)$title),
                                   selected = sort(subset(search, Horror == 1)$title)[1]),
           "Musical" =  selectInput("mInput1", "Movie you like ",
                                    choices = sort(subset(search, Musical == 1)$title),
                                    selected = sort(subset(search, Musical == 1)$title)[1]),
           "Mystery" =  selectInput("mInput1", "Movie you like ",
                                    choices = sort(subset(search, Mystery == 1)$title),
                                    selected = sort(subset(search, Mystery == 1)$title)[1]),
           "Romance" =  selectInput("mInput1", "Movie you like ",
                                    choices = sort(subset(search, Romance == 1)$title),
                                    selected = sort(subset(search, Romance == 1)$title)[1]),
           "Sci.Fi" =  selectInput("mInput1", "Movie you like ",
                                   choices = sort(subset(search, Sci.Fi == 1)$title),
                                   selected = sort(subset(search, Sci.Fi == 1)$title)[1]),
           "Thriller" =  selectInput("mInput1", "Movie you like ",
                                     choices = sort(subset(search, Thriller == 1)$title),
                                     selected = sort(subset(search, Thriller == 1)$title)[1]),
           "War" =  selectInput("mInput1", "Movie you like ",
                                choices = sort(subset(search, War == 1)$title),
                                selected = sort(subset(search, War == 1)$title)[1]),
           "Western" = selectInput("mInput1", "Movie you like ",
                                   choices = sort(subset(search, Western == 1)$title),
                                   selected = sort(subset(search, Western == 1)$title)[1])
    )
  })
  
  # generates calls to UI functions 
  #and make the results appear in a predetermined place in the UI.
  output$mov2 <- renderUI({
    if (is.null(input$GenreInput2))
      return()
    
    #Selection of the Genre input and
    switch(input$GenreInput2,
           
           #the movies you like in the input section for the first column
           #Sorting the movies according to the genres to select from the Second row in the first column
           "Action" = selectInput("mInput2", "Movie you like ",
                                  choices = sort(subset(search, Action == 1)$title),
                                  selected = sort(subset(search, Action == 1)$title)[1]),
           "Adventure" = selectInput("mInput2", "Movie you like ",
                                     choices = sort(subset(search, Adventure == 1)$title),
                                     selected = sort(subset(search, Adventure == 1)$title)[1]),
           "Animation" =  selectInput("mInput2", "Movie you like ",
                                      choices = sort(subset(search, Animation == 1)$title),
                                      selected = sort(subset(search, Animation == 1)$title)[1]),
           "Children" =  selectInput("mInput2", "Movie you like ",
                                     choices = sort(subset(search, Children == 1)$title),
                                     selected = sort(subset(search, Children == 1)$title)[1]),
           "Comedy" =  selectInput("mInput2", "Movie you like ",
                                   choices = sort(subset(search, Comedy == 1)$title),
                                   selected = sort(subset(search, Comedy == 1)$title)[1]),
           "Crime" =  selectInput("mInput2", "Movie you like ",
                                  choices = sort(subset(search, Crime == 1)$title),
                                  selected = sort(subset(search, Crime == 1)$title)[1]),
           "Documentary" =  selectInput("mInput2", "Movie you like ",
                                        choices = sort(subset(search, Documentary == 1)$title),
                                        selected = sort(subset(search, Documentary == 1)$title)[1]),
           "Drama" =  selectInput("mInput2", "Movie you like ",
                                  choices = sort(subset(search, Drama == 1)$title),
                                  selected = sort(subset(search, Drama == 1)$title)[1]),
           "Fantasy" =  selectInput("mInput2", "Movie you like ",
                                    choices = sort(subset(search, Fantasy == 1)$title),
                                    selected = sort(subset(search, Fantasy == 1)$title)[1]),
           "Film.Noir" =  selectInput("mInput2", "Movie you like ",
                                      choices = sort(subset(search, Film.Noir == 1)$title),
                                      selected = sort(subset(search, Film.Noir == 1)$title)[1]),
           "Horror" =  selectInput("mInput2", "Movie you like ",
                                   choices = sort(subset(search, Horror == 1)$title),
                                   selected = sort(subset(search, Horror == 1)$title)[1]),
           "Musical" =  selectInput("mInput2", "Movie you like ",
                                    choices = sort(subset(search, Musical == 1)$title),
                                    selected = sort(subset(search, Musical == 1)$title)[1]),
           "Mystery" =  selectInput("mInput2", "Movie you like ",
                                    choices = sort(subset(search, Mystery == 1)$title),
                                    selected = sort(subset(search, Mystery == 1)$title)[1]),
           "Romance" =  selectInput("mInput2", "Movie you like ",
                                    choices = sort(subset(search, Romance == 1)$title),
                                    selected = sort(subset(search, Romance == 1)$title)[1]),
           "Sci.Fi" =  selectInput("mInput2", "Movie you like ",
                                   choices = sort(subset(search, Sci.Fi == 1)$title),
                                   selected = sort(subset(search, Sci.Fi == 1)$title)[1]),
           "Thriller" =  selectInput("mInput2", "Movie you like ",
                                     choices = sort(subset(search, Thriller == 1)$title),
                                     selected = sort(subset(search, Thriller == 1)$title)[1]),
           "War" =  selectInput("mInput2", "Movie you like ",
                                choices = sort(subset(search, War == 1)$title),
                                selected = sort(subset(search, War == 1)$title)[1]),
           "Western" = selectInput("mInput2", "Movie you like ",
                                   choices = sort(subset(search, Western == 1)$title),
                                   selected = sort(subset(search, Western == 1)$title)[1])
    )
  })
  
  # generates calls to UI functions 
  #and make the results appear in a predetermined place in the UI.
  output$mov3 <- renderUI({
    if (is.null(input$GenreInput3))
      return()
    
    #Selection of the Genre input and
     switch(input$GenreInput3,
           
           #the movies you like in the input section for the first column
           #Sorting the movies according to the genres to select from the Second row in the first column
           "Action" = selectInput("mInput3", "Movie you like ",
                                  choices = sort(subset(search, Action == 1)$title),
                                  selected = sort(subset(search, Action == 1)$title)[1]),
           "Adventure" = selectInput("mInput3", "Movie you like ",
                                     choices = sort(subset(search, Adventure == 1)$title),
                                     selected = sort(subset(search, Adventure == 1)$title)[1]),
           "Animation" =  selectInput("mInput3", "Movie you like ",
                                      choices = sort(subset(search, Animation == 1)$title),
                                      selected = sort(subset(search, Animation == 1)$title)[1]),
           "Children" =  selectInput("mInput3", "Movie you like ",
                                     choices = sort(subset(search, Children == 1)$title),
                                     selected = sort(subset(search, Children == 1)$title)[1]),
           "Comedy" =  selectInput("mInput3", "Movie you like ",
                                   choices = sort(subset(search, Comedy == 1)$title),
                                   selected = sort(subset(search, Comedy == 1)$title)[1]),
           "Crime" =  selectInput("mInput3", "Movie you like ",
                                  choices = sort(subset(search, Crime == 1)$title),
                                  selected = sort(subset(search, Crime == 1)$title)[1]),
           "Documentary" =  selectInput("mInput3", "Movie you like ",
                                        choices = sort(subset(search, Documentary == 1)$title),
                                        selected = sort(subset(search, Documentary == 1)$title)[1]),
           "Drama" =  selectInput("mInput3", "Movie you like ",
                                  choices = sort(subset(search, Drama == 1)$title),
                                  selected = sort(subset(search, Drama == 1)$title)[1]),
           "Fantasy" =  selectInput("mInput3", "Movie you like ",
                                    choices = sort(subset(search, Fantasy == 1)$title),
                                    selected = sort(subset(search, Fantasy == 1)$title)[1]),
           "Film.Noir" =  selectInput("mInput3", "Movie you like ",
                                      choices = sort(subset(search, Film.Noir == 1)$title),
                                      selected = sort(subset(search, Film.Noir == 1)$title)[1]),
           "Horror" =  selectInput("mInput3", "Movie you like ",
                                   choices = sort(subset(search, Horror == 1)$title),
                                   selected = sort(subset(search, Horror == 1)$title)[1]),
           "Musical" =  selectInput("mInput3", "Movie you like ",
                                    choices = sort(subset(search, Musical == 1)$title),
                                    selected = sort(subset(search, Musical == 1)$title)[1]),
           "Mystery" =  selectInput("mInput3", "Movie you like ",
                                    choices = sort(subset(search, Mystery == 1)$title),
                                    selected = sort(subset(search, Mystery == 1)$title)[1]),
           "Romance" =  selectInput("mInput3", "Movie you like ",
                                    choices = sort(subset(search, Romance == 1)$title),
                                    selected = sort(subset(search, Romance == 1)$title)[1]),
           "Sci.Fi" =  selectInput("mInput3", "Movie you like ",
                                   choices = sort(subset(search, Sci.Fi == 1)$title),
                                   selected = sort(subset(search, Sci.Fi == 1)$title)[1]),
           "Thriller" =  selectInput("mInput3", "Movie you like ",
                                     choices = sort(subset(search, Thriller == 1)$title),
                                     selected = sort(subset(search, Thriller == 1)$title)[1]),
           "War" =  selectInput("mInput3", "Movie you like ",
                                choices = sort(subset(search, War == 1)$title),
                                selected = sort(subset(search, War == 1)$title)[1]),
           "Western" = selectInput("mInput3", "Movie you like ",
                                   choices = sort(subset(search, Western == 1)$title),
                                   selected = sort(subset(search, Western == 1)$title)[1])
    )
  })
  
  
  # used to render static tables in a Shiny app.
  output$table <- renderTable({
    mRecommender(input$mInput1, input$mInput2, input$mInput3)
  })
  
  # renderPrint captures any print output, converts it to a string, and
  # returns it.
  output$dynamic_value <- renderPrint({
    c(input$mInput1,input$mInput2,input$mInput3)
  })
  
})