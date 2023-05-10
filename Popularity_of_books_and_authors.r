library(funModeling)
library(readr)
library(tidyverse)
library(tidyr)
library(cowplot)
library(stringr)
library(dplyr)

#library(shiny)
#library(shinythemes)
#library(shinydashboard)
library(htmltools)
library(ggplot2)
library(forcats)

# Reading csv file
books <- read_csv("books.csv", 
                  col_types = cols(
                    index = col_integer(),
                    authors = col_character(),
                    average_rating = col_double(),
                    description = col_character(),
                    genres = col_character(),
                    original_publication_year = col_double(),
                    original_title = col_character(),
                    pages = col_integer(),
                    ratings_1 = col_integer(),
                    ratings_2 = col_integer(),
                    ratings_3 = col_integer(),
                    ratings_4 = col_integer(),
                    ratings_5 = col_integer(),
                    ratings_count = col_integer(),
                  ))

# Remove square brackets and single quotes from authors and genres
books <- books %>% 
  mutate(authors = str_replace_all(authors, "\\*|\\(|\\)|\\[|\\]|'", "")) %>%
  mutate(genres = str_replace_all(genres, "\\*|\\(|\\)|\\[|\\]|'", "")) %>%
  na.omit(books)

books <- books %>%
  rename(Author = 'authors') %>%
  rename(Genre = 'genres') %>%
  rename(Average_rating = 'average_rating') %>%
  rename(Year = 'original_publication_year') %>%
  rename(Name = 'original_title') %>%
  rename(Pages = 'pages') %>%
  rename(Reviews_total = 'ratings_count') %>%
  rename(Reviews_1 = 'ratings_1') %>%
  rename(Reviews_2 = 'ratings_2') %>%
  rename(Reviews_3 = 'ratings_3') %>%
  rename(Reviews_4 = 'ratings_4') %>%
  rename(Reviews_5 = 'ratings_5')

# Removing redundant people from authors' column (editors, illustrators, etc.)
books$Author <- gsub("\\,.*","",books$Author)

freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}

a <- books[,c("Author")]
b <- freqfunc(a,10)[order(freqfunc(a,10),decreasing = TRUE)]
b <- as.data.frame(b) %>% rename("Author" = "Var1") %>% rename("Number of Books" = "Freq")

funstuff <- data.frame(Funfact= character(0), Genre = character(0), Book= character(0), Author= character(0))
funstuff[1,1] <- "The oldest book"
funstuff[2,1] <- "The newest book"
funstuff[3,1] <- "The worst rating"
funstuff[4,1] <- "The least reviews"

minyear <- books %>% slice_min(Year)
funstuff[1,2] <- minyear[5]
funstuff[1,3] <- minyear[7]
funstuff[1,4] <- minyear[2]
maxyear <- books %>% slice_max(Year)
funstuff[2,2] <- maxyear[5]
funstuff[2,3] <- maxyear[7]
funstuff[2,4] <- maxyear[2]

minrating <- books %>% slice_min(Average_rating)
funstuff[3,2] <- minrating[5]
funstuff[3,3] <- minrating[7]
funstuff[3,4] <- minrating[2]

leastreviews <- books %>% slice_min(Reviews_total)
funstuff[4,2] <- leastreviews[5]
funstuff[4,3] <- leastreviews[7]
funstuff[4,4] <- leastreviews[2]

###############################################
ui = fluidPage(
  tabsetPanel(
    tabPanel("Books popularity", fluid = TRUE,
             sidebarLayout(
               # Inputs
               sidebarPanel(width = 4,
                            selectInput(inputId = "Choice", 
                                        label = "Sort by:",
                                        choices = c("Reviews_1",
                                                    "Reviews_2",
                                                    "Reviews_3",
                                                    "Reviews_4",
                                                    "Reviews_5",
                                                    "Reviews_total",
                                                    "Average_rating","Total_Rating_and_Reviews"),
                                        selected = "Reviews_total"),
                            selectInput(inputId = "Genre",
                                        label = "Genres:",
                                        choices = c("all", 
                                                    "art", 
                                                    "biography", 
                                                    "business", 
                                                    "chick-lit", 
                                                    "christian", 
                                                    "classics", 
                                                    "comics", 
                                                    "contemporary", 
                                                    "cookbooks", 
                                                    "crime", 
                                                    "fantasy", 
                                                    "fiction", 
                                                    "graphic-novels", 
                                                    "historical-fiction", 
                                                    "history", 
                                                    "horror", 
                                                    "manga", 
                                                    "memoir", 
                                                    "music", 
                                                    "mystery", 
                                                    "nonfiction", 
                                                    "paranormal", 
                                                    "philosophy", 
                                                    "poetry", 
                                                    "psychology", 
                                                    "religion", 
                                                    "romance", 
                                                    "science", 
                                                    "science-fiction", 
                                                    "self-help", 
                                                    "spirituality", 
                                                    "sports", 
                                                    "thriller", 
                                                    "travel", 
                                                    "young-adult"),
                                        selected = "All"),
                            sliderInput("Range", 
                                        label = "Date Range:", 
                                        min = min(books$Year),
                                        max = max(books$Year),
                                        step = 1,
                                        value = c(min(books$Year),max(books$Year)),
                                        sep = ""
                            ),
                            sliderInput("Pages", 
                                        label = "Number of Pages:", 
                                        min = min(books$Pages),
                                        max = max(books$Pages),
                                        step = 1,
                                        value = c(min(books$Pages),max(books$Pages)),
                                        sep = ""
                            )
               ),
               mainPanel(plotOutput("books_popularity"))
             )
             
    ),
    tabPanel("Feature Correlation", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(width = 4,
                            radioButtons(inputId = "Choice_1", 
                                         label = "Choose first feature:",
                                         choices = c("Pages",
                                                     "Year",
                                                     "Reviews_total",
                                                     "Average_rating"),
                                         selected = "Reviews_total"),
                            radioButtons(inputId = "Choice_2", 
                                         label = "Choose second feature:",
                                         choices = c("Pages",
                                                     "Year",
                                                     "Reviews_total",
                                                     "Average_rating"),
                                         selected = "Reviews_total"
                            ),
                            sliderInput("FeatureRange", 
                                        label = "Date Range:", 
                                        min = min(books$Year),
                                        max = max(books$Year),
                                        step = 1,
                                        value = c(min(books$Year),max(books$Year)),
                                        sep = ""
                            ),
                            sliderInput("FeaturePages", 
                                        label = "Number of Pages:", 
                                        min = min(books$Pages),
                                        max = max(books$Pages),
                                        step = 1,
                                        value = c(min(books$Pages),max(books$Pages)),
                                        sep = ""
                            ),
                            sliderInput("FeatureRating", 
                                        label = "Choose rating range:", 
                                        min = min(books$Average_rating),
                                        max = max(books$Average_rating),
                                        step = 0.1,
                                        value = c(min(books$Average_rating),max(books$Average_rating)),
                                        sep = ""
                            ),
                            sliderInput("FeatureReviews", 
                                        label = "Choose reviews range:", 
                                        min = min(books$Reviews_total),
                                        max = max(books$Reviews_total),
                                        step = 1000,
                                        value = c(min(books$Reviews_total),max(books$Reviews_total)),
                                        sep = ""
                            )
               ),
               mainPanel(plotOutput("feature_correlation"))
             )
    ),
    
    tabPanel("Unique authors", fluid = TRUE,
             mainPanel( column(12,
                               tableOutput("most_books")))
    ),
    tabPanel("Fun facts", fluid = TRUE,
             mainPanel( column(12,
                               tableOutput("stuff")))
    )
  )
)



###############################################

server <- function(input, output) {
  
  output$books_popularity <- renderPlot({
    
    # Checking Genre
    if (input$Genre != "all"){
      books_Genre <- books[str_detect(books$Genre, paste0("^", input$Genre)), ]
    }
    else {books_Genre <- books}
    
    books_Genre <- books_Genre %>% filter(Year >= input$Range[1] & Year <= input$Range[2])
    books_Genre <- books_Genre %>% filter(Pages >= input$Pages[1] & Pages <= input$Pages[2])
    
    # Checking on the basis of what type of data we should display the chart
    if (input$Choice != "Total_Rating_and_Reviews"){
      books_temp <- books_Genre %>% arrange(desc(input$Choice)) %>% slice(1:10) %>% mutate(Name = fct_reorder(Name, !! sym(input$Choice)))
      ggplot(books_temp, aes_string(x = input$Choice, y = 'Name')) + geom_bar(stat="identity", fill="steelblue")      
    }
    else{
      books_temp <- books_Genre %>% arrange(desc(Average_rating),desc(Reviews_total)) %>% slice(1:10) %>% mutate(Name = fct_reorder(Name, Reviews_total))
      ggplot(books_temp, aes_string(x = 'Reviews_total', y = 'Name')) + geom_bar(stat="identity", fill="steelblue")
    }
  })
  
  
  output$feature_correlation <- renderPlot({
    books <- books %>% filter(Year >= input$FeatureRange[1] & Year <= input$FeatureRange[2])
    books <- books %>% filter(Pages >= input$FeaturePages[1] & Pages <= input$FeaturePages[2])
    books <- books %>% filter(Average_rating >= input$FeatureRating[1] & Average_rating <= input$FeatureRating[2])
    books <- books %>% filter(Reviews_total >= input$FeatureReviews[1] & Reviews_total <= input$FeatureReviews[2])
    ggplot(books, aes_string(x = input$Choice_1, y = input$Choice_2)) + 
      geom_point(stat="identity")
  })
  output$most_books <- renderTable(b)
  output$stuff <- renderTable(funstuff)
  
  
}
###############################################
shinyApp(ui, server)






