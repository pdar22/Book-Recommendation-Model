#Set the working directory
setwd("/users/pranavdar/Desktop/PGPBABI/Capstone Project/Capstone Data")

#Load the required libraries
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(data.table))
suppressMessages(library(stringr))
suppressMessages(library(tidyr))
suppressMessages(library(corrplot))
suppressMessages(library(recommenderlab))
suppressMessages(library(Amelia))
suppressMessages(library(cowplot))
suppressMessages(library(methods))

#Import the datasets
books <- read.csv("book_metadata.csv", stringsAsFactors = FALSE)
book_tags <- read.csv("book_tags.csv", stringsAsFactors = FALSE)
tags <- read.csv("tags.csv", stringsAsFactors = FALSE)
ratings <- read.csv("ratings.csv", stringsAsFactors = FALSE)

str(books)
str(book_tags)
str(tags)
str(ratings)

#################### Data Cleaning ######################
#Set any blank cells to NA before checking for missing values
books[books == ""] <- NA
ratings[ratings == ""] <- NA
tags[tags == ""] <- NA
book_tags[book_tags == ""] <- NA

#Check for any NA values in our data
any(is.na(books))
any(is.na(ratings))
any(is.na(tags))
any(is.na(book_tags))

#The books dataset seems to have quite a few missing values. We will see where they are visually
missmap(books, col = c("yellow", "black")) #package {Amelia}

#ISBN and ISBN13 missing are by design so we will just impute them with 'Not Available'
books$isbn[is.na(books$isbn) == TRUE] <- 'Not Available'
books$isbn13[is.na(books$isbn13) == TRUE] <- 'Not Available'

#Very few missing values in the publication year
check <- books %>% filter(is.na(original_publication_year) == TRUE)

#21 books don't have a publication year. A quick glance on the internet doesn't provide any 
#answers so we can ignore these

#Plenty of books are not filled with their original title names. This is not an issue as we only need
#the 'title' variable. We can safely drop this one
books <- books[ , -10]



################ Data Exploration ####################
#Check the summary of the number of books rated by the reader
ratings_per_user <- ratings %>% 
  group_by(user_id) %>% 
  mutate(no_of_ratings_per_user = n()) %>% 
  arrange(desc(no_of_ratings_per_user))

summary(ratings_per_user$no_of_ratings_per_user)

#Average number of ratings per user
ggplot(ratings_per_user, aes(no_of_ratings_per_user)) + 
  geom_bar(fill = "orange", aes(color = "black")) + 
  theme_grey() + labs(x = "Books Rated", title = "Average Number of Ratings")

#Distribution of ratings
ggplot(ratings, aes(rating, fill = factor(rating))) + geom_bar() + theme_grey()

#Average ratings per user
ratings %>% 
  group_by(user_id) %>% 
  summarise(average_user_rating = mean(rating)) %>% 
  ggplot(aes(average_user_rating)) + geom_histogram(aes(color = "black"), fill = "orange") + 
  theme_grey() + labs(x = "Ratings", title = "Average Book Ratings")

#Different languages
with_english <- books %>% 
  filter(!is.na(language_code)) %>% 
  group_by(language_code) %>% 
  summarise(no_of_books = n()) %>% 
  arrange(desc(no_of_books)) %>%
  ggplot(aes(reorder(language_code, no_of_books), no_of_books)) + 
  geom_bar(stat = "identity", fill = "orange") + coord_flip() +
  labs(x = "Language", y = "Number of Books") + theme_grey()

#Excluding English
without_english <- books %>% 
  filter(!is.na(language_code), !language_code %in% c("eng", "en-US", "en-GB", "en-CA")) %>% 
  group_by(language_code) %>% 
  summarise(no_of_books = n()) %>% 
  arrange(desc(no_of_books)) %>% 
  ggplot(aes(reorder(language_code, no_of_books), no_of_books)) + 
  geom_bar(stat = "identity", fill = "orange") + coord_flip() + 
  labs(x = "Language", y = "Number of books") + theme_grey()

plot_grid(with_english, without_english, align = 'h', labels = c("With English", "Without English"))

#Top 20 rated books
top20_rated <- books %>% 
  select(title, average_rating) %>% 
  arrange(desc(average_rating)) %>% 
  top_n(20, wt = average_rating)

#Bring in the number of ratings to verify
top20_rated <- books %>% 
  select(title, average_rating, ratings_count) %>% 
  arrange(desc(average_rating)) %>% 
  top_n(20, wt = average_rating)

#Top 20 popular books
top20_popular <- books %>% 
  select(title, ratings_count, average_rating) %>% 
  arrange(desc(ratings_count)) %>% 
  top_n(20, wt = ratings_count)

#Top 10 rated authors
top10rated_authors <- books %>%
  select(authors, average_rating, ratings_count) %>% 
  arrange(desc(average_rating)) %>% 
  top_n(10, wt = average_rating)
  
#Top 10 popular authors
top10popular_authors <- books %>%
  select(authors, average_rating, ratings_count) %>% 
  arrange(desc(ratings_count)) %>% 
  top_n(10, wt = ratings_count)

#Number of books published per year
pop_year <- books %>% 
  group_by(original_publication_year) %>% 
  summarise(no_of_books_published = n()) %>% 
  arrange(desc(no_of_books_published))

#Defining and exploring genres
genres <- c("romance", "fantasy", "children", "travel", "love", "sports", "nonfiction", 
            "contemporary", "art", "classics", "religion", "science-fiction", "covers", 
            "mystery", "thriller", "horror", "sci-fi", "graphic-novels", "historical-romance", 
            "young-adult", "best", "history", "paranormal", "historical", "historical-fiction", 
            "literature", "women", "suspense", "poetry", "biography")

available_genres <- genres[genres %in% tags$tag_name]

available_tags <- tags$tag_id[match(available_genres, tags$tag_name)]

#Top tags in our dataset
top_tags <- book_tags %>% 
  filter(tag_id %in% available_tags) %>% 
  group_by(tag_id) %>% 
  summarise(count_of_tags = n()) %>% 
  arrange(desc(count_of_tags)) %>%
  left_join(tags, by = "tag_id")

#Because of the massive range difference, let's convert the count to a percentage
top_tags <- book_tags %>% 
  filter(tag_id %in% available_tags) %>% 
  group_by(tag_id) %>% 
  summarise(count_of_tags = n()) %>% 
  mutate(sum = sum(count_of_tags), percentage = count_of_tags/sum) %>% 
  arrange(desc(count_of_tags)) %>%
  left_join(tags, by = "tag_id")

#Visualize this
ggplot(top_tags, aes(reorder(tag_name, percentage), percentage)) + 
         geom_bar(stat = "identity", fill = "orange") + coord_flip() + 
  labs(x = "Genre", title = "Top Rated Genres") + theme_grey()

#Look at all the book series - these are enclosed in brackets
harrypotter <- books %>% 
  filter(str_detect(str_to_lower(title), '\\(harry potter')) %>% 
  select(title, average_rating)  

#Now let's generalise the string function to extract all book series
books <- books %>% 
  mutate(series = str_extract(title, "\\(.*\\)"), 
         series_name = str_sub(str_extract(series, '\\(.*,'),2,-2),
         no_of_books_in_series = as.numeric(str_sub(str_extract(series, ', #[0-9]+\\)$'),4,-2)))

book_series <- books %>%
  filter(!is.na(no_of_books_in_series) & !is.na(series_name)) %>%
  group_by(series_name) %>%
  summarise(no_of_books_in_series = n(), mean_rating = mean(average_rating)) %>%
  arrange(desc(mean_rating))

#Sequels v Originals
books %>%
  filter(!is.na(no_of_books_in_series) & !is.na(series_name) & no_of_books_in_series %in% c(1, 2, 3)) %>%
  group_by(no_of_books_in_series) %>% 
  summarise(average_rating = mean(average_rating))

#What factors potentially influence or do not influence a book's rating?
p <- books %>% 
  select(average_rating, ratings_count, work_ratings_count, work_text_reviews_count, books_count)

m <- cor(p)
corrplot(m)

#How do frequent users rate books?
freq_users <- ratings %>% 
  group_by(user_id) %>% 
  summarise(mean_rating = mean(rating), no_of_books_rated = n()) %>% 
  filter(no_of_books_rated > 118)

cor(freq_users$no_of_books_rated, freq_users$mean_rating) #-0.05



##################### Collaborative Filtering #########################

#Re-shape the data before going further
reshape_rating <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>% 
  select(-user_id)

column_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(unique(ratings$book_id)))

reshape_rating <- as.matrix(reshape_rating)

dimnames(reshape_rating) <- column_names

dim(reshape_rating)

#We will take the example of one user - we will show the recommendations for him/her in the end
pick_user <- "52512"

#### Recommenderlab ####
reshape_rating[is.na(reshape_rating)] <- 0

#Using the sparsematrix to save memory on our machine
sparse_ratings_matrix <- as(reshape_rating, "sparseMatrix")

#Recommenderlab matrix - realRatingMatrix
converted_ratings <- new("realRatingMatrix", data = sparse_ratings_matrix)

#Build the recommendation system model
params <- list(method = "pearson", nn = 10)
recommend_model <- Recommender(converted_ratings, method = "UBCF", param = params)

#Make recommendations
recommendations <- predict(recommend_model, converted_ratings[pick_user, ], type = "ratings")

#Final recommendations for our hand-picked user!
final_recommendation <- as(recommendations, "data.frame") %>% 
  arrange(desc(rating)) %>% .[1:10, ] %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, title, authors, book_id), by = "book_id") %>% 
  select(-item, -user, -book_id)

View(final_recommendation)

#### Verifying and evaluating the recommendations ####
#Create the evaluation criteria
criteria <- evaluationScheme(converted_ratings[1:1000, ], method = "cross-validation", k = 10, 
                             given = -1, goodRating = 5)

#List of the algorithms to compare
algorithms <- list("algo_80" = list(name = "UBCF", param = list(method = "pearson", nn = 8)), 
                   "algo_500" = list(name = "UBCF", param = list(method = "pearson", nn = 500)), 
                   "algo_Random" = list(name = "RANDOM", param = NULL))

#Evaluate and then check the RMSE
evaluation_result <- evaluate(criteria, algorithms, type = "ratings")

avg(evaluation_result)
