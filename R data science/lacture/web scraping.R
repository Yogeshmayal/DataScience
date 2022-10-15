# IMPORTING LIBRARIES
library(robotstxt)
library(rvest)

# SCRAPPING WEBSITE
url <- 'https://www.imdb.com/chart/top/'

# HTML ELEMENTS from WEBSITE
my_html <- read_html(url)

# SEGREGATING TABLES
my_tables <- html_nodes(my_html, 'table')

# STORING A TABLE
movie_table <- html_table(my_tables)[[1]]

# VIEWING THE TABLE
View(movie_table)

# SLICING COLUMNS
movies <- movie_table[,2:3]

# VIEW MODIFIED TABLE
View(movies)

# DATATYPES CHECK
str(movies)

# SAVING THE DATA
write.csv(movies, "My_movies.csv")