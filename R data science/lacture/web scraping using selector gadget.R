# IMPORTING LIBRARIES
library(rvest)
library(dplyr)

# SCRAPPING WEBSITE
link <- 
  'https://www.imdb.com/search/title/?groups=top_250&sort=user_rating'

# HTML ELEMENTS FROM WEBSITE
web <- read_html(link)

# SEGREGATING NAMES
name <- web %>% html_nodes(".lister-item-header a") %>% html_text()

# VIEWING NAME LIST
View(name)

# SEGREGATING YEARS
year <- web %>% html_nodes(".text-muted.unbold") %>% html_text()

# VIEW YEAR LIST
View(year)

# SEGREGATING RATINGS
rating <- web %>% 
  html_nodes(".ratings-imdb-rating strong") %>% html_text()

# VIEW RATINGS LIST
View(rating)

# SEGREGATING THE DURATION
duration <- web %>% html_nodes(".runtime") %>% html_text()

# VIEWING DURATION LIST
View(duration)

# CREATING DATAFRAME
imdb.ratings <- data.frame(name, year, rating, duration)

# VIEW DATAFRAME
View(imdb.ratings)

# SAVING DATA
write.csv (imdb.ratings, "My_movie data.csv")