#importing library
library(rvest)
library(dplyr)
#importing the data
url <- 'https://www.amazon.in/Apple-iPhone-Pro-Max-128/product-reviews/B09V4D7459/'
#html element from website
my_html <- read_html(url)
#segregation a table
my_tables <- html_nodes(my_html,'table')
# storing a table
review_tables <- html_table(my_tables)[[1]]
# view the table
view(review_tables)
#modified table
rview <- review_tables[2]
#view modified table
view(rview)
#saving the data in csv format
write.csv(review_tables, "Review of iphone pro max mobile")
