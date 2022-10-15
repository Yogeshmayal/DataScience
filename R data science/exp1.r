#  Importing Libraries
library(ggplot2)
library(dplyr)

exp <- read.csv("C:\\Users\\Yogesh\\Downloads\\dailyexpenses.csv")

#view data 

View(exp)
# types of data objects
class(exp)
# structure data
str(exp)


# date type type conversion
exp$Date <- as.Date()

exp$Date <- as.Date(exp$Date, "%d/%m/%y")

# data frame for comparing expenses

tot <-exp[,2:9]
total <- colSums(tot)
detail <- c("food", "groceries","fuel","toll")


tot_each <- data.frame(detail ,total)
View(tot_each)


# Bar plot of expencses if you want the height of the bars to representvalues in the data 
# use stat= " identity" and map a value to the y aesthetic


ggplot(data=tot_each,aes(x=detail,y=total))+
  geom_bar(stat ="identity",fill="red" )+
  labs(x="Item",y="Expenses",title="comparing expenses")

# dataframe for daily total expenses

cm <- exp[,2:9]
dt <- exp[, 1]
rs <- rowSums(cm)


daily_tot <- data.frame(dt,rs)
View(daily_tot)

#Line plot for daily expenses


install.packages("rmarkdown")
install.packages("flexdashboard")







