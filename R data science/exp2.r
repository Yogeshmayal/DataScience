library(ggplot2)
library(dplyr)

#importing data set

exp <- read.csv("C:\\Users\\Yogesh\\Downloads\\dailyexpenses.csv")
View(exp)
#type of object
class(exp)
#structure of data
str(exp)

#Date from for comparision
exp$Date <- as.Date(exp$Date, "%d/%m/%y")

#data frame for comparing Expenses
tot <-exp[,2:9]
total <- colSums(tot)
detail <- c("food", "groceries","fuel","toll")
tot_each <- data.frame(detail ,total)
View(tot_each)

#bar plot of expenses if you wnat the height of the bars to represent values in the data
#use stat ="identity and map a value to the y aesthetic

ggplot(data=tot_each,aes(x=detail,y=total))+
  geom_bar(stat ="identity",fill="red" )+
  labs(x="Item",y="Expenses",title="comparing expenses")

# Dataframe for daily total expenses
cm <- exp[,2:9]
dt <- exp[, 1]
rs <- rowSums(cm)
daily_tot <- data.frame(dt,rs)
View(daily_tot)

#line plot for daily total expenses
ggplot(data=daily_tot,aes(x=dt,y=rs))+
  geom_line(color="red",size=2)+
  labs(x="Date",y="Expenses",title="Total Expenses")

#Dataframe of daily medical Expenses

md <- exp[,3]
de <- exp[,1]

daily_med <- data.frame(de,md)
View(daily_med)

#Scatter plot of Daily MEdical Expenses
ggplot(data=daily_med,aes(x=de,y=md))+geom_point()+
  labs(x="Expenses",y="Medical",title = "Medical Expenses")


#histogram of medical expenses
ggplot(exp,aes(x=Medical))+
  geom_histogram(bins = 5,color="black",fill="blue")



#pie chart
#importing dataset for pie chart
#axis ticks an dtick mark label can be removed can using 
#just contol horizontal justification and vjust controls

pie <- ggplot(exp,aes(x="",y=Temp,fill=factor(exp)))+
  geom_bar(width = 1,stat="identity")+
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  labs(fill="exp",x=NULL,y=NULL,
       title = "pie chart pf exp")

#the function coord_polar is used to char 
# which is start from ploar condition

pie + coord_polar(theta = "y",start = 0)



# lets explore some more
#airquality dataset














