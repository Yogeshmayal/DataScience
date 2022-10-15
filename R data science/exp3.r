# importing dataset
Titanic <- read.csv("C:\\Users\\Yogesh\\Downloads\\titanic.csv")

#view dataset 
View(Titanic)

# datatypes check
str(Titanic)

# Dataypes conversion
Titanic$sex <- as.factor(Titanic$sex)
Titanic$pclass <- as.factor(Titanic$pclass)
Titanic$survived <- as.factor(Titanic$survived)
Titanic$embarked <- as.factor(Titanic$embarked)

# Datatypes recheck
str(Titanic)

# Express Table for people survived
prop.table(table(Titanic$survived))

#import library
library(ggplot2)

# people survival count bar graph
#theme classes : a classic-looking theme,with x and y axis line
ggplot(Titanic,aes(x=survived))+theme_classic()+
  geom_bar()+
  labs(y="Passenger count", title="Titanic survival count")

# geom_bar(aes(y=..count..)/sun(..count..))
# gender wise survival plot
ggplot(Titanic,aes(x=sex,fill=survived))+
  theme_light()+
  geom_bar()+
  labs(y="Passenger count",
       title = "Titanic survival count by gender")

# plot for survival cased an class
ggplot(Titanic,aes(x=pclass,fill=survived))+theme_light()+
  geom_bar()+
  labs(y="Passenger count",
       title = "Titanic survival count by Class")

# plot for gender based survival for each class
# facet with one varible
# the graph is partitioned in multiple parels ny levels of sclass

ggplot(Titanic,aes(x=sex,fill=survived))+theme_light()+
  facet_grid(~pclass)+geom_bar()+
  labs(y="Passenger count",
       title="Titanic survival count")

# pie chart for survival ration i each class
ggplot(Titanic,aes(x="",fill=survived))+
  geom_bar(position = "fill")+
  facet_grid(~pclass)+coord_polar(theta = "y")





