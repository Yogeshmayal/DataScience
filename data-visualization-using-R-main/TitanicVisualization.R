library(ggplot2)
library(dplyr)
df <- read.csv("E:\\data-science\\GCS training session\\titanic\\titanic.csv",stringsAsFactors = FALSE)
View(df)
class(df)
str(df)
#datatype conversation
df$sex <- as.factor(df$sex)
df$pclass <- as.factor(df$pclass)
df$survived <- as.factor(df$survived)
df$embarked <- as.factor(df$embarked)
# express table of survived
table(df$survived)
prop.table(table(df$survived))
#plotting graph
ggplot(df, aes(x = survived)) + theme_classic() +
  geom_bar(fill='blue') +
  labs(y = "Passenger count",title = "Titanic survived count")
# percentage
ggplot(df, aes(x = survived)) + theme_classic() +
  geom_bar(fill='blue',aes(y=(..count..)/sum(..count..))) +
  labs(y = "Passenger count",title = "Titanic survived count")

#gender wise survival plot
ggplot(df, aes(x = sex,fill = survived)) + theme_classic() +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  labs(y = "Passenger count",title = "Survival count by gender")

ggplot(df, aes(x = pclass,fill = survived)) + theme_classic() +
  geom_bar() +
  labs(y = "Passenger count",title = "Survival count by class")

#facet with one variable
# the graph is partitioned in multiple panel by levels of pclass
ggplot(df, aes(x = sex,fill = survived)) + theme_light() +
  facet_grid(~pclass) +
  geom_bar() +
  labs(y = "Passenger count",title = "Titanic Survival count by class & gender")
# pie chart

ggplot(df,aes(x="",fill = survived)) +
  geom_bar(position = "fill") +
  facet_grid(~pclass) + coord_polar(theta = "y")

ggplot(df,aes(x=sex,fill = survived)) +
  geom_bar(position = "fill") +
  facet_grid(~pclass) + coord_polar(theta = "y")

#continuous 
# age distribution
ggplot(df,aes(x = age)) + theme_bw() + 
  geom_histogram(bandwidth = 5) + 
  labs(y = "passenger count", x = "age", title = "titanic age distribuation")

ggplot(df,aes(x = age,fill = survived)) + theme_bw() + 
  geom_histogram(bandwidth = 5) + 
  labs(y = "passenger count", x = "age", title = "titanic age distribuation")



#summary 
summary(df)


#box plot
ggplot(df,aes(x = survived, y = age)) + theme_bw() +
  geom_boxplot()+
  labs(y = "age",x = "survived",title = "titanic survival rate by age")
#density plot
ggplot(df,aes(x = age,fill = survived)) + theme_bw() + 
  geom_density(alpha = 0.5) + 
  labs(y = "passenger count", x = "age", title = "titanic age distribuation")

#titanic survival rate by age, class and gender
ggplot(df,aes(x = age,fill = survived)) + theme_bw()+
  facet_grid(sex~pclass) + geom_density(alpha = 0.5) +
  labs(y="age",x = "survived",title = "titanic survival rate by age, class and gender" )

ggplot(df,aes(x = age,fill = survived)) + theme_bw()+
  facet_wrap(sex~pclass) + geom_density(alpha = 0.5) +
  labs(y="age",x = "survived",title = "titanic survival rate by age, class and gender" )






