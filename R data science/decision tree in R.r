# install packages
install.packages("ISLR")
install.packages("tree")

# Load the libraries
library(ISLR)
library(tree)

# load the dataset from ISLR library
data(package="ISLR")

# Load the carseats data
carseats<-Carseats
View(carseats)
# name of an objects
names(carseats)

# str of a dataset
str(carseats)

# summary of a dataset
summary(carseats)

# sales is a response variable
# histogram of sales
hist(carseats$Sales)


# convert sales into binary variable
High = ifelse(carseats$Sales<=8,"NO","Yes")

# merging High with carseats data
carseats = data.frame(carseats, High)

# str of dataset
str(carseats)

# Converting char variables to factor
carseats$High = as.factor(carseats$High)

# str of dataset
str(carseats)

# Fit of decision tree
tree.carseats = tree(High~.-Sales, data=carseats)

# summary of model
summary(tree.carseats)

# plot () function to display the tree struccture
plot(tree.carseats)

# text () function to display the node labels
text(tree.carseats, pretty = 0)
tree.carseats

# Prune th tree
set.seed(101)

train = sample(1:nrow(carseats),250)

carseats.test=carseats[-train ,]
High.test=High[-train]

tree.carseats = tree(High~,-Sales,carseats,subset=train)
plot(tree.carseats)
text(tree.carseats, pretty=0)
# plot() function to display the tree structure 
plot(tree.carseats)
text(tree.carseats, pretty=0)

# text() function to display the node labels.
text(tree.carseats, pretty=0)

# prediction
tree.pred = predict(tree.carseats,carseats.test, type="class")
table(tree.pred , HIgh.test)

# Accuracy of a model
(74+39)/(74+18+19+39)

set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
