data()
data <- iris
View(data)


#confidence interval for population mean
set.seed(123)
data_sample <- sample(data$Sepal.Length,size=130)
data_sample

sample_mean <-mean(data_sample)
sample_mean

# Z-critical

z_critical <- qnorm(0.975)
z_critical

#Standard deviation
sample_standard_deviation <- sd(data_sample)

#Calculating the margin error
margin_error <- z_critical*(sample_standard_deviation/sqrt(130))

#Confidence Interval
confidence_interval <-c(sample_mean - margin_error,
                        sample_mean + margin_error)

print(confidence_interval)

#view confidence interval
View(confidence_interval)

#Population mean
population_mean <- mean(data$Sepal.Length)
population_mean

