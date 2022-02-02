library(ggplot2)
library(dplyr)
library(caret)

# load in data from csv
df_punting_data = read.csv("punt_data.csv")[ , 2:3]

### Make model that predicts the resulting expected points after a punt

# Split data
set.seed(123)
training_samples_punt = createDataPartition(df_punting_data$resulting_ep, p = 0.8, list = FALSE)
punt_train = df_punting_data[training_samples_punt, ]
punt_test = df_punting_data[-training_samples_punt, ]

# aggregate test data for graph later
punt_test_agg = punt_test %>%
  group_by(start) %>%
  summarise("mean" = mean(resulting_ep), "count" = length(resulting_ep))

# I know from the plot in practicing_punting_data that the equation is likely a quadratic or cubic
punt_train$start_squared = punt_train$start^2
punt_train$start_cubed = punt_train$start^3

summary(lm(resulting_ep ~ ., data = punt_train))
anova(lm(resulting_ep ~ ., data = punt_train))

# Everything is significant

punt_model = lm(resulting_ep ~ ., data = punt_train)

punt_equation = function(yardline) {
  punt_model$coefficients[1] + punt_model$coefficients[2] * yardline + punt_model$coefficients[3] * yardline^2 + punt_model$coefficients[4] * yardline^3
}

punt_equation(90)

ggplot(data = punt_test_agg,
       mapping = aes(x = start,
                     y = mean))+
  ggtitle("Expected Points after Punt by Punting Field Position", subtitle = "2010 to 2020, Plotted on Test Data")+
  xlab("Punting Team Distance from Opposing Endzone before Punt (Yards)")+
  ylab("Punting Team Expected Points after Punt")+
  geom_point(aes(size = count))+
  geom_line(aes(y = punt_equation(start),
                size = 3, colour = "Model"))

# The fit on the test data is fantastic
