library(ggplot2)
library(dplyr)
library(caret)

# load in data from csv
fd = read.csv("first_down_expected_points.csv")[2:3]

### Make model that predicts the expected points on a first down

# Split data
set.seed(123)
training_samples_fd = createDataPartition(fd$points_scored, p = 0.8, list = FALSE)
fd_train = fd[training_samples_fd, ]
fd_test = fd[-training_samples_fd, ]

# aggregate test data for graph later
fd_test_agg = fd_test %>%
  group_by(distance) %>%
  summarise("mean" = mean(points_scored), "count" = length(points_scored))

fd_train$sq = fd_train$distance^2
fd_train$cu = fd_train$distance^3

summary(lm(points_scored ~ ., data = fd_train))
anova(lm(points_scored ~ ., data = fd_train))

# All values are significant
fd_model = lm(points_scored ~ ., data = fd_train)


fd_equation = function(yardline) {
  fd_model$coefficients[1] + fd_model$coefficients[2] * yardline + fd_model$coefficients[3] * yardline^2 + fd_model$coefficients[4] * yardline^3
}

fd_equation(10)

ggplot(data = fd_test_agg,
       mapping = aes(x = distance,
                     y = mean))+
  ggtitle("Expected Points For First and 10/Goal By Field Position", subtitle = "2010 to 2020, Plotted On Test Data")+
  xlab("Yards Needed For Touchdown")+
  ylab("Expected Points")+
  geom_point(aes(size = count))+
  geom_line(aes(y = fd_equation(distance),
                size = 3, colour = "Model"))

# Just like punt data, fit is as good as reasonably expected