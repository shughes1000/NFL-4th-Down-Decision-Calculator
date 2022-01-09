library(ggplot2)
library(caret)

seasons <- 2010:2020
pbp <- purrr::map_dfr(seasons, function(x) {
  con <- url(glue::glue("https://raw.githubusercontent.com/nflverse/nflfastR-data",
                        "/master/data/play_by_play_{x}.rds"))
  dat <- readRDS(con)
  close(con)
  dat
})

pbp_fg = subset(pbp, is.na(field_goal_result) == F)
pbp_fg$fg_result = ifelse(pbp_fg$field_goal_result == "made", 1, 0)

pbp_fg_simple = data.frame(pbp_fg$kick_distance, pbp_fg$fg_result)
colnames(pbp_fg_simple) = c("kick_distance", "fg_result")

pbp_fg_simple = pbp_fg_simple[order(pbp_fg_simple$kick_distance), ]

# zero padding because there are not many samples at virtually unmakable ranges
fg_fix_distance = rep(c(70:84), 100)
fg_fix_results = rep(0, 1500)

fg_fix_df = data.frame(fg_fix_distance, fg_fix_results)

pbp_fg_simple = rbind(pbp_fg_simple, setNames(fg_fix_df, names(pbp_fg_simple)))
pbp_fg_simple = pbp_fg_simple[order(pbp_fg_simple$kick_distance), ]

# Switch order of columns
pbp_fg_simple = pbp_fg_simple[, c(2, 1)]

# extra coefficients
pbp_fg_simple$square = pbp_fg_simple$kick_distance^2
pbp_fg_simple$cube = pbp_fg_simple$kick_distance^3


### COMMENTING ALL OF THIS OUT BECAUSE IT DOES NOT TRAIN TEST SPLIT
# # build model
# fg_model_3 = glm(fg_result ~ kick_distance + square + cube, data = pbp_fg_simple, family = "binomial")
# summary(fg_model_3)
# 
# fg_intercept_3 = fg_model_3$coefficients["(Intercept)"]
# fg_coef_3a = fg_model_3$coefficients["kick_distance"]
# fg_coef_3b = fg_model_3$coefficients["square"]
# fg_coef_3c = fg_model_3$coefficients["cube"]
# 
# fg_null_3 = fg_model_3$null.deviance/(-2)
# fg_proposed_3 = fg_model_3$deviance/(-2)
# 
# # McFadden R-squared
# (fg_null_3 - fg_proposed_3)/fg_null_3
# 
# # Chi-Squared
# 1 - pchisq(2*(fg_proposed_3-fg_null_3), df = length(fg_model_3$coefficients) - 1)
# 
# 
# 
# # draw really nice graph
# predicted.data = data.frame(probability.of.make = fg_model_3$fitted.values,
#                             result = ifelse(pbp_fg_simple$fg_result == 0, "miss", "make"), distance = pbp_fg_simple$kick_distance)
# 
# predicted.data = predicted.data[order(predicted.data$probability.of.make,
#                                       decreasing = FALSE), ]
# 
# predicted.data$rank = 1:nrow(predicted.data)
# 
# ggplot(data = predicted.data,
#        aes(x = distance, y = probability.of.make))+
#   geom_point(aes(color = result), position = position_jitter(0, 0.05), alpha = 1, shape = 4, stroke = 2)+
#   ggtitle("Predicted Probability of Making Field Goal By Distance")+
#   xlab("Kick Distance")+
#   ylab("Predicted Probability of Making Field Goal")+
#   labs(color = "Result")+
#   scale_color_manual(labels = c("Success", "Fail"), values = c("turquoise3", "salmon"))


### THIS IS THE GOOD STUFF
# TODO: Copy above ggplot to the split data

### THIS IS BAD BUT UNDERNEATH STUFF IS GOOD
# train_control = trainControl(method = "repeatedcv", number = 2, savePredictions = TRUE)
# pbp_fg_simple$fg_result = factor(pbp_fg_simple$fg_result)
# 
# fg_model_caret = train(fg_result ~ kick_distance + square + cube, data = pbp_fg_simple,
#                        trControl = train_control, method = "glm", family = "binomial")
# summary(fg_model_caret)
# 
# # I may be doing something wrong but I think due to the size of the dataset, I get the exact same results as fg_model_3
# summary(fg_model_3)

# try simple test train splitting
pbp_fg = subset(pbp, is.na(field_goal_result) == F)
pbp_fg$fg_result = ifelse(pbp_fg$field_goal_result == "made", 1, 0)

pbp_fg_simple = data.frame(pbp_fg$kick_distance, pbp_fg$fg_result)
colnames(pbp_fg_simple) = c("kick_distance", "fg_result")

pbp_fg_simple = pbp_fg_simple[order(pbp_fg_simple$kick_distance), ]

# Zero padding because there are not many samples at virtually unmakable ranges
fg_fix_distance = rep(c(70:84), 100)
fg_fix_results = rep(0, 1500)

fg_fix_df = data.frame(fg_fix_distance, fg_fix_results)

pbp_fg_simple = rbind(pbp_fg_simple, setNames(fg_fix_df, names(pbp_fg_simple)))
pbp_fg_simple = pbp_fg_simple[order(pbp_fg_simple$kick_distance), ]

# Switch order of columns
pbp_fg_simple = pbp_fg_simple[, c(2, 1)]

# Extra coefficients (because the fit was extremely poor when just using x or using both x^2 and x)
pbp_fg_simple$square = pbp_fg_simple$kick_distance^2
pbp_fg_simple$cube = pbp_fg_simple$kick_distance^3
pbp_fg_simple$ln = log(pbp_fg_simple$kick_distance)
pbp_fg_simple$exp = exp(pbp_fg_simple$kick_distance)

# Split data
set.seed(123)
training_samples = createDataPartition(pbp_fg_simple$kick_distance, p = 0.8, list = FALSE)
fg_train = pbp_fg_simple[training_samples, ]
fg_test = pbp_fg_simple[-training_samples, ]

# Build polynomial model
fg_model_3 = glm(fg_result ~ kick_distance + square + cube, data = fg_train, family = "binomial")
summary(fg_model_3)

# Obtain equation constants from model
fg_intercept_3 = fg_model_3$coefficients["(Intercept)"]
fg_coef_3a = fg_model_3$coefficients["kick_distance"]
fg_coef_3b = fg_model_3$coefficients["square"]
fg_coef_3c = fg_model_3$coefficients["cube"]

fg_null_3 = fg_model_3$null.deviance/(-2)
fg_proposed_3 = fg_model_3$deviance/(-2)

# McFadden R-squared
(fg_null_3 - fg_proposed_3)/fg_null_3

# Chi-Squared P-Value
1 - pchisq(2*(fg_proposed_3-fg_null_3), df = length(fg_model_3$coefficients) - 1)

# Equation made by training model
fg_equation = function(x) {
  return(exp(fg_intercept_3 + fg_coef_3a*x + fg_coef_3b*x^2 + fg_coef_3c*x^3)/(1 + exp(fg_intercept_3 + fg_coef_3a*x + fg_coef_3b*x^2 + fg_coef_3c*x^3)))
}
fg_equation(50)

# Score model
fg_test$prediction = fg_equation(fg_test$kick_distance)
fg_test$binary = 0
fg_test[which(fg_test$prediction >= 0.5), "binary"] = 1
sum(fg_test$fg_result == fg_test$binary)/nrow(fg_test)

fg_test$fg_result = as.factor(fg_test$fg_result)
fg_test$binary = as.factor(fg_test$binary)
str(fg_test)
confusionMatrix(fg_test$binary, fg_test$fg_result)




# Build logorithmic model
fg_model_ln = glm(fg_result ~ ln, data = fg_train, family = "binomial")
summary(fg_model_ln)

# Obtain equation constants from model
fg_intercept_ln = fg_model_ln$coefficients["(Intercept)"]
fg_coef_ln = fg_model_ln$coefficients["ln"]


fg_null_ln = fg_model_ln$null.deviance/(-2)
fg_proposed_ln = fg_model_ln$deviance/(-2)

# McFadden R-squared
(fg_null_ln - fg_proposed_ln)/fg_null_ln

# Chi-Squared P-Value
1 - pchisq(2*(fg_proposed_ln-fg_null_ln), df = length(fg_model_ln$coefficients) - 1)

# Equation made by training model
fg_equation_ln = function(x) {
  return(exp(fg_intercept_ln + fg_coef_ln*log(x))/(1 + exp(fg_intercept_ln + fg_coef_ln*log(x))))
}


# Score model
fg_test$prediction_ln = fg_equation_ln(fg_test$kick_distance)
fg_test$binary_ln = 0
fg_test[which(fg_test$prediction_ln >= 0.5), "binary_ln"] = 1
fg_test$binary_ln = as.factor(fg_test$binary_ln)
str(fg_test)
sum(fg_test$fg_result == fg_test$binary_ln)/nrow(fg_test)
confusionMatrix(fg_test$binary_ln, fg_test$fg_result)

# Logarithmic performs close to polynomial model in terms of Accuracy and kappa. However, the model is way too optimistic about long field goals.








# Exponential model
fg_model_exp = glm(fg_result ~ exp, data = fg_train, family = "binomial")
summary(fg_model_exp)

# Obtain equation constants from model
fg_intercept_exp = fg_model_exp$coefficients["(Intercept)"]
fg_coef_exp = fg_model_exp$coefficients["exp"]


fg_null_exp = fg_model_exp$null.deviance/(-2)
fg_proposed_exp = fg_model_exp$deviance/(-2)

# McFadden R-squared
(fg_null_exp - fg_proposed_exp)/fg_null_exp

# Chi-Squared P-Value
1 - pchisq(2*(fg_proposed_exp-fg_null_exp), df = length(fg_model_exp$coefficients) - 1)

# Equation made by training model
fg_equation_exp = function(x) {
  return(exp(fg_intercept_exp + fg_coef_exp*exp(x))/(1 + exp(fg_intercept_exp + fg_coef_exp*exp(x))))
}


# Score model
fg_test$prediction_exp = fg_equation_exp(fg_test$kick_distance)
fg_test$binary_exp = 0
fg_test[which(fg_test$prediction_exp >= 0.5), "binary_exp"] = 1
fg_test$binary_exp = as.factor(fg_test$binary_exp)
str(fg_test)
sum(fg_test$fg_result == fg_test$binary_exp)/nrow(fg_test)
confusionMatrix(fg_test$binary_exp, fg_test$fg_result)

# Basically the opposite effect of the logarithmic model. Accuracy and Kappa are similar but model way under predicts at close distances. 



# Aggregate rows of test data to draw onto a ggplot
fg_test_agg = aggregate((as.numeric(fg_result) - 1)~kick_distance, fg_test, FUN = function(x) c(mean(x), length(x)))

# Convert back to dataframe
fg_test_agg = data.frame(c(fg_test_agg[1], fg_test_agg[2]))
colnames(fg_test_agg)[2:3] = c("percentage", "count")

# Add rows of NA if no field goal was kicked from distance i
i = 18
while(i <= 84) {
  if(sum(fg_test_agg[, "kick_distance"] == i) == 0) {
    fg_test_agg = rbind(fg_test_agg, c(i, NA, 0))
  }
  i = i + 1
}

# Order the aggregated test dataframe
fg_test_agg = fg_test_agg[order(fg_test_agg$kick_distance), ]









# Make plot for visualization
ggplot(data = fg_test_agg,
       mapping = aes(x = kick_distance,
                     y = percentage))+
  ggtitle("Field Goal Percentage by Distance", subtitle = "2010 to 2020")+
  xlab("Distance")+
  ylab("Make Percentage")+
  geom_point(aes(size = count))+
  geom_line(aes(y = fg_equation(kick_distance),
                 size = 7, colour = "cubed"))+
  geom_line(aes(y = fg_equation_ln(kick_distance),
                size = 7, colour = "ln"))+
  geom_line(aes(y = fg_equation_exp(kick_distance),
                size = 7, colour = "exp"))

# Visually, the polynomial clearly has the best fit on the test data
