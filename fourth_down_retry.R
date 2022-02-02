library(ggplot2)
library(dplyr)
library(caret)

# get play by play data of certain NFL seasons
seasons <- 2010:2020
pbp <- purrr::map_dfr(seasons, function(x) {
  con <- url(glue::glue("https://raw.githubusercontent.com/nflverse/nflfastR-data",
                        "/master/data/play_by_play_{x}.rds"))
  dat <- readRDS(con)
  close(con)
  dat
})


# get play by play only for when teams went for it on fourth down
df_fourth_go = subset(pbp, down == 4 & (play_type == "run" | play_type == "pass"))
df_fourth_go$conversion = ifelse(df_fourth_go$yards_gained >= df_fourth_go$ydstogo, 1, 0)

# find aggregate data for 4th down conversion percentage based on ydstogo (for first down) and yards away from endzone, also get count
agg_df_fourth_go = df_fourth_go %>%
  group_by(yardline_100, ydstogo) %>%
  summarise("percentage" = mean(conversion), "count" = length(conversion))

agg_df_fourth_go = data.frame(agg_df_fourth_go)

# # df with only 4th yardline for grouping
# agg_df_fourth_go_yardline = df_fourth_go %>%
#   group_by(yardline_100) %>%
#   summarize("count" = length(conversion))
# 
# # new df that will have groupings to create adequate samples
# fourth_down_grouped = data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("yardline_small", "yardline_big", "count"))))
# 
# con = 0
# # n keeps track of the beginning yardline
# n = 1
# i = 1
# while(i <= nrow(agg_df_fourth_go_yardline)) {
#   cat("i:", i, sep = " ")
#   cat(sep = "\n\n")
#   # creates a weighted percentage that will be divided by count when added to the df
#   # adds up the count (sample size)
#   con = con + agg_df_fourth_go_yardline[i, "count"]
#   # cat("count:", con, sep = " ")
#   # cat(sep = "\n\n")
#   # if sample size reaches 30, create a new data frame row and reset variables
#   if(con >= 300) {
#     # cat("yeet")
#     # cat(sep = "\n\n")
#     fourth_down_grouped[nrow(fourth_down_grouped) + 1, ] = c(n, i, con)
#     con = 0
#     n = i + 1
#   }
#   i = i + 1
# }
# # make a last row with the remaining data that was not able to get to achieve a sample of 30
# # fourth_down_grouped[nrow(fourth_down_grouped) + 1, ] = c(n, i, con)
# 
# # or combine last row with remaining data because remaining data is small
# fourth_down_grouped[nrow(fourth_down_grouped), ] = c(fourth_down_grouped[nrow(fourth_down_grouped), "yardline_small"],
#                                                      i,
#                                                      fourth_down_grouped[nrow(fourth_down_grouped),"count"] + con)
# each yardline grouping now has at least 100 samples which can be used as a grouping

# simplify df_fourth_go
df_fourth_go_simple = df_fourth_go[ , c("yardline_100", "ydstogo", "yards_gained", "conversion")]
colnames(df_fourth_go_simple) = c("yardline", "ydstogo", "yards_gained", "conversion")

# add groupings to df_fourth_go_simple
# df_fourth_go_simple$group = NA
# 
# 
# for(index in 1:nrow(fourth_down_grouped)) {
#   df_fourth_go_simple = df_fourth_go_simple %>% mutate(group = ifelse(yardline >= fourth_down_grouped[index, 1] & yardline <= fourth_down_grouped[index, 2], index, group))
# }


# df_fourth_go_simple$group = as.factor(df_fourth_go_simple$group)
# df_fourth_go_simple$conversion = as.factor(df_fourth_go_simple$conversion)
# 
# df_fourth_go_simple$ydstogo_squared = df_fourth_go_simple$ydstogo^2
# df_fourth_go_simple$ydstogo_cubed = df_fourth_go_simple$ydstogo^3
# 
# df_fourth_go_simple$yardline_squared = df_fourth_go_simple$yardline^2
# df_fourth_go_simple$yardline_cubed = df_fourth_go_simple$yardline^3

# Split data
set.seed(123)
training_samples_gfi = createDataPartition(df_fourth_go_simple$conversion, p = 0.8, list = FALSE)
gfi_train = df_fourth_go_simple[training_samples_gfi, ]
gfi_test = df_fourth_go_simple[-training_samples_gfi, ]

gfi_test_agg = gfi_test %>%
  group_by(ydstogo, yardline) %>%
  summarise("mean" = mean(conversion), "count" = length(conversion))

gfi_train$ydstogo_squared = gfi_train$ydstogo^2
gfi_train$ydstogo_cubed = gfi_train$ydstogo^3
gfi_train$ydstogo_ln = log(gfi_train$ydstogo)
gfi_train$yardline_squared = gfi_train$yardline^2
gfi_train$yardline_cubed = gfi_train$yardline^3
gfi_train$yardline_ln = log(gfi_train$yardline)

# First simpler model
gfi_model = glm(conversion ~ ydstogo + yardline, data = gfi_train, family = "binomial")
summary(gfi_model)

gfi_model_intercept = gfi_model$coefficients[1]
gfi_model_coef_a = gfi_model$coefficients[2]
gfi_model_coef_b = gfi_model$coefficients[3]

ydstogo_and_yardline = function(ydstogo, yardline) {
  exp(gfi_model_intercept + gfi_model_coef_a * ydstogo + gfi_model_coef_b * yardline)/(1 + exp(gfi_model_intercept + gfi_model_coef_a * ydstogo + gfi_model_coef_b * yardline))
}

ydstogo_and_yardline(1, 15)

# Score model
gfi_test$prediction1 = ydstogo_and_yardline(gfi_test$ydstogo, gfi_test$yardline)
gfi_test$binary1 = 0
gfi_test[which(gfi_test$prediction1 >= 0.5), "binary1"] = 1
gfi_test$binary1 = as.factor(gfi_test$binary1)
gfi_test$conversion = as.factor(gfi_test$conversion)
str(gfi_test)
sum(gfi_test$conversion == gfi_test$binary1)/nrow(gfi_test)
confusionMatrix(gfi_test$binary1, gfi_test$conversion)


# second model with squared and cubed terms for ysdtogo
gfi_model_2 = glm(conversion ~ ydstogo + ydstogo_squared + ydstogo_cubed + yardline, data = gfi_train, family = "binomial")
summary(gfi_model_2)

gfi_model_intercept2 = gfi_model_2$coefficients[1]
gfi_model_coef_a2 = gfi_model_2$coefficients[2]
gfi_model_coef_b2 = gfi_model_2$coefficients[3]
gfi_model_coef_c2 = gfi_model_2$coefficients[4]
gfi_model_coef_d2 = gfi_model_2$coefficients[5]

ydstogo_and_yardline_2 = function(ydstogo, yardline) {
  exp(gfi_model_intercept2 + gfi_model_coef_a2 * ydstogo + gfi_model_coef_b2 * ydstogo^2 + gfi_model_coef_c2 * ydstogo^3 + gfi_model_coef_d2 * yardline)/(1 + exp(gfi_model_intercept2 + gfi_model_coef_a2 * ydstogo + gfi_model_coef_b2 * ydstogo^2 + gfi_model_coef_c2 * ydstogo^3 + gfi_model_coef_d2 * yardline))
}

ydstogo_and_yardline_2(1, 15)

# Score model
gfi_test$prediction2 = ydstogo_and_yardline_2(gfi_test$ydstogo, gfi_test$yardline)
gfi_test$binary2 = 0
gfi_test[which(gfi_test$prediction2 >= 0.5), "binary2"] = 1
gfi_test$binary2 = as.factor(gfi_test$binary2)
gfi_test$conversion = as.factor(gfi_test$conversion)
str(gfi_test)
sum(gfi_test$conversion == gfi_test$binary2)/nrow(gfi_test)
confusionMatrix(gfi_test$binary2, gfi_test$conversion)





gfi_model_3 = glm(conversion ~ ydstogo_ln + yardline_ln, data = gfi_train, family = "binomial")
summary(gfi_model_3)

ydstogo_and_yardline_3 = function(ydstogo, yardline) {
  exp(gfi_model_3$coefficients[1] + gfi_model_3$coefficients[2] * log(ydstogo) + gfi_model_3$coefficients[3] * log(yardline))/(1 + exp(gfi_model_3$coefficients[1] + gfi_model_3$coefficients[2] * log(ydstogo) + gfi_model_3$coefficients[3] * log(yardline)))
}
ydstogo_and_yardline_3(1, 15)

# Score model
gfi_test$prediction3 = ydstogo_and_yardline_3(gfi_test$ydstogo, gfi_test$yardline)
gfi_test$binary3 = 0
gfi_test[which(gfi_test$prediction3 >= 0.5), "binary3"] = 1
gfi_test$binary3 = as.factor(gfi_test$binary3)
gfi_test$conversion = as.factor(gfi_test$conversion)
str(gfi_test)
sum(gfi_test$conversion == gfi_test$binary3)/nrow(gfi_test)
confusionMatrix(gfi_test$binary3, gfi_test$conversion)





ggplot(data = subset(gfi_test_agg, ydstogo == 1),
       mapping = aes(x = yardline,
                     y = mean))+
  ggtitle("4th and 1 Conversion by Yards to Endzone", subtitle = "2010 to 2020, Test Data")+
  xlab("Yards to Endzone")+
  ylab("Conversion Percentage")+
  geom_point(aes(size = count))+
  geom_line(aes(y = ydstogo_and_yardline(ydstogo, yardline),
                size = 7, colour = "Simple Model"))+  
  geom_line(aes(y = ydstogo_and_yardline_2(ydstogo, yardline),
                size = 7, colour = "Complex Model"))+
  geom_line(aes(y = ydstogo_and_yardline_3(ydstogo, yardline),
                size = 7, colour = "Ln Model"))

ggplot(data = subset(gfi_test_agg, yardline >= 30 & yardline <= 70),
       mapping = aes(x = ydstogo,
                     y = mean))+
  ggtitle("4th Down Conversion by Yards for First Down", subtitle = "2010 to 2020, Test Data, Near Midfield")+
  xlab("Yards for First Down")+
  ylab("Conversion Percentage")+
  geom_point(aes(size = count))+
  geom_line(aes(y = ydstogo_and_yardline(ydstogo, yardline),
                size = 3, colour = "Simple Model"))+
  geom_line(aes(y = ydstogo_and_yardline_2(ydstogo, yardline),
                size = 3, colour = "Complex Model"))+
  geom_line(aes(y = ydstogo_and_yardline_3(ydstogo, yardline),
                size = 7, colour = "Ln Model"))

# I think the logarithmic model is the best because of its performance at short yardage (when going for it is a realistic possibility). 
# Fourth and 1 mean percentage --> 0.65, 2 --> 0.56, 3 --> 0.49, all of which the logarithmic model is closest to predicting






### find out yards gained while converting and not converting based on ydstogo and yardline

# while converting
gfi_success = subset(df_fourth_go_simple, conversion == 1)
gfi_success$excess = gfi_success$yards_gained - gfi_success$ydstogo

# Split data
set.seed(123)
training_samples_gfi = createDataPartition(gfi_success$excess, p = 0.8, list = FALSE)
gfi_train = gfi_success[training_samples_gfi, ]
gfi_test = gfi_success[-training_samples_gfi, ]

gfi_test_agg = gfi_test %>%
  group_by(ydstogo, yardline) %>%
  summarise("mean" = mean(excess), "count" = length(excess))

gfi_train$ydstogo_squared = gfi_train$ydstogo^2
gfi_train$ydstogo_cubed = gfi_train$ydstogo^3

gfi_train$yardline_squared = gfi_train$yardline^2
gfi_train$yardline_cubed = gfi_train$yardline^3

# simple model
summary(lm(excess ~ ydstogo * yardline, data = gfi_train))
anova(lm(excess ~ ydstogo * yardline, data = gfi_train))

# interaction term is insignificant
summary(lm(excess ~ ydstogo + yardline, data = gfi_train))
anova(lm(excess ~ ydstogo + yardline, data = gfi_train))

# looks solid

excess_model1 = lm(excess ~ ydstogo + yardline, data = gfi_train)
excess_model1_intercept = excess_model1$coefficients[1]
excess_model1_coef_a = excess_model1$coefficients[2]
excess_model1_coef_b = excess_model1$coefficients[3]

excess_model1_equation = function(ydstogo, yardline) {
  excess_model1_intercept + excess_model1_coef_a * ydstogo + excess_model1_coef_b * yardline
}

excess_model1_equation(5, 10)
excess_model1_equation(30, 90)

# check performance with test data

gfi_test$prediction1 = excess_model1_equation(gfi_test$ydstogo, gfi_test$yardline)
gfi_test$error1 = abs(gfi_test$excess - gfi_test$prediction1)

# MAE
sum(gfi_test$error1)/nrow(gfi_test)


# more advanced model
summary(lm(excess ~ ydstogo + ydstogo_squared + ydstogo_cubed + yardline + yardline_squared + yardline_cubed, data = gfi_train))
anova(lm(excess ~ ydstogo + ydstogo_squared + ydstogo_cubed + yardline + yardline_squared + yardline_cubed, data = gfi_train))

# every single term here is significant

excess_model2 = lm(excess ~ ydstogo + ydstogo_squared + ydstogo_cubed + yardline + yardline_squared + yardline_cubed, data = gfi_train)

excess_model2_equation = function(ydstogo, yardline) {
  excess_model2$coefficients[1] + excess_model2$coefficients[2] * ydstogo + excess_model2$coefficients[3] * ydstogo^2 + excess_model2$coefficients[4] * ydstogo^3 + excess_model2$coefficients[5] * yardline + excess_model2$coefficients[6] * yardline^2 + excess_model2$coefficients[7] * yardline^3
}

excess_model2_equation(1, 1)
excess_model2_equation(30, 90)

gfi_test$prediction2 = excess_model2_equation(gfi_test$ydstogo, gfi_test$yardline)
gfi_test$error2 = abs(gfi_test$excess - gfi_test$prediction2)

# MAE
sum(gfi_test$error2)/nrow(gfi_test)

# MAE is slightly lower

ggplot(data = subset(gfi_test_agg, yardline >= 30 & yardline <= 70),
       mapping = aes(x = ydstogo,
                     y = mean))+
  ggtitle("Excess Yards Gained on 4th Down Conversions", subtitle = "2010 to 2020, Test Data, Near Midfield")+
  xlab("Yards for First Down")+
  ylab("Excess Yards Gained")+
  geom_point(aes(size = count))+
  geom_line(aes(y = excess_model1_equation(ydstogo, yardline),
                size = 3, colour = "Simple Model"))+
  geom_line(aes(y = excess_model2_equation(ydstogo, yardline),
                size = 3, colour = "Complex Model"))

# There really does not seem to be much difference at all between the two equations
# I would say the simpler one is slightly better because the complex model sometimes gives negatives



# non conversion equation

gfi_fail = subset(df_fourth_go_simple, conversion == 0)
gfi_fail$shortage = gfi_fail$ydstogo - gfi_fail$yards_gained

# Split data
set.seed(123)
training_samples_gfi = createDataPartition(gfi_fail$shortage, p = 0.8, list = FALSE)
gfi_train = gfi_fail[training_samples_gfi, ]
gfi_test = gfi_fail[-training_samples_gfi, ]

# make simple model

summary(lm(shortage ~ ydstogo + yardline, data = gfi_train))
anova(lm(shortage ~ ydstogo + yardline, data = gfi_train))

# Yardline seems insignificant. Let's remove it

summary(lm(shortage ~ ydstogo, data = gfi_train))
anova(lm(shortage ~ ydstogo, data = gfi_train))

# Looks better now

shortage_model1 = lm(shortage ~ ydstogo, data = gfi_train)
shortage_model1_intercept = excess_model1$coefficients[1]
shortage_model1_coef_a = excess_model1$coefficients[2]

shortage_model1_equation = function(ydstogo) {
  shortage_model1_intercept + shortage_model1_coef_a * ydstogo
}

shortage_model1_equation(1)
shortage_model1_equation(10)

# check performance with test data

gfi_test$prediction1 = shortage_model1_equation(gfi_test$ydstogo)
gfi_test$error1 = abs(gfi_test$shortage - gfi_test$prediction1)

# MAE
sum(gfi_test$error1)/nrow(gfi_test)

# try more complex model

gfi_train$sq = gfi_train$ydstogo^2
gfi_train$cu = gfi_train$ydstogo^3

summary(lm(shortage ~ ydstogo + sq + cu, data = gfi_train))
anova(lm(shortage ~ ydstogo + sq + cu, data = gfi_train))

shortage_model2 = lm(shortage ~ ydstogo + sq + cu, data = gfi_train)
shortage_model2_intercept = shortage_model2$coefficients[1]
shortage_model2_coef_a = shortage_model2$coefficients[2]
shortage_model2_coef_b = shortage_model2$coefficients[3]
shortage_model2_coef_c = shortage_model2$coefficients[4]

shortage_model2_equation = function(ydstogo) {
  shortage_model2_intercept + shortage_model2_coef_a * ydstogo + shortage_model2_coef_b * ydstogo^2 + shortage_model2_coef_c * ydstogo^3
}

shortage_model2_equation(1)
shortage_model2_equation(10)

# check performance with test data

gfi_test$prediction2 = shortage_model2_equation(gfi_test$ydstogo)
gfi_test$error2 = abs(gfi_test$shortage - gfi_test$prediction2)

# MAE
sum(gfi_test$error2)/nrow(gfi_test)

# MAE is quite low


# finally, try interaction term

summary(lm(shortage ~ ydstogo * yardline, data = gfi_train))
anova(lm(shortage ~ ydstogo * yardline, data = gfi_train))

shortage_model3 = lm(shortage ~ ydstogo * yardline, data = gfi_train)
shortage_model3_intercept = shortage_model3$coefficients[1]
shortage_model3_coef_a = shortage_model3$coefficients[2]
shortage_model3_coef_b = shortage_model3$coefficients[3]
shortage_model3_coef_c = shortage_model3$coefficients[4]

shortage_model3_equation = function(ydstogo, yardline) {
  shortage_model3_intercept + shortage_model3_coef_a * ydstogo + shortage_model3_coef_b * yardline + shortage_model3_coef_c * ydstogo * yardline
}

shortage_model3_equation(5, 10)

gfi_test$prediction3 = shortage_model3_equation(gfi_test$ydstogo, gfi_test$yardline)
gfi_test$error3 = abs(gfi_test$shortage - gfi_test$prediction3)

# MAE
sum(gfi_test$error3)/nrow(gfi_test)

# This MAE is actually the lowest

ggplot(data = subset(gfi_test_agg, yardline >= 0 & yardline <= 100),
       mapping = aes(x = ydstogo,
                     y = mean))+
  ggtitle("Yards Short on Failed 4th Down Conversions", subtitle = "2010 to 2020, Test Data")+
  xlab("Yards for First Down")+
  ylab("Shortage of Yards")+
  geom_point(aes(size = count))+
  geom_line(aes(y = shortage_model1_equation(ydstogo),
                size = 3, colour = "Simple Model"))+
  geom_line(aes(y = shortage_model2_equation(ydstogo),
                size = 3, colour = "Complex Model"))+
  geom_line(aes(y = shortage_model3_equation(ydstogo, yardline),
                size = 3, colour = "Interaction Model"))

# It seems as if either the interaction term or complex model are the best
# I think the best bet is the interaction term model because it has the lowest MAE