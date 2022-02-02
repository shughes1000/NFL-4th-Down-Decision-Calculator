library(ggplot2)

# get play by play data of certain NFL seasons
seasons <- 2010:2020
pbp <- purrr::map_dfr(seasons, function(x) {
  con <- url(glue::glue("https://raw.githubusercontent.com/nflverse/nflfastR-data",
                        "/master/data/play_by_play_{x}.rds"))
  dat <- readRDS(con)
  close(con)
  dat
})

# go through each first down to try and find the next scoring play
unique(pbp$play_type)
df2 = subset(pbp, play_type != "no_play" & play_type != "extra_point" & is.na(play_type) == F)
unique(df2$play_type)

df3 = subset(df2, field_goal_result == "made" | is.na(td_team) == F | safety == 1 | (down == 1 & ydstogo == 10) | (down == 1 & ydstogo == yardline_100 & yardline_100 < 10))
unique(df3$play_type)

# which(df3$down == 1)
# which(is.na(df3$td_team) == F)
# which(df3$field_goal_result == "made")

df_xp = subset(pbp, play_type == "extra_point")
df_xp = subset(df_xp, kick_distance == 33)
df_xp$xp_result = ifelse(df_xp$extra_point_result == "good", 1, 0)
xp_mean = mean(df_xp$xp_result)


# i = 1
# distance = c()
# points_scored = c()
# for(i in which(df3$down == 1)) {
#   cat("first down:", i)
#   cat("" , sep = "\n")
#   pos = df3[i, "posteam"]
#   half = df3[i, "game_half"]
#   game = df3[i, "game_id"]
#   
#   next_td = subset(which(is.na(df3$td_team) == F), (which(is.na(df3$td_team) == F) >= i) == T)[1]
#   next_fg = subset(which(df3$field_goal_result == "made"), (which(df3$field_goal_result == "made") >= i) == T)[1]
#   next_saf = subset(which(df3$safety == 1), (which(df3$safety == 1) >= i) == T)[1]
#   # cat("next_td:", next_td)
#   # cat("" , sep = "\n")
#   # cat("next_fg:", next_fg)
#   # cat("" , sep = "\n")
#   
#   if(is.na(next_td) == T & is.na(next_fg) == T) {
#     next_score = NA
#   } else if(is.na(next_td) == T) {
#     score_type = "fg"
#     next_score = next_fg
#   } else if(is.na(next_fg) == T) {
#     score_type = "td"
#     next_score = next_td
#   } else if(next_td < next_fg) {
#     score_type = "td"
#     next_score = next_td
#   } else {
#     score_type = "fg"
#     next_score = next_fg
#   }
#   
#   if(is.na(next_saf) == F & next_saf < next_score) {
#     score_type = "saf"
#     next_score = next_saf
#   }
#   # cat("next_score:", next_score)
#   # cat("" , sep = "\n")
#   # cat("" , sep = "\n")
#   
#   dis = df3[i, "yardline_100"]
#   distance = c(distance, dis)
#   
#   if(!is.na(next_score)) {
#     if(half == df3[next_score, "game_half"] & game == df3[next_score, "game_id"]) {
#     
#       if(score_type == "td" & pos == df3[next_score, "posteam"]) {
#         points_scored = c(points_scored, 6.94)
#       } else if(score_type == "td" & pos != df3[next_score, "posteam"]) {
#         points_scored = c(points_scored, -6.94)
#       } else if(score_type == "fg" & pos == df3[next_score, "posteam"]) {
#         points_scored = c(points_scored, 3)
#       } else if(score_type == "fg" & pos != df3[next_score, "posteam"]) {
#         points_scored = c(points_scored, -3)
#       } else if(score_type == "saf" & pos == df3[next_score, "posteam"]) {
#         points_scored = c(points_scored, -2)
#       } else {
#         points_scored = c(points_scored, 2)
#       }
#     
#     } else {
#       points_scored = c(points_scored, 0)
#     }
#   } else {
#     points_scored = c(points_scored, 0)
#   }
# }

i = 1
distance = c()
points_scored = c()
for(i in which(df3$down == 1)) {
  
  # printout so I know what iteration I am on
  cat("first down:", i)
  cat("" , sep = "\n")
  
  # some useful values from current iteration to be used later
  pos = df3[i, "posteam"]
  half = df3[i, "game_half"]
  game = df3[i, "game_id"]
  
  # find the next score for each different type of score
  next_td = subset(which(is.na(df3$td_team) == F), (which(is.na(df3$td_team) == F) >= i) == T)[1]
  next_fg = subset(which(df3$field_goal_result == "made"), (which(df3$field_goal_result == "made") >= i) == T)[1]
  next_saf = subset(which(df3$safety == 1), (which(df3$safety == 1) >= i) == T)[1]
  
  # create array with the 3 types of scores so I can easily find the minimum
  arr = c(next_td, next_fg, next_saf)
  
  # edge case: if all the values are NA, then there was not another score which will cause errors
  if(length(na.omit(arr)) == 0) {
    next_score = NA
  } else {
    # find minimum value from arr (in other words, figure out which of the scoring plays came first)
    ans = which.min(arr)
    
    if(ans == 1) {
      # if touchdowns came first, award touchdown points
      next_score = next_td
      points = 6 + xp_mean
    } else if(ans == 2) {
      # if field goals came first, award field goal points
      next_score = next_fg
      points = 3
    } else {
      # if safeties came first, award safety points. This is negative because the team without possession is the one that gains 2 points 
      next_score = next_saf
      points = -2
    }
  }
  
  # make sure next_score is not NA, else award 0 points
  if(!is.na(next_score)) {
    
    # make sure the score came in the same half and game (i.e. the same sequence of plays, else award 0 points)
    if(game == df3[next_score, "game_id"] & half == df3[next_score, "game_half"]) {
      
      # check if the team that originally had the ball scored the points, else multiply the points by -1 because the points were scored by the other team
      if(pos == df3[next_score, "posteam"]) {
        points_scored = c(points_scored, points)
      } else {
        points_scored = c(points_scored, -points)
      }
    } else {
      points_scored = c(points_scored, 0)
    }
  } else {
    points_scored = c(points_scored, 0)
  }
  
  # simply add the distance from end zone to the variable so I can take the aggregate later
  dis = df3[i, "yardline_100"]
  distance = c(distance, dis)
  
}

# I was having some issues with the variables turning into lists so I unlist them just in case
distance = unlist(distance)
points_scored = unlist(points_scored)

# create a data frame with distance and points scored and turn it into a dataframe with the aggregate distance, points for, and sample size
df_fd = data.frame(distance, points_scored)
agg_df_fd = aggregate(df_fd$points_scored ~ df_fd$distance,
                      FUN = function(x) c("points_scored" = mean(x), "count" = length(x)))

agg_df_fd = data.frame(c(agg_df_fd[1], agg_df_fd[2]))
colnames(agg_df_fd) = c("distance", "points_scored", "count")

# write to csv
write.csv(df_fd, "first_down_expected_points.csv")
write.csv(agg_df_fd, "aggregated_first_down_expected_points.csv")

# make a nice plot :)
ggplot(data = agg_df_fd,
       mapping = aes(x = distance,
                     y = points_scored))+
  ggtitle("Expected Points by Distance", subtitle = "First and 10 or Goal, 2010 to 2020")+
  xlab("Distance")+
  ylab("Expected Points")+
  geom_point(aes(size = count))+
  geom_smooth(se = FALSE)