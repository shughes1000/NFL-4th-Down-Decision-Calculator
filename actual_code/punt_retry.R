library(ggplot2)
library(dplyr)

seasons <- 2010:2020
pbp <- purrr::map_dfr(seasons, function(x) {
  con <- url(glue::glue("https://raw.githubusercontent.com/nflverse/nflfastR-data",
                        "/master/data/play_by_play_{x}.rds"))
  dat <- readRDS(con)
  close(con)
  dat
})

pbp_clean = subset(pbp, is.na(play_type) == F & play_type != "no_play")
punts = which(pbp_clean$play_type == "punt")
punts_next_play = which(pbp_clean$play_type == "punt") + 1
punts_and_next_play = c(punts, punts_next_play)
punts_and_next_play = sort(punts_and_next_play)

df_punting = pbp_clean[punts_and_next_play, ]

unique(df_punting$play_type)

punts_in_punting = which(df_punting$play_type == "punt")

# check if there is a situation with two punts on back to back plays
sum(punts_in_punting %% 2 == 0)

df_xp = subset(pbp, play_type == "extra_point")
df_xp = subset(df_xp, kick_distance == 33)
df_xp$xp_result = ifelse(df_xp$extra_point_result == "good", 1, 0)
xp_mean = mean(df_xp$xp_result)

fd_ep = read.csv("aggregated_first_down_expected_points.csv")[ , c(2:4)]

start_array = c()
end_array = c()

punt_turnover_array = c()



for(i in punts_in_punting) {
  # printout so I know what iteration I am on
  cat("iteration:", i)
  cat("" , sep = "\n")
  
  # starting field position
  start = df_punting[i, "yardline_100"]
  start_array = c(start_array, start)
  
  # edge cases
  # punt return for touchdown or safety
  if(df_punting[i + 1, "play_type"] == "extra_point" | df_punting[i + 1, "two_point_attempt"] == 1) {
    end = 6 + xp_mean
  } else if(df_punting[i + 1, "play_type"] == "kickoff") {
    end = 2
  } else {
    end = fd_ep[as.numeric(df_punting[i + 1, "yardline_100"]), "points_scored"]
  }
  
  
  
  # Check for turnover. If there is not a turnover, multiply by -1 (because the ep metric is from the team with ball's perspective)
  if(df_punting[i, "posteam"] != df_punting[i + 1, "posteam"]) {
    end = end * (-1)
  }
  end_array = c(end_array, end)
  
}

start_array = unlist(start_array)
end_array = unlist(end_array)

df_punting_data = data.frame(start_array, end_array)
colnames(df_punting_data) = c("start", "resulting_ep")

# Aggregate data
agg_punt = df_punting_data %>%
  group_by(start) %>%
  summarise("mean" = mean(resulting_ep), "count" = length(resulting_ep))

# Write CSV
write.csv(df_punting_data, "punt_data.csv")
write.csv(agg_punt, "aggregated_punt.csv")

# Make plot
ggplot(data = agg_punt,
       mapping = aes(x = start,
                     y = mean))+
  ggtitle("Expected Points After Punt by Punting Field Position", subtitle = "2010 to 2020")+
  xlab("Punting Team Distance From Opposing Endzone Before Punt")+
  ylab("Expected Points After Punt")+
  geom_point(aes(size = count))+
  geom_smooth(se = FALSE, col = "red")


