# Make sure dplyr is installed or else the calculator will not work
library(dplyr)
library(ggplot2)
library(reshape2)

# Load in first down equation
fd_equation = function(yardline) {
  # outputs expected points on first down and 10/goal from yardline
  6.071684 + -0.09228537 * yardline + 0.0005127535 * yardline^2 + -2.21205e-06 * yardline^3
}


# calculate punts
punt_eq = function(yardline) {
  # Outputs resulting expected points after punt play for punting team
  -2.630846 + 0.122967 * yardline + -0.001934314 * yardline^2 + 6.787437e-06 * yardline^3
}


# Load in field goal equation
field_goal_eq = function(x) {
  # outputs percentage kick is a make
  # Note: yardline + 18 = x, and a miss would set the kicking team back 8 yards or to the 25, whichever is worse for kicking team
  exp(17.69459  + -0.9973467*x + 0.02175257*x^2 + -0.0001707057*x^3)/(1 + exp(17.69459  + -0.9973467*x + 0.02175257*x^2 + -0.0001707057*x^3))
}


# Load in 4th Down Equations
gfi_conversion_eq = function(ydstogo, yardline) {
  exp(0.1534778 + -0.6859447 * log(ydstogo) + 0.1693902  * log(yardline))/(1 + exp(0.1534778 + -0.6859447 * log(ydstogo) + 0.1693902 * log(yardline)))
}

gfi_success_eq = function(ydstogo, yardline) {
  # output is excess yards after first down marker
  round(1.87 + 0.347 * ydstogo + 0.0937 * yardline, 0)
}

gfi_fail_eq = function(ydstogo, yardline) {
  # output is yards short of first down
  round(0.617 + 0.968 * ydstogo + 0.0115 * yardline - 0.00149 * ydstogo * yardline, 0)
}


user_input = function() {
  valid_entry = FALSE
  while(valid_entry == FALSE) {
    fd_yds = as.integer(readline(prompt="How many yards for a first down? "))
    if(fd_yds > 0 & fd_yds < 100) {
      valid_entry = TRUE
    } else {
      message("This entry is invalid. Try again.")
    }
  }
  
  valid_entry = FALSE
  while(valid_entry == FALSE) {
    td_yds = as.integer(readline(prompt="How many yards for a touchdown? "))
    if(td_yds > 0 & td_yds < 100 & td_yds >= fd_yds & fd_yds > td_yds - 90) {
      valid_entry = TRUE
    } else {
      message("This entry is invalid. Try again.")
    }
  }
  return(c(fd_yds, td_yds))
}




master_decision = function(fd_yds, td_yds) {
  # make sure entry is possible
  if(td_yds > 0 & td_yds < 100 & td_yds >= fd_yds & fd_yds > td_yds - 90) {
    
  } else {
    return(NA)
  }
  
  punt_ep = NA
  if(td_yds >= 30) {
    punt_ep = punt_eq(td_yds)
  }
  
  fg_ep = NA
  fg_percentage = NA
  fg_make_ep = NA
  fg_miss_ep = NA
  if(td_yds <= 50) {
    fg_percentage = field_goal_eq(td_yds + 17)
    fg_make_ep = 3
    fg_miss_ep = -fd_equation(100 - (td_yds + 7))
    fg_ep = fg_make_ep * fg_percentage + fg_miss_ep * (1 - fg_percentage)
  }
  
  gfi_ep = NA
  gfi_percentage = NA
  gfi_success_ep = NA
  gfi_fail_ep = NA
  if(fd_yds < 30) {
    gfi_percentage = gfi_conversion_eq(fd_yds, td_yds)
    gfi_success_yardline = gfi_success_eq(fd_yds, td_yds) + fd_yds + td_yds
    if(gfi_success_yardline < 0) {
      gfi_success_ep = 6.94
    } else {
      gfi_success_ep = fd_equation(gfi_success_yardline)
    }
    
    gfi_fail_yardline = td_yds + fd_yds - gfi_fail_eq(fd_yds, td_yds)
    if(gfi_fail_yardline > 99) {
      gfi_fail_ep = -2
    } else {
      gfi_fail_ep = -fd_equation(100 - gfi_fail_yardline)
    }
    
    gfi_ep = gfi_success_ep * gfi_percentage + gfi_fail_ep * (1 - gfi_percentage)
  }
  
  # Output
  answer = which.max(c(punt_ep, fg_ep, gfi_ep))
  if(answer == 1){
    return("Punt")
  } else if(answer == 2) {
    return("Field Goal")
  } else {
    return("Go For It")
  }
}

# create matrix
mat = matrix(nrow = 99, ncol = 10)
# mat = outer(X = 1:nrow(mat), Y = 1:ncol(mat) , FUN = function(r, c) master(r, c))
# mat[1:5, ] = "gfi"
# mat[6:10, ] = "fg"
# mat[11:12, ] = "punt"


melt_mat = melt(mat)
colnames(melt_mat) = c("td_yds", "fd_yds", "Suggestion")

for(i in 1:nrow(melt_mat)) {
  melt_mat[i, 3] = master_decision(melt_mat[i, 2], melt_mat[i, 1])
}

melt_mat = melt_mat[!is.na(melt_mat$Suggestion), ]
melt_mat$fd_yds = melt_mat$fd_yds - 0.5
melt_mat$td_yds = melt_mat$td_yds - 0.5

ggplot(melt_mat, aes(x = td_yds, y = fd_yds, fill = Suggestion))+
  geom_tile()+
  labs(title = "Fourth Down Suggestions", x = "Yards Needed for Touchdown", y = "Yards Needed for First Down")+
  scale_fill_manual(values = c("#619cff", "#00ba38", "#f8766d"))+
  scale_x_continuous(breaks = round(seq(0, 100, by = 10),1))+
  scale_y_continuous(breaks = round(seq(0, 10, by = 1),1))
