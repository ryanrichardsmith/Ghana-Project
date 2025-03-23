install.packages("pacman")
library(pacman)

p_load("haven","dplyr","labelled","tableone","ggplot2","Gmisc","gtsummary",
       "lubridate", "tidyr", "purrr", "stringr", "glue")

endline <- readRDS("endline.rds")

#generating DOBs based on hypothetical overestimations of months and +1 year  
months <- 0:11

endline <- months %>%
  purrr::reduce(~ mutate(.x, 
                         !!paste0("dob_", .y, "mo_over_1yr_over") := if_else(
                           p2b_mo > (12 - .y) | todayyr == p2b_yr, NA,
                           make_date(year = p2b_yr + 1, month = p2b_mo + .y, day = day)),
                         !!paste0("dob_", .y, "mo_over_1yr_under") := if_else(
                           p2b_mo > (12 - .y), NA,
                           make_date(year = p2b_yr - 1, month = p2b_mo + .y, day = day))),
                .init = endline)

endline <- rev(months) %>%
  purrr::reduce(~ mutate(.x, 
                         !!paste0("dob_", .y, "mo_under_1yr_over") := if_else(
                           p2b_mo < .y | todayyr == p2b_yr, NA,
                           make_date(year = p2b_yr + 1, month = p2b_mo - .y, day = day)),
                         !!paste0("dob_", .y, "mo_under_1yr_under") := if_else(
                           p2b_mo < .y, NA,
                           make_date(year = p2b_yr - 1, month = p2b_mo - .y, day = day))),
                .init = endline)

endline <- endline %>%
  mutate(across(starts_with("dob_"), 
                ~ wday(.x) - 1, 
                .names = "day1_{.col}"))  

#adding value labels to day of the week for consistency with original day1
endline <- endline %>%
  mutate(across(starts_with("day1_"), 
                ~ labelled(as.numeric(.), 
                           labels = c(
                             "Sunday" = 0,
                             "Monday" = 1,
                             "Tuesday" = 2,
                             "Wednesday" = 3,
                             "Thursday" = 4,
                             "Friday" = 5,
                             "Saturday" = 6
                           ))))

#determining hypothetical concordances
endline <- endline %>%
  mutate(across(starts_with("day1_"),
                ~ ifelse(is.na(.x) | is.na(day2), NA,
                         ifelse(.x == day2, "Cordant", "Discordant")),
                .names = "{str_remove(.col, '^day1_')}_day2_discordant"))


#hypothetical day2 concordances heatmap
