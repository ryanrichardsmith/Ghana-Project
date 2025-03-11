install.packages("pacman")
library(pacman)

p_load("haven","dplyr","labelled","tableone","ggplot2","Gmisc","gtsummary","lubridate")

endline <- read_dta("endline_cleaned.dta")

#creating numeric chilid
endline$num_childid <- seq_len(nrow(endline))

#creating numeric womanid
endline <- endline %>%
  mutate(num_womanid = as.numeric(factor(womanid)))

#creating numeric hhid
endline <- endline %>%
  mutate(num_hhid = as.numeric(factor(hhid)))

#creating a dichotomous variable based on day2
endline <- endline %>%
  mutate(day2_missing = ifelse(is.na(day2), "Missing", "Not Missing"))

#creating a dichotomous variable based on agreement between day1 and day2
endline <- endline %>%
  mutate(day2_discordant = ifelse(is.na(day1) | is.na(day2), NA, 
                                  ifelse(day1 == day2, 0, 1))) %>%
  mutate(day2_discordant = factor(day2_discordant, 
                                  levels = c(0, 1), 
                                  labels = c("Cordant", "Discordant")))

var_label(endline$day2_discordant) <- "Day2 Discordance"

#generating DOBs based on hypothetical misestimations of +/- 1 year  
endline <- endline %>%
  mutate(childdob_overestimate = case_when(
    todayyr == p2b_yr ~ NA,
    todayyr != p2b_yr ~ make_date(year = p2b_yr + 1, month = p2b_mo, day = day)
  )) %>%
  mutate(childdob_underestimate = make_date(year = p2b_yr - 1, month = p2b_mo, day = day))

endline <- endline %>%
  mutate(day1_overestimate = wday(childdob_overestimate) - 1) %>%
  mutate(day1_underestimate = wday(childdob_underestimate) - 1) 
  
#adding value labels to day of the week for consistency with original day1
endline <- endline %>%
  mutate(across(c(day1_overestimate, day1_underestimate), 
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

#determining hypothetical day2 concordances 
endline <- endline %>%
  mutate(day2_overestimate_discordant = ifelse(is.na(day1_overestimate) | is.na(day2), NA, 
                                  ifelse(day1_overestimate == day2, "Cordant Day2", "Discordant Day 2"))) %>%
  mutate(day2_overestimate_discordant = factor(day2_overestimate_discordant, levels = c("Cordant Day2", "Discordant Day 2")))

endline <- endline %>%
  mutate(day2_underestimate_discordant = ifelse(is.na(day1_underestimate) | is.na(day2), NA, 
                                               ifelse(day1_underestimate == day2, "Cordant Day2", "Discordant Day 2"))) %>%
  mutate(day2_underestimate_discordant = factor(day2_underestimate_discordant, levels = c("Cordant Day2", "Discordant Day 2")))

var_label(endline) <- list(
  day2_overestimate_discordant = "Day2 Discordance (Overestimated Year of Birth)",
  day2_underestimate_discordant = "Day2 Discordance (Overestimated Year of Birth)"
)
