install.packages("pacman")
library(pacman)

p_load("haven","dplyr","labelled","tableone","ggplot2","Gmisc","gtsummary",
       "lubridate", "tidyr", "purrr", "stringr", "glue")

endline <- readRDS("endline.rds")

#filtering to only include discordant observations
endline_discordant <- endline %>%
  filter(day2_discordant == "Discordant Day 2")

#generating DOBs based on hypothetical overestimations of months and +1 year  
months <- 0:11
years <- c(-2, -1, 1, 2)

endline_discordant <- months %>%
  purrr::reduce(~ mutate(.x, 
                         !!paste0("dob_", .y, "mo_over_1yr_over") := if_else(
                           p2b_mo > (12 - .y) | todayyr == p2b_yr, NA,
                           make_date(year = p2b_yr + 1, month = p2b_mo + .y, day = day)),
                         !!paste0("dob_", .y, "mo_over_1yr_under") := if_else(
                           p2b_mo > (12 - .y), NA,
                           make_date(year = p2b_yr - 1, month = p2b_mo + .y, day = day))),
                .init = endline_discordant)

endline_discordant <- rev(months) %>%
  purrr::reduce(~ mutate(.x, 
                         !!paste0("dob_", .y, "mo_under_1yr_over") := if_else(
                           p2b_mo < .y | todayyr == p2b_yr, NA,
                           make_date(year = p2b_yr + 1, month = p2b_mo - .y, day = day)),
                         !!paste0("dob_", .y, "mo_under_1yr_under") := if_else(
                           p2b_mo < .y, NA,
                           make_date(year = p2b_yr - 1, month = p2b_mo - .y, day = day))),
                .init = endline_discordant)

#*******************************************************************************
endline_discordant <- months %>%
  purrr::reduce(~ purrr::reduce(years, function(.x, yr) {
    mutate(.x, 
           !!paste0("dob_", .y, "mo_over_", ifelse(yr < 0, paste0(abs(yr), "yr_under"), paste0(abs(yr), "yr_over"))) := if_else(
             p2b_mo > (12 - .y) | todayyr == p2b_yr, NA,
             make_date(year = p2b_yr + yr, month = p2b_mo + .y, day = day)),
           !!paste0("dob_", .y, "mo_under_", ifelse(yr < 0, paste0(abs(yr), "yr_under"), paste0(abs(yr), "yr_over"))) := if_else(
             p2b_mo > (12 - .y), NA,
             make_date(year = p2b_yr + yr, month = p2b_mo + .y, day = day))
    )
  }, .init = .), .init = endline_discordant)


endline_discordant <- endline_discordant %>%
  mutate(across(starts_with("dob_"), 
                ~ wday(.x) - 1, 
                .names = "day1_{.col}"))  

#adding value labels to day of the week for consistency with original day1
endline_discordant <- endline_discordant %>%
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
endline_discordant %>%
  pivot_longer(cols = matches("^day1_|^dob_|day2_discordant$"), names_to = "columns", 
               values_to = "values") %>%
  mutate(years_displaced = ifelse(grepl("mo_under", columns), 
                                  as.numeric(substring(columns, 2, 2), "Savory")))

endline_discordant_long <- endline_discordant %>%
  select(matches("^dob_|day2_discordant$")) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(), 
               names_to = "columns", 
               values_to = "values") %>%
  mutate(
    months_displaced = case_when(
      grepl("mo_under", columns) ~ -as.numeric(str_extract(columns, "\\d+(?=mo_under)")),  
      grepl("mo_over", columns) ~ as.numeric(str_extract(columns, "\\d+(?=mo_over)"))
    ),
    years_displaced = case_when(
      grepl("yr_under", columns) ~ -as.numeric(str_extract(columns, "\\d+(?=yr_under)")),  
      grepl("yr_over", columns) ~ as.numeric(str_extract(columns, "\\d+(?=yr_over)"))    
    )
  )

#hypothetical day2 concordances heatmap
