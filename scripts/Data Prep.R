p_load("haven","dplyr","labelled","tableone","ggplot2","Gmisc","gtsummary",
       "lubridate", "tidyr", "purrr", "stringr", "glue","rlang")

endline <- read_dta(here("data","endline_cleaned.dta"))

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
                                  ifelse(day1 == day2, "Cordant Day2", "Discordant Day 2"))) %>%
  mutate(day2_discordant = factor(day2_discordant, levels = c("Cordant Day2", "Discordant Day 2")))

#creating age categories
endline <- endline %>%
  mutate(womanage_group = case_when(
    womanage >= 15 & womanage <= 24 ~ "15-24",
    womanage >= 25 & womanage <= 34 ~ "25-34",
    womanage >= 35 & womanage <= 44 ~ "35-44",
    womanage >= 45 ~ "45+"
  ))

#creating variable labels for easier interpretation
var_label(endline) <- list(
  wlthind = "Mother's Wealth Index",
  womanage_group = "Mother's Age Group",
  ethnicity = "Mother's Ethnicity",
  mstatus = "Mother's Marital Status",
  mumedu = "Mother's Education",
  religion = "Mother's Religion",
  sex = "Child's Sex",
  alive = "Child's Alive Status"
)

endline <- endline %>%
  mutate(across(c(district, wlthind, ethnicity, mstatus, mumedu, religion, sex, alive), as_factor))