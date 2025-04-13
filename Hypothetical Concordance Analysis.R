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
years <- c(-2, -1, 0, 1, 2)

endline_discordant <- months %>%
  purrr::reduce(~ purrr::reduce(years, function(.x, yr) {
    mutate(.x, 
           !!paste0("dob_", .y, "mo_over_", ifelse(yr < 0, paste0(abs(yr), "yr_under"), paste0(abs(yr), "yr_over"))) := if_else(
             p2b_mo > (12 - .y) | todayyr == p2b_yr, NA,
             make_date(year = p2b_yr + yr, month = p2b_mo + .y, day = day)),
           !!paste0("dob_", .y, "mo_under_", ifelse(yr < 0, paste0(abs(yr), "yr_under"), paste0(abs(yr), "yr_over"))) := if_else(
             p2b_mo > (12 - .y), NA,
             make_date(year = p2b_yr + yr, month = p2b_mo - .y, day = day))
    )
  }, .init = .), .init = endline_discordant)


dob_cols <- names(endline_discordant)[str_starts(names(endline_discordant), "dob_")]

# filtering out scenarios where the reallocated DOB falls after the date of the survey
endline_discordant <- endline_discordant %>%
  mutate(todaymo_num = as.numeric(todaymo))  

for (col in dob_cols) {
  endline_discordant[[col]] <- if_else(
    !is.na(endline_discordant[[col]]) &
      (
        year(endline_discordant[[col]]) > endline_discordant$todayyr |
          (year(endline_discordant[[col]]) == endline_discordant$todayyr &
             month(endline_discordant[[col]]) > endline_discordant$todaymo_num)
      ),
    as.Date(NA),
    endline_discordant[[col]]
  )
}

#subtracting one to ensure days of the week are consistent with day1
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
endline_discordant <- endline_discordant %>%
  mutate(across(starts_with("day1_"), 
                ~ case_when(
                  . == day2 ~ "Concordant Day 2",
                  . != day2 ~ "Discordant Day 2"                ), 
                .names = "day2_discordant_{str_remove(.col, 'day1_')}")
  )


#transforming data to be long-form
dob_long <- endline_discordant %>%
  select(num_childid, starts_with("dob_")) %>%
  pivot_longer(-num_childid, names_to = "column", values_to = "dob_value")

discordant_long <- endline_discordant %>%
  select(num_childid, starts_with("day2_discordant_")) %>%
  pivot_longer(-num_childid, names_to = "column", values_to = "day2_discordant_value") %>%
  mutate(column = str_remove(column, "^day2_discordant_"))

endline_discordant_long <- dob_long %>%
  left_join(discordant_long, by = c("num_childid", "column"))

#preventing duplicated reallocated DOBs by filtering out repeated entries with 
#0 months reallocated in the same reallocated year
endline_discordant_long <- endline_discordant_long %>%
  mutate(
    months_disp = as.integer(str_extract(column, "\\d+(?=mo)")),
    years_disp = case_when(
      str_detect(column, "yr_over") ~ as.integer(str_extract(column, "\\d+(?=yr)")),
      str_detect(column, "yr_under") ~ -1 * as.integer(str_extract(column, "\\d+(?=yr)")),
      TRUE ~ 0
    ),
    total_months_disp = months_disp + (12 * years_disp)
  )

endline_discordant_long <- endline_discordant_long %>%
  group_by(num_childid, total_months_disp) %>%
  slice(1) %>%
  ungroup()

#counting how many births were in fact reallocated in each scenario and
#counting how many reallocated births were in fact concordant with the day name
probabilities <- endline_discordant_long %>%
  group_by(column) %>%
  summarise(
    Vs = n_distinct(dob_value),
    Cs = sum(day2_discordant_value == "Concordant Day 2", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cs = ifelse(is.na(Cs), 0, Cs))

#calculating the binomial probability of observing Cs concordant births given 
#Vs reallocated births
p <- 1/7

probabilities <- probabilities %>%
  mutate(Es = Vs * p) %>%
  mutate(Rs = ((Cs - Es)/Es)*100) %>%
  mutate(difference_expected_observed = Cs - Es) %>%
  mutate(binomial_prob = dbinom(Cs, Vs, p))

#labelling variables for readibility
var_label(probabilities) <- list(
  Vs = "Number of Reallocated Births",
  Cs = "Number of Concordant Reallocated Births",
  Es = "Expected No. of Concordant Births",
  Rs = "Relative Increase Over Expected",
  difference_expected_observed = "Difference Between Expected/Observed Concordance",
  binomial_prob = "Prob. of Observing Concordant births among Reallocated Births")

print(probabilities, n = 100)
