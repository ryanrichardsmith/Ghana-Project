install.packages("pacman")
library(pacman)

p_load("source","here")

source(here("scripts","Data Prep.R"))

#filtering to only include discordant observations & observations recorded as deceased 
endline_discordant <- endline %>%
  filter(alive == "No") %>%
  filter(day2_discordant == "Discordant Day 2")

#generating DOBs based on hypothetical overestimations of months and +1 year  
months <- 0:11 
years <- c(-2, -1, 0, 1, 2)

for (mo_shift in months) {
  for (yr_shift in years) {
    
    col_over <- paste0("dob_", mo_shift, "mo_over_", 
                       ifelse(yr_shift < 0, paste0(abs(yr_shift), "yr_under"), paste0(abs(yr_shift), "yr_over")))
    
    col_under <- paste0("dob_", mo_shift, "mo_under_", 
                        ifelse(yr_shift < 0, paste0(abs(yr_shift), "yr_under"), paste0(abs(yr_shift), "yr_over")))
    
    base_date <- make_date(endline_discordant$p2b_yr + yr_shift,
                           endline_discordant$p2b_mo,
                           endline_discordant$day)
    
    dob_over <- add_with_rollback(base_date, months(mo_shift))
    dob_over <- if_else(year(dob_over) != year(base_date), as.Date(NA), dob_over)
    
    dob_under <- add_with_rollback(base_date, months(-mo_shift))
    dob_under <- if_else(year(dob_under) != year(base_date), as.Date(NA), dob_under)
    
    endline_discordant[[col_over]] <- dob_over
    endline_discordant[[col_under]] <- dob_under
  }
}

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
    months_disp = case_when(
      str_detect(column, "mo_over") ~ as.integer(str_extract(column, "\\d+(?=mo)")),
      str_detect(column, "mo_under") ~ -1 * as.integer(str_extract(column, "\\d+(?=mo)")),
      TRUE ~ 0
    ),
    years_disp = case_when(
      str_detect(column, "yr_over") ~ as.integer(str_extract(column, "\\d+(?=yr)")),
      str_detect(column, "yr_under") ~ -1 * as.integer(str_extract(column, "\\d+(?=yr)")),
      TRUE ~ 0
    ),
    total_months_disp = months_disp + (12 * years_disp)
  )

endline_discordant_long <- endline_discordant_long %>%
  distinct(num_childid, dob_value, total_months_disp, .keep_all = TRUE)

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

#extracting number of months/years displaced
probabilities <- probabilities %>%
  mutate(
    months_disp = case_when(
      str_detect(column, "mo_over") ~ as.integer(str_extract(column, "\\d+(?=mo)")),
      str_detect(column, "mo_under") ~ -1 * as.integer(str_extract(column, "\\d+(?=mo)")),
      TRUE ~ 0
    ),
    years_disp = case_when(
      str_detect(column, "yr_over") ~ as.integer(str_extract(column, "\\d+(?=yr)")),
      str_detect(column, "yr_under") ~ -1 * as.integer(str_extract(column, "\\d+(?=yr)")),
      TRUE ~ 0
    ),
    total_months_disp = months_disp + (12 * years_disp)
  )


#labelling variables for readibility
var_label(probabilities) <- list(
  Vs = "Number of Reallocated Births",
  Cs = "Number of Concordant Reallocated Births",
  Es = "Expected No. of Concordant Births",
  Rs = "Relative Increase Over Expected",
  difference_expected_observed = "Difference Between Expected/Observed Concordance",
  binomial_prob = "Prob. of Observing Concordant births among Reallocated Births")

print(probabilities, n = 100)

#setting values at the origin equal to 0 to leave the color scale unaffected
probabilities <- probabilities %>%
  mutate(Rs = if_else(years_disp == 0 & total_months_disp == 0, NA_real_, Rs))

#plotting heat map
probabilities %>%
  ggplot(aes(x = years_disp, y = months_disp, fill = Rs)) +
  geom_tile() + 
  geom_tile(
    data = subset(probabilities, binomial_prob < (0.05/114)),
    aes(x = years_disp, y = months_disp),
    fill = NA,
    color = "black",
    linewidth = 1
  ) +
  labs(
    x = "Number of Years Displaced",
    y = "Number of Months Displaced"  ) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 1,
    name = "Alive Status: Deceased\n\n\n\n\n\nBlack Outline: p < 0.05/114\n
Relative Increase in Number\nof Concordant Births\nOver Expected Number"
  ) +
  scale_x_continuous(expand = expansion(), sec.axis = dup_axis()) +
  scale_y_continuous(expand = expansion(), sec.axis = dup_axis()) +
  coord_fixed() +
  geom_vline(xintercept = 0, color = "gray40", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "gray40", linetype = "dashed") +
  theme_minimal()

conditions <- c(
  "Effect of Year Displacement" = "years_disp != 0 & months_disp == 0",
  "Effect of Month Displacement" = "years_disp == 0 & months_disp != 0",
  "Effect of Year Underestimation & Month Overestimation" = "years_disp < 0 & months_disp > 0",
  "Effect of Year Overestimation & Month Overestimation" = "years_disp > 0 & months_disp > 0",
  "Effect of Year Underestimation & Month Underestimation" = "years_disp < 0 & months_disp < 0",
  "Effect of Year Overestimation & Month Underestimation" = "years_disp > 0 & months_disp < 0"
)

results <- list()

for (i in seq_along(conditions)) {
  condition_expr <- parse_expr(conditions[i])
  
  calculations <- endline_discordant_long %>%
    filter(!!condition_expr) %>%
    summarise(
      Vs = n_distinct(dob_value),
      Cs = sum(day2_discordant_value == "Concordant Day 2", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Cs = ifelse(is.na(Cs), 0, Cs)) %>%
    mutate(
      Es = Vs * p,
      Rs = ((Cs - Es) / Es) * 100,
      difference_expected_observed = Cs - Es,
      binomial_prob = dbinom(Cs, Vs, p),
      Effect = names(conditions)[i]
    )
  
  results[[i]] <- calculations
}

aggregated_probabilities <- bind_rows(results)

#putting label as first column
aggregated_probabilities <- aggregated_probabilities %>%
  select(Effect, everything())

#adding column labels for readability
var_label(aggregated_probabilities) <- list(
  Vs = "Number of Reallocated Births",
  Cs = "Number of Concordant Reallocated Births",
  Es = "Expected No. of Concordant Births",
  Rs = "Relative Increase Over Expected",
  difference_expected_observed = "Difference Between Expected/Observed Concordance",
  binomial_prob = "Prob. of Observing Concordant births among Reallocated Births"
)

print(aggregated_probabilities)