install.packages("pacman")
library(pacman)

p_load("haven","dplyr","labelled","tableone","ggplot2","Gmisc","gtsummary",
       "lubridate", "tidyr")

endline <- readRDS("endline.rds")

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

#plotting hypothetical day2 concordances
 endline %>%
       filter(day2_discordant == "Discordant Day 2") %>%
       group_by(sex) %>%
       summarise(
             Cordant_Day2_Overestimate = mean(day2_overestimate_discordant == "Cordant Day2", na.rm = TRUE),
             Cordant_Day2_Underestimate = mean(day2_underestimate_discordant == "Cordant Day2", na.rm = TRUE),
             Discordant_Day2_Either = mean(
                   day2_overestimate_discordant == "Discordant Day 2" | 
                         day2_underestimate_discordant == "Discordant Day 2", 
                   na.rm = TRUE
               )
         ) %>%
       mutate(across(-sex, ~ . / (Cordant_Day2_Overestimate + Cordant_Day2_Underestimate 
                                  + Discordant_Day2_Either) * 100)) %>%
       pivot_longer(cols = -sex, names_to = "Scenarios", values_to = "Percentage") %>%
       ggplot(aes(x = sex, y = Percentage, fill = Scenarios)) +
       geom_bar(stat = "identity", position = "stack") +
       labs(
             title = "Hypothetical Day 2 Concordance by Sex",
             x = "Sex",
             y = "Percentage",
             fill = "Category"
         ) +
       theme_minimal()