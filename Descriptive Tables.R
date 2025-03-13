install.packages("pacman")
library(pacman)

p_load("haven","dplyr","labelled","tableone","ggplot2","Gmisc","gtsummary",
       "lubridate", "tidyr")

endline <- readRDS("endline.rds")

table <- CreateTableOne(vars = c("wlthind","womanage_group","ethnicity",
                                 "mstatus","mumedu","religion","sex","alive"),
                        strata = "day2_missing",  
                        data = endline, 
                        test = TRUE)  

print(table, varLabels = TRUE)

#plotting percentage of children with day2 missing broken down by district
#code by prof. Helleringer
endline %>%
  filter(!is.na(district)) %>%
  group_by(day2_missing) %>%
  mutate(total = n()) %>%  
  ungroup() %>%
  count(day2_missing,district,total) %>%  
  mutate(percentage = n / total * 100) %>%
  mutate(district = factor(district, levels = unique(district))) %>%  
  ggplot(aes(y = district, x = percentage, fill = factor(day2_missing))) +
  geom_col(position = "dodge") +
  labs(y = "District", x = "Percentage of Children", fill = "Day2 Missing") +
  theme_minimal()


table2 <- endline %>% 
  getDescriptionStatsBy(wlthind, womanage_group,ethnicity, mstatus, mumedu, 
                        religion, sex, alive,
                        by = day2_discordant,
                        hrzl_prop = TRUE,
                        statistics = TRUE)


print(table2)
