install.packages("haven")
install.packages("dplyr")
install.packages("labelled")
install.packages("tableone")
install.packages("ggplot2")

library(haven)
library(dplyr)
library(labelled)
library(tableone)
library(ggplot2)

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


endline <- endline %>%
  mutate(day2_discordant = ifelse(is.na(day1) | is.na(day2), NA, 
                                  ifelse(day1 == day2, "Cordant Day2", "Discordant Day 2")))

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

table2 <- CreateTableOne(vars = c("wlthind","womanage_group","ethnicity",
                                 "mstatus","mumedu","religion","sex","alive"),
                        strata = "day2_discordant",  
                        data = endline, 
                        test = TRUE)  

print(table, varLabels = TRUE)
