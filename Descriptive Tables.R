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

#creating age categories
endline <- endline %>%
  mutate(womanage_group = case_when(
    womanage >= 15 & womanage <= 24 ~ "15-24",
    womanage >= 25 & womanage <= 34 ~ "25-34",
    womanage >= 35 & womanage <= 49 ~ "35-49"))

endline <- endline %>%
  mutate(across(c(district, wlthind, ethnicity, mstatus, mumedu, religion, sex, alive), as_factor))

table <- CreateTableOne(vars = c("wlthind","womanage_group","ethnicity",
                                 "mstatus","mumedu","religion","sex","alive"),
                        strata = "day2_missing",  
                        data = endline, 
                        test = TRUE)  

print(table)

endline %>%
  filter(!is.na(district)) %>%
  group_by(district) %>%
  mutate(total = n()) %>%  
  ungroup() %>%
  count(district, day2_missing, total) %>%  
  mutate(percentage = n / total * 100) %>%
  group_by(district) %>%
  mutate(ordering_value = percentage[day2_missing == "Missing"]) %>%
  ungroup() %>%
  arrange(ordering_value) %>%  
  mutate(district = factor(district, levels = unique(district))) %>%  
  ggplot(aes(y = district, x = percentage, fill = factor(day2_missing))) +
  geom_col(position = "dodge") +
  labs(y = "District", x = "Percentage of Children", fill = "Day2 Missing") +
  theme_minimal()
