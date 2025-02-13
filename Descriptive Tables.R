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

endline <- read_dta("~/Downloads/endline_cleaned.dta")

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
  mutate(across(c(district, wlthind, ethnicity, mstatus, educ, religion, sex, alive), as_factor))

household_vars <- c("district", "wlthind")
maternal_vars <- c("womanage", "ethnicity", "mstatus", "mumedu", "religion")
child_vars <- c("sex","alive")

table <- CreateTableOne(vars = c("district","wlthind","womanage","ethnicity",
                                 "mstatus","mumedu","religion","sex","alive"),
                        strata = "day2_missing",  
                        data = endline, 
                        test = TRUE)  # Perform statistical tests

print(table)

endline %>%
  filter(!is.na(district)) %>%
ggplot(aes(x = district, fill = factor(day2_missing))) +
  geom_bar(position = "dodge") +
  labs(x = "District", y = "Number of Children", fill = "Day2 Missing") +
  theme_minimal()
