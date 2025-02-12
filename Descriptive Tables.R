install.packages("haven")
install.packages("dplyr")
install.packages("labelled")
install.packages("tableone")

library(haven)
library(dplyr)
library(labelled)
library(tableone)

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
  mutate(across(c(region, wlthind, ethnic, currmarrstat, educ, religion, sex), as_factor))

household_vars <- c("region", "wlthind")
maternal_vars <- c("womanage", "ethnic", "currmarrstat", "educ", "religion")
child_vars <- c("sex")

table <- CreateTableOne(vars = c(household_vars, maternal_vars, child_vars),
                        strata = "day2_missing",  
                        data = endline, 
                        test = TRUE)  # Perform statistical tests

print(table)