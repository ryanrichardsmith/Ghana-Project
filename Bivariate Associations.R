install.packages("pacman")
library(pacman)

p_load("haven","dplyr","labelled","tableone","ggplot2","Gmisc","gtsummary")

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

#creating a dichotomous variable based on agreement between day1 and day2
endline <- endline %>%
  mutate(day2_discordant = ifelse(is.na(day1) | is.na(day2), NA, 
                                  ifelse(day1 == day2, 0, 1))) %>%
  mutate(day2_discordant = factor(day2_discordant, 
                                  levels = c(0, 1), 
                                  labels = c("Cordant", "Discordant")))

var_label(endline$day2_discordant) <- "Day2 Discordance"

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



#regressing the likelihood of birth day discordance on child's vital status 
#converting day2_discordant to be numeric (1 or 2) then subtracting one to make it 1 or 0
model1 <- glm(as.numeric(day2_discordant) - 1 ~ alive + religion + mumedu + mstatus +
                    ethnicity + womanage_group + wlthind, data = endline, 
                  family = binomial)

                                 #exponentiate presents coefficients as odds ratios
table3 <- tbl_regression(model1, exponentiate = TRUE) 
print(table3)

#plotting day2 discordance vs age at death (months)
endline %>%
  filter(alive != "Yes", !is.na(ageatdeath_mo), !is.na(day2_discordant)) %>%
  ggplot(aes(x = day2_discordant, y = ageatdeath_mo)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5)

endline %>%
  filter(alive != "Yes", !is.na(ageatdeath_mo), !is.na(day2_discordant)) %>%
  ggplot(aes(x = day2_discordant, y = ageatdeath_mo)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1)

#regressing the likelihood of day2_discordance on birth & insurance registration 
#converting day2_discordant to be numeric (1 or 2) then subtracting one to make it 1 or 0
model2 <- glm(as.numeric(day2_discordant) - 1 ~ as_factor(immcard, levels = "default") 
               + as_factor(nhis, levels = "default") + as_factor(registered) + 
               religion + mumedu + mstatus + ethnicity + womanage_group + wlthind,                                                   
               data = subset(endline, alive == "Yes"), family = binomial)

                                   #exponentiate presents coefficients as odds ratios
table4 <- tbl_regression(model2, exponentiate = TRUE) 
print(table4)

#plotting all3dpt against day2_discordant
endline %>%
  filter(!is.na(age_mon), !is.na(day2_discordant)) %>%
  mutate(age_weeks = age_mon * 4) %>%  
  group_by(age_weeks, day2_discordant) %>%  
  summarise(percent_all3dpt = mean(all3dpt == 1) * 100, .groups = 'drop') %>%
  ggplot(aes(x = factor(age_weeks), y = percent_all3dpt, fill = factor(day2_discordant))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(
    x = "Age (Weeks)",
    y = "Percentage Immunized on all 3 DPT (all3dpt = 1)",
    fill = "Day 2 Discordant"  
  ) +
  theme_minimal() +  
  theme(
    legend.position = "top") +
  scale_fill_manual(values = c("blue", "red"))  

#plotting vitAsup against day2_discordant
endline %>%
  filter(!is.na(age_mon), !is.na(day2_discordant)) %>%
  mutate(age_weeks = age_mon * 4) %>%  
  group_by(age_weeks, day2_discordant) %>%  
  summarise(percent_vitAsup = mean(vitAsup == 1) * 100, .groups = 'drop') %>%
  ggplot(aes(x = factor(age_weeks), y = percent_vitAsup, fill = factor(day2_discordant))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(
    x = "Age (Weeks)",
    y = "Percentage with 2 doses of Vit A Supplementation  (vitAsup = 1)",
    fill = "Day 2 Discordant"  
  ) +
  theme_minimal() +  
  theme(
    legend.position = "top") +
  scale_fill_manual(values = c("blue", "red"))  