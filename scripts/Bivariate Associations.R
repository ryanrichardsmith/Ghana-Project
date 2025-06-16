install.packages("pacman")
library(pacman)

p_load("source","here")

source(here("scripts","Data Prep.R"))

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

################################################################################
#below plots need to be revised pending further information on health variables#
################################################################################

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