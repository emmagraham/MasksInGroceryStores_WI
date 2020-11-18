# EMMA J. GRAHAM LINCK, Tung H. Nguyen, Nick L. Arp
# Nov 17, 2020

# Setup packages ####
## Setting up environment

### Install packages only if not present
#install.packages("tidyverse")
#install.packages("lme4")

### Loading packages
library(tidyverse); library(lme4)

merged <- read.csv("./InputData/meta_merged_observed_Oct31correction.csv", row.names = 1)

###########################################################
###SECTION 1: Table 1 for counties other than Dane
table1_new_names = merged %>% 
  group_by(county) %>% 
  summarize(n = n(), mask_usage = paste0(c(sum(mask),"/", n(), " (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  unite(name, county:n, sep = ", n = ", remove = FALSE)

table1_new_gender = merged %>% 
  group_by(county, gender) %>% 
  summarize(mask_usage = paste0(c(sum(mask), "/", n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  pivot_wider(names_from = gender, values_from = mask_usage)

table1_new_age = merged %>% 
  group_by(county, age) %>% 
  summarize(mask_usage = paste0(c(sum(mask), "/", n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  pivot_wider(names_from = age, values_from = mask_usage)

table_new_homemade = merged %>% 
  group_by(county) %>% 
  summarize(homemade_mask_usage = paste0(c(sum(homeade), "/", sum(mask)," (", round(sum(homeade)/sum(mask)*100, 1), ")"), collapse = ""))
#pivot_wider(names_from = gender, values_from = mask_usage)
table_new_homemade
#write.csv(table_new_homemade, file="homemade_new_state.csv")

new_table1 = table1_new_names %>% 
  left_join(table1_new_gender, by = "county") %>% 
  left_join(table1_new_age, by = "county") %>% 
  left_join(table_new_homemade, by = "county") %>% 
  rename(Males = `0`, 
         Females = `1`,
         adult = a,
         `young adult` = y,
         `older adult` = e,
         child = c) %>% 
  filter(county != "dane")

write.csv(new_table1, file="./Output/all_counties_except_dane_table1.csv")

###########################################################
##SECTION 2B: Table 1 for Dane county, separated out by store
merged_dane <- merged %>% 
  filter(county=="dane")

table1_names = merged_dane %>% 
  group_by(id) %>% 
  summarize(n = n(), mask_usage = paste0(c(sum(mask),"/", n(), " (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  unite(name, id:n, sep = ", n = ", remove = FALSE)

table1_gender = merged_dane %>% 
  group_by(id, gender) %>% 
  summarize(mask_usage = paste0(c(sum(mask), "/", n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  pivot_wider(names_from = gender, values_from = mask_usage)

table1_age = merged_dane %>% 
  group_by(id, age) %>% summarize(mask_usage = paste0(c(sum(mask), "/", n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
  pivot_wider(names_from = age, values_from = mask_usage)

## Finally, make a total row for bottom of table, as 1 row dataframe
total_dane = cbind("Dane", "Total", merged_dane %>% 
                     summarize(n = n(), mask_usage = paste0(c(sum(mask),"/",n(), " (", round(sum(mask)/n()*100, 1), ")"), collapse = "")),
                   merged_dane %>% group_by(gender) %>% summarize(mask_usage = paste0(c(sum(mask), "/",n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
                     pivot_wider(names_from = gender, values_from = mask_usage),
                   merged_dane %>% group_by(age) %>% summarize(mask_usage = paste0(c(sum(mask), "/",n()," (", round(sum(mask)/n()*100, 1), ")"), collapse = "")) %>% 
                     pivot_wider(names_from = age, values_from = mask_usage))
names(total_dane) <- c("name", "id", "n", "mask usage", "Males", "Females", "Adult", "Child", "Older adult", "Young adult")

## Setup the final Data Frame
table1 = table1_names %>% 
  left_join(table1_gender, by = "id") %>% 
  left_join(table1_age, by = "id")
names(table1) <- names(total_dane)
final_dane_table1 = rbind(table1, total_dane) %>% 
  select(-name) %>% 
  select(id, n, Males, Females, Child, `Young adult`, Adult, `Older adult`)# combine main table with total row
write.csv(final_dane_table1, "./Output/dane_by_store_table1.csv")

###########################################################
# SECTION 3.1: Mixed effects logistic regression on factors affecting mask wearing in Dane county stores ####
merged_dane$gender = as.factor(recode(merged_dane$gender, `0` = "Male", `1` = "Female"))
merged_dane$gender = fct_relevel(merged_dane$gender, ref = "Male")
merged_dane$age = fct_relevel(merged_dane$age, levels = c("a", "y", "c", "e"))
merged_dane$age = droplevels(merged_dane$age)

##Multivariate mixed effects logistic regression
#store as random effect
mixed_effects <- glmer(mask ~ gender + age + (1|id), family = binomial, data= merged_dane)
se <- sqrt(diag(vcov(mixed_effects)))
(tab <- cbind(Est = fixef(mixed_effects), LL = fixef(mixed_effects) - 1.96 * se, UL = fixef(mixed_effects) + 1.96 *
                se))
exp(tab)

##Univariate mixed effects logistic regression for gender
#store as random effect
mixed_effects_gender <- glmer(mask ~ gender + (1|id), family = binomial, data= merged_dane)
se <- sqrt(diag(vcov(mixed_effects_gender)))
(tab <- cbind(Est = fixef(mixed_effects_gender), LL = fixef(mixed_effects_gender) - 1.96 * se, UL = fixef(mixed_effects_gender) + 1.96 *
                se))
exp(tab)
summary(mixed_effects_gender)

##Univariate mixed effects logistic regression for age
#store as random effect
mixed_effects_age <- glmer(mask ~ age + (1|id), family = binomial, data= merged_dane)
se <- sqrt(diag(vcov(mixed_effects_age)))
(tab <- cbind(Est = fixef(mixed_effects_age), LL = fixef(mixed_effects_age) - 1.96 * se, UL = fixef(mixed_effects_age) + 1.96 *
                se))
exp(tab)
summary(mixed_effects_age)

###########################################################
##SECTION 3.2: Multivariate mixed effects logistic regression, looking at whether age and gender affect odds of wearing homemade mask (if wearing a mask at all)
#store as random effect
merged_dane_mks <- merged_dane %>% filter(mask == 1)
mixed_effects_homemade <- glmer(homeade ~ gender + age + (1|id), family = binomial, data= merged_dane_mks)
se <- sqrt(diag(vcov(mixed_effects_homemade)))
(tab_homemade <- cbind(Est = fixef(mixed_effects_homemade), LL = fixef(mixed_effects_homemade) - 1.96 * se, UL = fixef(mixed_effects_homemade) + 1.96 *
                se))
exp(tab_homemade)
summary(mixed_effects_homemade)

#Univariate mixed effects logistic regression for gender
#store as random effect
mixed_effects_homemade_g <- glmer(homeade ~ gender + (1|id), family = binomial, data= merged_dane_mks)
se <- sqrt(diag(vcov(mixed_effects_homemade_g)))
(tab_homemade <- cbind(Est = fixef(mixed_effects_homemade_g), LL = fixef(mixed_effects_homemade_g) - 1.96 * se, UL = fixef(mixed_effects_homemade_g) + 1.96 *
                         se))
exp(tab_homemade)
summary(mixed_effects_homemade_g)

##Univariate mixed effects logistic regression for age
#store as random effect
mixed_effects_homemade_a <- glmer(homeade ~ age + (1|id), family = binomial, data= merged_dane_mks)
se <- sqrt(diag(vcov(mixed_effects_homemade_a)))
(tab_homemade <- cbind(Est = fixef(mixed_effects_homemade_a), LL = fixef(mixed_effects_homemade_a) - 1.96 * se, UL = fixef(mixed_effects_homemade_a) + 1.96 *
                         se))
exp(tab_homemade)
summary(mixed_effects_homemade_a)

###########################################################
###SECTION 4: Calculating IQR for counties outside of Dane 
all = merged %>% 
  filter(county!="dane", gender==0) %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)

all = merged %>% 
  filter(county!="dane", gender==1) %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)

all = merged %>% 
  filter(county!="dane", age=="c") %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)

all = merged %>% 
  filter(county!="dane", age=="y") %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)

all = merged %>% 
  filter(county!="dane", age=="a") %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)

all = merged %>% 
  filter(county!="dane", age=="e") %>% 
  group_by(id) %>% 
  summarize(mask_we_perc = sum(mask)/n())
summary(all$mask_we_perc)

all = merged %>% 
  filter(county=="dane", mask == 1) %>% 
  summarize(mask_we_perc = sum(homeade)/n())
summary(all$mask_we_perc)

