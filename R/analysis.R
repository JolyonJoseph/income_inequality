rm(list = ls()) # clear the workspace
library(tidyverse) # packages
library(lme4)
library(lmerTest)
library(ggeffects)
data <- read_csv("./data/Inequality in Income.csv")
glimpse(data) # take a quick look at the data
# make names better, i.e. all lower case separated by underscore, no brackets
fix_names <- function(x){
  x %>% tolower() %>%
    gsub(" ","_", .) %>% 
    gsub("\\(","", .) %>%
    gsub("\\)","", .)
}
data <- tibble(data, .name_repair = fix_names) # apply function
unique(data$human_development_groups) # look at distinct levels 
unique(data$undp_developing_regions) # look at distinct levels 
filter(data, is.na(human_development_groups)) # investigate the NAs 
filter(data, is.na(hdi_rank_2021)) # investigate the NAs
filter(data, is.na(undp_developing_regions)) # investigate the NAs
# confirm that Human Development Group is allocated based on HDI rank. Yes:
data %>% select(hdi_rank_2021, human_development_groups) %>% arrange(hdi_rank_2021) %>% print(n = nrow(data))
# Data management
## Create a developing nations membership flag
data <- data %>%
  mutate(
    dev_region_flag = if_else(!is.na(undp_developing_regions), "Yes", "No")
  ) %>%
  relocate(dev_region_flag, .after = undp_developing_regions)

# Make characters factors
data <- data %>%
  mutate_if(is.character, as.factor)
  
## Get a longer data frame to compare across years
data_long <- data %>%
  # first, rename columns to year
  rename_with(., .fn = ~gsub("inequality_in_income_","", .x), cols = starts_with("inequality")) %>%
  pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "inequality_in_income") %>%
  mutate(
    year = as.numeric(year)
  )


# visualise

# Descriptives
## Number in each development group
hdg_count <- data %>%
  group_by(human_development_groups) %>%
  summarise(
    Count = n()
  )

# Global inequality
inequality_time <- data_long %>%
  group_by(year) %>%
  summarise(
    mean_inequality = mean(inequality_in_income, na.rm = T)
  )

ggplot(inequality_time, aes(year, mean_inequality)) + 
  geom_point() +
  geom_line() +
  ggtitle("Global inequality over time") +
  ylab("Mean Gini coefficient") + xlab("Year") +
  scale_y_continuous(breaks = seq(0,25,5)) + coord_cartesian(ylim = c(0,25)) +
  scale_x_continuous(breaks = seq(min(inequality_time$year),max(inequality_time$year),1)) +
  theme_bw()

# HDG over time
hdg_inequality <- data_long %>%
  group_by(human_development_groups, year) %>%
  summarise(
    mean_inequality = mean(inequality_in_income, na.rm = T)
  )

ggplot(hdg_inequality, aes(year, mean_inequality, colour = human_development_groups, group = human_development_groups)) + 
  geom_point() +
  geom_line()

undp_inequality <- data_long %>%
  group_by(dev_region_flag, year) %>%
  summarise(
    mean_inequality = mean(inequality_in_income, na.rm = T)
  )

ggplot(undp_inequality, aes(year, mean_inequality, colour = dev_region_flag, group = dev_region_flag)) + 
  geom_point() +
  geom_line()

undp_inequality_groups <- data_long %>%
  filter(!is.na(undp_developing_regions)) %>%
  group_by(undp_developing_regions, year) %>%
  summarise(
    mean_inequality = mean(inequality_in_income, na.rm = T)
  )

ggplot(undp_inequality_groups, aes(year, mean_inequality, colour = undp_developing_regions, group = undp_developing_regions)) + 
  geom_point() +
  geom_line()

undp_2021 <- data_long %>%
  filter(!is.na(undp_developing_regions) & year == 2021) %>%

  group_by(undp_developing_regions) %>%
  mutate(
    avg_ineq = mean(inequality_in_income, na.rm = T),
    med_ineq = median(inequality_in_income, na.rm = T)
  ) %>%
  ungroup() %>%
  arrange(desc(med_ineq), undp_developing_regions, desc(inequality_in_income)) %>%
  mutate(
    country = factor(country, levels = country),
    undp_developing_regions = factor(undp_developing_regions, levels = unique(undp_developing_regions))
  ) %>%
  drop_na()


ggplot(undp_2021, aes(country, inequality_in_income, fill = undp_developing_regions)) +
  geom_col(colour = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))

# visualise
# ggplot(data_long, aes(Year, `Inequality in income`, colour = Country, group = `Human Development Groups`)) +
#   geom_point() +
#   geom_line()

# Inferentials

mod_1 <- lmerTest::lmer(inequality_in_income ~ year + (1|country), data_long)
summary(mod_1)

mod_2 <- update(mod_1, ~.+ dev_region_flag)
summary(mod_2)

mod_3 <-  update(mod_2, ~.+ year:dev_region_flag)
summary(mod_3)

anova(mod_1, mod_2, mod_3)


data_2017 <- data_long %>% filter(year <= 2017)

mod_1 <- lmerTest::lmer(inequality_in_income ~ year + (1|country), data_2017)
summary(mod_1)

mod_2 <- update(mod_1, ~.+ dev_region_flag)
summary(mod_2)

mod_3 <-  update(mod_2, ~.+ year:dev_region_flag)
summary(mod_3)

data_long_hd <- data_long %>%
  filter(!is.na(human_development_groups)) %>%
  mutate(
    human_development_groups = factor(human_development_groups, levels = c("Low","Medium","High","Very High"))
  )

mod_1 <- lmerTest::lmer(inequality_in_income ~ year*undp_developing_regions + (1|country), data_long)
summary(mod_1)

mod_1 <- lmerTest::lmer(inequality_in_income ~ year*human_development_groups + (1|country), data_long_hd)
summary(mod_1)


ems <- ggemmeans(mod_1, terms = c("year","human_development_groups"))

ggplot(ems, aes(x, predicted, colour = group)) +
  geom_point() +
  geom_line(aes(group = group)) +
  # geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) + 
  theme_bw()
