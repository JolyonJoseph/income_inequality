rm(list = ls()) # clear the workspace
# Packages
packages <- c('tidyverse','lme4','lmerTest')
pkg_notinstall <- packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

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
# 1. Data management
## Create a developing nations membership flag
data <- data %>%
  mutate(
    dev_region_flag = if_else(!is.na(undp_developing_regions), "Yes", "No")
  ) %>%
  relocate(dev_region_flag, .after = undp_developing_regions)

# Make characters factors
data <- data %>%
  mutate_if(is.character, as.factor) %>%
  # relevel HDG
  mutate(
    human_development_groups = factor(human_development_groups, levels = c("Low","Medium","High","Very High", "NA")),
  )

## Get a longer data frame to compare across years
data_long <- data %>%
  # first, rename columns to year
  rename_with(., .fn = ~gsub("inequality_in_income_","", .x), cols = starts_with("inequality")) %>%
  pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "inequality_in_income") %>%
  mutate(
    year = as.numeric(year)
  )

### End 1 ###

# 2. Descriptives and visuals

## Descriptives
# Global inequality over time
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
  scale_y_continuous(breaks = seq(0,25,1)) + coord_cartesian(ylim = c(20,25)) +
  scale_x_continuous(breaks = seq(min(inequality_time$year),max(inequality_time$year),1)) +
  theme_bw()

ggsave(filename = "./outputs/global_inequality.png")

# UNDP dev flag
undp_inequality <- data_long %>%
  group_by(dev_region_flag, year) %>%
  summarise(
    mean_inequality = mean(inequality_in_income, na.rm = T)
  )

ggplot(undp_inequality, aes(year, mean_inequality, colour = dev_region_flag, group = dev_region_flag)) + 
  geom_point() +
  geom_line() +
  ylab("Mean Gini coefficient") + xlab("Year") +
  ggtitle("Global inequality over time by development status") +
  scale_colour_discrete(name = "UNDP Developing Region") + theme_bw() +
  scale_x_continuous(breaks = seq(min(inequality_time$year),max(inequality_time$year),1))

ggsave(filename = "./outputs/global_inequality_by_dev_status.png")

# Compare developing countries just for 2021
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
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7)) +
  ggtitle("Income inequality by country and UNDP developing region membership (2021)") +
  ylab("Gini coefficient") + xlab("Country") +
  scale_fill_discrete(name = "UNDP Developing Region") +
  scale_y_continuous(breaks = seq(0,60,10))

ggsave(filename = "./outputs/inequality_by_country_dev_region_2021.png")

# Inequality by UNDP region
undp_inequality_groups <- data_long %>%
  filter(!is.na(undp_developing_regions)) %>%
  group_by(undp_developing_regions, year) %>%
  summarise(
    mean_inequality = mean(inequality_in_income, na.rm = T)
  ) %>%
  mutate(
    undp_developing_regions = factor(undp_developing_regions, levels = c("LAC","SSA","AS","EAP","SA","ECA"))
  )

ggplot(undp_inequality_groups, aes(year, mean_inequality, colour = undp_developing_regions, group = undp_developing_regions)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Mean Gini coefficient") + xlab("Year") +
  scale_colour_discrete(name = "UNDP Developing Region") +
  scale_x_continuous(breaks = seq(min(inequality_time$year),max(inequality_time$year),1)) +
  ggtitle("Mean income inequality by UNDP developing region over time")

ggsave(filename = "./outputs/inequality_by_dev_region_time.png")

# HDG by development
hdg_by_dev <- data %>%
  group_by(undp_developing_regions, human_development_groups) %>%
  summarise(
    frequency = n()
  ) %>%
  mutate(
    proportion = 100 * (frequency / sum(frequency)),
    undp_developing_regions = factor(undp_developing_regions, levels = c("LAC","SSA","AS","EAP","SA","ECA")),
    undp_developing_regions = forcats::fct_na_value_to_level(undp_developing_regions, "Non-developing")
  )

ggplot(hdg_by_dev, aes(undp_developing_regions, proportion, fill = human_development_groups)) +
  geom_col(position = "dodge", colour = "black") +
  theme_bw() +
  ylab("Proportion of countries") + xlab("UNDP developing region") +
  ggtitle("Human Development Group composition of each UNDP developing region") +
  scale_fill_discrete(name = "Human Development Group") +
  scale_y_continuous(breaks = seq(0,100,10))
  
ggsave(filename = "./outputs/dev_region_HDG_composition.png")

### End 2 ###

# 3. Models
## First set of models explores inequalit over time and differences between 
## developing and non-developing

mod_1 <- lmerTest::lmer(inequality_in_income ~ year + (1|country), data_long)
summary(mod_1)

mod_2 <- update(mod_1, ~.+ dev_region_flag)
summary(mod_2)

mod_3 <-  update(mod_2, ~.+ year:dev_region_flag)
summary(mod_3)

anova(mod_1, mod_2, mod_3) # best fit is mod_2

## Second set of models explores how development status relates to inqequality
mod_1 <- lmerTest::lmer(inequality_in_income ~ year*human_development_groups + (1|country), data_long)
summary(mod_1)

# get marginal means and plot them
ems <- ggemmeans(mod_1, terms = c("year","human_development_groups"))


ggplot(ems, aes(x, predicted, colour = group)) +
  geom_point() +
  geom_line(aes(group = group)) +
  # geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) + 
  theme_bw() +
  ylab("Predicted Gini coefficient") + xlab("Year") +
  ggtitle("Estimated marginal means of the effect of time on inequality in each Human Development Group") +
  scale_colour_discrete(name = "Human Development Group") 

ggsave(filename = "./outputs/model_ems_hdg_time.png")

### End 3. ###
