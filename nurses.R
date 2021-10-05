library(tidyverse)
library(gganimate)

#load data
tuesdata <- tidytuesdayR::tt_load('2021-10-05')

nurses <- tuesdata$nurses

nurses

# How many years?
nurses %>% 
  group_by(Year) %>% 
  summarise(n = n())

# 2000 data
nurses %>%
  filter(Year == 2000) %>% 
  ggplot(aes(x = `Total Employed (Healthcare, State)_Aggregate`, 
             y = `Annual Salary Avg`,
             color = State)) + 
  geom_point() +
  theme(legend.position = "none")
# look into regional?
# or look into governor party

# try to animate
nurses %>%
  left_join(tibble(State = state.name,
                   Region = as.character(state.region))) %>% 
  mutate(Region = replace_na(Region, "D.C. and U.S. Territories"),
         Region = fct_rev(factor(Region))) %>%
  ggplot(aes(x = `Total Employed (Healthcare, State)_Aggregate`, 
             y = `Annual Salary Avg`,
             color = Region)) + 
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Average Salary vs Number of Employed Nurses",
       subtitle = "by Year: {round(frame_time)}",
       y = "",
       x = "") +
  scale_y_continuous(labels = scales::label_number(prefix = "$", 
                                                   suffix = " K", 
                                                   scale = 1e-3)) +
  scale_x_continuous(labels = scales::label_number(suffix = " K", 
                                                   scale = 1e-3)) +
  transition_time(Year) +
  ease_aes()

anim_save("salary_vs_number_by_region.gif")
