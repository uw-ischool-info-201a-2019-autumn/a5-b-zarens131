# Zachary Arenson
#INFO 201 A5

#install packages and load from library
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")

library(dplyr)
library(ggplot2)
library(plotly)

#Loading the df from data/

mass_shooting_df <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)
# Part 1: Summary Info Code

# How many shooting events occurred?
shooting_num <- nrow(mass_shooting_df)
print(shooting_num)

# How many lives were lost?
total_lives_lost <- mass_shooting_df %>%
  select(num_killed) %>%
  summarise(lives_lost = sum(num_killed))
View(total_lives_lost)

# Which city was most impacted by shootings (make sure to clarify  how you are measuring "impact")?
most_impacted_city <- mass_shooting_df %>%
  mutate(impacted = (num_killed + num_injured)) %>%
  group_by(city) %>%
  #summarise city
  #max # of impacted
  select(city, impacted)
  


print(most_impacted_city)
# Two other insights of your choice

# Summary Table
summary_table <- mass_shooting_df %>%
  group_by(state) %>%
  summarize(sum = sum(num_killed)) %>% 
  arrange(-sum)

# An interactive map

  
#plot 
p <- ggplot(data = mass_shooting_df, aes(state, num_killed)) +
  geom_bar(stat = "identity")
  
View(p)

