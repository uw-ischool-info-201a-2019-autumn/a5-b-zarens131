# Zachary Arenson
#INFO 201 A5

#install packages and load from library

library(dplyr)
library(ggplot2)
library(plotly)
library(lintr)

library("knitr")
#Loading & Reading the df from data
mass_shooting_df <- read.csv("./data/shootings-2018.csv", stringsAsFactors = FALSE)

# Part 1: Summary Info Code

# How many shooting events occurred?
shooting_num <- nrow(mass_shooting_df)
print(shooting_num)

# How many lives were lost?
total_lives_lost <- mass_shooting_df %>%
  select(num_killed) %>%
  summarise(lives_lost = sum(num_killed)) 

# Which city was most impacted by shootings (make sure to clarify  how you are measuring "impact")?
most_impacted_city <- mass_shooting_df %>%
  mutate(total_impacted = (num_killed + num_injured)) %>%
  group_by(city) %>%
  summarise(impacted = sum(total_impacted)) %>%
  filter(impacted == max(impacted))
print(most_impacted_city)

# How many injured?
total_injured <- mass_shooting_df %>%
  select(num_injured) %>%
  summarise(total_hurt = sum(num_injured)) 

# Which state was the most impacted by mass shootings?
most_impacted_state <- mass_shooting_df %>%
  mutate(total_impacted = (num_killed + num_injured)) %>%
  group_by(state) %>%
  summarise(impacted = sum(total_impacted)) %>%
  filter(impacted == max(impacted)) %>% 
  select(state)

print(most_impacted_state)

# Summary Table
summary_table <- mass_shooting_df %>%
  group_by(state) %>%
  summarize(sum = sum(num_killed)) %>% 
  arrange(-sum)

# Interactive map
#set map and color for data to display on.
geo <- list(scope = "usa", showland = TRUE, landcolor = toRGB("gray95"), countrycolor = toRGB("gray80"))

#set data onto map (num of deaths/injured)
interactive_map <- plot_geo(mass_shooting_df, lat = ~lat, lon = ~long) %>%
  layout(
    title = "Areas Impacted from U.S. Shootings <br />", geo = geo) %>%
  add_markers(
    text = ~paste("State: ", state, "<br />", "City: ", city,  "<br />", "Death Total: ", num_killed, "<br />", "Injured Total: ", num_injured),
  colors = c(
    "blue", "darkblue", "blue", "blue2", "blue4"), color = ~num_injured + num_killed, symbol = I("circle"), hoverinfo = "text", marker = list(size = ~num_injured + num_killed)) %>%
  colorbar(
    title = "Number of Victims<br /> (Mouse over = info)") 
  
#plot 
p <- ggplot(data = mass_shooting_df, aes(state, num_killed)) + geom_bar(stat="identity") + scale_x_discrete(labels = abbreviate) +
  theme(axis.text.x = element_text(size  = 10, angle = 45, hjust = 1, vjust = 1))
