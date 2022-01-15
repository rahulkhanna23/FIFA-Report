
title: "Project Fifa"
author: "Rahul Gopikannan"
date: "10/18/2021"
  

# bar plot for market value
# loading the required libaries
library(scales)
library(ggplot2)

# to split the commas
options(scipen = 100, big.mark = ",")

# reading in the data csv file to a fifa_21 variable
fifa_21 <- read.csv("players_21.csv")

# renaming columns for better readability
fifa_21 <- fifa_21 %<>%
  rename( 
    "physique" = physic, 
    "marketValue" = value_eur,
    "weeklyWage" = wage_eur,
    "crossing" = attacking_crossing,
    "finishing" = attacking_finishing,
    "heading" = attacking_heading_accuracy,
    "penalties" = mentality_penalties,
    "freekicks" = skill_fk_accuracy,
    "shortPassing" = attacking_short_passing,
    "shotPower" = power_shot_power,
    "speed" = movement_sprint_speed
  )





# deleting unnecessary columns that are of no use to the analysis
fifa_21 <- fifa_21 %<>%
  select(-sofifa_id, -body_type, -real_face, -player_tags, -ls, -st, -rs, -lw, -lf, -cf, -rf, 
         -rw, -lam, -cam, -ram, -lm, -lcm, -cm,
         -rcm, -rm, -lwb, -ldm, -cdm, -rdm, -rwb, -lb, -lcb, -cb, -rcb, -rb)




# getting the indices of the rows that fall under each league category
spanish <- which(fifa_21$league_name == "Spain Primera Division")
english <- which(fifa_21$league_name == "English Premier League")
italian <- which(fifa_21$league_name == "Italian Serie A")
german <- which(fifa_21$league_name == "German 1. Bundesliga")
french <- which(fifa_21$league_name == "French Ligue 1")

# using sum function to add the market value of each league
sum_spanish <- sum(fifa_21$marketValue[spanish])
sum_english <- sum(fifa_21$marketValue[english])
sum_italian <- sum(fifa_21$marketValue[italian])
sum_german <- sum(fifa_21$marketValue[german])
sum_french <- sum(fifa_21$marketValue[french])

# creating a new column with the sum values
scientific_notation <- c(sum_spanish, sum_english, sum_italian, sum_german, sum_french)


# using data.frame function to create a new dataset with league and sum values
bargraph_marketvalue <- data.frame(name=c("Spanish League", "English League", "Italian League", 
                                          "German League", "French League"),
                                   value = c(sum_spanish, sum_english, 
                                             sum_italian, sum_german, sum_french))

# using ggplot to plot a bargraph
legend <- ggplot(bargraph_marketvalue) + 
  geom_bar(mapping = aes(x = reorder(name, -value), y=value, fill = name), stat = "identity") +
  ggtitle("Bar Graph of Market Value of different leagues") +
  labs(x = "Leagues", y = "Total Value (Euros)")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)  +
  theme_bw()

legend + labs(fill = "Leagues")



# calculating per-player value since the number of players are not the same in every league
spanish_per_player <- sum_spanish / length(spanish)
english_per_player <- sum_english / length(english)
italian_per_player <- sum_italian / length(italian)
german_per_player <- sum_german / length(german)
french_per_player <- sum_french / length(french)

# creating a new dataset for the new values with leagues
bargraph_per_player <- data.frame(name_per_player = 
                                    c("Spanish", "English", "Italian", "German", "French"),
                                  value_per_player = 
                                    c(spanish_per_player, english_per_player, 
                                      italian_per_player, german_per_player, french_per_player))


# plotting a new barplot for the average value graph
legend <- ggplot(bargraph_per_player) + 
  geom_bar(mapping = aes(x = reorder(name_per_player, -value_per_player), 
                         y=value_per_player, fill = name_per_player), stat = "identity") +
  ggtitle("Bar Graph of Per Player Value in each league") +
  labs(x = "Leagues", y = "Player Value (Euros)")  +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)  +
  theme_bw()

legend + labs(fill = "Leagues")



# NEW PLOT
# GOAT plot
# loading thr required libraries
library(magrittr)
library(magrittr)
library(dplyr)
library(tidyr)

fifa_21 <- fifa_21 %>%
  rename (
  )


# using filer() to separate the Ronaldo-Messi rows
# then selecting the required attributes to compare
# sort them by Skill and Ratings and make a new dataset
data_goats <- fifa_21 %>%
  filter(shortName %in% c("Cristiano Ronaldo", "L. Messi")) %>%
  select(shortName, dribbling, shooting, passing, defending, 
         physique, heading, freeKicks, speed, penalties) %>% 
  gather(Skill, Ratings, dribbling, shooting, passing, 
         defending, physique, heading, freeKicks, speed, penalties, -shortName)
head(data_goats)



# using position dodge and ggplot, plotting the comparsion graph
legend <- ggplot(data = data_goats) + 
  geom_bar(mapping = aes(y = Ratings, x= Skill, fill = shortName), 
           stat = "identity", position = "dodge") +
  ggtitle("Comparison between two greatest legends") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

legend + labs(fill = "Player Names")





# NEW PLOT
# Player evolution growth plot

library(magrittr)
library(magrittr)
library(dplyr)

library(tidyr)

# reading in data
fifa_17 <- read.csv("players_17.csv")




# renaming columns
fifa_17 <- fifa_17 %<>%
  rename(
    "physique" = physic, 
    "marketValue" = value_eur,
    "weeklyWage" = wage_eur,
    "crossing" = attacking_crossing,
    "finishing" = attacking_finishing,
    "heading" = attacking_heading_accuracy,
    "penalties" = mentality_penalties,
    "freeKicks" = skill_fk_accuracy,
    "shortPassing" = attacking_short_passing,
    "shotPower" = power_shot_power,
    "speed" = movement_sprint_speed,
    "shortName" = short_name
  )

# renaming mbappe's name to add year at the end for better readability
fifa_17$shortName[fifa_17$shortName == "K. Mbappe Lottin"] <- "K. Mbappé_17"

# filtering required rows
mbappe_17 <-
  filter(fifa_17, shortName == "K. Mbappé_17") %>%
  select(shortName, overall, potential, dribbling, shooting, 
         passing, defending, physique, heading, pace)



# reading and doing similar process for other fifa datasets as well
fifa_18 <- read.csv("players_18.csv")

fifa_18 <- fifa_18 %<>%
  rename(
    "physique" = physic, 
    "marketValue" = value_eur,
    "weeklyWage" = wage_eur,
    "crossing" = attacking_crossing,
    "finishing" = attacking_finishing,
    "heading" = attacking_heading_accuracy,
    "penalties" = mentality_penalties,
    "freeKicks" = skill_fk_accuracy,
    "shortPassing" = attacking_short_passing,
    "shotPower" = power_shot_power,
    "speed" = movement_sprint_speed,
    "shortName" = short_name
  )

fifa_18$shortName[fifa_18$shortName == "K. Mbappé"] <- "K. Mbappé_18"

fifa_18 <- fifa_18 %<>%
  rename (
    
  )


mbappe_18 <-
  filter(fifa_18, shortName == "K. Mbappé_18") %>%
  select(shortName, overall, potential, dribbling, shooting, passing, defending, physique, heading, pace)



fifa_19 <- read.csv("players_19.csv")


fifa_19 <- fifa_19 %<>%
  rename (
    "physique" = physic, 
    "marketValue" = value_eur,
    "weeklyWage" = wage_eur,
    "crossing" = attacking_crossing,
    "finishing" = attacking_finishing,
    "heading" = attacking_heading_accuracy,
    "penalties" = mentality_penalties,
    "freeKicks" = skill_fk_accuracy,
    "shortPassing" = attacking_short_passing,
    "shotPower" = power_shot_power,
    "speed" = movement_sprint_speed,
    "shortName" = short_name
  )

fifa_19$shortName[fifa_19$shortName == "K. Mbappé"] <- "K. Mbappé_19"

mbappe_19 <-
  filter(fifa_19, shortName == "K. Mbappé_19") %>%
  select(shortName, overall, potential, dribbling, shooting, passing, defending, physique, heading, pace)



fifa_20 <- read.csv("players_20.csv")


fifa_20 <- fifa_20 %<>%
  rename (
    "physique" = physic, 
    "marketValue" = value_eur,
    "weeklyWage" = wage_eur,
    "crossing" = attacking_crossing,
    "finishing" = attacking_finishing,
    "heading" = attacking_heading_accuracy,
    "penalties" = mentality_penalties,
    "freeKicks" = skill_fk_accuracy,
    "shortPassing" = attacking_short_passing,
    "shotPower" = power_shot_power,
    "speed" = movement_sprint_speed,
    "shortName" = short_name
  )

fifa_20$shortName[fifa_20$shortName == "K. Mbappé"] <- "K. Mbappé_20"

mbappe_20 <-
  filter(fifa_20, shortName == "K. Mbappé_20") %>%
  select(shortName, overall, potential, dribbling, shooting, 
         passing, defending, physique, heading, pace)


fifa_21$shortName[fifa_21$shortName == "K. Mbappé"] <- "K. Mbappé_21"


mbappe_21 <-
  filter(fifa_21, shortName == "K. Mbappé_21") %>%
  select(shortName, overall, potential, dribbling, shooting, passing, 
         defending, physique, heading, pace)



# creating a year column
year <- c("2017", "2018","2019", "2020", "2021")

# binding every row and column
mbappe_career <- rbind(mbappe_17, mbappe_18, mbappe_19, mbappe_20, mbappe_21)
mbappe_career <- cbind(mbappe_career, year)


# using skill as a parameter
skill <- c("dribbling", "overall", "potential", "shooting", 
           "passing", "defending", "physique", "heading", "freeKicks", "pace",
           "penalties")


library(tidyr)

# pivot_longer used as alterntive to gather() as the data is lengthy
pivot_longer(
  mbappe_career, !c(shortName, year),
  names_to = 'skill', values_to = 'rating'
) %>% 
  #plotting here geom_point and geom_line
  ggplot(aes(x = year, y = rating)) %+%
  geom_point() %+%
  geom_line(group=1) %+%
  facet_wrap(~skill) %+%
  ggtitle("Mbappé's evolution from 2017 to 2021") +
  labs(x = "Years", y = "Ratings") +
  theme_bw()








# NEW PLOT
# World map player plot
# loading the required libraries to get world map
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(mapproj)

#installing viridis to get a color scheme for maps
library(viridis)

# duplicate data
fifa_21_mapData <- fifa_21

# renaming country names here since maps doesn't recognize them
fifa_21_mapData$nationality[fifa_21_mapData$nationality == "United States"] = "USA"
fifa_21_mapData$nationality[fifa_21_mapData$nationality == "England"] = "UK"
fifa_21_mapData$nationality[fifa_21_mapData$nationality == "China PR"] = "China"
fifa_21_mapData$nationality[fifa_21_mapData$nationality == "Korea Republic"] = "South Korea"

# creating a variable to load world map and pick them up by nationality
map_world <- map_data("world")
names(map_world)[names(map_world) == 'region'] = 'nationality'

# loading into new variable and attaching the fifa_21 dataset by counting nationality
nationality_map <- map_world %>%
  left_join((fifa_21_mapData %>%
               dplyr::count(nationality)), by = "nationality")

# plotting using ggplot and geom_ploygon
map <- ggplot(nationality_map) +
  geom_polygon(aes(long,lat, group = group, fill = n), color = "black", 
               show.legend = TRUE) +
  ggtitle("Number of players in each nationality - World Map") +
  labs(fill = "Number of players")


map + scale_fill_viridis(option = "turbo") +
  theme_void()




# New plot
# Foot-wage graph
ggplot(data = fifa_21) + geom_point(mapping = aes(x = preferred_foot, 
                             y = weeklyWage), color = "red") + 
  labs(x = "Preferred Foot", y = "Wage in U.S Dollars", 
                            title = "Relationship between Preferred Foot and Wage")
+ theme_bw()

