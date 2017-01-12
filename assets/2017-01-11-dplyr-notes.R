#loading in packages
library(dplyr)
library(ggplot2)

surveys <- read.csv("Data/CleanData/portal_data_joined.csv")

selectedcol <- select(surveys, 
                      species_id, plot_type, 
                      weight)
head(selectedcol)

surveys2002 <- filter(surveys, year==2002)
head(surveys2002)

surveys_sml <- surveys %>% filter(weight < 5) %>% 
  select(species_id, sex, weight)

surveys %>% 
  mutate(weight_kg = weight / 1000) %>% 
  head()

surveys %>% filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000) %>% 
  head

surveys %>% filter(year <1995) %>% 
  select(year, sex, weight) #challenge sol

challenge6 <- surveys %>% 
  mutate(hindfoot_sqrt = sqrt(hindfoot_length)) %>% 
  filter(!is.na(hindfoot_sqrt), hindfoot_sqrt < 3) %>% 
  select(species_id, hindfoot_sqrt)

surveys %>% group_by(sex) %>% tally()

surveys %>% group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm=TRUE))

surveys %>% group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>% 
  filter(!is.na(mean_weight))

surveys %>% group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>% 
  filter(!is.na(mean_weight)) %>% 
  arrange(mean_weight)

surveys %>% group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>% 
  filter(!is.na(mean_weight)) %>% 
  arrange(desc(mean_weight))

surveys %>% group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE),
            min_weight = min(weight, na.rm=TRUE)) %>% 
  filter(!is.na(mean_weight)) %>% 
  arrange(desc(mean_weight))

surveys %>% group_by(plot_id) %>% tally()

surveys %>% group_by(plot_type) %>%  tally() # challenge sol

surveys %>% group_by(species) %>% 
  summarize(mean_hindfoot=mean(hindfoot_length, na.rm=TRUE),
            min_hindfoot =min(hindfoot_length, na.rm=TRUE),
            max_hindfoot =max(hindfoot_length, na.rm=TRUE)) #challenge 8 sol

surveys %>% group_by(species) %>% 
  summarize(mean_hindfoot=mean(hindfoot_length, na.rm=TRUE),
            min_hindfoot =min(hindfoot_length, na.rm=TRUE),
            max_hindfoot =max(hindfoot_length, na.rm=TRUE)) %>% 
  arrange(desc(mean_hindfoot))

surveys_complete <- surveys %>%
  filter(species_id != "", sex != "") %>% 
  filter(!is.na(weight), !is.na(hindfoot_length))
dim(surveys)
dim(surveys_complete)

species_counts <- surveys %>% group_by(species_id) %>% 
  tally()
species_counts

frequent_speices <- species_counts %>% 
  filter(n >=10) %>% 
  select(species_id)
head(frequent_speices)

reduced <- surveys_complete %>% 
  filter(species_id %in% frequent_speices$species_id)
dim(surveys_complete)

write.csv(reduced, "Data/CleanData/portal_data_reduced.csv")


