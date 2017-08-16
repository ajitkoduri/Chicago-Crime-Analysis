library(tidyverse)
library(ggmap)
library(zipcode)

crime1217 <- read_csv("~/RProjects/crime/Chicago_Crimes_2012_to_2017.csv")

crime1217 <- crime1217 %>%
  mutate(BlockName = substring(Block, 8)) %>%

blocks <- crime1217 %>% 
  group_by(BlockName) %>% 
  summarise(
    count = n()
  ) %>% 
  arrange(desc(count)) %>%
  mutate(BlockName = factor(BlockName, levels = BlockName))


top50 <- ggplot(head(blocks,50)) +
  geom_col(aes(x=BlockName, y = count)) +
  coord_flip()

ggplot(crime1217) +
  geom_histogram(aes(x=BlockName), stat = "count") +
  coord_flip()

map<-get_map(location='chicago', zoom=13, maptype = "terrain",
             source='google',color='color')


ggmap(map) + 
geom_jitter(data=crime1217, mapping=aes(x=Longitude, y=Latitude, colour=`Primary Type`), alpha = 0.3)
