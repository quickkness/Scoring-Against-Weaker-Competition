##### Scoring Against Inferior Competition #####
##### Started: 8/24/17 Last Edited: 8/24/17 #####

### Load Required Packages ###
require(dplyr);

### Load & Mutate TOI Data, Source: Hockey Abstract ###
TOI1617 <- read.csv(file = "C:/Users/Steve/Desktop/Hockey Analytics/Blog Research/20162017 5V5 TOI.csv")

TOI1617 <- TOI1617 %>%
  mutate(First.Last = paste(First.Name,".",Last.Name, sep = "")) %>%
  select(7,3:6)

TOI1617$First.Last <- toupper(TOI1617$First.Last)

### Load 2016-2017 PBP Data % Filter by Goals, Source: Corsica Hockey ###
pbp1617 <- readRDS(file = "./Corsica PBP Data/pbp20162017.rds")

pbp1617 <-
pbp1617 %>%
  filter(Event == "GOAL",
         Strength.State == "5v5")

## Develop Rankings by Position ##

Rankings <- data.frame(Position = c(rep("F",30),rep("D",20),rep("G",10)),
                       Rank = c(1:30,1:20,1:10),
                       Line = c(rep(1,3),rep(2,3),rep(3,3,),rep(4,3),rep("Depth",18),
                                rep(1,2),rep(2,2),rep(3,2),rep("Depth",14),
                                rep(1,1),rep(2,1),rep("Depth",8)))

## Seperate & Rank TOI by Position ##

rbind(inner_join(x = TOI1617 %>%
                           filter(Position == "F") %>%
                           group_by(Team) %>%
                           arrange(Team,desc(TOI)) %>%
                           mutate(Rank = row_number()),
                         y = Rankings %>%
                           filter(Position == "F")),
      inner_join(x = TOI1617 %>%
                   filter(Position == "D") %>%
                   group_by(Team) %>%
                   arrange(Team,desc(TOI)) %>%
                   mutate(Rank = row_number()),
                 y = Rankings %>%
                   filter(Position == "D")),
      inner_join(x = TOI1617 %>%
                   filter(Position == "G") %>%
                   group_by(Team) %>%
                   arrange(Team,desc(TOI)) %>%
                   mutate(Rank = row_number()),
                 y = Rankings %>%
                   filter(Position == "G"))
) %>% data.frame() %>%
  mutate(Team.First.Last = paste(Team,".",First.Last,sep = "")) %>%
  select(8,1:7) %>%
  data.frame -> Rankings.Summary
  



