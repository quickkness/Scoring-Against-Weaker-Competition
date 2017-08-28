##### Scoring Against Inferior Competition #####
##### Started: 8/24/17 Last Edited: 8/27/17 #####

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
                       Line = c(rep(1,3),rep(2,3),rep(3,3,),rep(4,3),rep(5,18), # 5 = Depth
                                rep(1,2),rep(2,2),rep(3,2),rep(4,14), # 4 = Depth
                                rep(1,1),rep(2,1),rep(3,8))) # 3 = Depth

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
  
## Filter PBP Data & Combine Team & Player to match Rankings.Summary ##

pbp <- pbp1617 %>%
  select(54,6,5,1,2,71,3,7:10,38,14,16,18,20,22,24,40,39,26,28,30,32,34,36,41) %>%
  mutate(Away.P1 = paste(Away.Team,".",a1.num,sep = ""),
         Away.P2 = paste(Away.Team,".",a2.num,sep = ""),
         Away.P3 = paste(Away.Team,".",a3.num,sep = ""),
         Away.P4 = paste(Away.Team,".",a4.num,sep = ""),
         Away.P5 = paste(Away.Team,".",a5.num,sep = ""),
         Away.P6 = paste(Away.Team,".",a6.num,sep = ""),
         Away.Goalie = paste(Away.Team,".",Away.Goalie,sep = ""),
         Home.P1 = paste(Home.Team,".",h1.num,sep = ""),
         Home.P2 = paste(Home.Team,".",h2.num,sep = ""),
         Home.P3 = paste(Home.Team,".",h3.num,sep = ""),
         Home.P4 = paste(Home.Team,".",h4.num,sep = ""),
         Home.P5 = paste(Home.Team,".",h5.num,sep = ""),
         Home.P6 = paste(Home.Team,".",h6.num,sep = ""),
         Home.Goalie = paste(Home.Team,".",Home.Goalie,sep = ""),
         Event.P1 = paste(ev.team,".",p1,sep = ""),
         Event.P2 = ifelse(is.na(p2),NA,paste(ev.team,".",p2,sep = "")),
         Event.P3 = ifelse(is.na(p3),NA,paste(ev.team,".",p3,sep = ""))) %>%
  select(1:8,40:42,28:33,19,20,34:39,27)

### Merges Line for each Position/Player ###

mergepbp <- #Adds line for Event P1
  merge(x = pbp, y = Rankings.Summary,
        by.x = c("Event.P1"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:9,1,33,10:26) %>%
  rename(Event.P1.Line = Line)

mergepbp <- #Adds line for Event P2
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Event.P2"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:11,1,34,12:27) %>%
  rename(Event.P2.Line = Line)

mergepbp <- #Adds line for Event P3
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Event.P3"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:13,1,35,14:28) %>%
  rename(Event.P3.Line = Line)

mergepbp <- #Adds line for Away P1
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Away.P1"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:15,1,36,16:29) %>%
  rename(Away.P1.Line = Line)

mergepbp <- #Adds line for Away P2
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Away.P2"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:17,1,37,18:30) %>%
  rename(Away.P2.Line = Line)

mergepbp <- #Adds line for Away P3
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Away.P3"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:19,1,38,20:31) %>%
  rename(Away.P3.Line = Line)

mergepbp <- #Adds line for Away P4
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Away.P4"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:21,1,39,22:32) %>%
  rename(Away.P4.Line = Line)

mergepbp <- #Adds line for Away P5
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Away.P5"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:23,1,40,24:33) %>%
  rename(Away.P5.Line = Line)

mergepbp <- #Adds line for Away P6
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Away.P6"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:25,1,41,26:34) %>%
  rename(Away.P6.Line = Line)

mergepbp <- #Adds line for Away Goalie
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Away.Goalie"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:27,1,42,28:35) %>%
  rename(Away.Goalie.Line = Line)

mergepbp <- #Adds line for Home P1
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Home.P1"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:30,1,43,31:36) %>%
  rename(Home.P1.Line = Line)

mergepbp <- #Adds line for Home P2
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Home.P2"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:32,1,44,33:37) %>%
  rename(Home.P2.Line = Line)

mergepbp <- #Adds line for Home P3
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Home.P3"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:34,1,45,35:38) %>%
  rename(Home.P3.Line = Line)

mergepbp <- #Adds line for Home P4
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Home.P4"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:36,1,46,37:39) %>%
  rename(Home.P4.Line = Line)

mergepbp <- #Adds line for Home P5
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Home.P5"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:38,1,47,38:40) %>%
  rename(Home.P5.Line = Line)

mergepbp <- #Adds line for Home P6
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Home.P6"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:40,1,48,40:41) %>%
  rename(Home.P6.Line = Line)

mergepbp <- #Adds line for Home Goalie
  merge(x = mergepbp, y = Rankings.Summary,
        by.x = c("Home.Goalie"), by.y = c("Team.First.Last"), all.x = TRUE) %>%
  select(2:42,1,49,42) %>%
  rename(Home.Goalie.Line = Line)

PBP.Summary <- mergepbp
remove(mergepbp,pbp,Rankings,Rankings.Summary)



