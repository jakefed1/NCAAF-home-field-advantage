#WARNING: DO NOT ATTEMPT TO REPRODUCE THIS STUDY. I DID NOT LOAD IN THE RAW DATA FROM A CSV:
#I COPIED AND PASTED IT IN FROM SPREASDHEETS THAT ARE NOT ATTACHED.
#SO, IF YOU TRY TO RUN THIS CODE, WHATEVER IS COPIED ONTO YOUR CLIPBOARD WILL BE PASTED INTO THE DATASET,
#NOT THE ACTUAL RAW DATA. HOWEVER, I AM STILL POSTING THIS CODE FOR THE SAKE OF TRANSPARENCY:
#SO YOU GUYS CAN SEE WHAT I DID AND HOW I DID IT. I WELCOME ANY SUGGESTIONS ON HOW TO IMPROVE THE CODE,
#OR QUESTIONS ABOUT WHAT A CERTAIN LINE DOES. YOU CAN EMAIL ME AT JAKE_FEDERMAN@HORACEMANN.ORG

library(tidyverse)

#Load in the data... copying and pasting the data from excel

ACC2019 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
ACC2018 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
ACC2017 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
ACC2016 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
ACC2015 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
ACC2014 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
ACC2013 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big122019 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big122018 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big122017 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big122016 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big122015 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big122014 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big122013 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big102019 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big102018 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big102017 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big102016 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big102015 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big102014 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Big102013 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SEC2019 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SEC2018 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SEC2017 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SEC2016 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SEC2015 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SEC2014 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SEC2013 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Pac122019 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Pac122018 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Pac122017 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Pac122016 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Pac122015 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Pac122014 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
Pac122013 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SoCon2019 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SoCon2018 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SoCon2017 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SoCon2016 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SoCon2015 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SoCon2014 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)
SoCon2013 <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)

#Inner joining the data to get all the data for each conference in 1 dataframe

ACC<-
  ACC2019 %>%
  inner_join(ACC2018, by="Team")
ACC<-
  ACC %>%
  inner_join(ACC2017, by="Team")
ACC<-
  ACC %>%
  inner_join(ACC2016, by="Team")
ACC<-
  ACC %>%
  inner_join(ACC2015, by="Team")
ACC<-
  ACC %>%
  inner_join(ACC2014, by="Team")
#ACC<-
#ACC %>%
#full_join(ACC2013, by="Team")
#I didn't inner join ACC 2013 because there were different teams in the conference for this year so inner join wouldn't work...
#so I edited the data in Excel later

#Cleaning up the data

ACC <-
  ACC %>% 
  mutate(Home_wins = Home.wins.x + Home.wins.y + Home.wins.x.x + Home.wins.y.y + Home.wins.x.x.x + Home.wins.y.y.y) %>% 
  mutate(Away_wins = Away.wins.x + Away.wins.y + Away.wins.x.x + Away.wins.y.y + Away.wins.x.x.x + Away.wins.y.y.y) %>% 
  mutate(Home_losses = Home.losses.x + Home.losses.y + Home.losses.x.x + Home.losses.y.y + Home.losses.x.x.x + Home.losses.y.y.y) %>% 
  mutate(Away_losses = Away.losses.x + Away.losses.y + Away.losses.x.x + Away.losses.y.y + Away.losses.x.x.x + Away.losses.y.y.y) %>% 
  mutate(Total_wins = Total.wins.x + Total.wins.y + Total.wins.x.x + Total.wins.y.y + Total.wins.x.x.x + Total.wins.y.y.y) %>% 
  mutate(Total_losses = Total.losses.x + Total.losses.y + Total.losses.x.x + Total.losses.y.y + Total.losses.x.x.x + Total.losses.y.y.y)

ACC <-
  ACC %>% 
  select(Team, Home_wins, Home_losses, Away_wins, Away_losses, Total_wins, Total_losses)

#Creating WP

ACC <-
  ACC %>% 
  mutate(Home_WP = ((Home_wins)/((Home_wins)+(Home_losses)))) %>% 
  mutate(Away_WP = ((Away_wins)/((Away_wins)+(Away_losses))))  %>% 
  mutate(Total_WP = ((Total_wins)/((Total_wins)+(Total_losses))))
view(ACC)

#Doing the same thing for other conferences

Big12<-
  Big122019 %>%
  inner_join(Big122018, by="Team")
Big12<-
  Big12 %>%
  inner_join(Big122017, by="Team")
Big12<-
  Big12 %>%
  inner_join(Big122016, by="Team")
Big12<-
  Big12 %>%
  inner_join(Big122015, by="Team")
Big12<-
  Big12 %>%
  inner_join(Big122014, by="Team")
Big12<-
  Big12 %>%
  inner_join(Big122013, by="Team")

Big12 <-
  Big12 %>% 
  mutate(Home_wins = Home.wins.x + Home.wins.y + Home.wins.x.x + Home.wins.y.y + Home.wins.x.x.x + Home.wins.y.y.y + Home.wins) %>% 
  mutate(Away_wins = Away.wins.x + Away.wins.y + Away.wins.x.x + Away.wins.y.y + Away.wins.x.x.x + Away.wins.y.y.y + Away.wins) %>% 
  mutate(Home_losses = Home.losses.x + Home.losses.y + Home.losses.x.x + Home.losses.y.y + Home.losses.x.x.x + Home.losses.y.y.y + Home.losses) %>% 
  mutate(Away_losses = Away.losses.x + Away.losses.y + Away.losses.x.x + Away.losses.y.y + Away.losses.x.x.x + Away.losses.y.y.y + Away.losses) %>% 
  mutate(Total_wins = Total.wins.x + Total.wins.y + Total.wins.x.x + Total.wins.y.y + Total.wins.x.x.x + Total.wins.y.y.y + Total.wins) %>% 
  mutate(Total_losses = Total.losses.x + Total.losses.y + Total.losses.x.x + Total.losses.y.y + Total.losses.x.x.x + Total.losses.y.y.y + Total.losses)
Big12 <-
  Big12 %>% 
  select(Team, Home_wins, Home_losses, Away_wins, Away_losses, Total_wins, Total_losses)
Big12 <-
  Big12 %>% 
  mutate(Home_WP = ((Home_wins)/((Home_wins)+(Home_losses)))) %>% 
  mutate(Away_WP = ((Away_wins)/((Away_wins)+(Away_losses))))  %>% 
  mutate(Total_WP = ((Total_wins)/((Total_wins)+(Total_losses))))
view(Big12)

Big10<-
  Big102019 %>%
  inner_join(Big102018, by="Team")
Big10<-
  Big10 %>%
  inner_join(Big102017, by="Team")
Big10<-
  Big10 %>%
  inner_join(Big102016, by="Team")
Big10<-
  Big10 %>%
  inner_join(Big102015, by="Team")
Big10<-
  Big10 %>%
  inner_join(Big102014, by="Team")
#Big10<-
#Big10 %>%
#inner_join(Big102013, by="Team")

Big10 <-
  Big10 %>% 
  mutate(Home_wins = Home.wins.x + Home.wins.y + Home.wins.x.x + Home.wins.y.y + Home.wins.x.x.x + Home.wins.y.y.y) %>% 
  mutate(Away_wins = Away.wins.x + Away.wins.y + Away.wins.x.x + Away.wins.y.y + Away.wins.x.x.x + Away.wins.y.y.y) %>% 
  mutate(Home_losses = Home.losses.x + Home.losses.y + Home.losses.x.x + Home.losses.y.y + Home.losses.x.x.x + Home.losses.y.y.y) %>% 
  mutate(Away_losses = Away.losses.x + Away.losses.y + Away.losses.x.x + Away.losses.y.y + Away.losses.x.x.x + Away.losses.y.y.y) %>% 
  mutate(Total_wins = Total.wins.x + Total.wins.y + Total.wins.x.x + Total.wins.y.y + Total.wins.x.x.x + Total.wins.y.y.y) %>% 
  mutate(Total_losses = Total.losses.x + Total.losses.y + Total.losses.x.x + Total.losses.y.y + Total.losses.x.x.x + Total.losses.y.y.y)
Big10 <-
  Big10 %>% 
  select(Team, Home_wins, Home_losses, Away_wins, Away_losses, Total_wins, Total_losses)
Big10 <-
  Big10 %>% 
  mutate(Home_WP = ((Home_wins)/((Home_wins)+(Home_losses)))) %>% 
  mutate(Away_WP = ((Away_wins)/((Away_wins)+(Away_losses))))  %>% 
  mutate(Total_WP = ((Total_wins)/((Total_wins)+(Total_losses))))
view(Big10)

SEC<-
  SEC2019 %>%
  inner_join(SEC2018, by="Team")
SEC<-
  SEC %>%
  inner_join(SEC2017, by="Team")
SEC<-
  SEC %>%
  inner_join(SEC2016, by="Team")
SEC<-
  SEC %>%
  inner_join(SEC2015, by="Team")
SEC<-
  SEC %>%
  inner_join(SEC2014, by="Team")
SEC<-
  SEC %>%
  inner_join(SEC2013, by="Team")

SEC <-
  SEC %>% 
  mutate(Home_wins = Home.wins.x + Home.wins.y + Home.wins.x.x + Home.wins.y.y + Home.wins.x.x.x + Home.wins.y.y.y + Home.wins) %>% 
  mutate(Away_wins = Away.wins.x + Away.wins.y + Away.wins.x.x + Away.wins.y.y + Away.wins.x.x.x + Away.wins.y.y.y + Away.wins) %>% 
  mutate(Home_losses = Home.losses.x + Home.losses.y + Home.losses.x.x + Home.losses.y.y + Home.losses.x.x.x + Home.losses.y.y.y + Home.losses) %>% 
  mutate(Away_losses = Away.losses.x + Away.losses.y + Away.losses.x.x + Away.losses.y.y + Away.losses.x.x.x + Away.losses.y.y.y + Away.losses) %>% 
  mutate(Total_wins = Total.wins.x + Total.wins.y + Total.wins.x.x + Total.wins.y.y + Total.wins.x.x.x + Total.wins.y.y.y + Total.wins) %>% 
  mutate(Total_losses = Total.losses.x + Total.losses.y + Total.losses.x.x + Total.losses.y.y + Total.losses.x.x.x + Total.losses.y.y.y + Total.losses)
SEC <-
  SEC %>% 
  select(Team, Home_wins, Home_losses, Away_wins, Away_losses, Total_wins, Total_losses)
SEC <-
  SEC %>% 
  mutate(Home_WP = ((Home_wins)/((Home_wins)+(Home_losses)))) %>% 
  mutate(Away_WP = ((Away_wins)/((Away_wins)+(Away_losses))))  %>% 
  mutate(Total_WP = ((Total_wins)/((Total_wins)+(Total_losses))))
view(SEC)

Pac12<-
  Pac122019 %>%
  inner_join(Pac122018, by="Team")
Pac12<-
  Pac12 %>%
  inner_join(Pac122017, by="Team")
Pac12<-
  Pac12 %>%
  inner_join(Pac122016, by="Team")
Pac12<-
  Pac12 %>%
  inner_join(Pac122015, by="Team")
Pac12<-
  Pac12 %>%
  inner_join(Pac122014, by="Team")
Pac12<-
  Pac12 %>%
  inner_join(Pac122013, by="Team")

Pac12 <-
  Pac12 %>% 
  mutate(Home_wins = Home.wins.x + Home.wins.y + Home.wins.x.x + Home.wins.y.y + Home.wins.x.x.x + Home.wins.y.y.y + Home.wins) %>% 
  mutate(Away_wins = Away.wins.x + Away.wins.y + Away.wins.x.x + Away.wins.y.y + Away.wins.x.x.x + Away.wins.y.y.y + Away.wins) %>% 
  mutate(Home_losses = Home.losses.x + Home.losses.y + Home.losses.x.x + Home.losses.y.y + Home.losses.x.x.x + Home.losses.y.y.y + Home.losses) %>% 
  mutate(Away_losses = Away.losses.x + Away.losses.y + Away.losses.x.x + Away.losses.y.y + Away.losses.x.x.x + Away.losses.y.y.y + Away.losses) %>% 
  mutate(Total_wins = Total.wins.x + Total.wins.y + Total.wins.x.x + Total.wins.y.y + Total.wins.x.x.x + Total.wins.y.y.y + Total.wins) %>% 
  mutate(Total_losses = Total.losses.x + Total.losses.y + Total.losses.x.x + Total.losses.y.y + Total.losses.x.x.x + Total.losses.y.y.y + Total.losses)
Pac12 <-
  Pac12 %>% 
  select(Team, Home_wins, Home_losses, Away_wins, Away_losses, Total_wins, Total_losses)
Pac12 <-
  Pac12 %>% 
  mutate(Home_WP = ((Home_wins)/((Home_wins)+(Home_losses)))) %>% 
  mutate(Away_WP = ((Away_wins)/((Away_wins)+(Away_losses))))  %>% 
  mutate(Total_WP = ((Total_wins)/((Total_wins)+(Total_losses))))
view(Pac12)

#Writing CSV's so I can further clean the data in Excel

write_csv(ACC, "file=ACC_total_football.csv")
write_csv(SEC, "file=SEC_total_football.csv")
write_csv(Big10, "file=Big10_total_football.csv")
write_csv(Big12, "file=Big12_total_football.csv")
write_csv(Pac12, "file=Pac12_total_football.csv")

#Loading in the new cleaned datatable with all 5 conferences

Full <- read.table(pipe("pbpaste"), sep="\t", header = TRUE)

#Creating datatables to show who's the best and worst at home and on the road

Full_best_home <-
  Full %>% 
  arrange(desc(Home_WP)) %>% 
  select(Team, Home_wins, Home_losses, Home_WP)
Full_worst_home <-
  Full %>% 
  arrange(Home_WP) %>% 
  select(Team, Home_wins, Home_losses, Home_WP)
Full_best_away <-
  Full %>% 
  arrange(desc(Away_WP)) %>% 
  select(Team, Away_wins, Away_losses, Away_WP)
Full_worst_away <-
  Full %>% 
  arrange(Away_WP) %>% 
  select(Team, Away_wins, Away_losses, Away_WP)
Full_best_total <-
  Full %>% 
  arrange(desc(Total_WP)) %>% 
  select(Team, Total_wins, Total_losses, Total_WP)
Full_worst_total <-
  Full %>% 
  arrange(Total_WP) %>% 
  select(Team, Total_wins, Total_losses, Total_WP)

#There's a bug somewhere in here. When I run Full_best_total and Full_worst_total,
#Kansas, who should be at the very bottom, as they are 5-58, shows up near the top.
#The data isn't attached here, so I'm not asking you guys to run the code for yourself
#and see if it happens for you too. I'm just wondering, is there a way for me to manually
#move a row in a tibble? Thanks!