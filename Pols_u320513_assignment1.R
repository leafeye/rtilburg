## Levi Pols
## u320513

## Load packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

## Load data -------------------------------------------------------------------
olympics <- read.delim("input/olympics.txt", stringsAsFactors = FALSE)

## Question 1 -----------------------------------------------------------------
wrestling <- olympics %>%
  filter(Sport == "Wrestling") %>%
  arrange(desc(Year), Event, Name)

answer1 <- wrestling

## Question 2 -----------------------------------------------------------------
olympic_prize <- olympics %>%
  mutate(Prize = ifelse(is.na(Medal), FALSE, TRUE) )

answer2 <- olympic_prize

## Question 3 -----------------------------------------------------------------
sex_season_sports <- olympics %>%
  group_by(Sex, Season) %>%
  summarize(Num_Sports = n_distinct(Sport)) %>%
  rename(Competitor_Sex = Sex, Olympic_Season = Season)

answer3 <- sex_season_sports

## Question 4 -----------------------------------------------------------------
thousand_meters <- olympics %>%
  filter(grepl("1,000 metres", Event))
  
answer4 <- thousand_meters

## Question 5 -----------------------------------------------------------------
answer5 <- ggplot(data = olympics, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "darksalmon")

## Question 6 -----------------------------------------------------------------
answer6 <- ggplot(data = olympics, aes(x = Height, y = Weight, color = Sex)) +
  geom_point() +
  scale_y_continuous(name = "Weight (kg)") +
  scale_x_continuous(name = "Height (cm)")

## Question 7 -----------------------------------------------------------------
answer7 <- ggplot(data = olympics, aes(x = Sex, y = Weight, color = Sex)) +
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values = c("pink", "blue")) 

## Question 8 -----------------------------------------------------------------
answer8 <- olympics %>%
  group_by(Season, Year = as.factor(Year)) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Season, y = count, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge")
  
## Question 9 -----------------------------------------------------------------
bmi_sport <- olympics %>%
  mutate(BMI = Weight / (Height / 100) ^ 2) %>% 
  group_by(Sport) %>%
  summarize(Mean_BMI = mean(BMI)) %>%
  top_n(5, Mean_BMI) %>%
  arrange(Sport)

answer9 <- bmi_sport
  
## Question 10 -----------------------------------------------------------------
answer10 <- olympics %>%
  filter(NOC == "NED" | NOC == "BEL" | NOC == "LUX") %>%
  group_by(Season) %>%
  ggplot(aes(x = Season) ) +
  geom_bar(position = "dodge") + 
  facet_grid(.~NOC) +
  scale_y_continuous(name = "Number of Records", limits = c(0, 600), 
                     position = "right")

## Question 11 -----------------------------------------------------------------
answer11 <- olympics %>%
  filter(Team == "Refugee Olympic Athletes") %>%
  distinct(Name, .keep_all = TRUE) %>%
  mutate(Name_Length = nchar(Name)) %>%
  arrange(Name_Length) %>%
  ggplot(aes(x = reorder(Name, -Name_Length), y = Age)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_x_discrete(name = "Name") + 
  theme(text = element_text(family = "mono"))

## Question 12 -----------------------------------------------------------------
answer12 <- olympics %>%
  group_by(Season) %>%
  summarise(Height_Mean = mean(Height), Height_SE = sd(Height) / sqrt(n())) %>%
  ggplot(aes(x = Season, y = Height_Mean)) + 
  geom_point() +
  geom_errorbar(aes(ymin = Height_Mean - Height_SE,
                    ymax = Height_Mean + Height_SE), width = 0.1) +
  theme(plot.background = element_rect(fill = 'cadetblue'),
        panel.background = element_rect(fill = 'azure'),
        panel.grid = element_line(color = "grey"))
