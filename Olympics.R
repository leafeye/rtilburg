
## Load packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)


# Deze opdrachten maken gebruik van een dataset genaamd olympics. Hierin staat informatie van de deelnemers van 
# alle sportevenementen gehouden op de Zomer en Winter spelen van 2010 - 2016. De meeste kolommen spreken voor zich.
# Height is in centimerers en weight is in kilo. De NOC kolom staat voor de 'National Olympic Comittee' wie de atleet
# heeft gestuurd. 

## Load data -------------------------------------------------------------------
olympics <- read.delim("olympics.txt", stringsAsFactors = FALSE)



# Create an object that’s a copy of olympics, but which contains only records concerning "Wrestling", sorted by Year,
# most recent first, and then alphabetically by Event and Name, in that order.

## Vraag 1 -----------------------------------------------------------------
wrestling <- olympics %>%
  filter(Sport == "Wrestling") %>%
  arrange(desc(Year), Event, Name)


# Create an object that’s a copy of olympics, but with a new final column, Prize. For each record, it should contain
# TRUE if a medal was won by that competitor in that event and FALSE otherwise

## Question 2 -----------------------------------------------------------------
olympic_prize <- olympics %>%
  mutate(Prize = ifelse(is.na(Medal), FALSE, TRUE) )


#Create an object which contains, for each combination of Sex and Season, the number of different sports in the
# olympics data set. The columns of this object should be called Competitor_Sex, Olympic_Season, and Num_Sports,
# respectively

## Question 3 -----------------------------------------------------------------
sex_season_sports <- olympics %>%
  group_by(Sex, Season) %>%
  summarize(Num_Sports = n_distinct(Sport)) %>%
  rename(Competitor_Sex = Sex, Olympic_Season = Season)


#Create an object that’s a copy of olympics, but which includes only records with "1,000 metres" in the name of
#the Event

## Question 4 -----------------------------------------------------------------
thousand_meters <- olympics %>%
  filter(grepl("1,000 metres", Event))
  

# Using ggplot2, create a histogram of the Age variable. Each bar should represent 5 years and be entirely "darksalmon"
# in color. Leave all other settings at their defaults.

## Question 5 -----------------------------------------------------------------
answer5 <- ggplot(data = olympics, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "darksalmon")

#Using ggplot2, create a scatterplot of Weight versus Height, with Weight on the y-axis and Height on the x-axis.
#The points’ color should represent the competitor’s Sex. Label the y-axis "Weight (kg)" and the x-axis "Height
#(cm)". Leave all other settings at their defaults. 


## Question 6 -----------------------------------------------------------------
answer6 <- ggplot(data = olympics, aes(x = Height, y = Weight, color = Sex)) +
  geom_point() +
  scale_y_continuous(name = "Weight (kg)") +
  scale_x_continuous(name = "Height (cm)")

# Using ggplot2, create a boxplot of Weight per Sex. Hide outliers, and make the lines of the women’s boxplot "pink"
#and of the men’s "blue". Leave all other settings at their defaults

## Question 7 -----------------------------------------------------------------
answer7 <- ggplot(data = olympics, aes(x = Sex, y = Weight, color = Sex)) +
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values = c("pink", "blue"))

#Using ggplot2, create a barplot. It shows the total number of records in the data set for each combination of
# Season and Year.

## Question 8 -----------------------------------------------------------------
answer8 <- olympics %>%
  group_by(Season, Year = as.factor(Year)) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = Season, y = count, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge")

#For each Sport in the data set, calculate the mean BMI of all corresponding records, where BMI is equal to weight
#divided by height squared, with weight in kilograms and height in metres. Store the top 5 results in terms of mean
#BMI in a new object, consisting of the columns named Sport and Mean_BMI only; make sure it is sorted alphabetically
#by Sport.


## Question 9 -----------------------------------------------------------------
bmi_sport <- olympics %>%
  mutate(BMI = Weight / (Height / 100) ^ 2) %>% 
  group_by(Sport) %>%
  summarize(Mean_BMI = mean(BMI)) %>%
  top_n(5, Mean_BMI) %>%
  arrange(Sport)

#Using ggplot2, create a barplot showing the number of records relating to the NED, BEL and LUX ‘National Olympic
#Committees’. Each country should be represented by its own panel, with a separate bar for the "Summer" and
#"Winter" Season in each panel. Ensure that the y-axis is labelled "Number of Records", that it runs from 0 to 600,
# and that it is placed on the right side of the plot. Leave all other settings at their defaults.
  
## Question 10 -----------------------------------------------------------------
answer10 <- olympics %>%
  filter(NOC == "NED" | NOC == "BEL" | NOC == "LUX") %>%
  group_by(Season) %>%
  ggplot(aes(x = Season) ) +
  geom_bar(position = "dodge") + 
  facet_grid(.~NOC) +
  scale_y_continuous(name = "Number of Records", limits = c(0, 600), 
                     position = "right")

#Re-create this barplot, concerning the Team "Refugee Olympic Athletes"; the athletes are sorted by the number of
#letters in their Name, and all text uses the "mono" font family.

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


#Using ggplot2, create a scatterplot with one point per Season, each showing the mean Height of all records related
#to that Season. Each point should be surrounded by error bars with width = 0.1, and these should show the
#standard error of the corresponding mean, calculated as the standard deviation divided by the square root of the
#number of records. The background of the plotting area should be "azure" and the background of the rest of the
#plot, e.g. behind the axes, etc., should be "cadetblue"; the gridlines should be "grey". Leave all other settings at
#their defaults.

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


