# Load in the tidyverse package
library(tidyverse)

# Read datasets/yearly_deaths_by_clinic.csv into yearly
yearly_deaths<- read_csv("D:/AFTER COLLAGE/MARIANA NEWWWW/DATACAMP PROJECT/yearly_deaths_by_clinic.csv")

# Print out yearly
yearly_deaths

# Adding a new column to yearly with proportion of deaths per no. births
yearly_deaths<-yearly_deaths%>%
  mutate(proportion_deaths=deaths/births)

# Print out yearly
yearly_deaths

# Setting the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=4)

# Plot yearly proportion of deaths at the two clinics
ggplot(yearly_deaths,aes(x=year,y=proportion_deaths,col=clinic))+
  geom_line()

# THE HANDWASHING BEGIN 

# Read datasets/monthly_deaths.csv into monthly
monthly_death<- read_csv("D:/AFTER COLLAGE/MARIANA NEWWWW/DATACAMP PROJECT/monthly_deaths.csv")

# Adding a new column with proportion of deaths per no. births
monthly_death<- monthly_death%>%
  mutate(proportion_deaths=deaths/births)

# Print out the first rows in monthly
head(monthly_death)


# From this date handwashing was made mandatory
handwashing_start = as.Date('1847-06-01')

# Add a TRUE/FALSE column to monthly called handwashing_started
monthly_death <- monthly_death  %>% 
  mutate(handwashing_started = ifelse(date >= handwashing_start, TRUE, FALSE))

# Plot monthly proportion of deaths before and after handwashing
ggplot(monthly_death, aes(x = date, y = proportion_deaths, col =  handwashing_started)) + 
  geom_line()

# Calculating the mean proportion of deaths 
# before and after handwashing.

monthly_summary <- monthly_death  %>% group_by(handwashing_started) %>% 
  summarise(mean_proportion_deaths = mean(proportion_deaths))

# Printing out the summary.
monthly_summary

# Calculating a 95% Confidence intrerval using t.test 
test_result <- t.test(proportion_deaths ~handwashing_started, data = monthly_death)
test_result

# The data Semmelweis collected points to that:
doctors_should_wash_their_hands <- TRUE
