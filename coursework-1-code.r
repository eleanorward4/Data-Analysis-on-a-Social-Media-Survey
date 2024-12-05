library(tidyverse)
survey = read_csv("survey.csv")
print(survey)

### Part 1

## to find how many people responded to the survey
survey %>% 
  summarise(total_number = n())
# 771 people responded

## to find what time period the survey ran
survey %>% 
  summarise(min = min(time),
            max = max(time))
# the survey ran from 4th February 2021 to 10th March 2021

## to find which social media platforms survey respondents use, and how
## many respondents use each platform
## using Q2: " Which social media account do you use usually"
survey %>% 
  group_by(Q2) %>%
  summarise(total_number = n())
# Social media platforms survey respondents use: Facebook (659 respondents), 
# Instagram (64 respondents), Snapchat (15 respondents), Twitter (16 respondents),
# and Whatsapp (17 respondents).

# Bar chart plotted using code below to visualise this information
socialmediaplot = ggplot(survey) +
  geom_bar(aes(x=Q2), fill='pink', colour='pink4') +
  theme(text = element_text(size=12)) +
  labs(x='Social media platform', y='Number of respondents', 
       title='Social media platforms survey respondents usually use')
ggsave('socialmediaplot.png', socialmediaplot, width=6, height=4, dpi=500)

## to analyse age distribution of respondents
survey %>%
  summarise(min_age = min(age),
            lower_quartile = quantile(age, 0.25),
            median_age = median(age),
            upper_quartile = quantile(age, 0.75),
            max_age = max(age))
# Age distribution: median = 25, lower quartile = 20, upper quartile = 27.

# Box plot plotted using code below to visualsie this information.
ageboxplot = ggplot(survey) +
  geom_boxplot(aes(y=age), fill='pink', colour='pink4') +
  theme(text = element_text(size=15)) +
  labs(y='Age', title='Age distribution of respondents')
ggsave('ageboxplot.png', ageboxplot, width=6, height=4, dpi=500)

## to produce a small map that highlights Bangladesh
world = map_data('world2')
location = tibble(long = 90.3563, lat = 23.6850, name = 'Bangladesh')
worldplot = ggplot() +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(data=location, aes(x=long, y=lat), colour='pink4', size=7) +
  xlim(70,105) + ylim(10,35) + theme_void()
ggsave('worldplot.png', worldplot, width=6, height=4, dpi=500)

### Part 2

## to summarise responses to social media usage question (Q1 - How much time do 
## you spend on social daily on social media? In hours)
survey %>%
  group_by(Q1) %>%
  summarise(total_number = n())

## to summarise responses to sleep quality question (Q5 - In the past month, how
## would you rate your sleep quality overall)
survey %>%
  group_by(Q5) %>%
  summarise(total_number = n())

## to summarise responses to both questions together
survey %>%
  filter(Q5 == 'good') %>%
  group_by(Q1) %>%
  summarise(total_number = n())
survey %>%
  filter(Q5 == 'poor') %>%
  group_by(Q1) %>%
  summarise(total_number = n())

## to plot a bar chart visualising sleep quality among the different groups of
## daily social media usage
sleepsocialplot = ggplot(survey) +
  geom_bar(aes(x=Q1), fill='pink', colour='pink4') +
  theme(text = element_text(size=11.5)) +
  labs(x='How many hours daily spent on social media', y='Number of respondents', 
       title='Relationship between sleep quality and daily social media use') +
  facet_wrap(~Q5)
ggsave('sleepsocialplot.png', sleepsocialplot, width=6, height=4, dpi=500)

### Part 3

## Choosing to answer question 3 - Is there evidence that excessive social media
## usage has a negative effect on concentration?
## Using responses from Q1 (How much time do you spend daily in hours on social 
## media?) and Q4 (In the last 30 days, I had trouble concentrating on things 
## such as reading or watching television?)

## bar chart plotted using code below to visualise data in this question
socialconcplot = ggplot(survey) +
  geom_bar(aes(x=Q1), fill='pink', colour='pink4') +
  theme(text = element_text(size=12)) +
  labs(x='How many hours daily spent on social media', y='Number of respondents', 
       title='The impact of social media usage on concentration',
       subtitle='A graph showing daily time spent on social media for each 
       frequency of concentration problems') +
  facet_wrap(~Q4)
ggsave('socialconcplot.png', socialconcplot, width=6, height=4, dpi=500)

## to find data to use as summary measures

## to see how many respondents picked never, often or daily for Q4
survey %>%
  group_by(Q4) %>%
  summarise(total_number = n())
# never 326, often 307, daily 138

## to see how many people spend each amount of time on social media per day
survey %>%
  group_by(Q1) %>%
  summarise(total_number =n())
# 0-1 113, 1-3 282, 3-5 201, 5+ 175

## to see how many daily hours respondents spend on social media for each answer 
## to question 4
survey %>%
  filter(Q4 == '1_never') %>%
  group_by(Q1) %>%
  summarise(total_number = n())
survey %>%
  filter(Q4 == '2_often') %>%
  group_by(Q1) %>%
  summarise(total_number = n())
survey %>%
  filter(Q4 == '3_daily') %>%
  group_by(Q1) %>%
  summarise(total_number = n())

## using how many respondents picked never often or daily and how many hours 
## daily each of them spent on social media to find percentages to use to 
## further analyse data
## creating values to put into a data frame later on
# Never had trouble concentrating in last 30 days = 326
# Of these, 181 spent 0-3 hours on social media daily and 145 at least 3
Percentage_never_03 = 181/326*100
Percentage_never_3plus = 145/326*100
# Often had trouble concentrating in last 30 days = 307
# Of these, 154 spent 0-3 hours on social media daily and 153 at least 3
Percentage_often_03 = 154/307*100
Percentage_often_3plus = 153/307*100
# Daily had touble concentrating in last 30 days = 138
# Of these, 60 spent 0-3 hours on social media daily and 78 at least 3
Percentage_daily_03 = 60/138*100
Percentage_daily_3plus = 78/138*100

## to create a data frame to show these proportions
df=data.frame(Conc.prob = c('1_Never', '2_Often', '3_Daily'), 
              upto3hours.percentage = 
                c(Percentage_never_03, Percentage_often_03, Percentage_daily_03),
              over3hours.percentage = 
                c(Percentage_never_3plus, Percentage_often_3plus, Percentage_daily_3plus))
print(df)

# this data frame shows the correlation between concentration problems and time
# spent on social media each day. The more time spent, the more concentration problems.

## to create visualization for this created data frame by plotting a graph
timeconcplot = ggplot(df) +
  geom_point(aes(x=Conc.prob, y=over3hours.percentage), colour='pink4', size=5) +
  theme(text = element_text(size=10)) +
  labs(x='How often respondent has concentration problems', y='Percentage spending 3+ hours daily on social media', 
       title='Relationship between time on social media and concentration problems') 
ggsave('timeconcplot.png', timeconcplot, width=6, height=4, dpi=500)

#### Part 4 - extra analysis
## to show if there is a correlation between highest level of education of 
## respondent and how long spent on social media daily through a bar chart

educationplot = ggplot(survey) +
  geom_bar(aes(x=education), fill='pink', colour='pink4') +
  theme(text = element_text(size=10)) +
  facet_wrap(~Q1) +
  labs(x='Highest level of education', y='Number of respondents', 
       title='Relationship between education and social media usage') +
  theme(text = element_text(size=12))
ggsave('educationplot.png', educationplot, width=6, height=4, dpi=500)