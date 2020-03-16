library(dplyr)
library(caret)
library(tidyverse)

data <- read.csv('world-happiness-report-2019.csv')

a <- data

a %>% glimpse()


a <- mutate(a , Region = NA )



Europe <- c('Finland', 'Denmark', 'Norway', 'Iceland','Netherlands',
            'Switzerland', 'Sweden', 'Austria', 'Luxembourg', 'United Kingdom', 
            'Ireland', 'Germany', 'Belgium', 'Malta', 'France', 'Spain', 'Italy',
            'Slovakia', 'Poland', 'Lithuania', 'Slovenia', 'Kosovo', 'Romania',
            'Cyprus', 'Latvia', 'Estonia', 'Hungary', 'Northern Cyprus', 'Portugal',
            'Russia', 'Serbia', 'Moldova', 'Montenegro', 'Croatia', 
            'Turkey', 'Belarus', 'Greece', 'Macedonia', 'Azerbaijan', 'Bulgaria',
            'Albania', 'Ukraine', 'Georgia', 'Bosnia and Herzegovina ')

Oceania <- c('New Zealand', 'Australia')

America <- c('Canada', 'Costa Rica', 'United States', 'Czech Republic', 'Mexico', 
             'Chile', 'Guatemala', 'Panama', 'Brazil', 'Uruguay', 'El Salvador',
             'Trinidad and Tobago', 'Colombia', 'Nicaragua', 'Argentina', 'Ecuador',
             'Jamaica', 'Honduras', 'Bolivia', 'Paraguay', 'Peru', 'Dominican Republic',
             'Venezuela', 'Haiti')

Asia <- c('China', 'Malaysia', 'Israel', 'United Arab Emirates', 'Taiwan', 'Saudi Arabia',
          'Qatar', 'Singapore', 'Bahrain', 'Uzbekistan', 'Kuwait', 'Thailand',
          'South Korea', 'Japan', 'Kazakhstan', 'Pakistan', 'Philippines', 'Tajikistan',
          'Hong Kong', 'Mongolia', 'Kyrgyzstan', 'Turkmenistan', 'Lebanon', 'Indonesia', 
          'Vietnam', 'Bhutan', 'Nepal', 'Jordan', 'Laos','Cambodia', 'Palestinian Territories',
          'Afghanistan','Yemen', 'Syria', 'India', 'Myanmar', 'Sri Lanka', 'Iran',
          'Armenia', 'Bangladesh', 'Iraq')

Africa <- c('Mauritius', 'Libya', 'Nigeria', 'Algeria', 'Morocco', 'Cameroon', 'Ghana',
            'Ivory Coast', 'Benin', 'Congo (Brazzaville)', 'Gabon', 'South Africa',
            'Somalia', 'Senegal', 'Namibia', 'Niger', 'Central African Republic', 'Tanzania',
            'Rwanda', 'Malawi', 'Botswana', 'Zimbabwe', 'Burundi', 'Lesotho', 'Madagascar',
            'Comoros', 'Liberia', 'Togo', 'Zambia', 'Egypt', 'Uganda', 'Swaziland', 'Ethiopia',
            'Chad', 'Sierra Leone', 'Mali', 'Guinea', 'Burkina Faso', 'South Sudan', 'Gambia',
            'Kenya', 'Mauritania', 'Mozambique', 'Tunisia', 'Congo (Kinshasa)')

a$Region <- ifelse(a$Country..region. %in% Europe, "Europe", 
            ifelse(a$Country..region. %in% Asia, "Asia",
            ifelse(a$Country..region. %in% Oceania, "Oceania",
            ifelse(a$Country..region. %in% America, "America",
            ifelse(a$Country..region. %in% Africa, "Africa","NA")))))


boxplot(a$Ladder~a$Region, ylab= 'Ladder',xlab = 'Regions', main='Box-whisker plot of Ladder among Regions')

plot(density(a$Ladder, adjust = 1), ylab = 'Distribution of Countries',main = 'Density Plot For Default Bandwidth'  )

plot(density(a$Ladder, adjust = 0.1), ylab = 'Distribution of Countries', main = 'Density Plot For Smaller Bandwidth'  )

plot(density(a$Ladder, adjust = 100), ylab = 'Distribution of Countries',main = 'Density Plot For Greater Bandwidth')


expv<-(log10(a$Ladder))

plot(density(expv, adjust = 1), ylab = 'Distribution of Countries', main = "Density plot for Logarithmic Value")

densityplot(a$Ladder, ylab = 'Distribution of Countries', xlab = 'Ladder', adjust = 100)

ggplot(a,aes(Ladder))+geom_density(fill = "green", adjust= 2 ) +labs(x= "Ladder", y = "Distribution of Countries")
            