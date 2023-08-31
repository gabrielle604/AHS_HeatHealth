library(tidyverse)
library(here)
library(ggpubr) # for some graphic applications that extend ggplot2
library(janitor)
library(broom) # used to make tables
library(knitr) # used to make table
library(dplyr)
# install.packages("stringr")
library(stringr)
library(ggplot2)

##### Load in the data + clean it:

## 2017 AHS Metropolitan PUF

metro <- read_csv(here("/Users/gabriellebenoit/Documents/GitHub/data_AHS/household_metro_17.csv"))

## 2017 AHS National PUF

national <- read_csv(here("/Users/gabriellebenoit/Documents/GitHub/data_AHS/household_nat_17.csv"))

# california cbsas = 
  # 31080 = Los Angeles-Long Beach-Anaheim, CA
  # 40140	= Riverside-San Bernardino-Ontario, CA
  # 41860	= San Francisco-Oakland-Hayward, CA
  # 41940 = San Jose-Sunnyvale-Santa Clara, CA


# use janitor() and clean up the names

# metro
metro_clean <- metro %>% clean_names()

table(metro_clean$omb13cbsa)

# national
national_clean <- national %>% clean_names()

table(national_clean$omb13cbsa)

## jacprimary, jacsecndry, acprimary, acsecndry
## Nearly every variable in the 2015 AHS PUF and beyond includes a corresponding 
# variable representing an edit or imputation flag. The edit-imputation variable 
# will be named the same as the variable to which it refers but will begin with 
# the letter J

# subset each dataset to have acprimary, acsecndry, yrbuilt, multigen, hhrace, hhsex, hholdkids, weight, and omb13cbsa

# metro
metro_sml <- select(metro_clean, control, acprimary, acsecndry, yrbuilt, multigen, hhrace, hhsex, hholdkids, weight, omb13cbsa)

# national
national_sml <- select(national_clean, control, acprimary, acsecndry, yrbuilt, multigen, hhrace, hhsex, hholdkids, weight, omb13cbsa)

# subset just to the california cbsa

# metro
metro_sml_ca <- filter(metro_sml, 
                           omb13cbsa == "'41940'")

# national
national_sml_ca <- filter(national_sml, 
                                omb13cbsa == "'31080'" | 
                                omb13cbsa == "'40140'" | 
                                omb13cbsa == "'41860'")

# merge national and metro --> not quite working, so I will just work with them separately
# ahs_17 <- full_join(metro_sml_ca, national_sml_ca, by = 'control')

# figures

# metro (this is only 41940, which is San Jose-Sunnyvale-Santa Clara, CA)
ggplot(data = metro_sml_ca, mapping = aes(x = acprimary)) +
  geom_histogram(stat="count") +
  theme_minimal()

ggplot(data = metro_sml_ca, mapping = aes(x = acsecndry)) +
  geom_histogram(stat="count") +
  theme_minimal()

ggplot(data = metro_sml_ca, mapping = aes(x = yrbuilt)) +
  geom_histogram(stat="count") +
  theme_minimal()


# national
ggplot(data = national_sml_ca) + 
  geom_point(mapping = aes(x = omb13cbsa, y = acprimary)) + 
  facet_wrap(~ acprimary, nrow = 3)

