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
library(readr)
library(scales)

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

# metro = San Jose-Sunnyvale-Santa Clara, CA
metro_sml_ca <- filter(metro_sml, 
                           omb13cbsa == "'41940'")

# national
  # 31080 = Los Angeles-Long Beach-Anaheim, CA
  # 40140	= Riverside-San Bernardino-Ontario, CA
  # 41860	= San Francisco-Oakland-Hayward, CA
national_sml_ca <- filter(national_sml, 
                                omb13cbsa == "'31080'" | 
                                omb13cbsa == "'40140'" | 
                                omb13cbsa == "'41860'")


# figures

# metro (this is only 41940, which is San Jose-Sunnyvale-Santa Clara, CA)
ggplot(data = metro_sml_ca, mapping = aes(x = acprimary)) +
  geom_histogram(stat="count") +
  theme_minimal()


## trying out adding in weight to the above plot
ggplot(data = metro_sml_ca, mapping = aes(x = acprimary, weight = weight)) +
  geom_histogram(stat="count") +
  theme_minimal()


ggplot(data = metro_sml_ca, mapping = aes(x = acsecndry)) +
  geom_histogram(stat="count") +
  theme_minimal()

ggplot(data = metro_sml_ca, mapping = aes(x = yrbuilt)) +
  geom_histogram(stat="count") +
  theme_minimal()

# these ones are good
ggplot(data = metro_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = acprimary))

ggplot(data = metro_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = acsecndry))

ggplot(data = metro_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = yrbuilt))

ggplot(data = metro_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = multigen))

ggplot(data = metro_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = hhrace))

ggplot(data = metro_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = hhsex))

ggplot(data = metro_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = hholdkids))


metro_sml_ca %>% 
  count(omb13cbsa, acprimary) %>%  
  ggplot(mapping = aes(x = omb13cbsa, y = acprimary)) +
  geom_tile(mapping = aes(fill = n))


ggplot(metro_sml_ca,aes(x = omb13cbsa,fill = acprimary)) + 
  geom_bar(position = "fill") +
  theme_minimal() +
  xlab("California CBSAs in AHS Metro PUF")


# national
ggplot(data = national_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = acprimary))

ggplot(data = national_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = acsecndry))

ggplot(data = national_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = yrbuilt))

ggplot(data = national_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = multigen))

ggplot(data = national_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = hhrace))

ggplot(data = national_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = hhsex))

ggplot(data = national_sml_ca) +
  geom_count(mapping = aes(x = omb13cbsa, y = hholdkids))



national_sml_ca %>% 
  count(omb13cbsa, acprimary) %>%  
  ggplot(mapping = aes(x = omb13cbsa, y = acprimary)) +
  geom_tile(mapping = aes(fill = n)) +
  theme_minimal() +
  xlab("California CBSAs in AHS National PUF") + 
  ylab("Primary Air Conditioning Status and Type")

ggplot(national_sml_ca,aes(x = omb13cbsa,fill = acprimary)) + 
  geom_bar(position = "fill") +
  theme_minimal() +
  xlab("California CBSAs in AHS National PUF")

## try the above plot with weights
ggplot(national_sml_ca,aes(x = omb13cbsa, fill = acprimary, weight = weight)) + 
  geom_bar(position = "fill") +
  theme_minimal() +
  xlab("California CBSAs in AHS National PUF")


# merge national and metro --> working now!!
ahs_17 <- full_join(metro_sml_ca, national_sml_ca)

ahs_17_ca <- ahs_17

# save this merged dataset
# write_csv(ahs_17_ca, file = '/Users/gabriellebenoit/Documents/GitHub/AHS_HeatHealth/ahs_17_ca.csv')

## figures with all four AHS California CBSAs

ggplot(ahs_17,aes(x = omb13cbsa, fill = acprimary)) + 
  geom_bar(position = "fill") +
  theme_minimal() +
  xlab("California CBSAs in AHS National and Metro PUF")

# add in weights

ggplot(ahs_17_ca,aes(x = omb13cbsa, fill = acprimary, weight = weight)) + 
  geom_bar(position = "fill") +
  theme_minimal() +
  ggtitle("California CBSAs in AHS National and Metro PUF (weighted)") +
  xlab("California CBSAs") +
  ylab("Count in Each Status Category of Primary Air Conditioning")

ggsave("ac_type_CA_cbsa_ahs_2017.pdf")

ggplot(data = ahs_17_ca, mapping = aes(x = yrbuilt, y = acprimary, colour = omb13cbsa, group = omb13cbsa, weight = weight)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("California CBSAs in AHS National and Metro PUF (weighted)") +
  xlab("Year the home was built") +
  ylab("Status of Primary Air Conditioning")

ggsave("ac_type_yrbuilt_CA_cbsa_ahs_2017.pdf")

# percentages

ggplot(ahs_17_ca,aes(x = omb13cbsa, y = after_stat(count/sum(count)), fill = acprimary, weight = weight)) + 
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab("California CBSAs in AHS National and Metro PUF (weighted)") +
  ylab("percentage")

ggsave("ac_type_percentage_CA_cbsa_ahs_2017.pdf")

# frequency (using this video: https://www.youtube.com/watch?v=MHbzCs05Luo)
# just using cbsa variable
df <- ahs_17_ca %>% 
  dplyr::group_by(omb13cbsa) %>% 
  tally()

df <- df %>% 
  dplyr::mutate(perc = n/sum(n))
  
pl <- ggplot(data = df, aes(x=omb13cbsa, y = perc))
pl <- pl + geom_bar(stat = "identity", fill = "orange") # stat = "identity" means whatever is the value of y is what is dictating the bars
pl <- pl + theme_minimal()
pl <- pl + labs(title = "AHS CA National and Metro CBSAs")
pl <- pl + labs(subtitle = "Showing ....")
pl <- pl + labs(caption = paste("Total cbsas", nrow(df)))
pl <- pl + scale_y_continuous(labels = scales::percent)
pl <- pl + labs(x = "CBSA", y = "Percent")
pl

# using cbsa and acprimary
df <- ahs_17_ca %>% 
  dplyr::group_by(omb13cbsa, acprimary) %>% 
  tally()

df <- df %>% 
  dplyr::mutate(perc = n/sum(n))

pl <- ggplot(data = df, aes(x=acprimary, y = perc))
pl <- pl + geom_bar(stat = "identity", fill = "orange") # stat = "identity" means whatever is the value of y is what is dictating the bars
pl <- pl + theme_minimal()
pl <- pl + labs(title = "AHS CA National and Metro CBSAs")
pl <- pl + labs(subtitle = "Showing ....")
pl <- pl + labs(caption = paste("...", nrow(df)))
pl <- pl + scale_y_continuous(labels = scales::percent)
pl <- pl + labs(x = "...", y = "Percent")
pl

# create a data frame with just the 4 california cbsas and those with "no primary air conditioning'
df <- ahs_17_ca %>% 
  dplyr::group_by(omb13cbsa, acprimary, weight) %>% 
  tally()

df <- df %>% 
  dplyr::mutate(perc = n/sum(n))

df <- df[df$acprimary == "'12'",]

no_ac <- ggplot(data = df,aes(x = acprimary, y = after_stat(count/sum(count)), fill = omb13cbsa, weight = weight)) + 
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab("No Primary AC in AHS National and Metro PUF (weighted)") +
  ylab("percentage")

no_ac

# rename CBSAs in legend
no_ac <- ggplot(data = df,aes(x = acprimary, y = after_stat(count/sum(count)), fill = omb13cbsa, weight = weight)) + 
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  xlab("No Primary AC in AHS National and Metro PUF (weighted)") +
  ylab("percentage") +
  scale_fill_discrete(name = "CBSA", labels = c("Los Angeles-Long Beach-Anaheim", "Riverside-San Bernardino-Ontario", "San Francisco-Oakland-Hayward", "San Jose-Sunnyvale-Santa Clara"))

no_ac

ggsave("no_ac_percentage_CA_cbsa_ahs_2017.pdf")


## california cbsas = 
  # 31080 = Los Angeles-Long Beach-Anaheim, CA
  # 40140	= Riverside-San Bernardino-Ontario, CA
  # 41860	= San Francisco-Oakland-Hayward, CA
  # 41940 = San Jose-Sunnyvale-Santa Clara, CA

## acprimary
  # 1 = electric powered central air conditioner
  # 2 = piped gas powered central air conditioner
  # 3 = liquefied petroleum gas powered central air conditioner
  # 4 = other fuel source powered air conditioner
  # 5 = 1 room air conditioner
  # 6 = 2 room air conditioner
  # 7 = 3 room air conditioner
  # 8 = 4 room air conditioner
  # 9 = 5 room air conditioner
  # 10 = 6 room air conditioner
  # 11 = 7+ room air conditioner
  # 12 = no air conditioning

## acsecndry
  # 1 = secondary electric powered central air conditioner
  # 2 = secondary piped gas powered central air conditioner
  # 3 = secondary liquefied petroleum gas powered central air conditioner
  # 4 = secondary other fuel source powered air conditioner
  # 5 = secondary 1 room air conditioner
  # 6 = secondary 2 room air conditioner
  # 7 = secondary 3 room air conditioner
  # 8 = secondary 4 room air conditioner
  # 9 = secondary 5 room air conditioner
  # 10 = secondary 6 room air conditioner
  # 11 = secondary 7+ room air conditioner
  # 12 = secondary no air conditioning

## multigen
  # 1 = Householder only
  # 2 = Householder and one younger generation
  # 3 = Householder and two or more younger generations
  # 4 = Householder and at least one younger generation, at least one older generation
  # 5 = Householder and one older generation
  # 6 = Householder and two or more older generations
  # N = not applicable

## hhrace
  # 1	= White only
  # 2	= Black only
  # 3	= American Indian, Alaska Native only
  # 4 =	Asian only
  # 5	= Hawaiian, Pacific Islander only
  # 6 =	White / Black
  # 7	= White / American Indian, Alaska Native
  # 8 =	White / Asian
  # 9	= White / Hawaiian, Pacific Islander
  # 10	= Black / American Indian, Alaska Native
  # 11	= Black / Asian
  # 12	= Black / Hawaiian, Pacific Islander
  # 13	= American Indian, Alaska Native / Asian
  # 14	= Asian / Hawaiian, Pacific Islander
  # 15	= White / Black / American Indian, Alaska Native
  # 16	= White / Black / Asian
  # 17	= White / American Indian, Alaska Native / Asian
  # 18	= White / Asian / Hawaiian, Pacific Islander
  # 19	= White / Black / American Indian, Alaska Native / Asian
  # 20	= Other combinations of 2 or 3 races
  # 21	= Other combinations of 4 or more races
  # N = not applicable

## hhsex
  # 1 = female
  # 2 = male
  # N = not applicable


