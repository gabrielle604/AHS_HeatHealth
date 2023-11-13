library(tidyverse)
library(here)
library(ggpubr) # for some graphic applications that extend ggplot2
library(janitor)
library(broom) # used to make tables
library(knitr) # used to make table
library(dplyr)
library(stringr)
library(ggplot2)
# install.packages("ggridges")
library(ggridges)
library(readr)
library(tidyr)
library(forcats)
# install.packages("viridis")  
library("viridis")

# use this website for the variable descriptions:
# https://www.icpsr.umich.edu/web/NADAC/studies/36801/variables?q=smkr


# load the data
ahs_17_ca_oct <- read_csv(here("/Users/gabriellebenoit/Documents/GitHub/AHS_HeatHealth/ahs_17_ca_oct.csv"))

ahs_noAC <- ahs_17_ca_oct %>% 
  filter(acprimary == "'12'")

#### renaming the CBSAs
# Define the mapping of old values to new labels
cbsa_mapping <- c("'31080'" = 'Los Angeles-Long Beach-Anaheim', "'40140'" = 'Riverside-San Bernardino-Ontario', "'41860'" = 'San Francisco-Oakland-Hayward', "'41940'" = 'San Jose-Sunnyvale-Santa Clara')
# Use mutate and recode the values in omb13cbsa
ahs_noAC <- ahs_noAC %>%
  mutate(omb13cbsa_relabel = recode(omb13cbsa, !!!cbsa_mapping))

write_csv(ahs_noAC, file = '/Users/gabriellebenoit/Documents/GitHub/AHS_HeatHealth/ahs_noAC.csv')

## california cbsas = 
# 31080 = Los Angeles-Long Beach-Anaheim, CA
# 40140	= Riverside-San Bernardino-Ontario, CA
# 41860	= San Francisco-Oakland-Hayward, CA
# 41940 = San Jose-Sunnyvale-Santa Clara, CA

# poverty ["Household income as percent of poverty threshold (rounded)"]
p_boxplot <- ggplot(ahs_noAC, aes(x = perpovlvl, fill = omb13cbsa)) + 
  geom_boxplot(lower=25, middle=50, upper=75, outlier.size = 2.5) + # these arguments control your quantiles and tell you how large to plot the outliers!
  labs(x = "Poverty Level") +
  scale_fill_discrete(name = "CBSA", labels = c("Los Angeles-Long Beach-Anaheim", "Riverside-San Bernardino-Ontario", "San Francisco-Oakland-Hayward", "San Jose-Sunnyvale-Santa Clara")) +
  ggtitle("Household income as % of poverty threshold in households with no AC in CA CBSAs ")
p_boxplot

# housing age
p_dodged <- ggplot(ahs_noAC, aes(x = yrbuilt, fill = omb13cbsa)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("skyblue", "seagreen", "blue", "violet")) +
  labs(x = "Year Built", y = "Count", fill = "CA CBSA") +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size=20,face="bold")) +
  scale_fill_discrete(name = "CBSA", labels = c("Los Angeles-Long Beach-Anaheim", "Riverside-San Bernardino-Ontario", "San Francisco-Oakland-Hayward", "San Jose-Sunnyvale-Santa Clara"))
p_dodged

p_boxplot <- ggplot(ahs_noAC, aes(x = yrbuilt, fill = omb13cbsa)) + 
  geom_boxplot(lower=25, middle=50, upper=75, outlier.size = 2.5) + # these arguments control your quantiles and tell you how large to plot the outliers!
  labs(x = "Year Built") +
  scale_fill_discrete(name = "CBSA", labels = c("Los Angeles-Long Beach-Anaheim", "Riverside-San Bernardino-Ontario", "San Francisco-Oakland-Hayward", "San Jose-Sunnyvale-Santa Clara")) +
  ggtitle("Year Built of homes with no AC in CA CBSAs")
p_boxplot






# "hhgrad" = education level of householder; highest level of school you have completed or the highest degree you have received?
pie(table(ahs_noAC$hhgrad), main = "Distribution of hhgrad")
barplot(table(ahs_noAC$hhgrad), main = "Distribution of hhgrad", xlab = "hhgrad", ylab = "Frequency")

# Recategorize the variable into 4 levels (adjust the grouping as needed)
ahs_noAC$new_hhgrad <- ifelse(ahs_noAC$hhgrad %in% c("'31'", "'32'", "'33'", "'34'", "'35'", "'36'", "'37'", "'38'", 
                                                     "'39'", "'40'" ), "Some College and Below",
                         ifelse(ahs_noAC$hhgrad %in% c("'41'", "'42'", "'43'", "'44'"), "Certificate, AA, or Bachelor's",
                                ifelse(ahs_noAC$hhgrad %in% c("'45'", "'46'", "'47'"), "Beyond Bachelor's", "Missing")))

# Display the results
table(ahs_noAC$new_hhgrad)

pie(table(ahs_noAC$new_hhgrad), main = "Distribution of Education Categories")

# Assuming "new_hhgrad" and "omb13cbsa_relabel" are categorical variables in your dataset
ahs_noAC$omb13cbsa_relabel <- as.character(ahs_noAC$omb13cbsa_relabel)

# Create a mosaic plot with color
mosaic_data <- table(ahs_noAC$omb13cbsa_relabel, ahs_noAC$new_hhgrad)
mosaicplot(mosaic_data, 
           main = "Mosaic Plot of Education Level of Householder and No Air Conditioning By CA CBSA", 
           color = c("lightblue", "lightgreen", "lightcoral", "lightyellow", "lightpink", "lightgray"))

# household race and CBSAs with no AC
p_dodged <- ggplot(ahs_noAC, aes(x = hhrace, fill = omb13cbsa)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("skyblue", "seagreen", "blue", "violet")) +
  labs(x = "HH Race", y = "Count", fill = "CA CBSA") +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size=20,face="bold")) +
  scale_fill_discrete(name = "CBSA", labels = c("Los Angeles-Long Beach-Anaheim", "Riverside-San Bernardino-Ontario", "San Francisco-Oakland-Hayward", "San Jose-Sunnyvale-Santa Clara"))
p_dodged

# not helpful at all - majority are listed as not listed/unknown
table(ahs_noAC$hhracepi)

