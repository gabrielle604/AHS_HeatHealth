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

# load the data
ahs_17_ca <- read_csv(here("/Users/gabriellebenoit/Documents/GitHub/AHS_HeatHealth/ahs_17_ca.csv"))

ahs_noAC <- ahs_17_ca %>% 
  filter(acprimary == "'12'")

ahs_noAC %>% filter()



ggplot(data = ahs_noAC) +
  geom_count(mapping = aes(x = omb13cbsa, y = acprimary))


ggplot(data = ahs_noAC, mapping = aes(x = yrbuilt, y = hhrace, colour = omb13cbsa, group = omb13cbsa)) +
  geom_boxplot() +
  theme_minimal()

# ridgeline plot, "joyplot"
# probably not a useful plot

ggplot(ahs_noAC, aes(x = omb13cbsa, y = hhrace, fill = hhrace)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

# violin plot
ggplot(ahs_17_ca, aes(x=omb13cbsa, y=yrbuilt, fill=omb13cbsa)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin() +
  theme_minimal() +
  ggtitle("California CBSAs in AHS National and Metro PUF (not weighted)") +
  xlab("CA CBSAs") +
  ylab("Year Home was Built")
# not weighted because when I added in weights, R said it dropped the weight aesthetic
ggsave("ac_yrbuilt_CA_cbsa_ahs_2017.pdf")

# multiple histograms
install.packages("hrbrthemes")
library(hrbrthemes)

ahs_noAC %>%
  ggplot( aes(x=omb13cbsa, fill=hhrace)) +
  geom_histogram(stat="count") +
  theme_ipsum() +
  scale_color_viridis()
