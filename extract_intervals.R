# Calculate the flowering period for lodgepole pine - an interval for pollen shed and receptivity separately.

# Determine the first date we knew a tree was flowering and the last date we knew a tree was flowering. Observations are sometimes end and always interval censored.
# Calculate the very first and last flowering date
# Calculate the 20th percentile of start dates and the 80th percentile of end dates
# Calculate the full range and the 80% range

# There are also several visualizations of phenological period.

library(dplyr)
library(ggplot2)
library(tidyr)

library(flowers)

phen <- flowers::phenology %>%
    filter(Phenophase_Derived==2) %>%
    rename(state = Phenophase_Derived) # only consider days when trees are flowering
forcing <- read.csv("../phenolology/data/all_clim_PCIC.csv", header=TRUE, stringsAsFactors = FALSE) %>%
    filter(forcing_type=="ristos") # only consider forcing units calculated based on work of Sarvas 1972
spus <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv") %>%
    select(SPU_Name, Orchard) # provenance information for each orchard in phen


# Phenology recorded at irregular intervals. Turn it into day ranges of known flowering and connect to the forcing at those days.

pt <- phen %>%
    group_by(Sex, Year, Site, Orchard, Clone, Tree, X, Y) %>%
    mutate(firstflower = min(DoY), lastflower = max(DoY)) %>% # calculate first and last RF that reflect inference
    select(Sex, Year, Site, Orchard, Clone, Tree, X, Y, contains("flower")) %>%
    distinct() %>%
    pivot_longer(cols = contains("flower"), names_to = "side", values_to = "DoY") %>%
    left_join(forcing) %>% # add forcing
    left_join(spus) %>% # add names for provenances
    distinct()



# plot the distribution of forcing


# Accumulated forcing during flowering period
ggplot(pt, aes(x=sum_forcing, fill=Site)) +
    geom_density(alpha=0.5) +
    facet_grid(Site ~ Sex) +
    ggtitle("Flowering forcing by Site") +
    theme(legend.position = "bottom")

ggplot(pt, aes(x=sum_forcing, fill=SPU_Name)) +
    geom_density(alpha=0.5) +
    facet_grid(SPU_Name ~ Sex) +
    scale_fill_brewer(type="qual") +
    ggtitle("Flowering forcing by Provenance") +
    theme(legend.position = "bottom")

# Day of year for flowering
ggplot(pt, aes(x=DoY, fill=Site)) +
    geom_density(alpha=0.5) +
    facet_grid(Site ~ Sex) +
    ggtitle("Flowering days")


# Flowering by site and provenance
ggplot(pt, aes(x = Site, y=sum_forcing, colour = Sex)) +
    geom_line(size=1) +
    theme_bw(base_size=13) +
    coord_flip() +
    facet_grid(SPU_Name ~ Sex) +
    scale_color_viridis_d(option="B", begin= 0.2, end=0.5) +
    theme(strip.text.y.right = element_text(angle = 0)) +
    theme(legend.position = "none") +
    ggtitle("Flowering by site and provenance")

ggplot(pt, aes(x = as.factor(Clone), y = sum_forcing, color= as.factor(Clone))) +
    geom_line() +
    coord_flip() +
    facet_grid(Site ~ Sex) +
    scale_color_viridis_d(option="A") +
    theme(strip.text.y.right = element_text(angle = 0),
          legend.position = "none",
          axis.text.y = element_blank()) +
    ggtitle("Flowering by clone") +
    ylab("Clone")

# Sampling has a pretty big impact on forcing.
ggplot(filter(pt, Year %in% c(2006, 2008, 2010, 2011)), aes(x = Site, y=sum_forcing, color=as.factor(Year))) +
    geom_line(size=1) +
    coord_flip() +
    facet_grid(Year ~ Sex) +
    scale_color_viridis_d(option="B", end=0.8) +
    theme_bw() +
    theme(legend.position = "none")

# What is the relationship between length of the (observed) forcing period and days observed?

observation_bias <- phen %>%
    group_by(Year, Site, Sex, Orchard) %>%
    summarise(length = max(Last_RF) - min(First_RF), census_days = max(DoY)- min(DoY))

ggplot(observation_bias, aes(x=census_days, y=length)) +
    geom_point(alpha=0.5) +
    theme_classic() +
    ylab("Length of observed flowering period") +
    xlab("Length of observation period") +
    ggtitle("Trees are sometimes observed longer than they flower")

# Choose an interval




# get full range and 20-80% quantile
splitflower <- pt %>%
    filter(!is.na(sum_forcing)) %>%
    ungroup() %>%
    group_by(Sex) %>%
    split(.$side)

starts <- splitflower$firstflower %>%
    summarise(start = min(sum_forcing), start20=quantile(sum_forcing, 0.2))

ends <- splitflower$lastflower %>%
    summarise(end = max(sum_forcing), end80 = quantile(sum_forcing, 0.8))

flowerforce <- full_join(starts, ends) %>%
    mutate(full_range = end-start, range80 = end80 - start20)

ggplot(pt, aes(x=sum_forcing, y = Sex, color=side)) +
    geom_boxplot() +
    geom_vline(xintercept = c(flowerforce$start20, flowerforce$end80), linetype=2) +
    theme_classic(base_size=18) +
    theme(legend.position = "bottom") +
    scale_color_viridis_d(end = 0.8) +
    ggtitle("")







