library(dplyr)
library(ggplot2)
library(tidyr)

library(flowers)

phen <- phenology %>%
    filter(Phenophase_Derived==2) %>%
    rename(state = Phenophase_Derived)
forcing <- read.csv("../phenolology/data/all_clim_PCIC.csv", header=TRUE, stringsAsFactors = FALSE) %>%
    filter(forcing_type=="ristos")
spus <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv") %>%
    select(SPU_Name, Orchard)

# Get *every* day of forcing data within flowering periods.
# Phenology data collected at irregular intervals, but I want to represent the full distribution of forcing as well as I can.

# unique groups in the phenology data
# phen_groups <- phen %>%
#     select(Sex, Year, Site, Orchard, Clone, Tree, X, Y) %>%
#     distinct()

# pt <- phen %>%
#     # filter forcing data to only include days when trees were flowering
#     select(Year, Site, First_RF, Last_RF) %>%
#     group_by(Year, Site) %>%
#     distinct() %>%
#     mutate(First_RF = min(First_RF), Last_RF = max(Last_RF)) %>%
#     left_join(forcing) %>%
#     filter(DoY >= First_RF & DoY <= Last_RF) %>%
#     select(-contains("_RF")) %>%
#     # add phenology metadata
#     left_join(phen_groups)

# Phenology recorded at irregular intervals. Turn it into day ranges of known flowering and connect to the forcing at those days.

pt <- phen %>%
    group_by(Sex, Year, Site, Orchard, Clone, Tree, X, Y) %>%
    mutate(First_RF = min(DoY), Last_RF = max(DoY)) %>% # calculate first and last RF that reflect inference
    select(Sex, Year, Site, Orchard, Clone, Tree, X, Y, contains("_RF")) %>%
    distinct() %>%
    pivot_longer(cols = contains("_RF"), names_to = "side", values_to = "DoY") %>%
    left_join(forcing) %>% # add forcing
    left_join(spus) # add names for provenances



# plot the distribution of forcing for
ggplot(pt, aes(x = Site, y=sum_forcing, colour = Sex)) +
    geom_line(size=1) +
    theme_classic() +
    coord_flip() +
    facet_grid(Sex ~ .) +
    scale_color_viridis_d(option="B", begin = 0.1, end=0.5) +
    ggtitle("Accumulated forcing by Site")

ggplot(pt, aes(x = SPU_Name, y=sum_forcing, colour = Sex)) +
    geom_line(size=1) +
    theme_classic() +
    coord_flip() +
    facet_grid(Sex ~ .) +
    scale_color_viridis_d(option="B", begin= 0.1, end=0.5) +
    ggtitle("Accumulated forcing by Provenance")

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

ggplot(filter(pt, Year %in% c(2006, 2008, 2010, 2011)), aes(x = Site, y=sum_forcing, color=as.factor(Year))) +
    geom_line(size=1) +
    coord_flip() +
    facet_grid(Year ~ Sex) +
    scale_color_viridis_d(option="B", end=0.8) +
    theme_bw() +
    theme(legend.position = "none")

# What is the relationship between length of the forcing period and days observed?

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

ggplot(pt, aes(x=sum_forcing, y = Sex, color=side)) +
    geom_boxplot() +
    #facet_grid(. ~ Sex) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_color_viridis_d(end = 0.8) +
    ggtitle("")


# get full range and 10-90% quantile
flowerforce <- pt %>%
    filter(!is.na(sum_forcing)) %>%
    group_by(Sex) %>%
    summarise(lower = min(sum_forcing), upper = max(sum_forcing), range = upper-lower, lower10 = quantile(sum_forcing, 0.1), upper90 = quantile(sum_forcing, 0.9), range90 = upper90-lower10)

# extract dates when trees will be flowering

ff90 <- flowerforce %>%
    select(Sex, lower10, upper90)

mforce <- forcing %>%
    filter(sum_forcing > ff90$lower10[2] & sum_forcing < ff90$upper90[2]) %>%
    mutate(Sex = "MALE")

fforce <- forcing %>%
    filter(sum_forcing > ff90$lower10[1] & sum_forcing < ff90$upper90[1]) %>%
    mutate(Sex = "FEMALE")

force <- full_join(mforce, fforce)

ggplot(force, aes(x=DoY, y=sum_forcing))




