# Phenology thru time


library(caTools)

source('extract_intervals.R')

# Phenology recorded at irregular intervals. Turn it into day ranges of known flowering and connect to the forcing at those days.
# extract dates when trees will be flowering

ff80 <- flowerforce %>%
    select(Sex, start20, end80)

# consider window function

mforce <- forcing %>%
    filter(sum_forcing > ff80$start20[2] & sum_forcing < ff80$end80[2]) %>%
    mutate(Sex = "MALE")

fforce <- forcing %>%
    filter(sum_forcing > ff80$start20[2] & sum_forcing < ff80$end80[2]) %>%
    mutate(Sex = "FEMALE")

force <- full_join(mforce, fforce) %>% # full period of forcing for all sites
    group_by(Year, Site)

ggplot(force, aes(x=sum_forcing, y=DoY, color=Site, group=interaction(Year, Site))) +
    geom_line(alpha=0.7) +
    ggtitle("Forcing and Day of Year for flowering periods", subtitle = "For 1997-2011 at all sites") +
    theme_bw(base_size=18)

# Flowering period
ggplot(force, aes(x=as.factor(Year), y=DoY, colour=Year)) +
    geom_line(size=2)+
    facet_grid(Site ~ Sex) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), strip.text.y = element_text(angle = 0)) +
    coord_flip()

# plot first and last day trends

forceends <- force %>%
    group_by(Sex, Site, Year) %>%
    summarise(begin = min(DoY), end=max(DoY), length=end-begin)

forceends_long <- forceends %>%
    select(-length) %>%
    pivot_longer(cols=c(begin, end), names_to="side", values_to="DoY")


# Plot trends in start and end of phenological period
ggplot(forceends_long, aes(x=Year, y=DoY, color=Site, linetype=side)) +
    geom_point() +
    geom_line() +
    ggtitle("Start and end of phenology period time") +
    facet_grid(. ~ Sex)

# Plot flowering period length thru time
ggplot(forceends, aes(x=Year, y=length, color=Site)) +
    geom_point() +
    geom_line() +
    facet_grid(. ~ Sex)

# calculate running average and sd

w <- 3 # set moving window width
running <- forceends_long %>%
    group_by(Site, Sex, side) %>%
    arrange(side, Sex, Site, Year, DoY) %>%
    mutate(rollingmean = caTools::runmean(DoY, w), rollingsd = caTools::runsd(DoY, w))

ggplot(running, aes(x=Year, y=rollingmean, color=Site, linetype=side)) +
    geom_point() +
    geom_line() +
    facet_grid(side ~ Sex) +
    ggtitle(paste(w,"year running average"))

ggplot(running, aes(x=Year, y=rollingsd, color=Site, linetype=side)) +
    geom_point() +
    geom_line() +
    facet_grid(side ~ Sex) +
    ggtitle(paste(w,"year running sd"))
