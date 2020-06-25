# extract site weather data from PCIC netcdf files. netcdf files are for relatively large blocks that include the points of interest. Also record and store grid point locations and elevations for site and gridpoints.

library(ncdf4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgbif)

## functions ####

index_get <- function(nc_object) { #nc object is an object created with ncvar_get. pull the metadata for netcdf variable for use in indexing
    temp_meta <- nc_object$var[[1]]

    longitudes <- temp_meta$dim[[1]]$vals
    latitudes <- temp_meta$dim[[2]]$vals
    times <- temp_meta$dim[[3]]$vals

    meta <- list(lons=longitudes, lats=latitudes, times=times)
    return(meta)
}

# get the closest lat lon
get_closest_ref <- function(locations, netcdf_meta) { # locations is a dataframe with id1, lat, and lon columns. netcdf_meta is a list that contains dim information for a netcdf file with named lat and lon lists
    refs <- dplyr::select(locations, id1, lat, lon)
    netcdf_lats <- netcdf_meta$lats
    netcdf_lons <- netcdf_meta$lons

    for (i in 1:nrow(locations)) {
        refs$closest_lat_loc[i] <- which.min(abs(netcdf_lats-locations$lat[i]))
        refs$closest_lat[i] <- netcdf_lats[refs$closest_lat_loc[i]]
        refs$closest_lon_loc[i] <- which.min(abs(netcdf_lons-locations$lon[i]))
        refs$closest_lon[i] <- netcdf_lons[refs$closest_lon_loc[i]]
    }

    return(refs)
}

# pull only temperatures associated with lat/lon of interest
pick_temp_by_location <- function(nc_locations, nc_connection) { # locations is output from get_closest_ref and has the lat and lon indexes of the sites of interest. nc_connection is the connection to the netcdf file with the data
    # build a dataframe where each column shows the max temp at a different location
    temps <- data.frame(Date=firstday:lastday)

    for (i in 1:nrow(locations)) {
        temps[,i+1] <- ncvar_get(nc_connection,
                                 start = c(nc_locations$closest_lon_loc[i],
                                           nc_locations$closest_lat_loc[i], 1),
                                 count = c(1,1,-1)) # get the temperature
    }

    colnames(temps)[2:ncol(temps)] <- nc_locations$id1

    # tidy the data
    temp_tidy <- tidyr::gather(temps, key = "site", value = "temp_in_c", -Date)
    return(temp_tidy)
}

# read in site locations #############
site_locs <- read.csv("data/seed_orchard_site_coordinates.csv", stringsAsFactors = FALSE, header=TRUE)

# extract weather data from PCIC netcdf file #################


# dates in these files are days since Jan 1, 1945. Get date locations of dates of interest by replacing dates in first and lastday with your dates of interest
timestart <- as.Date("1945-01-01")
firstday <- as.integer(as.Date("1945-01-01") - timestart + 1) # check this
lastday <- as.integer(as.Date("2012-12-31") - timestart + 1)

# the netcdf files each contain either maximum or minimum temperatures.

max_temp_nc <- nc_open("data/weather_data/pcic/PNWNAmet_tasmax.nc.nc")
min_temp_nc <- nc_open("data/weather_data/pcic/PNWNAmet_tasmin.nc.nc")

# We can also ask the 'nc' object for metadata _about_ the variable we loaded, such as
# getting its name, units, dimensions, etc. Accessing nc$var this way is accessing metadata,
# not the variable itself. That was loaded with ncvar_get.

# get metadata for each variable to use as indexes for extracting the right temperatures


max_temp_meta <- index_get(max_temp_nc)
min_temp_meta <- index_get(min_temp_nc)


locations <- get_closest_ref(site_locs, max_temp_meta) # what is the closest PCIC gridpoint to each site? Should be identical for max and min temp

# extract only needed temperature data from the netcdf files

# This is slow
max_temp <- pick_temp_by_location(locations, max_temp_nc)
min_temp <- pick_temp_by_location(locations, min_temp_nc)

# ncvar_get can't read entire file at once

# Combine all temp and sites ##################

max_temps <- dplyr::rename(max_temp, max_temp=temp_in_c)
min_temps <- dplyr::rename(min_temp, min_temp=temp_in_c)

alltemps <- dplyr::full_join(max_temps, min_temps) %>%
    mutate(mean_temp = (max_temp + min_temp)/2) %>%
    mutate(Date = timestart - 1 + Date) %>%
    rename(Site=site)

#correct site names
alltemps <- alltemps %>%
    mutate(Site = case_when(Site == "Sorrento Seed Orchard" ~ "Sorrento",
                             Site == "Kalamalka Seed Orchard" ~ "Kalamalka",
                             Site == "Vernon Seed Orchard Company" ~ "Vernon",
                             Site == "Pacific Regeneration Technologies" ~ "PRT",
                             Site == "TOLKO" ~ "Tolko",
                             Site == "Kettle River Seed Orchards" ~ "KettleRiver",
                             Site == "Prince George Tree Improvement Station" ~ "PGTIS"))

# Test #

#make sure no date at a site has more than one temperature associated with it
toomanytest <- alltemps %>%
    select(Site, Date, mean_temp) %>%
    group_by(Site, Date) %>%
    summarise(tempsperdate = length(mean_temp)) %>%
    filter(tempsperdate > 1)
nrow(toomanytest)==0 #TRUE

write.csv(alltemps, 'data/weather_data/pcic/seed_orchard_sites_full_timeseries.csv', row.names = FALSE)

# locations of both sites and the closest gridded weather point with corrected elevation ###########
all_locations <- locations %>%
    select(-closest_lat_loc, -closest_lon_loc)

colnames(all_locations) <- c("SeedOrchardSite", "Sitelat", "Sitelon", "Gridlat", "Gridlon")
all_locations$shortname <- c("Sorrento", "Kalamalka", "Vernon", "PRT", "TOLKO", "Kettle River", "Prince George")

# get elevation for sites and gridpoints ###########

siteelevs <- rgbif::elevation(latitude= all_locations$Sitelat, longitude = all_locations$Sitelon, username="susannah2")
colnames(siteelevs) <- c("Sitelat", "Sitelon", "Siteelev")
all_locations <- full_join(siteelevs, all_locations)

# get elevation for gridpoints
gridelevs <- rgbif::elevation(latitude = all_locations$Gridlat, longitude=all_locations$Gridlon, username="susannah2")
colnames(gridelevs) <- c("Gridlat", "Gridlon", "Gridelev")
all_locations <- full_join(gridelevs, all_locations)

# write
write.csv(all_locations, 'data/seed_orchard_sites_pcic.csv', row.names = FALSE)

