# meta for flowerwhen

`netcdf_extract.R` 
- extracts data from netcdf files with [maximum](data/weather_data/pcic/PNWNAmet_tasmax.nc.nc) and [minimum](data/weather_data/pcic/PNWNAmet_tasmin.nc.nc) temperatures for [seed orchard sites](data/seed_orchard_site_coordinates.csv)
- calculates the mean daily temperature for each day of 1945-2012
- writes the mean daily temperature for each day out to a [file](data/weather_data/seed_orchard_sites_pcic_ts.csv)
- combines site locations with gridpoint locations and their elevations and [writes it out](seed_orchard_sites_pcic.csv)