# Fires Data ====
# Julie Turner
# Started: 05 January 2024

## load packages ----
require(data.table)
require(sf)
require(terra)
require(tidyterra)
require(dplyr)
require(ggplot2)


### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')


#### from the cwfis files on the datamart https://cwfis.cfs.nrcan.gc.ca/datamart ----
# M3 hotspots
hotspots <- fread(file.path(raw, 'cwfis', 'hotspots2023.csv'))

#### files from GNWT ----
# fire progression shapefile
# pretty sure this is the same as the one progressions from the datamart
progression <- st_read(file.path(raw, 'FireShapefiles', 'progression.shp'))

# GNWT made file
burns <- st_read(file.path(raw, 'FireShapefiles', 'brnGEE_2023_Merge.shp'))

# canada polygon downloaded from statscan https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
# used to limit data to that within NWT
canPoly <- st_read(file.path(raw, 'CanadaPoly', 'lpr_000b16a_e.shp'))

# prep ----
# set crs
crs <- st_crs(4326)$wkt
p.crs <- st_crs(progression)$wkt

# make NWT polygon
nwt <- filter(canPoly, PREABBR %in% c('N.W.T.'))
# transform to common crs
nwt.wgs <- st_transform(nwt, crs)


# get fires within NWT ----
## hotspots ----
# make sf object for future filtering
fire_coords <- st_as_sf(hotspots, coords = c('lon', 'lat')) %>%
  st_set_crs(4326) 

# join and filter to points within NWT
fire_nwt <- st_join(fire_coords, nwt.wgs, join = st_within) %>%
  filter(PREABBR == 'N.W.T.')
fire_nwt.df <- setDT(sfheaders::sf_to_df(fire_nwt, fill = T))
fire_nwt.df$datetime <- fire_nwt.df$rep_date

#save NWT hotspots
saveRDS(fire_nwt.df, file.path(raw, 'hotspots2023_nwt.RDS'))

## fire progressions ----
# filter to those in NWT polygon
progression_nwt <- progression %>%
  st_transform(crs) %>%
  st_intersection(nwt.wgs)


# create datetime in common format
## set hours to end of day so there's a full datetime
progression_nwt$datetime <- as.POSIXct(paste0(as.character(progression_nwt$DATE), ' 23:59:59'), tz = 'UTC', 
                                       format ='%Y%m%d %H:%M:%OS')
# save NWT progressions
st_write(progression_nwt, file.path(raw, 'progression_nwt.shp'), append = F)



# take a quick look
ggplot() +
  geom_sf(data = nwt.wgs) +
  geom_point(data = fire_nwt.df, aes(x=x, y=y, color = 'darkorange'), shape = 17, size = 0.1, show.legend = F) +
  geom_sf(data = progression_nwt)


