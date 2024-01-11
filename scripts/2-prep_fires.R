### Fires Data ====
# Julie Turner
# Started: 05 January 2024

require(data.table)
require(sf)
require(terra)
require(dplyr)
require(ggplot2)
require(ggmap)
require(gganimate)
require(gifski)

### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')


hotspots <- fread(file.path(raw, 'hotspots2023.csv'))

progression <- st_read(file.path(raw, 'FireShapefiles', 'progression.shp'))
burns <- st_read(file.path(raw, 'FireShapefiles', 'brnGEE_2023_Merge.shp'))

canPoly <- st_read(file.path(canada, 'CanadaPoly', 'lpr_000b16a_e.shp'))

# prep ----
crs <- st_crs(4326)$wkt

nwt <- filter(canPoly, PREABBR %in% c('N.W.T.'))
nwt.wgs <- st_transform(nwt, crs)


# get fires within NWT ----
fire_coords <- st_as_sf(hotspots, coords = c('lon', 'lat')) %>%
  st_set_crs(crs) 

fire_nwt <- st_join(fire_coords, nwt.wgs, join = st_within) %>%
  filter(PREABBR == 'N.W.T.')

progression_nwt <- progression %>%
  st_transform(crs) %>%
  st_join(nwt.wgs, join = st_within) %>%
  filter(PREABBR == 'N.W.T.')

