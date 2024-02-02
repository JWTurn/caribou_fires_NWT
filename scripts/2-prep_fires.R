### Fires Data ====
# Julie Turner
# Started: 05 January 2024

require(data.table)
require(sf)
require(terra)
require(tidyterra)
require(dplyr)
require(ggplot2)
require(ggmap)
#require(basemaps)
require(gganimate)
require(gifski)

### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')


hotspots <- fread(file.path(raw, 'cwfis', 'hotspots2023.csv'))

perims <- st_read(file.path(raw, 'cwfis', 'perimeters.shp'))

#progression <- st_read(file.path(raw, 'cwfis', 'progression.shp')) # doesn't have associated crs?
progression <- st_read(file.path(raw, 'FireShapefiles', 'progression.shp'))

burns <- st_read(file.path(raw, 'FireShapefiles', 'brnGEE_2023_Merge.shp'))

canPoly <- st_read(file.path(canada, 'CanadaPoly', 'lpr_000b16a_e.shp'))

# prep ----
# pseudo mercator
#crs <- st_crs(3857)$wkt
crs <- st_crs(4326)$wkt
p.crs <- st_crs(progression)$wkt

nwt <- filter(canPoly, PREABBR %in% c('N.W.T.'))
nwt.wgs <- st_transform(nwt, crs)


# get fires within NWT ----
fire_coords <- st_as_sf(hotspots, coords = c('lon', 'lat')) %>%
  st_set_crs(4326) #%>%
  #st_transform(crs)

fire_nwt <- st_join(fire_coords, nwt.wgs, join = st_within) %>%
  filter(PREABBR == 'N.W.T.')
fire_nwt.df <- setDT(sfheaders::sf_to_df(fire_nwt, fill = T))
fire_nwt.df$datetime <- fire_nwt.df$rep_date
saveRDS(fire_nwt.df, file.path(raw, 'hotspots2023_nwt.RDS'))

# I don't think this is catching all fires in the progression
progression_nwt <- progression %>%
 # st_set_crs(p.crs) %>%
  st_transform(crs) %>%
  # st_join(nwt.wgs, join = st_intersects) %>%
  # filter(PREABBR == 'N.W.T.') %>%
  st_intersection(nwt.wgs)
  #st_crop(st_bbox(nwt.wgs))
#progression_nwt <- st_crop(progression_nwt, nwt.wgs)

# create datetime in common format
## set hours to end of day
progression_nwt$datetime <- as.POSIXct(paste0(as.character(progression_nwt$DATE), ' 23:59:59'), tz = 'UTC', 
                                       format ='%Y%m%d %H:%M:%OS')
st_write(progression_nwt, file.path(raw, 'progression_nwt.shp'), append = F)

perims_nwt <- perims %>%
  st_set_crs(p.crs) %>%
  st_transform(crs) %>%
  st_join(nwt.wgs, join = st_intersects) %>%
  filter(PREABBR == 'N.W.T.')

# take a quick look
ggplot() +
  geom_sf(data = nwt.wgs) +
  geom_point(data = fire_nwt.df, aes(x=x, y=y, color = 'darkorange'), shape = 17, size = 0.1, show.legend = F) +
  geom_sf(data = progression_nwt)


# get baselayer ----
myloc <- st_bbox(nwt.wgs)
names(myloc) <- c('left', 'bottom', 'right', 'top')

# setting defaults outside of this b/c need an API from stadiamaps.com
## register_stadiamaps(key = "xxxxx")
nwtmap <- get_map(location = myloc, source = 'stadia', maptype = 'stamen_terrain', crop = F)

ggmap(nwtmap)

# hotspot animation
p.fire.data <- ggmap(nwtmap) +
  geom_point(data = fire_nwt.df, aes(x=x, y=y, color = 'darkorange'), show.legend = F) 
p.fire.data

p.prog.data <- ggmap(nwtmap) +
  geom_sf(data = progression_nwt, inherit.aes = FALSE) 
p.prog.data

p.fire.prog.data <- ggmap(nwtmap) +
  geom_sf(data = progression_nwt, inherit.aes = FALSE, color = 'maroon') +
  geom_point(data = fire_nwt.df, aes(x=x, y=y, color = 'darkorange'), size =0.05, 
             show.legend = F) +
  coord_sf(crs = crs)
p.fire.prog.data
  
p.fire.anim <- p.fire.data +
  shadow_wake(wake_length = 0.5) +
  transition_time(datetime) +
  labs(title="{frame_time}", x="Longitude", y="Latitude")

num_frames <- length(unique(fire_nwt.df$datetime))
animate(p.fire.anim, nframes = 100, fps = 2)


###########
# mapbox.com for `basemaps`
## set_defaults(map_service = "mapbox", map_token = 'usr_API')

ggplot() +
  basemap_gglayer(myloc, map_type = 'light')

