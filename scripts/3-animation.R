### Fires and Caribou ====
# Julie Turner
# Started: 08 January 2024

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

cbou <- readRDS(file.path(raw, 'NWTdat.RDS'))

hotspots <- readRDS(file.path(raw, 'hotspots2023_nwt.RDS'))

progression <- st_read(file.path(raw, 'progression_nwt.shp'))
burns <- st_read(file.path(raw, 'FireShapefiles', 'brnGEE_2023_Merge.shp'))

canPoly <- st_read(file.path(canada, 'CanadaPoly', 'lpr_000b16a_e.shp'))

# prep ----
# pseudo mercator
#crs <- st_crs(3857)$wkt
crs <- st_crs(4326)$wkt

nwt <- filter(canPoly, PREABBR %in% c('N.W.T.'))
nwt.wgs <- st_transform(nwt, crs)

## caribou ----
cbou.MayOct <- cbou[timestamp>= as.POSIXct('2023-05-01', tz='UTC') & timestamp < as.POSIXct('2023-10-01', tz='UTC')]
cbou.MayOct$datetime <- cbou.MayOct$timestamp
ggplot() +
  geom_point(data = cbou.MayOct, aes(x=x, y=y), color = 'purple', show.legend = F) 

summary(cbou.MayOct$habitat)

dehcho <- cbou.MayOct[study_area == 'dehcho']
sahtu <- cbou.MayOct[study_area == 'sahtu']
s.slave <- cbou.MayOct[study_area == 'south.slave']


cbou.coords <- st_as_sf(cbou.MayOct, coords = c('x', 'y')) %>%
  st_set_crs(4326)
dehcho.coords <- st_as_sf(dehcho, coords = c('x', 'y')) %>%
  st_set_crs(4326)
sahtu.coords <- st_as_sf(sahtu, coords = c('x', 'y')) %>%
  st_set_crs(4326)
s.slave.coords <- st_as_sf(s.slave, coords = c('x', 'y')) %>%
  st_set_crs(4326)

cbou.ext <- st_bbox(cbou.coords)
dehcho.ext <- st_bbox(dehcho.coords)
sahtu.ext <- st_bbox(sahtu.coords)
s.slave.ext <- st_bbox(s.slave.coords)

## fires to bou area ----
hotspots.bou <- st_as_sf(hotspots, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(cbou.ext)
plot(hotspots.bou$geometry)

hotspots.dehcho <- st_as_sf(hotspots, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(dehcho.ext)

hotspots.sahtu <- st_as_sf(hotspots, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(sahtu.ext)

hotspots.sslave <- st_as_sf(hotspots, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(s.slave.ext)



hotspots.df <- setDT(sfheaders::sf_to_df(hotspots.bou, fill = T))
hotspots.dehcho.df <- setDT(sfheaders::sf_to_df(hotspots.dehcho, fill = T))
hotspots.sahtu.df <- setDT(sfheaders::sf_to_df(hotspots.sahtu, fill = T))
hotspots.sslave.df <- setDT(sfheaders::sf_to_df(hotspots.sslave, fill = T))


# get baselayer ----
myloc <- st_bbox(cbou.coords)
names(myloc) <- c('left', 'bottom', 'right', 'top')
names(dehcho.ext) <- c('left', 'bottom', 'right', 'top')
names(sahtu.ext) <- c('left', 'bottom', 'right', 'top')
names(s.slave.ext) <- c('left', 'bottom', 'right', 'top')

# setting defaults outside of this b/c need an API from stadiamaps.com
## register_stadiamaps(key = "xxxxx")
nwtmap <- get_map(location = myloc, source = 'stadia', maptype = 'stamen_terrain', 
                  crop = FALSE, zoom = 6)
dehchomap <- get_map(location = dehcho.ext, source = 'stadia', maptype = 'stamen_terrain', 
                  crop = FALSE, zoom = 8)
sahtumap <- get_map(location = sahtu.ext, source = 'stadia', maptype = 'stamen_terrain', 
                     crop = FALSE, zoom = 6)
sslavemap <- get_map(location = s.slave.ext, source = 'stadia', maptype = 'stamen_terrain', 
                     crop = FALSE, zoom = 6)

p.data <- ggmap(nwtmap) +
  geom_point(data = cbou.MayOct, aes(x=x, y=y, group = id, color = id), show.legend = F) +
  # geom_path(data = cbou.MayOct, 
  #           aes(x=x, y=y, group = id, color = id), 
  #           alpha = 0.3, show.legend = F) +
  geom_point(data = hotspots.df, aes(x=x, y=y), color = 'darkorange', show.legend = F) +
  scale_color_viridis_d()
p.data

p.anim <- p.data +
  transition_time(datetime) +
  shadow_trail(distance = 0.01) + 
  ease_aes('linear') +
  labs(title="{frame_time}", x="Longitude", y="Latitude")

#num_frames <- length(unique(cbou.MayOct$datetime))
num_frames <- as.integer(as.POSIXct('2023-10-01', tz='UTC') - as.POSIXct('2023-05-01', tz='UTC'))*3
animate(p.anim, nframes = num_frames)

### Dehcho ----
p.dehcho <- ggmap(dehchomap) +
  geom_point(data = dehcho, aes(x=x, y=y, group = id, color = id), show.legend = F) +
  geom_path(data = dehcho,
            aes(x=x, y=y, group = id, color = id),
            alpha = 0.3, show.legend = F) +
  geom_point(data = hotspots.dehcho.df, aes(x=x, y=y, group = datetime), color = 'darkorange', fill = NA, show.legend = F) +
  scale_color_viridis_d()
p.dehcho

p.dehcho.anim <- p.dehcho +
  transition_reveal(datetime) +
  # transition_time(datetime) +
  # shadow_trail(distance = 0.01, max_frames = 10) + 
  ease_aes('linear') +
  labs(title="{frame_along}", x="Longitude", y="Latitude")

#num_frames <- length(unique(cbou.MayOct$datetime))
num_frames <- as.integer(as.POSIXct('2023-10-01', tz='UTC') - as.POSIXct('2023-05-01', tz='UTC'))*3
animate(p.dehcho.anim, nframes = num_frames, fps = 3)
length(unique(dehcho$id))
