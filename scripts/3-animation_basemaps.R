# Fires and Caribou animations ====
# using `basemaps` base maps
# Julie Turner
# Started: 08 January 2024

## load packages ----
# need older `transformr` for `gganimate`
#devtools::install_version("transformr", version = "0.1.3")

require(data.table)
require(sf)
require(terra)
require(tidyterra)
require(dplyr)
require(tidyr)
require(amt)
require(ggplot2)
require(ggspatial)
require(basemaps)
require(gganimate)
require(gifski)

### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')

# downloaded data from movebank
cbou <- readRDS(file.path(raw, 'NWTdat.RDS'))

# prepared fires data
hotspots <- readRDS(file.path(derived, 'hotspots2023_nwt.RDS'))

burns <- st_read(file.path(raw, 'FireShapefiles', 'brnGEE_2023_Merge.shp'))


# prep ----
# pseudo mercator for web maps (needed for `basemaps` baselayers)
crs.web <- st_crs(3857)$wkt
crs <- st_crs(4326)$wkt


## focal fires ----
# 15km buffered enterprise fire
enterprise.f <- burns %>%
  filter(FIRENUM == 11345411) %>%
  st_buffer(dist = 15000) %>%
  st_transform(crs = crs) 
plot(enterprise.f$geometry)

# 15km buffered dehcho zoom fire
dehchozoom.f <- burns %>%
  filter(FIRENUM == 11345439) %>%
  st_buffer(dist = 15000) %>%
  st_transform(crs = crs) 
plot(dehchozoom.f$geometry)

# 15km buffered sahtu zoom fire
sahtuzoom.f <- burns %>%
  filter(FIRENUM == 11346855) %>%
  st_buffer(dist = 15000) %>%
  st_transform(crs = crs) 
plot(sahtuzoom.f$geometry)

## caribou ----
# double checking fixrate so can chose how to align timings
fixrate <- cbou %>% make_track(x,y, timestamp, crs = crs, all_cols = T) %>% 
  nest(data = -"id") %>% 
  mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% 
  unnest(cols = c(sr))
quantile(fixrate$median)
# 8 hours

# make a grouping variable for time of day to align timings for animations
cbou[,tod:=lubridate::round_date(timestamp, unit = '8 hours')]

# limit caribou data to "fire season"
cbou.MayOct <- cbou[timestamp>= as.POSIXct('2023-05-01', tz='UTC') & timestamp < as.POSIXct('2023-10-01', tz='UTC')]
# create common datetime label across data
cbou.MayOct$datetime <- cbou.MayOct$timestamp

# take a quick look
ggplot() +
  geom_point(data = cbou.MayOct, aes(x=x, y=y), color = 'purple', show.legend = F) 

# just checking out how many points per study herd at this point
summary(cbou.MayOct$habitat)

## prepare data for mapping ----
# sub datasets for each study area
dehcho <- cbou.MayOct[study_area == 'dehcho']
sahtu <- cbou.MayOct[study_area == 'sahtu']
s.slave <- cbou.MayOct[study_area == 'south.slave']


# create study area coords to define extents and make dfs for mapping
cbou.coords <- st_as_sf(cbou.MayOct, coords = c('x', 'y')) %>%
  st_set_crs(4326) 

dehcho.coords <- st_as_sf(dehcho, coords = c('x', 'y')) %>%
  st_set_crs(4326)
dehcho.web <- dehcho.coords %>% st_transform(crs.web)
dehcho.web.df <- setDT(dehcho.web %>%
                         sfheaders::sf_to_df(fill = T))


sahtu.coords <- st_as_sf(sahtu, coords = c('x', 'y')) %>%
  st_set_crs(4326) 
sahtu.web <- sahtu.coords %>% st_transform(crs.web)
sahtu.web.df <- setDT(sahtu.web %>%
                        sfheaders::sf_to_df(fill = T))


s.slave.coords <- st_as_sf(s.slave, coords = c('x', 'y')) %>%
  st_set_crs(4326) 
sslave.web <- s.slave.coords %>% st_transform(crs.web)
sslave.web.df <- setDT(sslave.web %>%
                         sfheaders::sf_to_df(fill = T))


# caribou gps points within 15km of the zoomed in fire to define area
# also creating with web crs
enterprise <- cbou.coords %>%
  st_join(enterprise.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
enterprise.df.web <- setDT(enterprise %>%
                             st_transform(crs.web) %>%
                             sfheaders::sf_to_df(fill = T))

dehchozoom <- cbou.coords %>%
  st_join(dehchozoom.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
dehchozoom.df.web <- setDT(dehchozoom %>%
                             st_transform(crs.web) %>%
                             sfheaders::sf_to_df(fill = T))

sahtuzoom <- cbou.coords %>%
  st_join(sahtuzoom.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
sahtuzoom.df.web <- setDT(sahtuzoom %>%
                            st_transform(crs.web) %>%
                            sfheaders::sf_to_df(fill = T))

## now finally defining the extents
cbou.ext <- st_bbox(cbou.coords)
dehcho.ext <- st_bbox(dehcho.coords)
sahtu.ext <- st_bbox(sahtu.coords)
s.slave.ext <- st_bbox(s.slave.coords)
enterprise.ext <- st_bbox(enterprise.f)
dehchozoom.ext <- st_bbox(dehchozoom.f)
sahtuzoom.ext <- st_bbox(sahtuzoom.f)

## subset fires data to areas with caribou ----
# ignore fires after October
hotspots.sub <- hotspots[datetime < as.POSIXct('2023-10-01', tz='UTC')]

# make a grouping variable for time of day to align for animations and create dfs for easier plotting
hotspots.sub[, tod := lubridate::ceiling_date(datetime, unit = '8 hours')]
hotspots.bou <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(cbou.ext)


hotspots.dehcho <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(dehcho.ext)
hotspots.dehcho.web <- setDT(hotspots.dehcho %>% 
                               st_transform(crs.web) %>%
                               sfheaders::sf_to_df(fill = T))

hotspots.sahtu <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(sahtu.ext)
hotspots.sahtu.web <- setDT(hotspots.sahtu %>% 
                              st_transform(crs.web) %>%
                              sfheaders::sf_to_df(fill = T))

hotspots.sslave <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(s.slave.ext)
hotspots.sslave.web <- setDT(hotspots.sslave %>% 
                               st_transform(crs.web) %>%
                               sfheaders::sf_to_df(fill = T))


hotspots.enterprise <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(enterprise.ext)

hotspots.dehchozoom <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(dehchozoom.ext)

hotspots.sahtuzoom <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(sahtuzoom.ext)



### Dehcho ----
# option for `basemap` baselayer

p.dehcho <- basemap_ggplot(ext = dehcho.web, map_service = 'osm', map_type = 'topographic') +
  geom_point(data = hotspots.dehcho.web, aes(x=x, y=y, group = seq_along(tod)),
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = dehcho.web.df, aes(x=x, y=y, group = id, color = id),
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  theme_bw() +
  scale_color_viridis_d(option = 'mako')
p.dehcho

# animation 
p.dehcho.anim <- p.dehcho +
  transition_time(tod) +
  # create aftermark of hotspots only
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 3) +
  # shrinking trail as points disappear
  exit_shrink() +
  # makes animations smoother
  ease_aes('linear') +
  labs(title="{frame_time}")

num_frames <- length(unique(dehcho$tod))
# animates as gif
animate(p.dehcho.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "dehcho.gif"))
# animate as mp4
animate(p.dehcho.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "dehcho.mp4"))

#### dehcho zoom ----
# filter to right time period for the specific fire
# option for `basemap` baselayer
dehchozoom.Aug <- dehchozoom.df.web[datetime>= as.POSIXct('2023-08-01', tz='UTC')]
dehchozoom.web <- dehchozoom.f %>% st_transform(crs.web)
hotspots.dehchozoom.web <- hotspots.dehchozoom %>% 
  filter(datetime >= as.POSIXct('2023-08-01', tz='UTC')) %>% 
  st_transform(crs.web) 
hotspots.dehchozoom.web.df <- setDT(sfheaders::sf_to_df(hotspots.dehchozoom.web, fill = T))

p.dehchozoom <- basemap_ggplot(ext = dehchozoom.web, map_service = 'osm', map_type = 'topographic') +
  geom_point(data = hotspots.dehchozoom.web.df, aes(x=x, y=y, group = seq_along(tod)),
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = dehchozoom.Aug, aes(x=x, y=y, group = id, color = id),
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  theme_bw() +
  scale_color_viridis_d(option = 'mako')
p.dehchozoom

# animation 
p.dehchozoom.anim <- p.dehchozoom +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 3) + 
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="", y="")

num_frames <- length(unique(dehchozoom.Aug$tod))
# animate gif
animate(p.dehchozoom.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "dehchozoom.gif"))
# animate mp4
animate(p.dehchozoom.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "dehchozoom.mp4"))



### Sahtú ----
# option for `basemap` baselayer

p.sahtu <-
  basemap_ggplot(ext = sahtu.web, map_service = 'osm', map_type = 'topographic') +
  geom_point(data = hotspots.sahtu.web, aes(x=x, y=y, group = seq_along(tod)),
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = sahtu.web.df, aes(x=x, y=y, group = id, color = id),
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  theme_bw() +
  scale_color_viridis_d(option = 'mako')
p.sahtu

# animation 
p.sahtu.anim <- p.sahtu +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 3) + 
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="Longitude", y="Latitude")


num_frames <- length(unique(sahtu$tod))
# animate gif
animate(p.sahtu.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "sahtu.gif"))
# animate mp4
animate(p.sahtu.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "sahtu.mp4"))

#### sahtu zoom ----
# filter to time frame relevant for specfic fire
# option for `basemap` baselayer
sahtuzoom.Jul <- sahtuzoom.df.web[datetime %between% c(as.POSIXct('2023-07-01', tz='UTC'),
                                                       as.POSIXct('2023-08-01', tz='UTC'))]
sahtuzoom.web <- sahtuzoom.f %>% st_transform(crs.web)
hotspots.sahtuzoom.web <- hotspots.sahtuzoom %>% 
  filter(dplyr::between(datetime, as.POSIXct('2023-07-01', tz='UTC'),
                        as.POSIXct('2023-08-01', tz='UTC'))) %>% 
  st_transform(crs.web) 
hotspots.sahtuzoom.web.df <- setDT(sfheaders::sf_to_df(hotspots.sahtuzoom.web, fill = T))

p.sahtuzoom <- basemap_ggplot(ext = sahtuzoom.web, map_service = 'osm', map_type = 'topographic') +
  geom_point(data = hotspots.sahtuzoom.web.df, aes(x=x, y=y, group = seq_along(tod)),
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = sahtuzoom.Jul, aes(x=x, y=y, group = id, color = id),
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  theme_bw() +
  scale_color_viridis_d(option = 'viridis')
p.sahtuzoom

# animation
p.sahtuzoom.anim <- p.sahtuzoom +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 3) + 
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="", y="")



num_frames <- length(unique(sahtuzoom.Jul$tod))
# animate gif
animate(p.sahtuzoom.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "sahtuzoom.gif"))
# animate mp4
animate(p.sahtuzoom.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "sahtuzoom.mp4"))

### South Slave ----
# option for `basemap` baselayer

p.sslave <- basemap_ggplot(ext = sslave.web, map_service = 'osm', map_type = 'topographic') +
  geom_point(data = hotspots.sslave.web, aes(x=x, y=y, group = seq_along(tod)),
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = sslave.web.df, aes(x=x, y=y, group = id, color = id),
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  theme_bw() +
  scale_color_viridis_d(option = 'mako')
p.sslave

# animation 
p.sslave.anim <- p.sslave +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 3) + 
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="", y="")


num_frames <- length(unique(s.slave$tod))
# animate gif
animate(p.sslave.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "sslave.gif"))
# animate mp4
animate(p.sslave.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "sslave.mp4"))



#### Enterprise fire  ----
# filter to time frame for specific fire
# option for `basemap` baselayer
enterprise.Aug <- enterprise.df.web[datetime>= as.POSIXct('2023-08-01', tz='UTC')]
enterprise.web <- enterprise.f %>% st_transform(crs.web)
hotspots.enterprise.web <- hotspots.enterprise %>% 
  filter(datetime >= as.POSIXct('2023-08-01', tz='UTC')) %>% 
  st_transform(crs.web) 
hotspots.enterprise.web.df <- setDT(sfheaders::sf_to_df(hotspots.enterprise.web, fill = T))

p.enterprise <- basemap_ggplot(ext = enterprise.web, map_service = 'osm', map_type = 'topographic') +
  geom_point(data = hotspots.enterprise.web.df, aes(x=x, y=y, group = seq_along(tod)),
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = enterprise.Aug, aes(x=x, y=y, group = id, color = id),
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  theme_bw() +
  scale_color_viridis_d(option = 'mako')
p.enterprise

# animation based on either option
p.enterprise.anim <- p.enterprise +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 3) + 
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="", y="")


num_frames <- length(unique(enterprise.Aug$tod))
# animate gif
animate(p.enterprise.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "enterprise.gif"))
# animate mp4
animate(p.enterprise.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "enterprise.mp4"))

