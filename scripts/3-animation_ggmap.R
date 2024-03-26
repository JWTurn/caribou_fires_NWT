# Fires and Caribou animations ====
# using `ggmap` base maps
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
require(ggmap)
require(gganimate)
require(gifski)

### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')

# downloaded data from movebank
cbou <- readRDS(file.path(raw, 'NWTdat.RDS'))

# prepared fires data
hotspots <- readRDS(file.path(derived, 'hotspots2023_nwt.RDS'))

progression <- st_read(file.path(derived, 'progression_nwt.shp'))
burns <- st_read(file.path(raw, 'FireShapefiles', 'brnGEE_2023_Merge.shp'))


# prep ----
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

cbou.coords.df <- setDT(cbou.coords %>%
  sfheaders::sf_to_df(fill = T))


dehcho.coords <- st_as_sf(dehcho, coords = c('x', 'y')) %>%
  st_set_crs(4326)

dehcho.df <- setDT(dehcho.coords %>%
                   sfheaders::sf_to_df(fill = T))


sahtu.coords <- st_as_sf(sahtu, coords = c('x', 'y')) %>%
  st_set_crs(4326) 

sahtu.df <- setDT(sahtu.coords %>% 
                     sfheaders::sf_to_df(fill = T))

s.slave.coords <- st_as_sf(s.slave, coords = c('x', 'y')) %>%
  st_set_crs(4326) 

sslave.df <- setDT(s.slave.coords %>%
                     sfheaders::sf_to_df(fill = T))

# caribou gps points within 15km of the zoomed in fire to define area
# also creating with crs
enterprise <- cbou.coords %>%
  st_join(enterprise.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
enterprise.df <- setDT(sfheaders::sf_to_df(enterprise, fill = T))


dehchozoom <- cbou.coords %>%
  st_join(dehchozoom.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
dehchozoom.df <- setDT(sfheaders::sf_to_df(dehchozoom, fill = T))


sahtuzoom <- cbou.coords %>%
  st_join(sahtuzoom.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
sahtuzoom.df <- setDT(sfheaders::sf_to_df(sahtuzoom, fill = T))


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

# make a grouping variable for time of day to align for animations
hotspots.sub[, tod := lubridate::ceiling_date(datetime, unit = '8 hours')]
hotspots.bou <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(cbou.ext)


hotspots.dehcho <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(dehcho.ext)

hotspots.sahtu <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(sahtu.ext)

hotspots.sslave <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(s.slave.ext)

hotspots.enterprise <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(enterprise.ext)

hotspots.dehchozoom <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(dehchozoom.ext)

hotspots.sahtuzoom <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(sahtuzoom.ext)



# turn sf objects into dataframes for easier plotting
hotspots.df <- setDT(sfheaders::sf_to_df(hotspots.bou, fill = T))
hotspots.dehcho.df <- setDT(sfheaders::sf_to_df(hotspots.dehcho, fill = T))
hotspots.sahtu.df <- setDT(sfheaders::sf_to_df(hotspots.sahtu, fill = T))
hotspots.sslave.df <- setDT(sfheaders::sf_to_df(hotspots.sslave, fill = T))
hotspots.enterprise.df <- setDT(sfheaders::sf_to_df(hotspots.enterprise, fill = T))
hotspots.dehchozoom.df <- setDT(sfheaders::sf_to_df(hotspots.dehchozoom, fill = T))
hotspots.sahtuzoom.df <- setDT(sfheaders::sf_to_df(hotspots.sahtuzoom, fill = T))


## Not using the progressions currently about leaving the prep here in case we change our minds
progression.sub <- filter(progression, datetime < as.POSIXct('2023-10-01', tz='UTC'))
progression.sub$datetime <- as.POSIXct(paste0(as.character(progression.sub$DATE), ' 23:59:59'), tz = 'UTC',
                                      format ='%Y%m%d %H:%M:%OS')
progression.sub$tod <- lubridate::ceiling_date(progression.sub$datetime, "8 hours")

progression.bou <- progression.sub %>%
  st_transform(crs) %>%
  st_crop(cbou.ext)


progression.dehcho <- progression.sub %>%
  st_crop(dehcho.ext)

progression.sahtu <- progression.sub %>%
  st_crop(sahtu.ext)

progression.sslave <- progression.sub %>%
  st_crop(s.slave.ext)

progression.enterprise <- progression.sub %>%
  st_crop(enterprise.ext)

# get baselayer ----
## if using ggmap baselayers, need to change labels on extents
myloc <- st_bbox(cbou.coords)
names(myloc) <- c('left', 'bottom', 'right', 'top')
names(dehcho.ext) <- c('left', 'bottom', 'right', 'top')
names(sahtu.ext) <- c('left', 'bottom', 'right', 'top')
names(s.slave.ext) <- c('left', 'bottom', 'right', 'top')
names(enterprise.ext) <- c('left', 'bottom', 'right', 'top')
names(dehchozoom.ext) <- c('left', 'bottom', 'right', 'top')
names(sahtuzoom.ext) <- c('left', 'bottom', 'right', 'top')

# setting defaults outside of this script b/c need an API from stadiamaps.com -- don't share personal info
## stadia maps are free, but most free maps are starting to need APIs
## register_stadiamaps(key = "xxxxx")

dehchomap <- get_map(location = dehcho.ext, source = 'stadia', maptype = 'stamen_terrain_background', 
                  crop = FALSE, zoom = 8)
sahtumap <- get_map(location = sahtu.ext, source = 'stadia', maptype = 'stamen_terrain_background', 
                     crop = FALSE, zoom = 8)
sslavemap <- get_map(location = s.slave.ext, source = 'stadia', maptype = 'stamen_terrain_background', 
                     crop = FALSE, zoom = 9)

enterprisemap <- get_map(location = enterprise.ext, source = 'stadia', maptype = 'stamen_toner_lite', 
                    crop = FALSE, zoom = 10)
dehchozoommap <- get_map(location = dehchozoom.ext, source = 'stadia', maptype = 'stamen_toner_lite', 
                         crop = FALSE, zoom = 10)
sahtuzoommap <- get_map(location = sahtuzoom.ext, source = 'stadia', maptype = 'stamen_toner_lite', 
                         crop = FALSE, zoom = 10)



### Dehcho ----
# option for `ggmap` baselayer
p.dehcho <- ggmap(dehchomap) +
  geom_point(data = hotspots.dehcho.df, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = dehcho, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d(option = 'mako') 
p.dehcho

# animation 
p.dehcho.anim <- p.dehcho +
  transition_time(tod) +
  # create aftermark of hotspots only
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 5) + 
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
hotspots.dehchozoom.Aug <- hotspots.dehchozoom.df[datetime >= as.POSIXct('2023-07-01', tz='UTC')]
dehchozoom.Aug <- dehchozoom.df[datetime>= as.POSIXct('2023-07-01', tz='UTC')]

p.dehchozoom <- ggmap(dehchozoommap) +
  geom_point(data = hotspots.dehchozoom.Aug, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = dehchozoom.Aug, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d() 

p.dehchozoom

# animation
p.dehchozoom.anim <- p.dehchozoom +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 5) + 
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
# option for `ggmap` baselayer
p.sahtu <- ggmap(sahtumap) +
  geom_point(data = hotspots.sahtu.df, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = sahtu, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d() 

p.sahtu

# animation
p.sahtu.anim <- p.sahtu +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 5) + 
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
hotspots.sahtuzoom.Jul <- hotspots.sahtuzoom.df[datetime %between% c(as.POSIXct('2023-07-01', tz='UTC'),
                                                as.POSIXct('2023-08-01', tz='UTC'))]
sahtuzoom.Jul <- sahtuzoom.df[datetime %between% c(as.POSIXct('2023-07-01', tz='UTC'),
                                                   as.POSIXct('2023-08-01', tz='UTC'))]

p.sahtuzoom <- ggmap(sahtuzoommap) +
  geom_point(data = hotspots.sahtuzoom.Jul, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = sahtuzoom.Jul, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d() 

p.sahtuzoom

# animation 
p.sahtuzoom.anim <- p.sahtuzoom +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 5) + 
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
# option for `ggmap` baselayer
p.sslave <- ggmap(sslavemap) +
  geom_point(data = hotspots.sslave.df, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = s.slave, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d() 

p.sslave

# animation
p.sslave.anim <- p.sslave +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 5) + 
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
hotspots.enterprise.Aug <- hotspots.enterprise.df[datetime>= as.POSIXct('2023-08-01', tz='UTC')]
enterprise.Aug <- enterprise.df[datetime>= as.POSIXct('2023-08-01', tz='UTC')]

p.enterprise <- ggmap(enterprisemap) +
  geom_point(data = hotspots.enterprise.Aug, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = enterprise.Aug, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d() 

p.enterprise

# animation
p.enterprise.anim <- p.enterprise +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 5) + 
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

