### Fires and Caribou ====
# Julie Turner
# Started: 08 January 2024

# need older `transformr`
#devtools::install_version("transformr", version = "0.1.3")

require(data.table)
require(sf)
require(terra)
require(tidyterra)
require(dplyr)
require(tidyr)
require(amt)
require(ggplot2)
require(ggmap)
require(ggspatial)
require(basemaps)
require(gganimate)
require(gifski)

### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')

cbou <- readRDS(file.path(raw, 'NWTdat.RDS'))

hotspots <- readRDS(file.path(raw, 'hotspots2023_nwt.RDS'))

progression <- st_read(file.path(raw, 'progression_nwt.shp'))
burns <- st_read(file.path(raw, 'FireShapefiles', 'brnGEE_2023_Merge.shp'))


# prep ----
# pseudo mercator
crs.web <- st_crs(3857)$wkt
crs <- st_crs(4326)$wkt


## fires ----
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
fixrate <- cbou %>% make_track(x,y, timestamp, crs = crs, all_cols = T) %>% 
  nest(data = -"id") %>% 
  mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% 
  unnest(cols = c(sr))
quantile(fixrate$median)
# 8 hours

# make a grouping variable for time of day
cbou[,tod:=lubridate::round_date(timestamp, unit = '8 hours')]

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

cbou.coords.df <- setDT(cbou.coords %>%
  st_transform(crs.web) %>%
  sfheaders::sf_to_df(fill = T))


dehcho.coords <- st_as_sf(dehcho, coords = c('x', 'y')) %>%
  st_set_crs(4326)

dehcho.df <- setDT(dehcho.coords %>%
                          st_transform(crs.web) %>%
                          sfheaders::sf_to_df(fill = T))


sahtu.coords <- st_as_sf(sahtu, coords = c('x', 'y')) %>%
  st_set_crs(4326) 

sahtu.df <- setDT(sahtu.coords %>% 
                   st_transform(crs.web) %>%
                     sfheaders::sf_to_df(fill = T))

s.slave.coords <- st_as_sf(s.slave, coords = c('x', 'y')) %>%
  st_set_crs(4326) 

sslave.df <- setDT(s.slave.coords %>%
                     st_transform(crs.web) %>%
                     sfheaders::sf_to_df(fill = T))

# gps points within 15km of the zoomed in fire
enterprise <- cbou.coords %>%
  st_join(enterprise.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
enterprise.df <- setDT(sfheaders::sf_to_df(enterprise, fill = T))
enterprise.df.web <- setDT(enterprise %>%
                     st_transform(crs.web) %>%
                     sfheaders::sf_to_df(fill = T))

dehchozoom <- cbou.coords %>%
  st_join(dehchozoom.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
dehchozoom.df <- setDT(sfheaders::sf_to_df(dehchozoom, fill = T))
dehchozoom.df.web <- setDT(dehchozoom %>%
                             st_transform(crs.web) %>%
                             sfheaders::sf_to_df(fill = T))

sahtuzoom <- cbou.coords %>%
  st_join(sahtuzoom.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
sahtuzoom.df <- setDT(sfheaders::sf_to_df(sahtuzoom, fill = T))
sahtuzoom.df.web <- setDT(sahtuzoom %>%
                             st_transform(crs.web) %>%
                             sfheaders::sf_to_df(fill = T))

cbou.ext <- st_bbox(cbou.coords)
dehcho.ext <- st_bbox(dehcho.coords)
sahtu.ext <- st_bbox(sahtu.coords)
s.slave.ext <- st_bbox(s.slave.coords)
enterprise.ext <- st_bbox(enterprise.f)
dehchozoom.ext <- st_bbox(dehchozoom.f)
sahtuzoom.ext <- st_bbox(sahtuzoom.f)

## fires to bou area ----
hotspots.sub <- hotspots[datetime < as.POSIXct('2023-10-01', tz='UTC')]
# make a grouping variable for time of day
hotspots.sub[, tod := lubridate::ceiling_date(datetime, unit = '8 hours')]
hotspots.bou <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(cbou.ext)
#plot(hotspots.bou$geometry)

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




hotspots.df <- setDT(sfheaders::sf_to_df(hotspots.bou, fill = T))
hotspots.dehcho.df <- setDT(sfheaders::sf_to_df(hotspots.dehcho, fill = T))
hotspots.sahtu.df <- setDT(sfheaders::sf_to_df(hotspots.sahtu, fill = T))
hotspots.sslave.df <- setDT(sfheaders::sf_to_df(hotspots.sslave, fill = T))
hotspots.enterprise.df <- setDT(sfheaders::sf_to_df(hotspots.enterprise, fill = T))
hotspots.dehchozoom.df <- setDT(sfheaders::sf_to_df(hotspots.dehchozoom, fill = T))
hotspots.sahtuzoom.df <- setDT(sfheaders::sf_to_df(hotspots.sahtuzoom, fill = T))

# Don't need these currently
progression.sub <- filter(progression, datetime < as.POSIXct('2023-10-01', tz='UTC'))
progression.sub$datetime <- as.POSIXct(paste0(as.character(progression.sub$DATE), ' 23:59:59'), tz = 'UTC',
                                      format ='%Y%m%d %H:%M:%OS')
progression.sub$tod <- lubridate::ceiling_date(progression.sub$datetime, "8 hours")

progression.bou <- progression.sub %>%
  st_transform(crs.web) %>%
  st_crop(cbou.ext)
#plot(progression.bou$geometry)

progression.dehcho <- progression.sub %>%
  st_crop(dehcho.ext)

progression.sahtu <- progression.sub %>%
  st_crop(sahtu.ext)

progression.sslave <- progression.sub %>%
  st_crop(s.slave.ext)

progression.enterprise <- progression.sub %>%
  st_crop(enterprise.ext)

# get baselayer ----
myloc <- st_bbox(cbou.coords)
names(myloc) <- c('left', 'bottom', 'right', 'top')
names(dehcho.ext) <- c('left', 'bottom', 'right', 'top')
names(sahtu.ext) <- c('left', 'bottom', 'right', 'top')
names(s.slave.ext) <- c('left', 'bottom', 'right', 'top')
names(enterprise.ext) <- c('left', 'bottom', 'right', 'top')
names(dehchozoom.ext) <- c('left', 'bottom', 'right', 'top')
names(sahtuzoom.ext) <- c('left', 'bottom', 'right', 'top')

# setting defaults outside of this b/c need an API from stadiamaps.com
## register_stadiamaps(key = "xxxxx")

# nwtmap <- get_map(location = myloc, source = 'stadia', maptype = 'stamen_toner_lite', 
#                   crop = FALSE, zoom = 6)
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
p.dehcho <- ggmap(dehchomap) +
  geom_point(data = hotspots.dehcho.df, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = dehcho, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d(option = 'mako') 

dehcho.web <- dehcho.coords %>% st_transform(crs.web)
hotspots.dehcho.web <- setDT(hotspots.dehcho %>% 
                               st_transform(crs.web) %>%
                               sfheaders::sf_to_df(fill = T))

p.dehcho <-
 # ggplot() + #annotation_map_tile() +
  basemap_ggplot(ext = dehcho.web, map_service = 'osm', map_type = 'topographic') +
  #basemap_gglayer(ext = dehcho.web, map_service = 'osm', map_type = 'topographic') +
  geom_point(data = hotspots.dehcho.web, aes(x=x, y=y, group = seq_along(tod)),
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = dehcho.df, aes(x=x, y=y, group = id, color = id),
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  theme_bw() +
  #coord_sf(crs = crs) +
  scale_color_viridis_d(option = 'mako')
p.dehcho

p.dehcho.anim <- p.dehcho +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 3) + # 5 for ggmap
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="Longitude", y="Latitude")

num_frames <- length(unique(dehcho$tod))
animate(p.dehcho.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "dehcho.gif"))
animate(p.dehcho.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "dehcho.mp4"))

#### dehcho zoom ----
hotspots.dehchozoom.Aug <- hotspots.dehchozoom.df[datetime >= as.POSIXct('2023-07-01', tz='UTC')]
dehchozoom.Aug <- dehchozoom.df[datetime>= as.POSIXct('2023-08-01', tz='UTC')]

p.dehchozoom <- ggmap(dehchozoommap) +
  geom_point(data = hotspots.dehchozoom.Aug, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = dehchozoom.Aug, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d() 


p.dehchozoom.anim <- p.dehchozoom +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 5) +
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="Longitude", y="Latitude")

num_frames <- length(unique(dehchozoom.Aug$tod))
animate(p.dehchozoom.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "dehchozoom.gif"))
animate(p.dehchozoom.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "dehchozoom.mp4"))



### Sahtú ----
p.sahtu <- ggmap(sahtumap) +
  geom_point(data = hotspots.sahtu.df, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = sahtu, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d() 

sahtu.web <- sahtu.coords %>% st_transform(crs.web)
hotspots.sahtu.web <- setDT(hotspots.sahtu %>% 
                               st_transform(crs.web) %>%
                               sfheaders::sf_to_df(fill = T))

p.sahtu <-
  basemap_ggplot(ext = sahtu.web, map_service = 'osm', map_type = 'topographic') +
  geom_point(data = hotspots.sahtu.web, aes(x=x, y=y, group = seq_along(tod)),
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = sahtu.df, aes(x=x, y=y, group = id, color = id),
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  theme_bw() +
  #coord_sf(crs = crs) +
  scale_color_viridis_d(option = 'mako')
p.sahtu

p.sahtu.anim <- p.sahtu +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 3) + # 5 for ggmap
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="Longitude", y="Latitude")


num_frames <- length(unique(sahtu$tod))
animate(p.sahtu.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "sahtu.gif"))
animate(p.sahtu.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "sahtu.mp4"))

#### sahtu zoom ----
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

p.sahtuzoom.anim <- p.sahtuzoom +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 5) +
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="Longitude", y="Latitude")


num_frames <- length(unique(sahtuzoom.Jul$tod))
animate(p.sahtuzoom.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "sahtuzoom.gif"))
animate(p.sahtuzoom.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "sahtuzoom.mp4"))

### South Slave ----
p.sslave <- ggmap(sslavemap) +
  geom_point(data = hotspots.sslave.df, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = s.slave, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d() 

sslave.web <- s.slave.coords %>% st_transform(crs.web)
hotspots.sslave.web <- setDT(hotspots.sslave %>% 
                               st_transform(crs.web) %>%
                               sfheaders::sf_to_df(fill = T))

p.sslave <- basemap_ggplot(ext = sslave.web, map_service = 'osm', map_type = 'topographic') +
  geom_point(data = hotspots.sslave.web, aes(x=x, y=y, group = seq_along(tod)),
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = sslave.df, aes(x=x, y=y, group = id, color = id),
             size = 2.25, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  theme_bw() +
  #coord_sf(crs = crs) +
  scale_color_viridis_d(option = 'mako')
p.sslave

p.sslave.anim <- p.sslave +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 3) + # 5 for ggmap
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="Longitude", y="Latitude")


num_frames <- length(unique(s.slave$tod))
animate(p.sslave.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "sslave.gif"))
animate(p.sslave.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "sslave.mp4"))



#### Enterprise fire  ----
hotspots.enterprise.Aug <- hotspots.enterprise.df[datetime>= as.POSIXct('2023-08-01', tz='UTC')]
enterprise.Aug <- enterprise.df[datetime>= as.POSIXct('2023-08-01', tz='UTC')]

p.enterprise <- ggmap(enterprisemap) +
  geom_point(data = hotspots.enterprise.Aug, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = enterprise.Aug, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  # geom_path(data = s.slave,
  #           aes(x=x, y=y, group = id, color = id),
  #           alpha = 0.3, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d() 


p.enterprise.anim <- p.enterprise +
  transition_time(tod) +
  shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 5) +
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="Longitude", y="Latitude")

num_frames <- length(unique(enterprise.Aug$tod))
animate(p.enterprise.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "enterprise.gif"))
animate(p.enterprise.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "enterprise.mp4"))


#####
tods <- unique(enterprise.Aug$tod)
prog.hotspots.enterprise <- rbindlist(
  lapply(tods, function(td){
    sub <- hotspots.enterprise.Aug[tod<as.POSIXct(td,tz = 'UTC', format ='%Y%m%d %H:%M:%OS')]
    sub[, tod2 := as.POSIXct(td,tz = 'UTC', format ='%Y%m%d %H:%M:%OS')]
  })
)

setnames(prog.hotspots.enterprise, old = c('tod', 'tod2'), new = c('tod.sing', 'tod'))

p.enterprise2 <- ggmap(enterprisemap) +
  geom_point(data = prog.hotspots.enterprise, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'maroon', show.legend = F) +
  geom_point(data = hotspots.enterprise.Aug, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = enterprise.Aug, aes(x=x, y=y, group = id, color = id), 
             size = 2.25, show.legend = F) +
  # geom_path(data = enterprise.Aug,
  #           aes(x=x, y=y, group = id, color = id),
  #           alpha = 0.3, show.legend = F) +
  scale_color_viridis_d() +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs=crs)
p.enterprise2

p.enterprise.anim2 <- p.enterprise2 +
  #transition_states(tod) +
  transition_time(tod) +
  shadow_wake(wake_length = 0.05, exclude_layer = c(3,4)) +
  #shadow_trail(distance = 0.03, exclude_layer = c(3,4)) + 
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="Longitude", y="Latitude")
animate(p.enterprise.anim2, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "enterprise2.gif"))



p.enterprise <- ggmap(enterprisemap) +
  geom_point(data = hotspots.enterprise.Aug, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'maroon', show.legend = F) +
  geom_point(data = hotspots.enterprise.Aug, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = enterprise.Aug, aes(x=x, y=y, group = id, color = id), show.legend = F) +
  # geom_path(data = s.slave,
  #           aes(x=x, y=y, group = id, color = id),
  #           alpha = 0.3, show.legend = F) +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs = crs) + 
  scale_color_viridis_d() 
p.enterprise

p.enterprise.anim <- p.enterprise +
  transition_time(tod) +
  #transition_states(tod) +
  shadow_wake(wake_length = 0.01, exclude_layer = c(2, 3)) +
  #shadow_mark(color = 'maroon', alpha = 0.25, exclude_layer = 5) +
  #shadow_trail(distance = 0.01, exclude_layer = c(1)) + 
  exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="Longitude", y="Latitude")

num_frames <- length(unique(enterprise.Aug$tod))
animate(p.enterprise.anim, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "enterprise.gif"))
animate(p.enterprise.anim, nframes = num_frames, fps = 2, renderer = ffmpeg_renderer())
anim_save(file.path('anims', "enterprise.mp4"))




progression.enterprise.Aug <- filter(progression.enterprise, datetime>= as.POSIXct('2023-08-01', tz='UTC'))
p.enterprise2 <- ggmap(enterprisemap.bw) +
  geom_sf(data = progression.enterprise.Aug, aes(group = seq_along(tod)), 
          fill = 'maroon', color = 'maroon', inherit.aes = F) +
  geom_point(data = hotspots.enterprise.Aug, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  geom_point(data = enterprise.Aug, aes(x=x, y=y, group = id, color = id), show.legend = F) +
  # geom_path(data = enterprise.Aug,
  #           aes(x=x, y=y, group = id, color = id),
  #           alpha = 0.3, show.legend = F) +
  scale_color_viridis_d() +
  annotation_scale(location = 'tl', width_hint = 0.3) +
  coord_sf(crs=crs)
p.enterprise2

p.enterprise.anim2 <- p.enterprise2 +
  #transition_states(tod) +
  transition_time(tod) +
  #shadow_wake(wake_length = 0.05) +
  shadow_trail(distance = 0.05) + 
  #exit_shrink() +
  ease_aes('linear') +
  labs(title="{frame_time}", x="Longitude", y="Latitude")
animate(p.enterprise.anim2, nframes = num_frames, fps = 2)
anim_save(file.path('anims', "enterprise.gif"))

######OLD######
p.sslave <- ggmap(sslavemap) +
  #geom_polygon(data = progression.sslave, aes(group = datetime), fill = 'maroon', inherit.aes = F) +
  geom_point(data = s.slave, aes(x=x, y=y, group = id, color = id), show.legend = F) +
  geom_path(data = s.slave,
            aes(x=x, y=y, group = id, color = id),
            alpha = 0.3, show.legend = F) +
  geom_point(data = hotspots.sslave.df, aes(x=x, y=y, group = seq_along(tod)), 
             shape = 17, color = 'darkorange', show.legend = F) +
  scale_color_viridis_d()
p.sslave

p.sslave.anim <- p.sslave +
  transition_reveal(tod) +
  # transition_time(datetime) +
  # shadow_trail(distance = 0.01, max_frames = 10) + 
  ease_aes('linear') +
  labs(title="{frame_along}", x="Longitude", y="Latitude")

#num_frames <- as.integer(as.POSIXct('2023-10-01', tz='UTC') - as.POSIXct('2023-05-01', tz='UTC'))*3
num_frames <- length(unique(hotspots.sslave.df$tod))
animate(p.sslave.anim, nframes = num_frames, fps = 2)
anim_save(("sslave_test.gif"))
length(unique(sslave$id))

progression.sslave.prj <- st_transform(progression.sslave, crs.web)
#geom_sf(data = progression.sslave, aes(group=tod), fill = 'maroon', color = 'maroon', inherit.aes = F) +
#geom_point(data = hotspots.sslave.df, aes(x=x, y=y, group = seq_along(tod+1)), 
#           shape = 17, color = 'maroon', show.legend = F) +