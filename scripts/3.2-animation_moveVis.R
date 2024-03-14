### Fires and Caribou - MoveVis====
# Julie Turner
# Started: 08 January 2024

##############################################################################
############ At this point, this doesn't work for the fire ###################
############ animations, just animal movement.             ###################
##############################################################################

# Sarah Davidson suggested using [Ecodata software](https://www.movebank.org/cms/movebank-content/ecodata) if want to use `moveVis`

#devtools::install_github("16EAGLE/moveVis")

require(data.table)
require(sf)
require(terra)
require(tidyterra)
require(dplyr)
require(tidyr)
require(amt)
require(ggplot2)
require(ggmap)
require(moveVis)
require(move2)
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
crs.web <- st_crs(3857)$wkt
crs <- st_crs(4326)$wkt

nwt <- filter(canPoly, PREABBR %in% c('N.W.T.'))
nwt.wgs <- st_transform(nwt, crs)

## caribou ----
fixrate <- cbou %>% make_track(x,y, timestamp, crs = crs, all_cols = T) %>% 
  nest(data = -"id") %>% 
  mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% 
  unnest(cols = c(sr))
quantile(fixrate$median)
# 8 hours

# make a grouping variable for time of day
cbou[,tod:=lubridate::ceiling_date(timestamp, unit = '8 hours')]

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
hotspots.sub <- hotspots[datetime < as.POSIXct('2023-10-01', tz='UTC')]
# make a grouping variable for time of day
hotspots.sub[, tod := lubridate::ceiling_date(datetime, unit = '8 hours')]
hotspots.bou <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(cbou.ext)
plot(hotspots.bou$geometry)

hotspots.dehcho <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(dehcho.ext)

hotspots.sahtu <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(sahtu.ext)

hotspots.sslave <- st_as_sf(hotspots.sub, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>%
  st_crop(s.slave.ext)



hotspots.df <- setDT(sfheaders::sf_to_df(hotspots.bou, fill = T))
hotspots.dehcho.df <- setDT(sfheaders::sf_to_df(hotspots.dehcho, fill = T))
hotspots.sahtu.df <- setDT(sfheaders::sf_to_df(hotspots.sahtu, fill = T))
hotspots.sslave.df <- setDT(sfheaders::sf_to_df(hotspots.sslave, fill = T))


progression$tod <- lubridate::ceiling_date(progression$datetime, "8 hours")
progression.sub <- filter(progression, datetime < as.POSIXct('2023-10-01', tz='UTC'))
progression.sub$datetime <- as.POSIXct(paste0(as.character(progression.sub$DATE), ' 23:59:59'), tz = 'UTC', 
                                       format ='%Y%m%d %H:%M:%OS')

progression.bou <- progression.sub %>%
  st_crop(cbou.ext)
plot(progression.bou$geometry)

progression.dehcho <- progression.sub %>%
  st_crop(dehcho.ext)

progression.sahtu <- progression.sub %>%
  st_crop(sahtu.ext)

progression.sslave <- progression.sub %>%
  st_crop(s.slave.ext)



# set up for moveVis ----

dehcho.move <- df2move(dehcho, crs, 'x', 'y', time = 'timestamp', track_id = 'id')
dehcho.move <- align_move(dehcho.move, res = 8, unit = 'hours')

# need to load osm_token outside since that's personal
dehcho.frames <- frames_spatial(dehcho.move, map_service = "mapbox", map_type = "hybrid", 
                         equidistant = FALSE, map_token = mb_token, path_legend = F,# path_legend_title = "Geese", 
                         alpha = 0.5)
dehcho.frames[[100]]
dehcho.fires <- add_gg(dehcho.frames, 
                       gg = expr(geom_point(data = hotspots.sslave.df, aes(x=x, y=y), 
                                            shape = 17, color = 'darkorange', show.legend = F)))
dehcho.fires[[100]]

dehcho.frames.labs <- add_labels(dehcho.fires, x = "Longitude", y = "Latitude") %>% # axis labels
  add_progress() %>% # progress bar
  add_scalebar() %>% # scale bar
  add_northarrow() %>% # north arrow
  add_timestamps(dehcho.move, type = "label") # timestamps
dehcho.frames.labs[[100]]


animate_frames(dehcho.frames.labs, file.path('anims_wip', 'dehchoMV_test.mp4'), fps = 5, 
               width = 500, height = 800, res = 100, 
               display = TRUE, overwrite = TRUE, verbose = TRUE)


