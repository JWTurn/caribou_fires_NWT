# Fires and Caribou movement summary ====
# Julie Turner
# Started: 11 March 2024

## load packages ----

require(data.table)
require(sf)
require(terra)
require(tidyterra)
require(dplyr)
require(tidyr)
require(amt)
require(ggplot2)
require(viridis)

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
# use NWT crs for accurate step length estimates
crs.web <- st_crs(3580)$wkt

## focal fires ----
# 15km buffered enterprise fire
enterprise.f <- burns %>%
  filter(FIRENUM == 11345411) %>%
  st_buffer(dist = 15000) %>%
  st_transform(crs = crs.web) 
plot(enterprise.f$geometry)

# 15km buffered dehcho zoom fire
dehchozoom.f <- burns %>%
  filter(FIRENUM == 11345439) %>%
  st_buffer(dist = 15000) %>%
  st_transform(crs = crs.web) 
plot(dehchozoom.f$geometry)

# 15km buffered sahtu zoom fire
sahtuzoom.f <- burns %>%
  filter(FIRENUM == 11346855) %>%
  st_buffer(dist = 15000) %>%
  st_transform(crs = crs.web) 
plot(sahtuzoom.f$geometry)


progression.proj <- progression %>%
  st_transform(crs.web)

# limit caribou data to "fire season"
cbou.MayOct <- cbou[timestamp>= as.POSIXct('2023-05-01', tz='UTC') & timestamp < as.POSIXct('2023-10-01', tz='UTC')]
# create common datetime label across data
cbou.MayOct$datetime <- cbou.MayOct$timestamp

# project for easier distance calcs
cbou.proj <- setDT(st_as_sf(cbou.MayOct, coords = c('x', 'y')) %>%
  st_set_crs(4326) %>% 
  st_transform(crs.web) %>%
  sfheaders::sf_to_df(fill = T))



trk <- cbou.proj %>% make_track(x,y, datetime, crs = crs.web, all_cols = T) %>%
  nest(data = -'id') %>%
  mutate(steps = map(data, function(x) 
    x %>% track_resample(rate = hours(8), tolerance = minutes(20)) %>%
      steps_by_burst(keep_cols = 'end'))) %>%
  select(id, steps) %>% unnest(cols = steps)

trk.dt <- setDT(trk)

# make an sf object
cbou.coords <- st_as_sf(trk.dt, coords = c('x1_', 'y1_')) %>%
  st_set_crs(crs.web)

# Join locs and fires based on intersections
joined <- st_join(
  cbou.coords,
  progression.proj,
  st_intersects
)

# Cast as a data.table 
cbou.fires <- setDT(sfheaders::sf_to_df(joined, fill = T))

# Check when loc intersects fire polygon on the same date
cbou.fires[abs(difftime(t1_, datetime, unit = 'days')) < 1]


# time since fire on the landscape (negative means before fire happened)
cbou.fires[,diff.time := difftime(t1_, datetime, unit = 'days')]
# are they in a fire or not
cbou.fires[abs(difftime(t1_, datetime, unit = 'days')) < 1, in.fire:= 'y']
cbou.fires[abs(difftime(t1_, datetime, unit = 'days')) >= 1, in.fire:= 'n']
cbou.fires[is.na(datetime), in.fire:= 'n']


## prepare data for specific fires ----
# create study area coords to define extents and make dfs for mapping

# caribou gps points within 15km of the zoomed in fire to define area
cbou.fires.coords <- st_as_sf(cbou.fires, coords =  c('x', 'y')) %>%
  st_set_crs(crs.web)

enterprise <- cbou.fires.coords %>%
  st_join(enterprise.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
enterprise.df <- setDT(sfheaders::sf_to_df(enterprise, fill = T))

dehchozoom <- cbou.fires.coords %>%
  st_join(dehchozoom.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
dehchozoom.df <- setDT(sfheaders::sf_to_df(dehchozoom, fill = T))

sahtuzoom <- cbou.fires.coords %>%
  st_join(sahtuzoom.f, join = st_within) %>%
  filter(!is.na(FIRENUM))
sahtuzoom.df <- setDT(sfheaders::sf_to_df(sahtuzoom, fill = T))

#### dehcho zoom ----
# filter to right time period for the specific fire, starting ~1 month prior
dehchozoom.Aug <- dehchozoom.df[t1_>= as.POSIXct('2023-07-15', tz='UTC')]
# fire start 8-13
# fire "end" 9-24
# NOTE: there are very few days in the after period since data ends 9-29
dehchozoom.Aug[, fire:= ifelse(t1_<= as.POSIXct('2023-08-13', tz='UTC'), 'before',
                               ifelse(t1_>= as.POSIXct('2023-09-24', tz='UTC'), 'after', 'during'))]
summary(as.factor(dehchozoom.Aug$fire))
dehchozoom.Aug[,.(mean(sl_, na.rm = T)), by = .(fire)]

dehchozoom.Aug[abs(diff.time)<2, burned:= 'burning']
dehchozoom.Aug[diff.time>=2, burned:= 'burned']
dehchozoom.Aug[is.na(diff.time)|diff.time<=-2, burned:= 'not burned']
summary(as.factor(dehchozoom.Aug$burned))

dehchozoom.Aug$fire <- factor(dehchozoom.Aug$fire, levels = c('before', 'during', 'after'))
dehchozoom.Aug$burned <- factor(dehchozoom.Aug$burned, levels = c('not burned', 'burning', 'burned'))


sum.dehchozoom.fire <- dehchozoom.Aug[, .(median = median(sl_, na.rm = T), 
                       mean = mean(sl_, na.rm = T), sd = sd(sl_, na.rm = T)), by = .(fire)]
ggplot(dehchozoom.Aug, aes(sl_)) + 
  geom_density(aes(fill = fire), alpha = 0.4) +
  geom_vline(data = sum.dehchozoom.fire, aes(xintercept=mean, color = fire)) +
  xlim(c(0, 15000)) +
  scale_colour_viridis_d(name = "fire") +
  scale_fill_viridis_d(name = "fire") +
  xlab("Step lengths (m)/8-hours")+
  theme_bw() + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20)) 

ggplot(dehchozoom.Aug, aes(t1_, sl_, group = id)) +
  geom_point(aes(color = id)) +
  geom_smooth(aes(fill =id, color = id)) +
  geom_vline(xintercept = as.POSIXct('2023-08-13', tz='UTC'), color = 'darkorange') +
  geom_vline(xintercept = as.POSIXct('2023-09-24', tz='UTC'), color = 'darkorange') +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  ylab("Step lengths (m)/8-hours")+
  xlab('Date') +
  theme_bw() + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20)) 

sum.dehchozoom.infire <- dehchozoom.Aug[, .(median = median(sl_, na.rm = T), 
                                            mean = mean(sl_, na.rm = T), sd = sd(sl_, na.rm = T)), by = .(in.fire)]
ggplot(dehchozoom.Aug, aes(sl_)) + 
  geom_density(aes(fill = in.fire), alpha = 0.4) +
  geom_vline(data = sum.dehchozoom.infire, aes(xintercept=mean, color = in.fire)) +
  xlim(c(0, 15000)) +
  scale_colour_viridis_d(name = "In fire?") +
  scale_fill_viridis_d(name = "In fire?") +
  xlab("Step lengths (m)/8-hours")+
  theme_bw() + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20)) 

sum.dehchozoom.burned <- dehchozoom.Aug[, .(median = median(sl_, na.rm = T), 
                                            mean = mean(sl_, na.rm = T), sd = sd(sl_, na.rm = T)), by = .(burned)]
ggplot(dehchozoom.Aug, aes(sl_)) + 
  geom_density(aes(fill = burned), alpha = 0.4) +
  geom_vline(data = sum.dehchozoom.burned, aes(xintercept=mean, color = burned)) +
  xlim(c(0, 15000)) +
  scale_colour_viridis_d(name = "Burning?") +
  scale_fill_viridis_d(name = "Burning?") +
  xlab("Step lengths (m)/8-hours")+
  theme_bw() + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20)) 

#### sahtu zoom ----
# filter to time frame relevant for specfic fire
hotspots.sahtuzoom.Jul <- hotspots.sahtuzoom.df[datetime %between% c(as.POSIXct('2023-07-01', tz='UTC'),
                                                                     as.POSIXct('2023-08-01', tz='UTC'))]
sahtuzoom.Jul <- sahtuzoom.df[datetime %between% c(as.POSIXct('2023-07-01', tz='UTC'),
                                                   as.POSIXct('2023-08-01', tz='UTC'))]

#### Enterprise fire  ----
# filter to time frame for specific fire
# try to capture month before
enterprise.Aug <- enterprise.df[t1_>= as.POSIXct('2023-07-01', tz='UTC')]

# fire start 8-03
# fire "end" 9-24
# NOTE: there are very few days in the after period since data ends 9-29
enterprise.Aug[, fire:= ifelse(t1_<= as.POSIXct('2023-08-03', tz='UTC'), 'before',
                               ifelse(t1_>= as.POSIXct('2023-09-24', tz='UTC'), 'after', 'during'))]
summary(as.factor(enterprise.Aug$fire))
enterprise.Aug[,.(mean(sl_, na.rm = T)), by = .(fire)]

enterprise.Aug[abs(diff.time)<2, burned:= 'burning']
enterprise.Aug[diff.time>=2, burned:= 'burned']
enterprise.Aug[is.na(diff.time)|diff.time<=-2, burned:= 'not burned']
summary(as.factor(enterprise.Aug$burned))

enterprise.Aug$fire <- factor(enterprise.Aug$fire, levels = c('before', 'during', 'after'))
enterprise.Aug$burned <- factor(enterprise.Aug$burned, levels = c('not burned', 'burning', 'burned'))


quantile(enterprise.Aug$sl_)
sum.enterprise.fire <- enterprise.Aug[, .(median = median(sl_, na.rm = T), 
                                          mean = mean(sl_, na.rm = T), sd = sd(sl_, na.rm = T)), by = .(fire)]
ggplot(enterprise.Aug, aes(sl_)) + 
  geom_density(aes(fill = fire), alpha = 0.4) +
  geom_vline(data = sum.enterprise.fire, aes(xintercept=mean, color = fire)) +
  xlim(c(0, 15000)) +
  scale_colour_viridis_d(name = "fire") +
  scale_fill_viridis_d(name = "fire") +
  xlab("Step lengths (m)/8-hours")+
  theme_bw() + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20)) 

ggplot(enterprise.Aug, aes(t1_, sl_, group = id)) +
  geom_point(aes(color = id)) +
  geom_smooth(aes(fill =id, color = id)) +
  geom_vline(xintercept = as.POSIXct('2023-08-03', tz='UTC'), color = 'darkorange') +
  geom_vline(xintercept = as.POSIXct('2023-09-24', tz='UTC'), color = 'darkorange') +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  ylab("Step lengths (m)/8-hours")+
  xlab('Date') +
  ylim(c(0, 35000)) +
  theme_bw() + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20)) 


sum.enterprise.burned <- enterprise.Aug[, .(median = median(sl_, na.rm = T), 
                                            mean = mean(sl_, na.rm = T), sd = sd(sl_, na.rm = T)), by = .(burned)]
ggplot(enterprise.Aug, aes(sl_)) + 
  geom_density(aes(fill = burned), alpha = 0.4) +
  geom_vline(data = sum.enterprise.burned, aes(xintercept=mean, color = burned)) +
  xlim(c(0, 15000)) +
  scale_colour_viridis_d(name = "Burning?") +
  scale_fill_viridis_d(name = "Burning?") +
  xlab("Step lengths (m)/8-hours")+
  theme_bw() + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20)) 

