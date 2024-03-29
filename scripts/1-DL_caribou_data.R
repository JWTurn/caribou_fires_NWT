# Download Data ====
# Julie Turner
# Started: 05 January 2024

## Load packages ----
require('move2')
require('data.table')
require('sf')

### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')


# GPS DATA ----
# This will have to be run once if first time using `move2` to load your login credentials
# movebank_store_credentials("myUserName", "myPassword")

# study names to fill in loop
# had to go through names on movebank and accept terms for each... 
# there's a way to accept terms from R, but I couldn't get it to work, seems to be an error with this version of `move2`
dsNames <-c('Dehcho Boreal Woodland Caribou', 
            'Sahtu Boreal Woodland Caribou (2020)', 'South Slave Boreal Woodland Caribou') 



# make a list of all data downloaded from Movebank
# limited to just 2023 data for this project
# LAST downloaded 01 Feb. 2024
move <- list()
for (ds in 1:length(dsNames)) {
  #hh = 1
  move[[ds]]<-movebank_download_study(study =paste0( 'GNWT ', dsNames[[ds]]), 
                                      timestamp_start = as.POSIXct("2023-01-01 00:00:00"),
                                      timestamp_end = as.POSIXct("2023-12-31 23:59:59"))
}
# save the raw list if desired
saveRDS(move, file.path(raw, 'NWTdata_DL.RDS'))

# download the meta data
move.meta <- list()
for (ds in 1:length(dsNames)) {
  #hh = 1
  move.meta[[ds]]<-movebank_download_deployment(study =paste0( 'GNWT ', dsNames[[ds]]), 
                                                timestamp_start = as.POSIXct("2023-01-01 00:00:00"),
                                                timestamp_end = as.POSIXct("2023-12-31 23:59:59"))
}



### combine the data ----
# list of the 3 study areas to loop through with simplified names from `dsNames`
hab <-c('dehcho', 'sahtu', 'south.slave') 

move.combo <- data.table()
move.combo <- rbindlist(lapply(1:length(hab), function(hh){
  move.combo[,.(area=hab[[hh]], dat=list(setDT(
    sfheaders::sf_to_df(
      move[[hh]], fill=T))))]
}))


# gathering just the columns needed
move.expand <- rbindlist(lapply(1:length(hab), function(hh){
  move.combo$dat[[hh]][,area := hab[[hh]]]
}), fill = T)

nwt <- move.expand[is.na(algorithm_marked_outlier) | is.na(manually_marked_outlier),
                   .(id = individual_local_identifier, study_area = area, 
                      habitat, timestamp, x, y)]

# gathering metadata
metadat <- rbindlist(move.meta, fill =T)


# save 'clean' data 
saveRDS(nwt, file.path(raw, 'NWTdat.RDS'))
saveRDS(metadat, file.path(raw, 'NWTmetadata_DL.RDS'))

