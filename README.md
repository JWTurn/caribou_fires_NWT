
## Animations of fires and woodland caribou movement in NWT 2023

- Authors:
  - [Julie W. Turner](https://www.julwturner.com)
  - James Hodson
  - Nick Wilson

This repository contains code to animate caribou movement relative to
fire hotspots in Northwest Territories (NWT) from the 2023 fire season.
Caribou data comes from the Government of Northwest Territories’
woodland data on movebank. Fire data comes from Natural Resources Canada
fire hotspots [dataset](https://cwfis.cfs.nrcan.gc.ca/datamart).

### Project organization

The project folder structure is as follows:

``` src
-- caribou_fires_NWT
  |-- data
      |-- raw
          |-- CanadaPoly
          |-- cwfis
          |-- FireShapefiles
      |-- derived
  |-- scripts
  |-- anims
```

The `data` folder contains `raw` data from the source, and the `derived`
data that was prepared for the animations. `CanadaPoly` contains a
polygon of Canadian provinces and territories from [Statistics
Canada](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21)
to filter data to NWT. `cwfis` contains the NRCan fires
[data](https://cwfis.cfs.nrcan.gc.ca/datamart). The `FireShapefiles`
data was provided by GNWT.

The `scripts` folder contains the R code necessary to prepare and
animate the data in numerical order. The `anims` folder is where the
animations will be saved.
