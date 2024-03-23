# Data extraction pipeline for era5 reanalysis products from GEE

![Static Badge](https://img.shields.io/badge/development-passing-green?style=flat-square)


This repository is a pipeline to extract climate and weather data from era5 image collections in Google Earth Engine. 

This pipeline requires the user to have a Google Earth Engine account [info to signup](https://courses.spatialthoughts.com/gee-sign-up.html#:~:text=Visit%20https%3A%2F%2Fsignup.earthengine,1%2D2%20days%20for%20approval.) and for it to be connected to R, as well as the packages `rgee` and `reticulate` to be installed. This process can prove difficult. For information on how to setup `rgee` and link to your Google Earth Engine account, consult the following resources: [rgee](https://r-spatial.github.io/rgee/index.html).

Sometimes, `rgee` has a hard time connecting to Google Earth Engine. To make this easier, it is recommend to add a config file. A template config file is available in the `setup/` folder and is called in the `_get-weather-climate.R` script.







To cite ERA5: 
- Muñoz Sabater, J., (2019): ERA5-Land monthly averaged data from 1981 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). (<date of access>), doi:10.24381/cds.68d2bb30
