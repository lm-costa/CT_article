
# **The importance of Brazil’s biomes and the role of precipitation to achieve country-level net emissions** *(Provisional name)*

## **Initial Considerations**

This is the repository are a compilation of the
[repository](https://github.com/arpanosso/climate-trace-br) (this
repository is in PT-BR), here you will find the main codes used to
process the data from [Climate TRACE](https://climatetrace.org/),
[SEEG](https://seeg.eco.br/),
[Precipitation](https://power.larc.nasa.gov/) and also
[SIF](https://disc.gsfc.nasa.gov/datasets/OCO2_L2_Lite_SIF_11r/summary/).

## **Acquisition and preprocessing**

The first step is use the `data-raw/metadata_cleaning.R` code, in order
to acquire the Brazilian emission estimate from Climate TRACE.

The second step is to acquire the precipitation data from NASA-POWER. To
do so, you will have to use the code inside
`precipitation/r/data_download.R`.

The SIF data can be downloaded using the script inside `SIF/r/` called
`sif_data_download.R`. After downloading the data, please use the
`data_extraction.R`. Finally, use the `sif_agregation.R` script, in
order to resample for 0.5 degree.

## **Plotting and analizing**

After doing all the above metioned steps, you can use the `Graphics.R`
script, in order to plot our results.

***Notice that for spatial figures, we exported the data into csv file
and used these files on a GIS software, in order to better quality
mapping***
