# subroutine to convert df to raster, DW, 12/21/2018
# from https://stackoverflow.com/questions/19627344/how-to-create-a-raster-from-a-data-frame-in-r

df2raster <- function(df) {

    library(raster)

    # create spatial points data frame
    spg <- df
    coordinates(spg) <- ~ x + y

    # coerce to SpatialPixelsDataFrame
    gridded(spg) <- TRUE

    # coerce to raster
    rt <- raster(spg)
    crs(rt) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    return(rt)
}
