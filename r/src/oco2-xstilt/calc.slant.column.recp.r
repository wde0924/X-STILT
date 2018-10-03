#' script to calculate the receptor lat/lon in a slant column
#' given observation viewing zenith/azimuth angles and solar zenith/azimuth angles
#' @author: Dien Wu, 10/03/2018

#' pass the four different angles to simulation_stepv2.r

calc.slant.column.recp <- function(output, oco2.path) {

    # get OCO-2 profile first according to lat/lon of receptor, return a list
    receptor <- output$receptor
    oco2.info <- get.oco2.info(oco2.path, receptor)
    
    receptor$sza <- oco2.info$sza
    receptor$saa <- oco2.info$saa
    receptor$oza <- oco2.info$oza
    receptor$oaa <- oco2.info$oaa
    receptor$ga <- oco2.info$ga
    receptor$am <- oco2.info$air.mass

    # calculate the slant column receptors
    # in/out for insolation and reflected sunlight columns
    slant.recp <- data.frame(receptor) %>% 
        mutate(
            sdx = zagl * tan(sza * pi / 180) * sin(saa * pi / 180), 
            sdy = zagl * tan(sza * pi / 180) * cos(saa * pi / 180), 
            odx = zagl * tan(oza * pi / 180) * sin(oaa * pi / 180), 
            ody = zagl * tan(oza * pi / 180) * cos(oaa * pi / 180),
            in.dlat  = sdy / 111000, 
            in.dlon  = sdx / 111000 / cos(lati * pi / 180), 
            out.dlat = ody / 111000, 
            out.dlon = odx / 111000 / cos(lati * pi / 180), 
            in.slant.lati = lati + in.dlat, 
            in.slant.long = long + in.dlon, 
            out.slant.lati = lati + out.dlat, 
            out.slant.long = long + out.dlon, 
        )
    
    return(slant.recp)
}