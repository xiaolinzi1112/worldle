#' Shape file for Austria
#'
#' @examples
#' \dontrun{
#' # created from
#' url <- "https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_AUT_shp.zip"
#' austria <- get_shapes(url, level=1)
#' austria <- austria %>% thin(tolerance = 0.0001)
#' usethis::use_data(austria, overwrite=TRUE)
#' }
#' library(ggplot2)
#' library(dplyr)
#' data(austria)
#' austria %>% ggplot() + geom_sf()
"austria"
