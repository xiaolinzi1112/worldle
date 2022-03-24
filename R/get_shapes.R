#' Download a shapefile from GADM
#'
#' we need a bit more explanation here ... The function works with both versions 3.6 and 4.0 of the GADM library
#' @param url address to zipped file shapes
#' @param level which level to extract? 0 is country outline, 1 is state/territories, for some (but not all) countries exist lower-level country divisions (e.g. counties, municipalities)
#' @importFrom sf read_sf
#' @importFrom utils download.file unzip
#' @export
#' @examples
#' # url for Iran
#' url <- 'https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_IRN_shp.zip'
#' # url for Bahamas
#' url <- "https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_BHS_shp.zip"
#' country <- get_shapes(url)
#' library(ggplot2)
#' library(dplyr)
#' country %>% thin(tolerance=0.001) %>%  ggplot() + geom_sf() + theme_void()
get_shapes <- function(url, level = 0) {
  url_splitted <- strsplit(url, split = "/")[[1]]
  file_name <- url_splitted[length(url_splitted)]
  # Prepare for download
  datadir <- tempdir()
  destfile <- file.path(datadir, file_name)

  # Try to download
  download.file(url,
    destfile = destfile,
    mode = "wb"
  )
  # unzip
  new_folder <- gsub(".zip$", "", destfile)
  unzip(destfile,
    exdir = new_folder
  )
  # read shapefile
  # Read
  sf_file <- file.path(new_folder, gsub("shp.zip", paste0(level, ".shp"), file_name))

  sf::read_sf(sf_file)
}
