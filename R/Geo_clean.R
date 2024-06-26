#' @title Geo_clean
#'
#' @description Provides a CSV data with your own Geo data (That dowload from somewhere)
#'
#' @param ISO ISO3 code of your targeted country
#' @param Level Level of your targeted ADM shape file
#' @param dataset dataset by your own with NetCDF format
#'
#' @return A data frame/csv object that contains geodata for the specified time period.
#' @examples
#' data <- Geo_extract(ISO = "VNM", Level = "1", dataset = "Your nc path file")
#' @export
#' @importFrom dplyr %>%
#' @importFrom geodata gadm
#' @importFrom pRecipe download_data
#' @importFrom terra rast crop extract
#' @importFrom sf st_as_sf

Geo_clean <- function(ISO, Level, dataset) {
  main_path <- getwd()
  # ADM level 2 ----
  border_sf <- geodata::gadm(
    country = ISO,
    level = Level,
    path = main_path
  ) |>
    sf::st_as_sf()
  # Load your data in netcdf format----
  ## Load the downloaded .nc file (assuming there's only one for simplicity)----

  main_data <- terra::rast(dataset) |>
    terra::crop(border_sf)

  # Clean data and convert to CSV ----
  ## Add ID to border_sf ----
  border_sf$ID <- 1:nrow(border_sf)
  ## Extract mean data ----
  mean_data <- terra::extract(main_data, border_sf, fun = mean, na.rm = TRUE)
  ## Merge data ----
  border_data <- dplyr::left_join(border_sf, mean_data, by = "ID")
  ## Export data to CSV ----
  output_path <- file.path(main_path, paste0(ISO, "_", ".csv"))
  write.csv(border_data, output_path, row.names = FALSE)
  return(border_data)
}


