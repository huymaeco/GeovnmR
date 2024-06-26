#' @title Geo_extract
#'
#' @description Provides a CSV data of any geo data for every coumtry in the world.
#'
#' @param dataset A string representing the dataset to be used (e.g., "cru-ts").
#' @param timestep Time resolution for the data (e.g., "yearly", "monthly").
#'
#' @return A data frame/csv object that contains geodata for the specified time period.
#' @examples
#' data <- Geo_extract(ISO = "VNM", Level = "2", dataset = "cru-ts", timestep = "yearly")
#' @export
#' @importFrom dplyr %>%
#' @importFrom geodata gadm
#' @importFrom pRecipe download_data
#' @importFrom terra rast crop extract
#' @importFrom sf st_as_sf

Geo_extract <- function(ISO, Level, dataset, timestep) {
  main_path <- getwd()
  # ADM level 2 ----
  border_sf <- geodata::gadm(
    country = ISO,
    level = Level,
    path = main_path
  ) |>
    sf::st_as_sf()
  # Extract data in netcdf format----
  ## Download data ----
  pRecipe::download_data(
    dataset = dataset,
    path = main_path,
    domain = "raw",
    timestep = timestep
  )
  ## Load the downloaded .nc file (assuming there's only one for simplicity)----
  nc_files <- list.files(pattern = "\\.nc$")
  if (length(nc_files) == 0) stop("No .nc files found in the directory.")
  main_data <- terra::rast(nc_files[1]) |>
    terra::crop(border_sf)

  # Clean data and convert to CSV ----
  ## Add ID to border_sf ----
  border_sf$ID <- 1:nrow(border_sf)
  ## Extract mean data ----
  mean_data <- terra::extract(main_data, border_sf, fun = mean, na.rm = TRUE)
  ## Merge data ----
  border_data <- dplyr::left_join(border_sf, mean_data, by = "ID")
  ## Export data to CSV ----
  output_path <- file.path(main_path, paste0(ISO, dataset, "_", timestep, ".csv"))
  write.csv(border_data, output_path, row.names = FALSE)
  return(border_data)
}




