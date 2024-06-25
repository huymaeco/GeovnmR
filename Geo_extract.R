#' @title Geo_extract
#'
#' @description Provides a CSV data of any geo data for each district in Vietnam.
#'
#' @param dataset A string representing the dataset to be used (e.g., "cru-ts").
#' @param timestep Time resolution for the data (e.g., "yearly", "monthly").
#'
#' @return A data frame object that contains Vietnam geodata for the specified time period.
#' @examples
#' data <- Geo_extract(dataset = "cru-ts", timestep = "yearly")
#' @export
#' @importFrom dplyr %>%
#' @importFrom geodata gadm
#' @importFrom pRecipe download_data
#' @importFrom terra rast crop extract
#' @importFrom sf st_as_sf
Geo_extract <- function(dataset, timestep) {
  main_path <- getwd()
  # ADM level 2
  vietnam_2_sf <- geodata::gadm(
    country = "VNM",
    level = 2,
    path = main_path
  ) |>
    sf::st_as_sf()
  ##---- Extract data in netcdf format
  # Download data
  pRecipe::download_data(
    dataset = dataset,
    path = main_path,
    domain = "raw",
    timestep = timestep
  )
  # Load the downloaded .nc file (assuming there's only one for simplicity)
  nc_files <- list.files(pattern = "\\.nc$")
  if (length(nc_files) == 0) stop("No .nc files found in the directory.")
  main_data <- terra::rast(nc_files) |>
    terra::crop(vietnam_2_sf)
  ##---- Clean data and convert to CSV
  # Add ID to vietnam_2_sf
  vietnam_2_sf$ID <- 1:nrow(vietnam_2_sf)
  # Extract mean data
  mean_data <- terra::extract(main_data, vietnam_2_sf, fun = mean, na.rm = TRUE)
  # Merge data
  vietnam_2_data <- dplyr::left_join(vietnam_2_sf, mean_data, by = "ID")
  # Export data to CSV
  output_path <- file.path(main_path, paste0("Vietnam_geo_data_", dataset, "_", timestep, ".csv"))
  write.csv(vietnam_2_data, output_path, row.names = FALSE)

  return(vietnam_2_data)
}




