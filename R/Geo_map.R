#' @title VNM_map
#'
#' @description     Custom your map with excel data
#'
#' @param ISO       ISO3 code of your targeted country
#' @param Level     Level of your targeted ADM shape file
#' @param dataset   dataset by your own with NetCDF format
#' @param palette   palette color that you want for your map
#'
#' @return          A map that displays geodata for the specified time period.
#' @examples
#' map <- Geo_plot(ISO = "VNM", Level = "1", dataset = "Your nc path file", palette = "Blues")
#' @export
#' @importFrom      dplyr %>%
#' @importFrom      geodata gadm
#' @importFrom      pRecipe download_data
#' @importFrom      terra rast crop extract
#' @importFrom      sf st_as_sf

Geo_plot <- function(ISO, Level, dataset, palette) {
  main_path <- getwd()

  # ADM level
  border_sf <- geodata::gadm(
    country = ISO,
    level = Level,
    path = main_path
  ) |>
    sf::st_as_sf()

  # Load your data in netcdf format
  main_data <- terra::rast(dataset) |>
    terra::crop(border_sf)

  # Extract the variable name and unit from the dataset
  var_names <- names(main_data)
  var <- var_names[length(var_names)]
  map_name <- terra::varnames(main_data)
  unit <- terra::units(main_data[[1]])

  # Clean data and convert to CSV
  border_sf$ID <- 1:nrow(border_sf)
  mean_data <- terra::extract(main_data, border_sf, fun = mean, na.rm = TRUE)
  border_data <- dplyr::left_join(border_sf, mean_data, by = "ID")

  # Export data to CSV
  output_path <- file.path(main_path, paste0(ISO, "_", var, ".csv"))
  write.csv(border_data, output_path, row.names = FALSE)

  # set up break
  breaks <- classInt::classIntervals(
    border_data[[var]],
    n = 4,
    style = "pretty"
  )$brks

  # set up color group
  colors <- hcl.colors(
    n = length(breaks) - 1,
    palette = palette,
    rev = TRUE
  )

  # Plot map using ggplot2
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = border_data,
      ggplot2::aes(
        fill = .data[[var]]
      ),
      color = "white",
      size = 0.125
    ) +
    ggplot2::scale_fill_gradientn(
      name = var,
      colors = colors,
      breaks = breaks
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        direction = "horizontal",
        keyheight = grid::unit(2.5, "mm"),
        keywidth = grid::unit(10, "mm"),
        title.position = "top",
        label.position = "bottom",
        title.hjust = 0.5,
        label.hjust = 0.5,
        nrow = 1,
        byrow = TRUE
      )
    ) +
    ggplot2::labs(
      title = paste0(ISO, "_", var, " mapping"),
      caption = paste0("Data: ", ISO, "_", var, " | Creation: Huy_maeco | depocen.com")
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 13, family = "EB Garamond"),
      legend.position = "top",
      plot.title = ggplot2::element_text(
        size = 22, color = "grey10",
        hjust = 0.5, vjust = 1
      ),
      plot.caption = ggplot2::element_text(
        size = 10, color = "grey60",
        hjust = 0.5, vjust = 5
      ),
      legend.title = ggplot2::element_text(
        size = 12, color = "grey10",
        hjust = 0.5
      ),
      legend.text = ggplot2::element_text(
        size = 11, color = "grey10",
        hjust = 0.5
      ),
      plot.margin = grid::unit(
        c(t = 0, b = 0, r = 0, l = 0),
        "lines"
      )
    )

  print(map)
}



