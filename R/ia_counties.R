#' Shapefiles of Iowa's counties
#'
#' Shapefiles of Iowa counties (+proj=longlat  +datum=WGS84 +units=m +ellps=WGS84)
#' using the PL 94-171 Redistricting Data Summary File.
#' @format A data frame with 99 rows and 9 variables:
#' \describe{
#'   \item{co_number}{county number}
#'   \item{co_fips}{three-digit county fips code}
#'   \item{acres_sf}{square footage in acres}
#'   \item{acres}{county acreage, same as `ACRES_SF``}
#'   \item{geoid}{five-digit fips code}
#'   \item{name}{name of geographic entity (County)}
#'   \item{county}{county name (and it's `Obrien`)}
#'   \item{state}{two letter state abbreviation (`IA` all the way through)}
#'   \item{id}{identifier same as `CO_FIPS`}
#'   \item{census2010pop}{US Census Bureau count of 2010 county population.}
#'   \item{popestimate2019}{US Census Bureau estimate of 2019 county population.}
#'   \item{geometry}{simple feature object of polygons}
#' }
#' @source US Census Bureau
#' @examples
#' # county map of iowa in ggplot2
#' library(ggplot2)
#' library(dplyr) # for the pipe
#'
#' ia_counties %>%
#'   ggplot() +
#'   geom_sf(aes(fill = popestimate2019),
#'           colour = "grey80", size = 0.1) +
#'   ggthemes::theme_map() +
#'   theme(legend.position="right") +
#'   scale_fill_gradient("2019 Population\nEstimate", trans="log",
#'     low = "grey80", high="darkred")
#'
#' # leaflet map
#' library(leaflet)
#' library(sf)
#'
#' ia_counties %>%
#'   group_by(county) %>%
#'   mutate(
#'     hovertext = htmltools::HTML(paste0(county, "<br>",popestimate2019))
#'   ) %>%
#'   leaflet() %>%
#'     addTiles() %>%
#'     addPolygons(label=~hovertext)
"ia_counties"
