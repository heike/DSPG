path_to_box <- "~/Box/DSPG@ISU/Projects/allData/geoData/shapfiles/"
library(sf)
library(dplyr)

ia_counties <- read_sf(file.path(path_to_box, "Counties/"))
ia_cities <- read_sf(file.path(path_to_box, "IA_cities/"))

library(ggplot2)
ia_counties %>% ggplot() + geom_sf() + geom_sf(data = ia_cities)

ia_counties <- st_transform(ia_counties, '+proj=longlat  +datum=WGS84 +units=m +ellps=WGS84')
st_crs(ia_cities)
ia_cities <- st_transform(ia_cities, '+proj=longlat  +datum=WGS84 +units=m +ellps=WGS84')

#############
# include population estimates in county shape files
population <- read.csv("rawdata/co-est2019-alldata.csv") # same data as from shiny app training

ia_counties <- ia_counties %>% left_join(population %>% filter(STATE==19) %>%
                       select(COUNTY, CENSUS2010POP, POPESTIMATE2019),
                     by=c("CO_FIPS"= "COUNTY"))

ia_counties <- ia_counties %>% select(names(ia_counties)[-9], "geometry")

usethis::use_data(ia_counties, overwrite = TRUE)
usethis::use_data(ia_cities, overwrite = TRUE)

#sf_proj_info(type = "proj", file.path(path_to_box, "allData/geo data/shapfiles/Counties/"))

#spdf_lon_lat <- st_transform(ia_counties, 26915)
#spdf_lon_lat %>% ggplot() + geom_sf()

##############
library(tidycensus)

# need to have key for CENSUS_API set
ia_counties_2020 <- get_decennial(
  geography = "county",
  variables = c("P1_001N"),
  state="IA",
  year = 2020,
  geometry =T
)
ia_counties_2020  <- st_transform(ia_counties_2020, '+proj=longlat  +datum=WGS84 +units=m +ellps=WGS84')
names(ia_counties_2020) <- tolower(names(ia_counties_2020))
ia_counties_2020 <- ia_counties_2020 %>% mutate(census2020pop = value)
# merge old and new county information
ia_counties <- ia_counties %>% as_tibble() %>% select(-geometry)

ia_counties_2020 <- ia_counties_2020 %>% left_join(ia_counties)
ia_counties_2020 <- ia_counties_2020 %>% select(-variable)

ia_counties <- ia_counties_2020
ia_counties <- ia_counties %>% select(co_number, co_fips, acres_sf, acres, geoid, name, county, state, id, census2010pop,census2020pop, popestimate2019, geometry)
usethis::use_data(ia_counties, overwrite = TRUE)

ia_places <- get_decennial(
  geography = "place",
  variables = c("P1_001N"),
  state="IA",
  year = 2020,
  geometry =T
)
ia_places <- st_transform(ia_places, '+proj=longlat  +datum=WGS84 +units=m +ellps=WGS84')
names(ia_places) <- tolower(names(ia_places))

ia_places <- ia_places %>% mutate(
  name = gsub(" city, Iowa", "", name)
)

ia_places <- ia_places %>% mutate(
  name = gsub(", Iowa", "", name)
)
ia_places <- ia_places %>% mutate(
  geoid = as.numeric(geoid),
  pop20 = value
) %>% select(-variable, -value)


pop10 <- get_decennial(
  geography = "place",
  variables = "P001001",
  state="Iowa",
  year = 2010
)
names(pop10) <- tolower(names(pop10))
pop10 <- pop10 %>% mutate(
  geoid = as.numeric(geoid),
  pop10 = value,
) %>% select(-variable, -value)


pop00 <- get_decennial(
  geography = "place",
  variables = "PL001001",
  state="Iowa",
  year = 2000,
  sumfile = "pl"
)
names(pop00) <- tolower(names(pop00))
pop00 <- pop00 %>% mutate(
  geoid = as.numeric(geoid),
  pop00 = value,
) %>% select(-variable, -value)


ia_places <- ia_places %>%
  left_join(pop10 %>% select(-name), by="geoid")

ia_places <- ia_places %>%
  left_join(pop00 %>% select(-name), by="geoid")

# need a better source for the county information
cities_with_multiple_counties <- ia_cities %>% st_drop_geometry() %>% group_by(cityFIPS_1) %>% mutate( n = n()) %>% arrange(desc(n))
ia_cities_nest <- ia_cities %>% st_drop_geometry() %>% select(cityFIPS_1, county, countyFI_1, countyPr) %>% group_by(cityFIPS_1) %>% tidyr::nest()
ia_places <- ia_places %>% left_join(ia_cities_nest, by=c("geoid"="cityFIPS_1"))
#ia_places %>% anti_join(ia_cities %>% as_tibble() %>% select(cityFIPS_1, county, countyFI_1), by=c("geoid"="cityFIPS_1"))

names(ia_places)[4:6] <- c("pop2020", "pop2010", "pop2000")

# for which geoids do we not have names?
ids <- ia_places %>% filter(is.na(name)) %>% as_tibble() %>% select(geoid) %>% as.vector()
filter(pop00, geoid %in% ids$geoid)

ia_places$name[ia_places$geoid==1945750] <- "Littleport"
ia_places$name[ia_places$geoid==1952410] <- "Millville"

# from Cybox: https://iastate.app.box.com/folder/138625368211
istp <- readxl::read_xlsx("~/Downloads/ISTP_place_list.xlsx")
istp$ISTP <- "ISTP"
ia_places <- ia_places %>% left_join(istp %>% select(FIPS_PL, ISTP) %>% mutate(FIPS_PL=as.numeric(FIPS_PL)), by=c("geoid"="FIPS_PL"))

ia_places$ISTP <- ia_places$ISTP=="ISTP"
ia_places <- ia_places %>% mutate(
  ISTP = ifelse(is.na(ISTP), FALSE, TRUE)
)
# Nora Springs is in two counties, it represents Floyd County
ia_places %>% filter(geoid=="1956910")
#ia_places <- ia_places %>% mutate(
#  ISTP = ifelse(name=="Nora Springs", county=="Floyd", ISTP)
#)

# Nashua is in two counties, it represents Floyd County
ia_places %>% filter(geoid=="1956910")
#ia_places <- ia_places %>% mutate(
#  ISTP = ifelse(name=="Nora Springs", county=="Floyd", ISTP)
#)


ia_places <- ia_places %>% mutate(
  county = data %>% purrr::map_chr(.f = function(d) {
    if (is.null(d)) return(NA)
    if (nrow(d) == 1) return(d$county)
    d$county[d$countyPr=="x"]
  })
)
ia_places <- ia_places %>% mutate(
  county_geoid = data %>% purrr::map_dbl(.f = function(d) {
    if (is.null(d)) return(NA)
    if (nrow(d) == 1) return(d$countyFI_1)
    d$countyFI_1[d$countyPr=="x"]
  })
)

# # none of the CDPs have counties
filter(ia_places, is.na(county))
# # Kingston is in Des Moines County

# From USGS:
# https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/GeographicNames/DomesticNames/
zipfile <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/GeographicNames/DomesticNames/DomesticNames_IA_Text.zip"
img <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/GeographicNames/DomesticNames/DomesticNames_IA_Text.jpg"
meta <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/GeographicNames/DomesticNames/DomesticNames_IA_Text.xml"

download.file(url=meta, destfile="raw/USGS/places-meta.xml")
download.file(url=img, destfile="raw/USGS/places-image.jpg")
download.file(url=zipfile, destfile="raw/USGS/places-ia.zip")

names_ia <- read.delim("raw/USGS/places-ia/Text/DomesticNames_IA.txt", sep="|")

names_ia %>% filter(feature_class=="Census") %>% dim()

cdps <- grep(" CDP", ia_places$name)
ia_places$type <- "Town"
ia_places$type[cdps] <- "CDP"

ia_places <- ia_places %>% mutate(
  name = gsub(" CDP", " Census Designated Place", name)
)


cdps <- ia_places %>% filter(type=="CDP")

# the only problematic places are the St.
cdps %>% anti_join(names_ia %>% st_drop_geometry() %>% filter(feature_class=="Census"), by=c("name"= "feature_name")) %>% View()
grep("Benedict Census Designated Place", names_ia$feature_name, value = TRUE)

cdps <- cdps %>% mutate(
  name = gsub("St. ", "Saint ", name)
)
cdps %>% anti_join(names_ia %>% st_drop_geometry() %>% filter(feature_class=="Census"), by=c("name"= "feature_name")) %>% View()
# got them all, so now join

cdps <- cdps %>% left_join(
  names_ia %>% st_drop_geometry() %>% filter(feature_class=="Census") %>%
    select(feature_name, county_name, county_numeric),
  by=c("name"= "feature_name"))

# now let's join that back into ia_places
ia_places <- ia_places %>% left_join(cdps %>% st_drop_geometry() %>% select(geoid, county_name, county_numeric))

ia_places <- ia_places %>% mutate(
  county = ifelse(is.na(county), county_name, county),
  county_geoid = ifelse(is.na(county_geoid), county_numeric, county_geoid)
)

ia_places <- ia_places %>% mutate(
  county_other = data %>% purrr::map_chr(.f = function(d) {
    if (is.null(d)) return("")
    if (nrow(d) == 1) return("")
#    if (nrow(d) > 2) browser()
    paste(d$county[d$countyPr!="x"], collapse=", ")
  })
)

ia_places <- ia_places %>% mutate(
  name = gsub("Census Designated Place", "CDP",  name)
)

ia_places <- ia_places %>%
  select(geoid, name, county, county_geoid, county_other,
         type, ISTP,
         pop2020, pop2010, pop2000,
         geometry)

usethis::use_data(ia_places, overwrite = TRUE)
