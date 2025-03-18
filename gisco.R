
library(giscoR)
library(ggplot2)

nuts_3_hr <- gisco_get_nuts(country = 'HR',year = 2024, resolution = '01', nuts_level = 3
                              ,  verbose = TRUE) 
nuts_3_hr |> 
  sf::write_sf("nuts3_hr.geojson")
ggplot(data=nuts_3_hr) + geom_sf() +  theme_minimal() + theme(legend.position="none")


lau_hr <- gisco_get_lau(country = 'HR',year = 2021, verbose = TRUE) 
lau_hr|>  sf::write_sf("lau_hr.geojson")

ggplot(data=lau_hr) + geom_sf() +  theme_minimal() + theme(legend.position="none")


