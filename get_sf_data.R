
library(giscoR)
# Download and prepare Croatia counties data from Gisco (EUROSTAT)
gradovi_gisco <- gisco_get_lau(country = 'HR',epsg = '4326')
zupanije_gisco <- gisco_get_nuts(country = 'HR',nuts_level = 3,epsg = '4326')

library(sf)
gradovi_gisco |> write_sf("gisco_data/gradovi_gisco.geojson",driver = "geojson")
zupanije_gisco |> write_sf("gisco_data/zupanije_gisco.geojson",driver = "geojson")
