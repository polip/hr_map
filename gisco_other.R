
gisco_addressapi_provinces(country = "HR")
gisco_addressapi_cities(country = "HR",province = "SPLITSKO-DALMATINSKA")
gisco_addressapi_roads(country = "HR",province = "SPLITSKO-DALMATINSKA")
gisco_addressapi_search(country = "HR",province = "SPLITSKO-DALMATINSKA",city = "OMIŠ",road = "VANGRAD") |> 
  ggplot2::ggplot() + ggplot2::geom_sf() + ggplot2::theme_minimal() + ggplot2::theme(legend.position = "none")
