if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, sf, leaflet)


#use ons API to download the geojson file required
#https://geoportal.statistics.gov.uk/datasets/ons::cancer-alliances-july-2023-boundaries-en-bfc-2/about

map_ca <- read_sf('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Cancer_Alliances_July_2023_Boundaries_EN_BFE/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') 

# visualise basic CA map

map_ca %>% 
  leaflet() %>% 
  addPolygons(weight = 1)


# merging EoE north and south

map_ca_latest <- map_ca %>% 
  mutate(CA = if_else(CAL23NM %in% c('East of England - North', 'East of England - South'), "East of England", CAL23NM)) %>% 
  group_by(CA) %>% 
  summarise()
  
  
map_ca_latest %>% 
  leaflet() %>% 
  addPolygons(weight = 1)

# example dataset prep

map_demo_data <- map_demo_data %>% 
  mutate(Performance = Within/Total * 100)

# merge the shape file with the example dataset

map_ca_latest_demo_data <- map_ca_latest %>% 
  left_join(map_demo_data, by = "CA" )

# define the colours

paletteNum <- colorNumeric('Blues', domain = map_ca_latest_demo_data$Performance)

# visualise the performance based on the colours defined

map_ca_latest_demo_data %>% 
  leaflet() %>% 
  addPolygons(
    color = 'white',
    smoothFactor = .3,
    fillOpacity = 1,
    weight = 1,
    opacity = 1,
    fillColor = ~paletteNum(map_ca_latest_demo_data$Performance))


# introduce highlight visuals

map_ca_latest_demo_data %>% 
  leaflet() %>% 
  addPolygons(
    color = 'white',
    smoothFactor = .3,
    fillOpacity = .75,
    weight = 1,
    opacity = 1,
    fillColor = ~paletteNum(map_ca_latest_demo_data$Performance),
    highlightOptions = highlightOptions(
      weight = 2,
      color = 'black',
      fillOpacity = .9,
      bringToFront = TRUE
      )
    )

# define tooltip labels

labels <- sprintf(
  "<strong>%s</strong><br/>FDS Performance: %s%%",
  map_ca_latest_demo_data$CA, map_ca_latest_demo_data$Performance %>% round(digits = 1)
) %>% lapply(htmltools::HTML)

# introduce tooltip
 
map_ca_latest_demo_data %>% 
  leaflet() %>% 
  addPolygons(
    color = 'white',
    smoothFactor = .3,
    fillOpacity = .75,
    weight = 1,
    opacity = 1,
    fillColor = ~paletteNum(map_ca_latest_demo_data$Performance),
    highlightOptions = highlightOptions(
      weight = 2,
      color = 'black',
      fillOpacity = .9,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) 
# introduce legend

map_ca_latest_demo_data %>% 
  leaflet() %>% 
  addPolygons(
    color = 'white',
    smoothFactor = .3,
    fillOpacity = .75,
    weight = 1,
    opacity = 1,
    fillColor = ~paletteNum(map_ca_latest_demo_data$Performance),
    highlightOptions = highlightOptions(
      weight = 2,
      color = 'black',
      fillOpacity = .9,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>% 
  addLegend(pal = paletteNum, values = map_ca_latest_demo_data$Performance,
            title = '<small>FDS Performance by Cancer Alliance<br> source: CWT</small>',
            labFormat = labelFormat(suffix = "%"),
            position = 'bottomright')