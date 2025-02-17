pacman::p_load(dplyr, sf, leaflet, install = FALSE)


#use ons API to download the geojson file required
#https://geoportal.statistics.gov.uk/search?q=BDY_HLT&sort=Date%20Created%7Ccreated%7Cdesc
#https://geoportal.statistics.gov.uk/datasets/8e5561d496d74f9796d7fda2f2bc4065_0/explore

map_icb <- read_sf('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BFE/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson') 

# visualise basic ICB map

map_icb %>% 
  leaflet() %>% 
  addPolygons(weight = 1)


# merging Norfolk and Waveney and Suffolk and North East Essex ICBs as an example

map_icb_latest <- map_icb %>% 
  mutate(ICB = if_else(ICB23NM %in%  c('NHS Norfolk and Waveney Integrated Care Board',
                                       'NHS Suffolk and North East Essex Integrated Care Board'),
                       "East of England",
                       ICB23NM
                       )
         ) %>% 
  group_by(ICB) %>% 
  summarise(.groups = 'drop')
  
  
map_icb_latest %>% 
  leaflet() %>% 
  addPolygons(weight = 1)

# example: generate random number dataset prep

map_icb_latest_randomdata <- map_icb_latest %>% 
  mutate(Performance = sample(100,
                              size = nrow(map_icb_latest),
                              replace = TRUE
                              ))


# define the colours

paletteNum <- colorNumeric('Blues', domain = map_icb_latest_randomdata$Performance)

# visualise the performance based on the colours defined

map_icb_latest_randomdata %>% 
  leaflet() %>% 
  addPolygons(
    color = 'white',
    smoothFactor = .3,
    fillOpacity = 1,
    weight = 1,
    opacity = 1,
    fillColor = ~paletteNum(map_icb_latest_randomdata$Performance)
    )


# introduce highlight visuals

map_icb_latest_randomdata %>% 
  leaflet() %>% 
  addPolygons(
    color = 'white',
    smoothFactor = .3,
    fillOpacity = .75,
    weight = 1,
    opacity = 1,
    fillColor = ~paletteNum(map_icb_latest_randomdata$Performance),
    highlightOptions = highlightOptions(
      weight = 2,
      color = 'black',
      fillOpacity = .9,
      bringToFront = TRUE
      )
    )

# define tooltip labels

labels <- sprintf(
  "<strong>%s</strong><br/>Random Performance: %s%%",
  map_icb_latest_randomdata$ICB, 
  map_icb_latest_randomdata$Performance 
) %>% lapply(htmltools::HTML)

# introduce tooltip
 
map_icb_latest_randomdata %>% 
  leaflet() %>% 
  addPolygons(
    color = 'white',
    smoothFactor = .3,
    fillOpacity = .75,
    weight = 1,
    opacity = 1,
    fillColor = ~paletteNum(map_icb_latest_randomdata$Performance),
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

map_icb_latest_randomdata %>% 
  leaflet() %>% 
  addPolygons(
    color = 'white',
    smoothFactor = .3,
    fillOpacity = .75,
    weight = 1,
    opacity = 1,
    fillColor = ~paletteNum(map_icb_latest_randomdata$Performance),
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
  addLegend(pal = paletteNum, values = map_icb_latest_randomdata$Performance,
            title = '<small>Random Performance by incorrect ICB<br> source: Unknown</small>',
            labFormat = labelFormat(suffix = "%"),
            position = 'bottomright')
