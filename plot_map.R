# install.packages("osmdata")
# install.packages("webshot")

library(data.table)
library(osmdata)
library(ggmap)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(webshot)
library(cowplot)

residences <- fread("places_of_permanent_residence.csv")
residences_long <- melt(residences, id.vars = c("District", "Latitude", "Longitude"))
residences_long[value == 0, value := NA]

met_hum_bb <- matrix(c(residences[, range(Longitude)],residences[, range(Latitude)]),
                       nrow = 2, byrow = T) 
colnames(met_hum_bb) <- c("min", "max")
rownames(met_hum_bb) <- c("x", "y")

met_hum_major <- met_hum_bb %>%
    opq() %>%
    add_osm_feature(key = "highway",
                    value = c("motorway", "primary", "secondary")) %>%
    osmdata_sf()
met_hum_water <- met_hum_bb %>%
    opq() %>%
    add_osm_feature(key = "water",
                    value = c("lake", "river")) %>%
    osmdata_sf()
    
# met_hum_map1 <- get_map(met_hum_bb, maptype = "roadmap", source = "osm") 

map <- leaflet() %>% addTiles() %>%
    # addPolylines(data = met_hum_major$osm_lines, color = "black") %>%
    addCircles(data = residences)
saveWidget(map, "map.html", selfcontained = F)
webshot("map.html", file = "leaflet_map.png", cliprect = "viewport")
    
met_hum_map <- ggplot() +
    geom_sf(data = met_hum_major$osm_lines,
            inherit.aes = FALSE,
            color = "black",
            size = 0.2) +
    geom_sf(data = met_hum_water$osm_lines,
            inherit.aes = FALSE,
            color = "light blue") +
    geom_sf(data = met_hum_water$osm_polygons,
            inherit.aes = FALSE,
            color = "light blue",
            fill = "light blue") +
    geom_sf(data = met_hum_water$osm_multipolygons,
            inherit.aes = FALSE,
            color = "light blue",
            fill = "light blue") +
    geom_point(data = residences_long,
               aes(x = Longitude, y = Latitude, color = variable, size = value), position = position_dodge(width = 0.15)) + 
    geom_point(data = residences_long[District == "Korhumer"],
               aes(x = Longitude, y = Latitude), shape = 15) + 
    # geom_jitter(data = residences,
    #             aes(x = Longitude, y = Latitude, size = RC), width = 0.1, height = 0.1) +
    scale_color_discrete(name = "") +
    scale_size_continuous(name = "Number") + 
    annotate("text", x = 37.3, y = 12, label = "Lake Tana", color = "dark blue", size = 3) +
    annotate("text", x = 36.4, y = 13.7, label = "Abdurafi", size = 2) +
    annotate("text", x = 36.25, y = 13.2, label = "Delelo", size = 2) +
    # annotate("text", x = residences[District == "Bahir Dar", Longitude], y = residences[District == "Bahir Dar", Latitude - 0.05], label = "Bahir Dar", size = 3) +
    # annotate("text", x = residences[District == "Korhumer", Longitude], y = residences[District == "Korhumer", Latitude - 0.1], label = "Korhumer", size = 3) +
    theme_cowplot(font_size = 10)

met_hum_map_anntd <- met_hum_map
for (i in seq_len(nrow(residences))){
    met_hum_map_anntd <- met_hum_map_anntd + 
        annotate("text", x = residences[i, Longitude], y = residences[i, Latitude - 0.06], label = residences[i, District], size = 2)
}

met_hum_map_anntd
ggsave("map.pdf", met_hum_map_anntd, width = 6, height = 6)

