library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(htmlwidgets)
library(glue)

options(scipen = 999)

# setup / read data ---------------------------------------------------------------

## define colors
color_perc_soil <- 'brown'
color_well_gw_depth <- 'darkblue'
color_well_no_gw_depth <- 'lightblue'
color_wq_test <- 'orange'
color_latrine_good <- 'green'
color_latrine_almost_full <- 'yellow'
color_latrine_broken <- 'red'
color_latrine_new <- 'black'
color_households <- 'grey'

## read data
sf_enoc_wells <- st_read('output_data/enoc_ortez_wells.gpkg')
sf_enoc_wq <- st_read('output_data/enoc_ortez_wq_tests.gpkg')
sf_enoc_perc_soil <- st_read('output_data/enoc_ortez_percolation_soil_type.gpkg')
sf_enoc_latrines_existing <- st_read('output_data/enoc_ortez_latrines_existing.gpkg')
sf_enoc_latrines_new <- st_read('output_data/enoc_ortez_latrines_new.gpkg')
sf_enoc_households <- st_read('output_data/enoc_ortez_households.gpkg')

# ## Define coordinate systems to use for transformations
# ### projected
# projected_crs <- 3310 # see: https://epsg.io/3310 
# # other options: 26910 see: https://epsg.io/26910
# # resources: 
# # https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=109326&inline
# 
# ### geographic
# geographic_crs <- 4269 # see: https://epsg.io/4269
# # other options: 4326 # see: https://epsg.io/4326

            
# Define UI ---------------------------------------------------------------
ui <- fluidPage(

    # Application title
    # titlePanel("Enoc Ortez Data"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(12,
            leafletOutput(outputId = 'map_enoc', 
                          height = 600))#,
        # column(9),
        # column(3, 
        #        p(tags$strong('Note - marker sizes: '), 
        #          tags$li('Existing latrines = percent full'),
        #          tags$li('Water quality = # of colonies w/o air'),
        #          tags$li('Well = water depth (inverse)')
        #        )
        # ),
        # column(3, 
        #        p(tags$strong('Marker Sizes: '), br(), 
        #          HTML('&#8226 Existing latrines = percent full'), br(),
        #          HTML('&#8226 Water quality = # of colonies w/o air'), br(),
        #          HTML('&#8226 Well = water depth (inverse)'))
        # )
    )
)





# Define server logic to draw map -----------------------------------------
server <- function(input, output) {
    
    # function to make a custom legend
    # see: https://stackoverflow.com/a/52883895
    addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, group_name, layer_name, title, opacity = 0.5) {
        make_shapes <- function(colors, sizes, borders, shapes) {
            shapes <- gsub("circle", "50%", shapes)
            shapes <- gsub("square", "0%", shapes)
            paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:1.5px solid ", borders, "; border-radius:", shapes)
        }
        make_labels <- function(sizes, labels) {
            paste0("<div style='display: inline-block;height: ", 
                   sizes, "px;margin-top: 4px;line-height: ", 
                   sizes, "px;'>", labels, "</div>")
        }
        legend_colors <- make_shapes(colors, sizes, borders, shapes)
        legend_labels <- make_labels(sizes, labels)
        legend_group <- paste0(group_name)
        legend_layer <- paste0(layer_name)
        legend_title <- paste0(title)
        
        return(addLegend(map, 
                         colors = legend_colors, 
                         labels = legend_labels, 
                         opacity = opacity, 
                         group = legend_group, 
                         layerId = legend_layer, 
                         title = legend_title,
                         position = 'bottomright', 
                         )
               )
    }
    
    output$map_enoc <- renderLeaflet({
        
        # create empty map
        l_enoc <- leaflet()
        
        l_enoc <- l_enoc %>%
            addMapPane("water_quality", zIndex = 400) %>%
            addMapPane("wells", zIndex = 410) %>% 
            addMapPane("perc_soil", 420) %>% 
            addMapPane("households", zIndex = 421) %>%
            addMapPane("latrines_new", zIndex = 425) %>%
            addMapPane("latrines_existing", zIndex = 430) %>%
            {.}
            
            
        
        # add basemap options
        basemap_options <- c(
            'OpenTopoMap',
            'Esri.WorldTopoMap', 
            'CartoDB.Positron', 
            # 'Esri.WorldGrayCanvas',
            'Esri.WorldImagery',
            'Esri.WorldStreetMap'#,
            # 'Stamen.Terrain'#,
            # 'Esri.DeLorme'#,
            # 'USGS.USImagery'
        ) 
        
        # OpenTopoMap
        # Stamen.Terrain
        # Esri.DeLorme
        # USGS.USImagery
        
        for (provider in basemap_options) {
            l_enoc <- l_enoc %>% 
                addProviderTiles(provider, 
                                 group = provider)
        }
        
        # add the min-map window
        l_enoc <- l_enoc %>% 
            addMiniMap(tiles = basemap_options[[1]], 
                       toggleDisplay = TRUE, 
                       position = "bottomleft")
        
        # # code to make the basemap/min-map selector work (copied from: https://rstudio.github.io/leaflet/morefeatures.html)
        # l_enoc <- l_enoc %>% 
        #     onRender(
        #         "function(el, x) {
        #             var myMap = this;
        #             myMap.on('baselayerchange',
        #             function (e) {
        #             myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        #             })
        #             }"
        #     )
        
        # create a button to center the map on Enoc Ortez
        # get the bounds of the entire state
        bounds_enoc <- attributes(st_geometry(sf_enoc_latrines_existing))$bbox
        # bounds_enoc <- attributes(st_geometry(sf_enoc_wells %>%
        #                                           st_transform(crs = geographic_crs)))$bbox # have to convert to geographic coordinate system for leaflet
        # make button
        l_enoc <- l_enoc %>% 
            addEasyButton(
                easyButton(
                    icon="fa-globe", 
                    title="Re-center",
                    onClick=JS(paste0('function(btn, map){ map.fitBounds([[',
                                      round(bounds_enoc[[2]],4)-0.001, ', ',
                                      round(bounds_enoc[[1]],4)-0.001, '],[',
                                      round(bounds_enoc[[4]],4)+0.001, ', ',
                                      round(bounds_enoc[[3]],4)+0.001, ']]); }')))
            )
        
        # # add a 'locate me' button
        # l_map1 <- l_map1 %>% 
        #     addEasyButton(easyButton(
        #         icon="fa-crosshairs", title="Locate Me",
        #         onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
        
        # add measurement button
        l_enoc <- l_enoc %>% 
            addMeasure(position = 'bottomleft')
        
        # add the wells
        # # create the color palette 
        # redline_leaflet_pal <- colorFactor(palette = c('green', 'blue', 'yellow', 'red'), # 'YlOrBr'   # c("#FACD7B","#D1A149","#916714","#6B4703")
        #                                    domain = redline_polygons$holc_grade,
        #                                    levels = c('A', 'B', 'C', 'D'))
        # add well points to map ----
        l_enoc <- l_enoc %>% 
            addCircleMarkers(data = sf_enoc_wells %>% 
                                 # mutate(well_depth_plot = case_when(is.na(well_depth_m) ~
                                 #                                       5, 
                                 #                                    as.numeric(well_depth_m) > 20 ~
                                 #                                        5, 
                                 #                                   TRUE ~ well_depth_m)) %>% 
                                 mutate(well_depth_plot = case_when(is.na(well_depth_m) | as.numeric(well_depth_m) > 20 ~ "No",
                                                                    TRUE ~ "Yes")) %>% 
                                 filter(!is.na(well_depth_m) & as.numeric(well_depth_m < 20)) %>%
                                 {.},
                             options = pathOptions(pane = "wells"),
                             radius = ~ 1/well_depth_m * 10 + 3,
                             stroke = TRUE, 
                             weight = 0.5, 
                             color = 'black', 
                             opacity = 1,
                             fill = TRUE, 
                             fillOpacity = 0.5, 
                             fillColor = color_well_gw_depth, # 'black', # 'grey', # ~wqi.leaflet.pal(WQI),
                             # clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                             # clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11, 
                             #                                       maxClusterRadius = 60),
                             popup = ~paste0('<b>', '<u>', 'Well', '</u>','</b>','<br/>',
                                             '<b>', 'ID: ', '</b>', name,'<br/>',
                                             '<b>', 'Owner: ', '</b>', well_owner, '<br/>',
                                             '<b>', 'Ground Elevation: ', '</b>', elevation,'<br/>',
                                             '<b>', 'Groundwater Depth (m): ', '</b>', well_depth_m, '<br/>',
                                             '<b>', 'Well Type: ', '</b>', well_type, '<br/>',
                                             '<b>', 'Description: ', '</b>', description,'<br/>',
                                             '<b>', 'Notes: ', '</b>', well_notes, '<br/>',
                                             '<b>', 'Pictures: ', '</b>', glue('<a href="{link_to_photos}">Link</a>'), '<br/>'#,
                             ),
                             group = 'Wells',
                             label = ~glue('Well (Site ID: {name})')
            )
        
        # add well points w/o GW depth to map ----
        l_enoc <- l_enoc %>% 
            addCircleMarkers(data = sf_enoc_wells %>% 
                                 # mutate(well_depth_plot = case_when(is.na(well_depth_m) ~
                                 #                                       5, 
                                 #                                    as.numeric(well_depth_m) > 20 ~
                                 #                                        5, 
                                 #                                   TRUE ~ well_depth_m)) %>% 
                                 # mutate(well_depth_plot = case_when(is.na(well_depth_m) | as.numeric(well_depth_m) > 20 ~ "No",
                                 #                                    TRUE ~ "Yes")) %>% 
                                 filter(is.na(well_depth_m) | as.numeric(well_depth_m > 20)) %>%
                                 {.},
                             options = pathOptions(pane = "wells"),
                             radius = 5,
                             stroke = TRUE, 
                             weight = 0.5, 
                             color = 'black', 
                             opacity = 1,
                             fill = TRUE, 
                             fillOpacity = 0.7, 
                             fillColor = color_well_no_gw_depth, # 'orange', # 'black', # 'grey', # ~wqi.leaflet.pal(WQI),
                             # clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                             clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11, 
                                                                   maxClusterRadius = 60),
                             popup = ~paste0('<b>', '<u>', 'Well', '</u>','</b>','<br/>',
                                             '<b>', 'ID: ', '</b>', name,'<br/>',
                                             '<b>', 'Owner: ', '</b>', well_owner, '<br/>',
                                             '<b>', 'Ground Elevation: ', '</b>', elevation,'<br/>',
                                             '<b>', 'Groundwater Depth (m): ', '</b>', well_depth_m, '<br/>',
                                             '<b>', 'Well Type: ', '</b>', well_type, '<br/>',
                                             '<b>', 'Description: ', '</b>', description,'<br/>',
                                             '<b>', 'Notes: ', '</b>', well_notes, '<br/>',
                                             '<b>', 'Pictures: ', '</b>', glue('<a href="{link_to_photos}">Link</a>'), '<br/>'#,
                             ),
                             group = 'Wells',
                             label = ~glue('Well (Site ID: {name})')
            )
        
        # add water quality test data ----
        l_enoc <- l_enoc %>% 
            addCircleMarkers(data = sf_enoc_wq %>% 
                                 mutate(col_w_air_plot = case_when(colonies_with_air == 'TNTC' ~
                                                                       200, 
                                                                   TRUE ~ as.numeric(colonies_with_air)),
                                        col_wo_air_plot = case_when(colonies_without_air == 'TNTC' ~
                                                                        200, 
                                                                    TRUE ~ as.numeric(colonies_without_air))),
                             options = pathOptions(pane = "water_quality"),
                             # radius = ~log(col_wo_air_plot) * 4 + 5,
                             radius = ~ col_wo_air_plot / 8 + 5,
                             stroke = TRUE, 
                             weight = 0.5, 
                             color = 'black', 
                             opacity = 1,
                             fill = TRUE, 
                             fillOpacity = 0.8, 
                             fillColor = color_wq_test, # 'grey', # ~wqi.leaflet.pal(WQI),
                             # clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                             clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11, 
                                                                   maxClusterRadius = 60),
                             popup = ~paste0('<b>', '<u>', 'Water Quality Test', '</u>','</b>','<br/>',
                                             '<b>', 'ID: ', '</b>', name,'<br/>',
                                             '<b>', 'Name: ', '</b>', household_site_name, '<br/>',
                                             '<b>', 'Colonies With Air: ', '</b>', colonies_with_air,'<br/>',
                                             '<b>', 'Colonies Without Air: ', '</b>', colonies_without_air, '<br/>',
                                             '<b>', 'Data Form: ', '</b>', glue('<a href="{wq_data_form_link}">Link to Data Form</a>'), '<br/>',
                                             '<b>', 'Pictures: ', '</b>', glue('<a href="{wq_pictures_link}">Link to Pictures</a>'), '<br/>'#,
                             ),
                             group = 'Water Quality Tests',
                             label = ~glue('Water Quality Test (Site ID: {name})')
            )
        
        
        # add percolation / soil type data ----
        l_enoc <- l_enoc %>% 
            addCircleMarkers(data = sf_enoc_perc_soil,
                             options = pathOptions(pane = "perc_soil"),
                             radius = 5,
                             stroke = TRUE, 
                             weight = 0.5, 
                             color = 'black', 
                             opacity = 1,
                             fill = TRUE, 
                             fillOpacity = 1, 
                             fillColor = color_perc_soil, # 'grey', # ~wqi.leaflet.pal(WQI),
                             # clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                             clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11, 
                                                                   maxClusterRadius = 60),
                             popup = ~paste0('<b>', '<u>', 'Percolation / Soil Test', '</u>','</b>','<br/>',
                                             '<b>', 'ID: ', '</b>', name,'<br/>',
                                             '<b>', 'Name: ', '</b>', household_site_name, '<br/>',
                                             '<b>', 'Approx. Perc. Rate (min/25mm): ', '</b>', approx_perc_rate_min_25mm, '<br/>',
                                             '<b>', 'Soil Type: ', '</b>', soil_type,'<br/>',
                                             '<b>', 'Data Form: ', '</b>', glue('<a href="{perc_soil_data_form_link}">Link to Data Form</a>'), '<br/>',
                                             '<b>', 'Perc. Test Pictures: ', '</b>', glue('<a href="{perc_pictures_link}">Link to Pictures</a>'), '<br/>',
                                             '<b>', 'Soil Test Pictures: ', '</b>', glue('<a href="{soil_jar_pictures_link}">Link to Pictures</a>'), '<br/>'
                             ),
                             group = 'Perc/Soil Tests',
                             label = ~glue('Perc/Soil Test (Site ID: {name})')
            )
        
        # add existing latrine data ----
        l_enoc <- l_enoc %>% 
            addCircleMarkers(data = sf_enoc_latrines_existing %>% 
                                 mutate(broken = case_when(broken == 'X' ~ 'Yes',
                                                           TRUE ~ 'No'),
                                        near_full = case_when(near_full == 'X' ~ 'Yes',
                                                              TRUE ~ 'No')) %>% 
                                 mutate(percent_full_map = case_when(latrine_percent_full_percent == 'N/A' ~ 
                                                                         50,
                                                                     TRUE ~ as.numeric(latrine_percent_full_percent))),
                             options = pathOptions(pane = "latrines_existing"),
                             radius = ~ percent_full_map * 0.1 + 3,
                             stroke = TRUE, 
                             weight = 0.5, 
                             color = 'black', 
                             opacity = 1,
                             fill = TRUE, 
                             fillOpacity = 1, 
                             fillColor = ~ ifelse(broken == 'Yes', 
                                                  color_latrine_broken,
                                                  ifelse(near_full == 'Yes',
                                                         color_latrine_almost_full,
                                                         color_latrine_good)), # 'grey', # ~wqi.leaflet.pal(WQI),
                             # clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                             clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11, 
                                                                   maxClusterRadius = 60),
                             popup = ~paste0('<b>', '<u>', 'Existing Latrine', '</u>','</b>','<br/>',
                                             '<b>', 'ID: ', '</b>', name,'<br/>',
                                             '<b>', 'Status: ', '</b>', latrine_status, '<br/>',
                                             '<b>', 'Age (yr): ', '</b>', latrine_age_yr,'<br/>',
                                             '<b>', 'Number of Users: ', '</b>', number_of_latrine_users, '<br/>', 
                                             '<b>', 'Pit Dimensions (m): ', '</b>', round(as.numeric(pit_length_m), 1), ' x ', round(as.numeric(pit_width_m), 1), ' x ', round(as.numeric(pit_depth_m), 1), '<br/>',
                                             '<b>', 'Percent Full: ', '</b>', latrine_percent_full_percent, '%<br/>',
                                             '<b>', 'Sludge Accum. Rate (m<sup>3</sup>/person/yr): ', '</b>', round(as.numeric(sludge_accumulation_rate_m3_person_yr), 4), '<br/>',
                                             '<b>', 'Structure Materials: ', '</b>', latrine_structure_materials, '<br/>',
                                             '<b>', 'Site Elevation: ', '</b>', elevation, '<br/>',
                                             '<b>', 'Pictures: ', '</b>', glue('<a href="{pictures}">Link to Pictures</a>'), '<br/>'#,
                                             # '<b>', 'Soil Test Pictures: ', '</b>', glue('<a href="{soil_jar_pictures_link}">Link to Pictures</a>'), '<br/>'
                             ),
                             group = 'Existing Latrines',
                             label = ~glue('Existing Latrine (Site ID: {name})')
            )
        
        # add new latrine data ----
        l_enoc <- l_enoc %>% 
            addCircleMarkers(data = sf_enoc_latrines_new,
                             options = pathOptions(pane = "latrines_new"),
                             radius = 5,
                             stroke = TRUE, 
                             weight = 0.5, 
                             color = 'black', 
                             opacity = 1,
                             fill = TRUE, 
                             fillOpacity = 1, 
                             fillColor = color_latrine_new, # 'grey', # ~wqi.leaflet.pal(WQI),
                             # clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                             clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11,
                                                                   maxClusterRadius = 60),
                             popup = ~paste0('<b>', '<u>', 'New Latrine Site', '</u>','</b>','<br/>',
                                             '<b>', 'ID: ', '</b>', name,'<br/>',
                                             '<b>', 'Foot Traffic: ', '</b>', foot_traffic, '<br/>',
                                             '<b>', 'Dist. to Household (m): ', '</b>', distance_new_latrine_to_household_m,'<br/>',
                                             '<b>', 'Dist. to Water Source (m): ', '</b>', distance_new_latrine_to_water_source_m, '<br/>', 
                                             '<b>', 'Dist. to Property Line (m): ', '</b>', distance_new_latrine_to_property_line_m, '<br/>',
                                             '<b>', 'Site Elevation: ', '</b>', elevation, '<br/>',
                                             '<b>', 'Pictures: ', '</b>', glue('<a href="{pictures}">Link to Pictures</a>'), '<br/>'#,
                             ),
                             group = 'New Latrines',
                             label = ~glue('New Latrine (Site ID: {name})')
            )
        
        
        # add new household data ----
        l_enoc <- l_enoc %>% 
            addCircleMarkers(data = sf_enoc_households,
                             options = pathOptions(pane = "households"),
                             radius = 5,
                             stroke = TRUE, 
                             weight = 0.5, 
                             color = 'black', 
                             opacity = 1,
                             fill = TRUE, 
                             fillOpacity = 1, 
                             fillColor = color_households, # 'grey', # ~wqi.leaflet.pal(WQI),
                             # clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                             clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11,
                                                                   maxClusterRadius = 60),
                             popup = ~paste0('<b>', '<u>', 'Household', '</u>','</b>','<br/>',
                                             '<b>', 'ID: ', '</b>', name,'<br/>',
                                             '<b>', 'No. of Inhabitants: ', '</b>', number_of_inhabitants, '<br/>', 
                                             '<b>', 'No. of Latrine Users: ', '</b>', number_of_latrine_users, '<br/>', 
                                             '<b>', 'No. of Latrines: ', '</b>', number_of_latrines, '<br/>', 
                                             '<b>', 'No. of Handwash Stations: ', '</b>', number_of_hand_wash_stations, '<br/>', 
                                             '<b>', 'Regularly Wash Hands?: ', '</b>', do_you_regularly_wash_your_hands,'<br/>',
                                             '<b>', 'Family Sick Often?: ', '</b>', does_your_family_get_sick_often, '<br/>', 
                                             '<b>', 'Site Elevation: ', '</b>', elevation, '<br/>',
                                             '<b>', 'Pictures: ', '</b>', glue('<a href="{pictures}">Link to Pictures</a>'), '<br/>'#,
                             ),
                             group = 'Households',
                             label = ~glue('Household (Site ID: {name})')
            )
        
        # layers control ----
        l_enoc <- l_enoc %>% 
            addLayersControl(baseGroups = basemap_options,
                             overlayGroups = c('Wells', 
                                               'Water Quality Tests', 
                                               'Perc/Soil Tests', 
                                               'Existing Latrines', 
                                               'New Latrines',
                                               'Households',
                                               'Sites Legend'
                                               ),
                             options = layersControlOptions(collapsed = TRUE,
                                                            autoZIndex = TRUE))
        
        
        rr <- p(tags$strong('Marker Sizes: '), br(), 
                HTML('&#8226 Existing latrines = percent full'), br(),
                HTML('&#8226 Water quality = # of colonies w/o air'), br(),
                HTML('&#8226 Well = water depth (inverse)'))
        
        l_enoc <- l_enoc %>% addControl(rr, position = 'bottomright')
        
        
        # legend ----
        l_enoc <- l_enoc %>% 
            addLegendCustom(colors = c(color_well_gw_depth, color_well_no_gw_depth, 
                                       color_wq_test, color_perc_soil, 
                                       color_latrine_good, color_latrine_almost_full, 
                                       color_latrine_broken, color_latrine_new, 
                                       color_households),
                            labels = c('Well (w/ GW Depth)', 'Well (w/o GW Depth)', 
                                       'WQ Test (Coliform)', 'Perc/Soil Test', 
                                       'Existing Latrine', 'Existing Latrine (Almost Full)',
                                       'Existing Latrine (Broken)', 'New Latrine Site',
                                       'Household'), 
                            sizes = 15, 
                            shapes = 'circle', 
                            borders = 'black',
                            group_name = 'Sites Legend', 
                            layer_name = 'sites_legend', 
                            title = 'Site Types', 
                            opacity = 0.8) %>%
            {.}
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
