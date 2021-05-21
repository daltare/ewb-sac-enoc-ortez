# load packages ----
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(writexl)
library(readxl)
library(here)

options(scipen = 999)

# import data from kml file -----------------------------------------------
sf_enoc <- st_read(here('input_raw_data', 
                        'Enoc Ortez .kml')) %>% # file available here: https://drive.google.com/drive/folders/1ya2hKfcOM_aKhK2nAimTokBQETGtOFDZ
    clean_names()

    ## - check -
    # View(sf_enoc)
    # glimpse(sf_enoc)
    # st_crs(sf_enoc) # epsg: 4326 -- https://epsg.io/4326


# clean / format data -----------------------------------------------------
## revise some site names (IDs) may need to double check ----
sf_enoc <- sf_enoc %>% 
    mutate(name = case_when(
        name == 'COMUNITY WELL W16' ~ 'W16',
        name == 'W20CAPTACION EL CHAGUITE' ~ 'W20',
        name == 'NL22 - GT ' ~ 'NL22',
        name == 'NL42 - Perc - Sc' ~ 'NL42',
        name == 'NL75 - Perc - SC' ~ 'NL75',
        name == 'NL - Perc - SC' ~ 'NL81', # based on the tests listed in the description field ("Percoletion #6 Soil Compotion #6") and the household associated with those tests -- see: https://docs.google.com/document/d/1E_eLVppMIwEaTcc4XXIJnkMLVsPWgp-a/edit 
        name == 'P01' ~ 'NL06', # based on the fact that the description field ("MARTA PEREZ    SC01 HH6") associates this site with HH6, which is also where soil composition test / percolation test #1 was done -- see: https://docs.google.com/document/d/1-yhHRJ1-O7AIjOcMaRE0OD2G7SXhNCDk/edit
        TRUE ~ name
    ))


## re-arrange ----
sf_enoc <- sf_enoc %>% 
    arrange(name)

## add site type ----
sf_enoc <- sf_enoc %>% 
    mutate(site_type = case_when(
        str_detect(string = name, pattern = '^W') ~ 'Well',
        str_detect(string = name, pattern = '^HH') ~ 'Household',
        str_detect(string = name, pattern = '^ST') ~ 'Storage Tank',
        str_detect(string = name, pattern = '^NL') ~ 'New Latrine Site',
        str_detect(string = name, pattern = '^OL') ~ 'Existing Latrine Site',
        TRUE ~ NA_character_)
    )

## extract coordinates and add them as separate columns ----
coords <- sf_enoc %>% 
    st_coordinates() %>%
    as_tibble()
    
sf_enoc <- sf_enoc %>% 
    bind_cols(coords) %>% 
    rename(latitude = Y,
           longitude = X,
           elevation = Z)


## add datum info ----
sf_enoc <- sf_enoc %>% 
    mutate(datum = 'WGS 84')



# write data from kml (all sites) as csv ----------------------------------
write_csv(x = sf_enoc %>% st_drop_geometry(), 
          file = here('output_data', 
                      'enoc_ortez_sites_all.csv'))

# write as excel
write_xlsx(x = sf_enoc %>% st_drop_geometry(), 
           path = here('output_data', 
                       'enoc_ortez_sites_all.xlsx'))




# separate by sites type and add more info --------------------------------------

## wells ----
sf_wells <- sf_enoc %>% 
    filter(site_type == 'Well')

### add well depth
df_well_depth <- read_excel(here('input_raw_data', 
                                 'Copy of Groundwater table in Wells.xlsx'), 
                            skip = 3) %>% 
    clean_names() %>% 
    rename(well_type = type, 
           well_notes = notes, 
           well_owner = owner)

### join depths to locations
sf_wells <- sf_wells %>% 
    left_join(df_well_depth %>% 
                  select(code_gps, well_depth_m, well_type, 
                         well_notes, well_owner, link_to_photos), 
              by = c('name' = 'code_gps')
              ) %>% 
    mutate(well_depth_m = case_when(well_depth_m == '-' ~ NA_character_,
                                   TRUE ~ well_depth_m)) %>% 
    mutate(well_depth_m = as.numeric(well_depth_m))
    
### write to geopackage file
st_write(sf_wells, 
         here('output_data', 
              'enoc_ortez_wells.gpkg'), 
         append = FALSE)


    
## existing latrines ----
sf_latrines_existing <- sf_enoc %>% 
    select(-description) %>% 
    filter(site_type == 'Existing Latrine Site') %>% 
    mutate(id_number = parse_number(name))

df_latrine_ex <- read_excel(here('input_raw_data', 
                                 'Survey I Results_Mod.xlsx'), 
                            sheet = 'Form', 
                            skip = 1
                            ) %>% 
    clean_names() %>% 
    slice(1:96) %>% 
    # rename() %>% 
    mutate(id_number = parse_number(site_id)) %>% 
    # select(-starts_with('x')) %>% 
    {.}

df_latrine_summary <- read_excel(here('input_raw_data', 
                                 'Survey I Results_Mod.xlsx'), 
                            sheet = 'Summary', 
                            skip = 1) %>% 
    clean_names() %>% 
    slice(1:96) %>% 
    # rename() %>% 
    mutate(id_number = parse_number(site_id), 
           sludge_accumulation_rate_m3_person_yr = as.numeric(sludge_accumulation_rate_m3_person_yr)) %>% 
    select(-starts_with('x'))

df_latrine_links <- read_excel(here('input_raw_data', 
                                 'Survey I Results_Mod.xlsx'), 
                            sheet = 'Links') %>% 
    clean_names() %>% 
    mutate(id_number = parse_number(site_id)) %>% 
    select(-starts_with('x'))

sf_latrines_existing <- sf_latrines_existing %>% 
    left_join(df_latrine_ex %>% 
                  select(site_id , number_of_inhabitants:number_of_hand_wash_stations,
                         latrine_status:distance_existing_latrine_to_property_line_m,
                         id_number) %>% 
                  rename(foot_traffic = foot_traffic_20),
              by = c('id_number')) %>%
    left_join(df_latrine_summary %>% 
                  select(-site_id), 
              by = c('id_number')) %>% 
    left_join(df_latrine_links %>% 
                  select(-site_id), 
              by = c('id_number'))

### write to geopackage file
st_write(sf_latrines_existing, 
         here('output_data', 
              'enoc_ortez_latrines_existing.gpkg'), 
         append = FALSE)

    
## new latrines ----
sf_latrines_new <- sf_enoc %>% 
    filter(site_type == 'New Latrine Site') %>% 
    select(-description) %>% 
    mutate(id_number = parse_number(name)) %>% 
    left_join(df_latrine_ex %>% 
                  select(site_id , 
                         foot_traffic_26:distance_new_latrine_to_property_line_m,
                         id_number) %>% 
                  rename(foot_traffic = foot_traffic_26),
              by = c('id_number')) %>% 
    mutate(distance_new_latrine_to_household_m = round(as.numeric(distance_new_latrine_to_household_m), 1),
           distance_new_latrine_to_water_source_m = round(as.numeric(distance_new_latrine_to_water_source_m), 1),
           distance_new_latrine_to_property_line_m = round(as.numeric(distance_new_latrine_to_property_line_m), 1)) %>% 
    left_join(df_latrine_links %>% 
                  select(-site_id), 
              by = c('id_number'))

### write to geopackage file
st_write(sf_latrines_new, 
         here('output_data', 
              'enoc_ortez_latrines_new.gpkg'), 
         append = FALSE)



## households ----
sf_households <- sf_enoc %>% 
    filter(site_type == 'Household') %>% 
    select(-description) %>% 
    left_join(df_latrine_ex %>% 
                  select(site_id , 
                         number_of_inhabitants:does_your_family_get_sick_often),
              by = c('name' = 'site_id'))%>% 
    left_join(df_latrine_links %>% 
                  select(-id_number), 
              by = c('name' = 'site_id'))

### write to geopackage file
st_write(sf_households, 
         here('output_data', 
              'enoc_ortez_households.gpkg'), 
         append = FALSE)



## water quality ----
df_water_quality <- read_excel(here('input_raw_data', 
                                 'Assessment Data - Water Quality.xlsx'), 
                            sheet = 'Water Quality - Average By Site') %>% 
    clean_names() %>% 
    rename(wq_pictures_link = pictures_link, 
           wq_data_form_link = data_form_link)

### join wq data to locations
sf_water_quality <- sf_enoc %>% 
    filter(name %in% df_water_quality$household_site_id) %>% 
    left_join(df_water_quality %>% 
                  select(household_site_id, household_site_name, wq_pictures_link,
                         wq_data_form_link, date, data_collector:colonies_without_air), 
              by = c('name' = 'household_site_id')
    ) %>% 
    # mutate(colonies_without_air = case_when(colonies_without_air == 'TNTC' ~ NA_character_,
    #                                         TRUE ~ colonies_without_air)) %>% 
    # mutate(colonies_without_air = as.numeric(colonies_without_air)) %>% 
    {.}

### write to geopackage file
st_write(sf_water_quality, 
         here('output_data', 
              'enoc_ortez_wq_tests.gpkg'), 
         append = FALSE)


## percolation & soil type ----
df_percolation <- read_excel(here('input_raw_data', 
                                 'Assessment Data - Percolation.xlsx'), 
                            sheet = 'Summary_Percolation Data') %>% 
    clean_names() %>% 
    rename(perc_pictures_link = pictures_link, 
           perc_soil_data_form_link = data_form_link)

df_soil_jar <- read_excel(here('input_raw_data', 
                               'Assessment Data - Soil Jar Test.xlsx'), 
                          sheet = 'Soil Test Data') %>% 
    clean_names() %>% 
    rename(soil_jar_pictures_link = pictures_link, 
           perc_soil_data_form_link = data_form_link)

### join percolation data to locations
sf_percolation_soil <- sf_enoc %>% 
    select(-description) %>% 
    filter(name %in% df_percolation$household_site_id) %>% 
    left_join(df_percolation %>% 
                  select(household_site_id, household_site_name, perc_pictures_link,
                         perc_soil_data_form_link, date, data_collector, approx_perc_rate_min_25mm), 
              by = c('name' = 'household_site_id')
    ) %>% 
    left_join(df_soil_jar %>% 
                  select(household_site_id, soil_jar_pictures_link,
                         soil_type), 
              by = c('name' = 'household_site_id')
    ) %>% 
    {.}


### write to geopackage file
st_write(sf_percolation_soil, 
         here('output_data', 
              'enoc_ortez_percolation_soil_type.gpkg'), 
         append = FALSE)
