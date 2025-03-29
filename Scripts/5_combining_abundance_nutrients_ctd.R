# AIM: To combine abundance, nutrients and CTD data for NJ, PE477 & PE486 ####

# 0.0 Setting up ####
source("./scripts/0_source.R")

# 1.0 Importing data ####
ctd <- data.table::fread("./results/ctd_profiles/ctd_sampling_depths.csv") %>%
  dplyr::mutate(Station = as.factor(Station))

bv_nuts <- data.table::fread("./results/abundance_nutrients/nj2020_pe477_pe486_bv_abundance_abiotic.csv") %>%
  dplyr::mutate(Station = as.factor(Station))
  
phyto <- data.table::fread("./results/abundance_nutrients/phytoplankton_counts_nj2020_pe477_pe486.csv") %>%
  dplyr::mutate(Station = as.factor(Station))


# 2.0 Combingnn by station number and depth ####

# Keeping Salinity & Temperature of NJ2020 from bv_nuts whereas we use the ones from CTD for PE cruises
bv_nuts <- bv_nuts %>%
  dplyr::mutate(
    Salinity = case_when(Location == "NJ2020" ~ Salinity, TRUE ~ NA),
    Temperature = case_when(Location == "NJ2020" ~ Temperature, TRUE ~ NA)
  )


pcb_data <- ctd %>%
  dplyr::full_join(bv_nuts, by = c("Station", "Depth")) %>%
  dplyr::mutate(
    Temperature = dplyr::coalesce(Temperature.x, Temperature.y),
    Salinity    = dplyr::coalesce(Salinity.x, Salinity.y),
    Location    = dplyr::coalesce(Location.x, Location.y),
    Station_Number    = dplyr::coalesce(Station_Number.x, Station_Number.y),
    Location_Station_Number    = dplyr::coalesce(Location_Station_Number.x, Location_Station_Number.y)
  ) %>%
  dplyr::select(-c(Temperature.x, Temperature.y, Salinity.x, Salinity.y,
                   Location.x, Location.y, Station_Number.x, Station_Number.y,
                   Location_Station_Number.x, Location_Station_Number.y)) %>%
  dplyr::left_join(phyto, 
                   by = c("Station", "Depth")) %>%
  dplyr::mutate(
    Location    = dplyr::coalesce(Location.x, Location.y),
    Station_Number    = dplyr::coalesce(Station_Number.x, Station_Number.y),
    Location_Station_Number    = dplyr::coalesce(Location_Station_Number.x, Location_Station_Number.y)
  ) %>%
  dplyr::select(-c(Location.x, Location.y, Station_Number.x, Station_Number.y,
                   Location_Station_Number.x, Location_Station_Number.y)) %>%
  dplyr::mutate(
    Location = factor(as.character(Location), levels = c("NJ2020", "PE477", "PE486"), ordered = T),
    Depth = factor(as.character(Depth), levels = c(1, 7, 15, 30), ordered = T),
    Station = factor(as.character(Station), levels = as.character(c("NJ-1", "NJ-2", "NJ-3", "NJ-4", "NJ-5", "NJ-6", "NJ-7",
                                                      1:12, 13.1, 13.2, 14)), ordered = TRUE)
  ) %>%
  dplyr::arrange(Station, Depth)

# Arranging colnames 
pcb_data_arranged <- pcb_data %>%
  dplyr::select(Station, Depth, Original_station_cast, Expt_Date,
                Latitude, Longitude,
                Max_Depth,
                Temperature, Salinity, Turbidity, Chlorophyll, Oxygen,
                Conductivity, Density, Pressure, Measured_depth,
                Nitrate, Phosphate, Silicate, TON, Nitrite,
                Total_Phyto, Synecho, Dogger, Fraction_Syencho, Fraction_Dogger,
                Total_Bacteria, HNA, LNA, Fraction_HNA,
                Total_Viruses, V1, V2, V3 , Fraction_V1, Fraction_V2, Fraction_V3,
                VBR, ends_with("Sample_Name"), everything(), -Distance)
colnames(pcb_data_arranged)

data.table::fwrite(pcb_data_arranged, "./results/phyical_chemical_biological_metadata_NJ2020_PE477_PE486.csv", row.names = F)
