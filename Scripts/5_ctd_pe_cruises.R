# AIM: To extract CTD profiles for all PE477 & PE486 stations, and extract values for the sampling depths (7, 15, and 30 m)

# 0.0 Setting up ####
source("./scripts/0_source.R")

# 1.0 Importing CTD profiles and selecting stations ####
file_path <- "./data/ctd_profiles"
file_list <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE)

# Function to add file name as a column
read_and_label <- function(file) {
  
  df <- read.csv(file)
  df$file_name <- basename(file)
  return(df)
}


# Changing file names and keeping only the stations needed.
combined_df <- bind_rows(lapply(file_list, read_and_label))
combined_df$file_name <- sub("\\.csv$", "", combined_df$file_name)
combined_df <- combined_df[combined_df$file_name %in% c("PE486_S02C01", "PE486_S04C02", "PE486_S06C01", "PE486_S08C01", 
                                                        "PE486_S09C01", "PE486_S10C01", "PE486_S11C01", "PE486_S12C01", 
                                                        "64PE477_S00C01", "64PE477_S04C01", "64PE477_S05C01", "64PE477_S07C01", 
                                                        "64PE477_S10C01", "64PE477_S15C01", "64PE477_S17C01"), ]

# Simplifying station names and adding Location, Station_Number columns
name_mapping <- c(
  "PE486_S02C01" = "PE486_1",
  "PE486_S04C02" = "PE486_2",
  "PE486_S06C01" = "PE486_3",
  "PE486_S08C01" = "PE486_4",
  "PE486_S09C01" = "PE486_5",
  "PE486_S10C01" = "PE486_6",
  "PE486_S11C01" = "PE486_7",
  "PE486_S12C01" = "PE486_8",
  "64PE477_S00C01" = "PE477_1",
  "64PE477_S04C01" = "PE477_2",
  "64PE477_S05C01" = "PE477_3",
  "64PE477_S07C01" = "PE477_4",
  "64PE477_S10C01" = "PE477_5",
  "64PE477_S15C01" = "PE477_6",
  "64PE477_S17C01" = "PE477_7"
)

combined_df$Location_Station_Number <- name_mapping[combined_df$file_name]
combined_df <- combined_df %>%
  separate(Location_Station_Number, into = c("Location", "Station_Number"), sep = "_", remove = F)

unique(combined_df$Location_Station_Number)


# Removing station PE477_7 and PE486_8 as they do not have viral production data attached to them. 
# Will need these stations for 'omics work though.
# combined_df <- combined_df %>%
#   dplyr::filter(!Location_Station_Number %in% c("PE477_7", "PE486_8")) 


# 2.0 Selecting variables and simplifying names ####

combined_df <- combined_df %>%
  rename(
    Original_station_cast = file_name,
    Depth = depSM,
    Salinity = sal00,
    Temperature = t090C,
    Pressure = prDM,
    Conductivity = C0,
    Oxygen = sbeox0Mm.L,
    Turbidity = turb,
    Chlorophyll = flC
  )

# 3.0 Calculating density and removing surface values values ####

# Gibbs SeaWater 
combined_df$Density <- gsw::gsw_rho(combined_df$Salinity, combined_df$Temperature, combined_df$Pressure)

combined_df <- combined_df %>%
  dplyr::filter(Depth > 3) # Making sure first 3 meters are not considered as the disturbance from dropping the CTD frame creates large fluctuations in measurements.

ctd_profiles<- combined_df %>%
  dplyr::select(c("Original_station_cast", "Location_Station_Number", "Depth", "Salinity", "Temperature", "Pressure", "Density", "Conductivity", "Oxygen", "Turbidity", "Chlorophyll")) %>%
  separate(Location_Station_Number, c("Location", "Station_Number"), "_", remove = F)


# Max depth
ctd_profiles <- ctd_profiles %>%
  group_by(Location_Station_Number) %>%
  mutate(Max_Depth = max(Depth)) %>%
  ungroup()

# Recoding station names
ctd_profiles <- ctd_profiles %>%
  
  dplyr::mutate(Station = dplyr::recode(Location_Station_Number, 
                                        !!!setNames(as.character(c(1:12, 13.1, 13.2, 14)),
                                                    c("PE477_1", "PE477_2","PE477_3", "PE477_4",
                                                      "PE477_5", "PE477_6", "PE477_7",
                                                      "PE486_1", "PE486_2", "PE486_3", "PE486_4", 
                                                      "PE486_5","PE486_6", "PE486_7", "PE486_8")))) %>%
  dplyr::mutate(Station = factor(Station, levels = as.character(c(1:12, 13.1, 13.2, 14)), ordered = TRUE),
                Location = factor(Location, levels = c("PE477", "PE486")))

# Writing output as a csv  
write.csv(ctd_profiles, "./results/ctd_profiles/ctd_profiles.csv", row.names = F)




# 2.0 Extracting downcast ####
filter_downcast <- function(df) {
  max_depth <- unique(df$Max_Depth)  
  stop_index <- which(df$Depth >= max_depth)[1]  
  
  if (!is.na(stop_index)) {
    return(df[1:stop_index, ])  
  } else {
    return(df)  
  }
}

ctd_downcast <- ctd_profiles %>%
  dplyr::group_by(Original_station_cast) %>%
  dplyr::group_modify(~ filter_downcast(.x)) %>%
  dplyr::ungroup()
unique(paste(ctd_downcast$Location_Station_Number, ctd_downcast$Station))

write.csv(ctd_downcast, "./results/ctd_profiles/ctd_downcast.csv", row.names = F)


# Visualizing down cast for temperature and turbidity 


plot_depth_profile <- function(data, x_var, y_var = "Depth", location_var = "Location", station_var = "Station") {
  
  x_var <- rlang::sym(x_var)
  y_var <- rlang::sym(y_var)
  location_var <- rlang::sym(location_var)
  station_var <- rlang::sym(station_var)
  
  data <- data %>%
    dplyr::mutate(!!location_var := factor(!!location_var, levels = unique(!!location_var))) 
  
  max_depth <- max(data[[rlang::as_string(y_var)]], na.rm = TRUE) 
  
  station_counts <- data %>%
    dplyr::group_by(!!location_var) %>%
    dplyr::summarise(num_stations = n_distinct(!!station_var)) 
  
  plot_list <- list()
  
  for (loc in levels(data[[rlang::as_string(location_var)]])) {
    data_subset <- data %>% filter(!!location_var == loc)
    
    plot_list[[loc]] <- ggplot(data_subset, aes(x = !!x_var, y = !!y_var)) +
      geom_point() +
      geom_hline(yintercept = 7, color = "#850000") +
      facet_wrap(vars(!!station_var), scales = "fixed", nrow = 1) +  
      scale_y_reverse(limits = c(max_depth, 0)) +  
      labs(title = paste(loc, x_var), 
           x = rlang::as_string(x_var), 
           y = rlang::as_string(y_var)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90))
  }
  
  widths <- station_counts$num_stations / sum(station_counts$num_stations)
  
  combined_plot <- patchwork::wrap_plots(plot_list) + 
    plot_layout(widths = widths)
  
  ggsave(combined_plot, filename = paste0("./figures/ctd_profiles/ctd_profile_", x_var, ".svg"), dpi = 1000, width = 10)
  return(combined_plot)
  
}

plot_depth_profile(data = ctd_downcast, x_var = "Temperature")
plot_depth_profile(data = ctd_downcast, x_var = "Turbidity")
plot_depth_profile(data = ctd_downcast, x_var = "Salinity")
plot_depth_profile(data = ctd_downcast, x_var = "Chlorophyll")
plot_depth_profile(data = ctd_downcast, x_var = "Oxygen")
plot_depth_profile(data = ctd_downcast, x_var = "Pressure")
plot_depth_profile(data = ctd_downcast, x_var = "Conductivity")


# 3.0 Extracting values for closest depth 7m, 15 m and 30 m ####

target_depths <- c(7, 15, 30) 


get_closest_depths <- function(df, target_depths) {
  closest_values <- target_depths %>% 
    lapply(function(target) {
      df %>%
        dplyr::mutate(Distance = abs(Depth - target)) %>%  
        dplyr::slice_min(Distance, n = 1) %>%  
        dplyr::mutate(Target_Depth = target)  
    }) %>%
    dplyr::bind_rows()  
  return(closest_values)
}

ctd_closest_depths <- ctd_downcast %>%
  dplyr::group_by(Station) %>%
  dplyr::group_modify(~ get_closest_depths(.x, target_depths)) %>%
  dplyr::ungroup() %>%
  dplyr::select(Location_Station_Number, Target_Depth, Depth, everything()) %>%
  dplyr::rename(Measured_depth = Depth) %>%# Organize columns
  dplyr::rename(Depth = Target_Depth)

# After examining the distance between target depth and measured depth, it is evident that both PE477_5 and PE486_3 are not as deep as 30 m. 
# Also incase there are more than one values for the same measured_depths, I will take the first one
ctd_closest_depths <- ctd_closest_depths %>%
  dplyr::filter(Distance < 0.5) # to get rid of values for the depth where max depth is less than 30 m

write.csv(ctd_closest_depths, "./results/ctd_profiles/ctd_sampling_depths.csv", row.names = F)







