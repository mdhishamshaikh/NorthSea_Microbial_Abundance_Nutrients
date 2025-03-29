source("./scripts/0_source.R")
# Importing FCSExpress5 output

#fcs_express_phyto<- readxl::read_xlsx("C:/Users/hisham.shaikh/OneDrive - UGent/Projects/Microbial_Abundances/Microbial_Abundances_NJ2020_PE477_PE486/Algal_Abundances_NJ2020_PE477_PE486/Working_Algal_Abundances_NJ2020_PE477_PE486/FCS_Express5/PE477_PE486_algal_abundance_FCSExpress5_Output.xlsx")

fcs_express_phyto<- readxl::read_xlsx("./data/NJ2020_PE477_PE486_algal_abundance_with_total_FCSExpress5_Output.xlsx")


# As all the gates were added as columns and not rows, I will slice chunks of 7 columns and add them as rows
num_chunks <- ncol(fcs_express_phyto) / 8


stacked_data <- lapply(1:num_chunks, function(i) {
  # Select the 8 columns for this chunk
  chunk <- fcs_express_phyto[, ((i - 1) * 8 + 1):(i * 8)]
  
  # Rename the columns uniformly
  colnames(chunk) <- c("Overlay", "Filename", "Gate", "Total_Events", 
                       "SSCA_geometric_mean", "FL_geometric_mean",
                       "percent_gated_cells", "percent_all_cells")
  
  return(chunk)
})
# FBR - total phyto
# FBO - Synecho
# FGO - Dogger


# Extracting meadata####
# To figureout the measurement dates of FCS files to assign flowrate


fcs_dir <- "./data/NJ2020_PE477_PE486_Algal_Files"
fcs_files <- list.files(fcs_dir, pattern = "\\.fcs$", full.names = TRUE)

# Initialize a data frame to store results
measurement_info <- data.frame(File = basename(fcs_files), Date = NA, Acquisition_Duration = NA)

# Loop through each file and extract metadata
for (i in seq_along(fcs_files)) {
  tryCatch({
    fcs_data <- read.FCS(fcs_files[i], transformation = FALSE, truncate_max_range = FALSE)
    metadata <- keyword(fcs_data)  # Extract metadata
    
    # Extract measurement date
    measurement_info$Date[i] <- metadata[["$DATE"]]
    
    # Extract and compute measurement duration
    if (!is.null(metadata[["$BTIM"]]) && !is.null(metadata[["$ETIM"]])) {
      start_time <- as.POSIXct(metadata[["$BTIM"]], format="%H:%M:%S")
      end_time <- as.POSIXct(metadata[["$ETIM"]], format="%H:%M:%S")
      measurement_info$Acquisition_Duration[i] <- as.numeric(difftime(end_time, start_time, units="mins"))
    } else {
      measurement_info$Acquisition_Duration[i] <- NA  # Assign NA if time info is missing
    }
  }, error = function(e) {
    message(paste("Skipping file due to error:", fcs_files[i]))  # Print message
    measurement_info$Date[i] <- NA
    measurement_info$Acquisition_Duration[i] <- NA
  })
}

# Addingmissing ifo for 

measurement_info$Date[which(measurement_info$File == "Cruise_NorthSea_PE477_Algae_PE477_7_7m_Bottle3.fcs")] <- "11-MAY-2021"

measurement_info <- measurement_info %>%
  mutate(Tag = sub(".*_(PE\\d+_\\d+_\\d+).*", "\\1", File)) %>%
  filter(!grepl("Cruise_NorthSea", Tag))%>%
  separate(Tag, into = c("Location", "Station_Number", "Depth"), sep = "_", convert = TRUE) %>%
  mutate(Acquisition_Duration = round(replace_na(Acquisition_Duration, 10)))
measurement_info

# Replacing The station number for NJ and depthS

measurement_info[42:48,]$Station_Number <- 1:7
measurement_info[42:48,]$Depth <- 1


# Getting per mL counts #
# PE477 were measured on 11th may 2021
# PE486B measured on 10th May 2021
# Flow rate 10th May = 107 µL min-1
# Flow rate 11th May = 129 µL min-1
# All samples measured for ~ 10 mins

phyto_counts2 <- do.call(rbind, stacked_data)

phyto_counts2 <- phyto_counts2 %>%
  mutate(Filename = str_remove(Filename, " compensated$")) %>%
  left_join(measurement_info %>% 
              select(File, Location, Station_Number, Depth, Acquisition_Duration), 
            by = c("Filename" = "File")) %>%
  
  # Add Flow Rate based on Location
  mutate(Flow_Rate = case_when(
    Location == "NJ" ~ 129,
    Location == "PE477" ~ 129,   # Flow rate for PE477
    Location == "PE486" ~ 107,   # Flow rate for PE486
    TRUE ~ NA_real_            # Assign NA if Location is neither
  )) %>%
  
  # Calculate Cells per mL
  mutate(cells_per_mL = Total_Events / (Acquisition_Duration * Flow_Rate * 1e-3)) %>%
  dplyr::mutate(Location = case_when(Location == "NJ" ~ "NJ2020", TRUE ~ Location)) %>%
  dplyr::mutate(Location_Station_Number = paste(Location, Station_Number, sep = "_")) 


phyto_counts_wide <- phyto_counts2 %>%
  dplyr::select(
    Location, Station_Number, Location_Station_Number, Depth, Filename, Gate, cells_per_mL
  ) %>%
  dplyr::rename(Phyto_Sample_Name = Filename) %>%
  tidyr::pivot_wider(
    names_from = Gate,
    values_from = cells_per_mL
  ) %>%
  dplyr::rename(
    "Total_Phyto" = "total_phyto",
    "Synecho" = "synecho",
    "Dogger" = "dogger"
  ) %>%
  dplyr::mutate(Station = dplyr::recode(Location_Station_Number, 
                                        !!!setNames(as.character(c("NJ-1", "NJ-2", "NJ-3", "NJ-4", "NJ-5", "NJ-6", "NJ-7",
                                                                   1:12, 13.1, 13.2, 14)),
                                                    c("NJ2020_1", "NJ2020_2", "NJ2020_3", "NJ2020_4",
                                                      "NJ2020_5", "NJ2020_6", "NJ2020_7",
                                                      "PE477_1", "PE477_2","PE477_3", "PE477_4",
                                                      "PE477_5", "PE477_6", "PE477_7",
                                                      "PE486_1", "PE486_2", "PE486_3", "PE486_4", 
                                                      "PE486_5","PE486_6", "PE486_7", "PE486_8")))) %>%
  dplyr::mutate(Station = factor(Station, levels = as.character(c("NJ-1", "NJ-2", "NJ-3", "NJ-4", "NJ-5", "NJ-6", "NJ-7",
                                                                  1:12, 13.1, 13.2, 14)), ordered = TRUE),
                Location = factor(Location, levels = c("NJ2020", "PE477", "PE486"))) %>%
  dplyr::mutate(
    Fraction_Syencho = Synecho/Total_Phyto,
    Fraction_Dogger = Dogger/Total_Phyto
  )











write.csv(phyto_counts_wide, file = "./results/abundance_nutrients/phytoplankton_counts_nj2020_pe477_pe486.csv", row.names= F)


custom_palette <- c(
  "PE477_1" = "#1f77b4", "PE477_2" = "#ff7f0e", "PE477_3" = "#2ca02c",
  "PE477_4" = "#d62728", "PE477_5" = "#9467bd", "PE477_6" = "#8c564b", 
  "PE486_1" = "#e377c2", "PE486_2" = "#7f7f7f", "PE486_3" = "#bcbd22",
  "PE486_4" = "#17becf", "PE486_5" = "#aec7e8", "PE486_6" = "#ffbb78",
  "PE477_7" = "#98df8a", "PE486_7" = "#ff9896"
)

# Create the plot with facets for Depth and Gate
ggplot(phyto_counts2, aes(x = Location_Station_Number, y = cells_per_mL, fill = Location_Station_Number)) +
  geom_bar(stat = "identity") + # Bar plot with count of events
  facet_grid(Depth ~ Gate, scales = "fixed", switch = "both") + # Facets for Depth and Gate
  scale_fill_manual(values = custom_palette) +
  labs(
    title = "Distribution of Events Across Location_Station",
    x = "Location_Station",
    y = "Number of Events"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray", colour = "black"),
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 90)
  )

ggsave("phyto_abundance_facet_plot_depths.svg", width = 12, height = 8, dpi = 800)










