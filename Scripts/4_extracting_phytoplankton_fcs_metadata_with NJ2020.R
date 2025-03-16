# To figureout the measurement dates of FCS files to assign flowrate

library(flowCore)
library(tidyverse)

fcs_dir <- "./Data/NJ2020_PE477_PE486_Algal_Files"
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



#### NEED TO WORK FROM HERE

measurement_info <- measurement_info %>%
  mutate(Tag = sub(".*_(PE\\d+_\\d+_\\d+).*", "\\1", File)) %>%
  filter(!grepl("Cruise_NorthSea", Tag))%>%
  separate(Tag, into = c("Location", "Station_Number", "Depth"), sep = "_", convert = TRUE) %>%
  mutate(Acquisition_Duration = round(replace_na(Acquisition_Duration, 10)))
measurement_info

# Replacing The station number for NJ and depthS

measurement_info[42:48,]$Station_Number <- 1:7
measurement_info[42:48,]$Depth <- 1
