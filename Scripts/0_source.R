#1.0 Function to install and load packages ####


install_and_load_packages <- function(packages) {
  # Ensure BiocManager is installed for Bioconductor packages
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  
  # Ensuring devtools is installed for GitHub packages
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  library(devtools)
  
  # Installing and loading packages
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      if (package %in% c("flowCore", "flowWorkspace", "ggcyto")) {
        # Installing Bioconductor packages
        BiocManager::install(package)
      } else if (package == "viralprod") {
        # Installing viralprod from GitHub
        devtools::install_github("mdhishamshaikh/ViralProduction_R")
      } else {
        # Installing CRAN packages
        install.packages(package)
      }
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages_to_load <- c(
  "cowplot",
  "data.table",
  # "phyloseq",
  # "microeco",
  # "compositions",
  # "ComplexHeatmap",
  # "circlize",
  "colorspace",
  "flowWorkspace",
  "flowCore",
  "openxlsx",
  "patchwork",
  "scales",
  "readxl",
  "readr",
  "ggcyto",
  "ggsci",
  # "svglite",
  # "oce",
  # "terra", #
  "viridis",
  # "viralprod",
  # "pracma",
  # "tidyplots",
  "grid",
  "ggpubr",
  # "car",
  # "ggspatial",
  # "caret",
  # "FactoMineR",
  # "factoextra",
  "ggrepel",
  # "rstatix",
  # "MASS",
  # "vegan",
  "tidyverse"
)

# Loading packages
install_and_load_packages(packages_to_load)