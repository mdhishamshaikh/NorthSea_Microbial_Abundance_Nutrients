#### 0.0 Setting Up the Environment####

#Setting working directory
setwd("C:/Users/hisham.shaikh/OneDrive - UGent/Projects/Microbial_Abundances/Microbial_Abundances_NJ2020_PE477_PE486/Microbial Abundances/Microbial_Abundances/Microbial_Abundances")

#Install packages if needed.
if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}

BiocManager::install(c("tidyverse", "magrittr", "readxl"))


#Load packages
{
  library("tidyverse")
  library("magrittr")
  library("readxl")
  library("readr")
  library("openxlsx")
}


#### 1.0 Importing Data ####

{
  counts <- as.data.frame(read_csv("counts.csv"))
  dim(counts)
}

{
  #To get the counts per population for each file
  counts<- pivot_wider(counts, names_from = "pop", values_from = "count")
  dim(counts)
  colnames(counts)[colnames(counts) == 'root'] <- "Total"
  colnames(counts)[colnames(counts) == 'file_name'] <- "Sample_Name"
  head(counts)
}

#Import metadata which is saved as an Excel sheet
{
  metadata <- read_excel("Metadata/Metadata_Microbial_Abundances_NJ2020_PE477_PE486.xlsx")
  #adding flowrate
  flowrate<- read_excel("Metadata/Metadata_Microbial_Abundances_NJ2020_PE477_PE486.xlsx", 
                        sheet = "flowrate")
}

 if (length(setdiff(counts$Sample_Name, metadata$Sample_Name)) > 0) {
   print("There are samples that were counted but doesn't have associated metadata")
 } else {
   print("All samples processed have associated metadata")
 }

#Merge counts and metadata in one file & read the dates in appropriate format
{
    counts_metadata<- base::merge(counts, metadata, by = "Sample_Name") 
  counts_metadata<- dplyr::mutate(counts_metadata,Expt_Date = as.Date(as.character(Expt_Date), format= "%y%m%d"))
  counts_metadata<- dplyr::mutate(counts_metadata,Date_Measurement = as.Date(as.character(Date_Measurement), format= "%y%m%d"))
  #flowrate<- dplyr::mutate(flowrate, Date_Measurement = as.Date(as.character(Date_Measurement), format= "%y%m%d"))
  #we could have done flowrate per date of measurement, but because one of our sample is from 2020, the flowrate wasn't noted. So let's go with average.
   # dim(counts_metadata)
  # head(counts_metadata)
  counts_metadata[,'Flowrate'] <- mean(flowrate$Flow_rate)
}

#Adding Boolean columns for bacterial and viral total counts
{
counts_metadata$HNALNA <- counts_metadata$HNA_Bacteria + counts_metadata$LNA_Bacteria
counts_metadata$V1V2V3 <- counts_metadata$V1 + counts_metadata$V2 + counts_metadata$V3

#We can perform a simple linear regression to see if we could use Boolean 
# addition of HNA/LNA and V1/V2/V3 as replacements for total bacterial and viral 
# counts. Alternatively, these could have been added as Boolean gates during processing
scatter.smooth(counts_metadata$Bacteria, counts_metadata$HNALNA, main = "Bacterial Counts")
summary(lm(counts_metadata$Bacteria ~ counts_metadata$HNALNA))  #Gives an R-square value of 1.0
#Perhaps the next few lines aren't as important. 
# plot(fitted(lm(counts_metadata$Bacteria ~ counts_metadata$HNALNA)), resid(lm(counts_metadata$Bacteria ~ counts_metadata$HNALNA)))
# abline(0,0)
# qqnorm(resid(lm(counts_metadata$Bacteria ~ counts_metadata$HNALNA)))
# qqline(resid(lm(counts_metadata$Bacteria ~ counts_metadata$HNALNA)))

scatter.smooth(counts_metadata$Viruses, counts_metadata$V1V2V3, main = "Viral Counts")
summary(lm(counts_metadata$Viruses ~ counts_metadata$V1V2V3))  #Gives an R-square value of 0.999

}
#so we can use the Boolean gates/values for total counts

####Correcting for TE####

#Separate the dataframe containing TE
TE<- counts_metadata[counts_metadata$Sample_Type == 'TE',]
plot(TE[TE$Staining_Protocol == 'Bacteria',]$HNALNA) #Plotting TE counts in Bacteria
plot(TE[TE$Staining_Protocol == 'Viruses',]$V1V2V3) #Plotting TE counts in V1V2V3


#Checking if we can come up with a criterion for detecting outliers in TE samples
TE[TE$Staining_Protocol == 'Bacteria',]$HNALNA #Bacterial TE
boxplot(TE[TE$Staining_Protocol == 'Bacteria',]$HNALNA)
boxplot.stats(TE[TE$Staining_Protocol == 'Bacteria',]$HNALNA)$out


#Attempt at coding the TE statements
#Bacterial and viral files need to be treated differently, in case of determining abundance.
#I usually take only two TE files prior to a sample file to determine TE. 
#Usually, according to Corina, you collect these files, and then calculate the arithmetic mean,
#subtract it, and in the case, the TE is high before a sample, you highlight it, and treat 
#the sample with caution. 
#Alternatively, we only use TE value of the two files prior to the sample file to calculate TE for it. 
#This needs to be done in the main file. 


#Adding an empty TE column. The correct TE for viruses and for bacteria. 
counts_metadata[,'TE_value']<- NA

for (name in counts_metadata$Sample_Name){ #we first decide id we'll use TE values from viral samples or bacterial
  if (counts_metadata[counts_metadata$Sample_Name == name,]$Staining_Protocol == 'Viruses'
      #selecting rows with the specified file name that also had viral staining protocol
      ) {
  if (counts_metadata[counts_metadata$Sample_Name == name,]$Sample_Type == 'TE') {
    print("TE") #we don't want this. so we move on and look for a count file
  } else if (counts_metadata[counts_metadata$Sample_Name == name,]$Sample_Type == 'Count') {
    if (counts_metadata[which(counts_metadata$Sample_Name == name) + c(-1), ]$Sample_Type == 'TE'
        #looking to see if there is a TE above this count file. if yes, we record it as 'a'
        ){
      a<- counts_metadata[which(counts_metadata$Sample_Name == name) + c(-1), ]$V1V2V3
      if (counts_metadata[which(counts_metadata$Sample_Name == name) + c(-2), ]$Sample_Type == 'TE'
          #here we see if there is a TE above our first TE. we record this as 'b'
          ){
      b<- counts_metadata[which(counts_metadata$Sample_Name == name) + c(-2), ]$V1V2V3
      }
    }
    print(paste(name, a, b, mean(c(a,b)))) #too see what the output is
    counts_metadata[counts_metadata$Sample_Name == name,]$TE_value <- mean(c(a,b))
    #adding the output to TE_value column
  }
  } else { #we repeat the same for bacterial files
    if (counts_metadata[counts_metadata$Sample_Name == name,]$Staining_Protocol == 'Bacteria') {
      if (counts_metadata[counts_metadata$Sample_Name == name,]$Sample_Type == 'TE') {
        print("yes")
      } else if (counts_metadata[counts_metadata$Sample_Name == name,]$Sample_Type == 'Count') {
        if (counts_metadata[which(counts_metadata$Sample_Name == name) + c(-1), ]$Sample_Type == 'TE'){
          a<- counts_metadata[which(counts_metadata$Sample_Name == name) + c(-1), ]$HNALNA
          if (counts_metadata[which(counts_metadata$Sample_Name == name) + c(-2), ]$Sample_Type == 'TE'){
            b<- counts_metadata[which(counts_metadata$Sample_Name == name) + c(-2), ]$HNALNA
          }
        }
        print(paste(name, a, b, mean(c(a,b))))
        counts_metadata[counts_metadata$Sample_Name == name,]$TE_value <- mean(c(a,b))
    }}}
}

plot(counts_metadata[counts_metadata$Staining_Protocol=='Bacteria',]$TE_value)
boxplot(counts_metadata[counts_metadata$Staining_Protocol=='Bacteria',]$TE_value)
#looking at this plot for TE bacteria, its nicer to subtract following sample files with preceding TE ones, instead of average
summary(counts_metadata[counts_metadata$Staining_Protocol=='Bacteria',]$TE_value)

plot(counts_metadata[counts_metadata$Staining_Protocol=='Viruses',]$TE_value)
boxplot(counts_metadata[counts_metadata$Staining_Protocol=='Viruses',]$TE_value)
#looking at this plot for TE viruses, its nicer to subtract following sample files with preceding TE ones, instead of average
summary(counts_metadata[counts_metadata$Staining_Protocol=='Viruses',]$TE_value)


for (cols in c( "c_Bacteria", "c_HNA", "c_LNA", "c_Viruses", "c_V1", "c_V2", "c_V3")){
counts_metadata[, cols] <- NA
}



{
  counts_metadata$c_Viruses<- with(counts_metadata, ((V1V2V3-((V1V2V3/V1V2V3)*TE_value))*Dilution*60*1000)/(Flowrate*Acquisition_Duration))
  counts_metadata$c_V1<- with(counts_metadata, ((V1-((V1/V1V2V3)*TE_value))*Dilution*60*1000)/(Flowrate*Acquisition_Duration))
  counts_metadata$c_V2<- with(counts_metadata, ((V2-((V2/V1V2V3)*TE_value))*Dilution*60*1000)/(Flowrate*Acquisition_Duration))
  counts_metadata$c_V3<- with(counts_metadata, ((V3-((V3/V1V2V3)*TE_value))*Dilution*60*1000)/(Flowrate*Acquisition_Duration))
  counts_metadata$c_Bacteria<- with(counts_metadata, ((HNALNA-((HNALNA/HNALNA)*TE_value))*Dilution*60*1000)/(Flowrate*Acquisition_Duration))
  counts_metadata$c_HNA<- with(counts_metadata, ((HNA_Bacteria-((HNA_Bacteria/HNALNA)*TE_value))*Dilution*60*1000)/(Flowrate*Acquisition_Duration)) 
  counts_metadata$c_LNA<- with(counts_metadata, ((LNA_Bacteria-((LNA_Bacteria/HNALNA)*TE_value))*Dilution*60*1000)/(Flowrate*Acquisition_Duration))
}
{
  counts_metadata[counts_metadata$Staining_Protocol == 'Viruses',]$c_Bacteria <- NA 
  counts_metadata[counts_metadata$Staining_Protocol == 'Viruses',]$c_HNA <- NA 
  counts_metadata[counts_metadata$Staining_Protocol == 'Viruses',]$c_LNA <- NA 
  counts_metadata[counts_metadata$Staining_Protocol == 'Bacteria',]$c_Viruses <- NA 
  counts_metadata[counts_metadata$Staining_Protocol == 'Bacteria',]$c_V1 <- NA 
  counts_metadata[counts_metadata$Staining_Protocol == 'Bacteria',]$c_V2 <- NA
  counts_metadata[counts_metadata$Staining_Protocol == 'Bacteria',]$c_V3 <- NA
}


{
abundance<- counts_metadata[counts_metadata$Sample_Type== 'Count',]
abundance<- abundance[, c('Sample_Name', 'Staining_Protocol', 'Expt_Date', 
                          'Location', 'Expt_No','Depth', 'c_Bacteria', 'c_HNA', 'c_LNA', 
                          'c_Viruses', 'c_V1', 'c_V2', 'c_V3', 'Comments')]
abundance[abundance$Location == 'NJ2020',]$Depth <- 1
abundance<- abundance[
  with(abundance,
       order(
         abundance[, 'Staining_Protocol'],
         abundance[, 'Location'],
         abundance[, 'Expt_No'],
         abundance[, 'Depth']
         
       )),
]
}


#Need to figure out how to remove duplicates, and only take one value forward.

length(which(abundance$Location == "NJ2020"))
length(which(abundance$Location == "PE477"))
length(which(abundance$Location == "PE486"))


#for now I am manually deleting the ones that don't belong
#first only take the depths needed.
NJ2020<- abundance[abundance$Location == 'NJ2020',]

cruises<-abundance[abundance$Depth %in% c(7.0, 15.0, 30.0),]


to_delete<- c("BA210507.008", "VI210507.013", "VI210507.016")
for (delete in to_delete){
cruises<- cruises[!(cruises$Sample_Name == delete),]
}

{
viruses<- cruises[which(cruises$Staining_Protocol == 'Viruses'),] %>%
  select(-c(Staining_Protocol, c_Bacteria, c_HNA, c_LNA, Comments)) %>%
  stats::setNames(c("Viral_Sample_Name", "Expt_Date", "Location", "Expt_No", "Depth", "Total_Viruses", "V1", "V2", "V3"))
bacteria<- cruises[which(cruises$Staining_Protocol == 'Bacteria'),] %>%
select(-c(Staining_Protocol, c_Viruses, c_V1, c_V2, c_V3, Comments)) %>%
  stats::setNames(c("Bacterial_Sample_Name", "Expt_Date", "Location", "Expt_No", "Depth", "Total_Bacteria", "HNA", "LNA"))
cruise_abundance<- merge(viruses, bacteria, by = c("Location", "Expt_No", "Depth", "Expt_Date")) %>%
  select("Bacterial_Sample_Name", "Viral_Sample_Name", "Location", "Expt_No", 
         "Depth", "Expt_Date", "Total_Bacteria", "HNA", "LNA", "Total_Viruses", "V1", "V2", "V3")
rm(viruses)
rm(bacteria)
}

{
viruses<- NJ2020[which(NJ2020$Staining_Protocol == 'Viruses'),] %>%
  select(-c(Staining_Protocol, Depth, c_Bacteria, c_HNA, c_LNA, Comments)) %>%
  stats::setNames(c("Viral_Sample_Name", "Expt_Date", "Location", "Expt_No", "Total_Viruses", "V1", "V2", "V3"))
bacteria<- NJ2020[which(NJ2020$Staining_Protocol == 'Bacteria'),] %>%
  select(-c(Staining_Protocol, Depth, c_Viruses, c_V1, c_V2, c_V3, Comments)) %>%
  stats::setNames(c("Bacterial_Sample_Name", "Expt_Date", "Location", "Expt_No", "Total_Bacteria", "HNA", "LNA"))
NJ2020_abundance<- merge(viruses, bacteria, by = c("Location", "Expt_No", "Expt_Date")) %>%
  select("Bacterial_Sample_Name", "Viral_Sample_Name", "Location", "Expt_No",
         "Expt_Date", "Total_Bacteria", "HNA", "LNA", "Total_Viruses", "V1", "V2", "V3")
rm(viruses)
rm(bacteria)
} 

#Calculate VBR. For the same, you'll have to combine the tables of viruses and bacteria 

cruise_abundance$VBR<- cruise_abundance$Total_Viruses/cruise_abundance$Total_Bacteria
NJ2020_abundance$VBR<- NJ2020_abundance$Total_Viruses/NJ2020_abundance$Total_Bacteria


#Adding coordinates and nutrients to the data sets
coordinates <- openxlsx::read.xlsx("Metadata/Metadata_Microbial_Abundances_NJ2020_PE477_PE486.xlsx", 
                                                               sheet = "Coordinates")
nutrients_ts<- openxlsx::read.xlsx("Metadata/Metadata_Microbial_Abundances_NJ2020_PE477_PE486.xlsx", 
                             sheet = "Nutrients")

# Replacing negative nreints, i.e. ones below detectio limit by limit of quanittiation/2
# Recalculating Nitrate from there

limits <- c(TON = 0.01529 / 2, 
            Nitrite = 0.0393 / 2, 
            Phosphate = 0.0152 / 2, 
            Silicate = 0.0711 / 2)

# Replace negatives and zeros
nutrients_ts <- nutrients_ts %>%
  mutate(
    TON = ifelse(TON <= 0, limits["TON"], TON),
    Nitrite = ifelse(Nitrite <= 0, limits["Nitrite"], Nitrite),
    Phosphate = ifelse(Phosphate <= 0, limits["Phosphate"], Phosphate),
    Silicate = ifelse(Silicate <= 0, limits["Silicate"], Silicate)
  ) %>%
  dplyr::mutate(Nitrate = TON - Nitrite)

# Below detection values for TON 0.007645   
# Below detection values for Nitrite 0.019650     
# Below detection values for Phosphate 0.007600   
# Below detection values for Silicate 0.035550  

# Since there are still negative values in Nitrate. We will repalce them by half of minimum absolute avleus
nutrients_ts <- nutrients_ts %>%
  mutate(
    Nitrate = ifelse(Nitrate <= 0, min(abs(nutrients_ts$Nitrate))/2, Nitrate)
  )

# Below detection values for Nitrate 0.000175 

nutrients_ts[nutrients_ts$Location == 'NJ2020',]$Depth <- 1

NJ2020_abundance<- merge(NJ2020_abundance, coordinates[coordinates$Location == 'NJ2020', c(1,3,4)], by = "Location")
cruise_abundance<- merge(cruise_abundance, coordinates[coordinates$Location %in% c('PE477', 'PE486'),]  )

NJ2020_abundance<- merge(NJ2020_abundance, nutrients_ts[nutrients_ts$Location == 'NJ2020',], 
                         by = c("Location", "Expt_No"))
cruise_abundance<- merge(cruise_abundance, nutrients_ts[nutrients_ts$Location %in% c('PE477', 'PE486'),])


#Combine and save as csv Save as a csv file
abundance_abiotic <- rbind(NJ2020_abundance, cruise_abundance)

write.csv(abundance_abiotic, "nj2020_pe477_pe486_bv_abundance_abiotic.csv", row.names = F)

