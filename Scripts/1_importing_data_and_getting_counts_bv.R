#### 0.0 Setting Up the Environment####

#Setting working directory
setwd("C:/Users/hisham.shaikh/OneDrive - UGent/Projects/Microbial_Abundances/Microbial_Abundances_NJ2020_PE477_PE486/Microbial Abundances/Microbial_Abundances")

#Install packages if needed.
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install(c("flowWorkspace", "ggcyto", "magrittr"))


#Load packages
{
  library("flowWorkspace")
  library("ggcyto")
  library("magrittr")
}

#### 1.0 Importing Data ####
data<- list.files(path = "./Data") #list files from folder containing data
flowCore::isFCSfile(data) #none of the FCS files read as FCS. That's because the
#full path isn't mentioned
#Adding full path
data<- paste0("./Data/", data)
flowCore::isFCSfile(data) #All read as FCS



#### 2.0 Gating #####
#gating requires some trial and error. It's good to start with one plot and see
#if you can gate well 

#Read flowset function and transform
translist_bv<- transformList(c("FSC-H", "SSC-H", "FL1-H", "FL2-H", "FL3-H"), logTransform())
#cause i haven't yet figured out to use the gating hierarchy function instead of gatingset 
#So, i am going to use one of the fcs files as the other file.
#Insert a reference file. Always remember to take a file not from the dataset being analysed. 
#As, it will overwrite the name of the same file from the dataset. Best to create a folder 'ref' and dump that file there.
#Good to use a file containing viral data
ref_fcs<- paste0("./Ref/VI210507.031_ref")


read_transform_fs_bv <- function(x){ #function to read and transform fcs files
  flowCore::read.flowSet(c(ref_fcs, x)) %>%
    flowCore::transform(translist_bv)
}

#was just checking if the function works
#gsbv<- read_transform_fs_bv(data)
#ggcyto(gsbv[1], aes(x= "SSC-H", y= "FL1-H")) +geom_hex(bins=800)


#Writing a gatingset function
{
  polycut<- matrix(c(1.1, 2.0, 2.5, 3.0, 3.5, 3.4, 2.0, 0.6,
                     1.9, 1.5, 1.5, 2.0, 2.8, 3.4, 3.2, 2.75), nrow = 8, ncol=2)
  colnames(polycut)<- c("SSC-H", "FL1-H")
  bgate<- polygonGate(.gate = polycut, filterId = "Bacteria" )
  HNA_Bacteria<- rectangleGate(filterId="HNA_Bacteria","SSC-H"=c(0.6, 3.5), "FL1-H"=c(2.15, 3.5))
  LNA_Bacteria<- rectangleGate(filterId="LNA_Bacteria","SSC-H"=c(1.0, 3.5), "FL1-H"=c(1.5, 2.15))
  vgate<- rectangleGate(filterId= "Viruses", "SSC-H" = c(-Inf,1.2), "FL1-H" = c(-Inf, 1.7)) 
  v1<- rectangleGate(filterId="V1", "SSC-H"= c(-Inf, 0.90), "FL1-H"= c(-Inf, 0.75)) 
  v2<- rectangleGate(filterId="V2", "SSC-H"= c(-Inf, 0.90), "FL1-H"= c(0.75, 1.2)) 
  v3<- rectangleGate(filterId="V3", "SSC-H"= c(-Inf, 1.125), "FL1-H"= c(1.2, 1.65)) 
  
}

gatingset_bv_stats<- function(flowset){ #flowset here is already transformed, cleaned, and compensated
  gsbv_fs<- flowWorkspace::GatingSet(flowset)
  gs_pop_add(gsbv_fs, bgate, parent="root")
  gs_pop_add(gsbv_fs, HNA_Bacteria, parent= "Bacteria")
  gs_pop_add(gsbv_fs, LNA_Bacteria, parent= "Bacteria")
  gs_pop_add(gsbv_fs, vgate, parent= "root")
  gs_pop_add(gsbv_fs, v1, parent="Viruses")
  gs_pop_add(gsbv_fs, v2, parent="Viruses")
  gs_pop_add(gsbv_fs, v3, parent="Viruses")
  recompute(gsbv_fs)
  stats<- gs_pop_get_stats(gsbv_fs[[2]], c("root", "Bacteria", "HNA_Bacteria",
                                           "LNA_Bacteria", "Viruses",
                                           "V1", "V2", "V3"), "count")
  write.table(stats, file = "counts.csv", sep = ",", append = T, quote = FALSE,
              col.names = FALSE, row.names = FALSE)
  
}

gatingset_bv_plots<- function(flowset){ #flowset here is already transformed, cleaned, and compensated
  gsbv_fs<- flowWorkspace::GatingSet(flowset)
  gs_pop_add(gsbv_fs, bgate, parent="root")
  gs_pop_add(gsbv_fs, HNA_Bacteria, parent= "Bacteria")
  gs_pop_add(gsbv_fs, LNA_Bacteria, parent= "Bacteria")
  gs_pop_add(gsbv_fs, vgate, parent= "root")
  gs_pop_add(gsbv_fs, v1, parent="Viruses")
  gs_pop_add(gsbv_fs, v2, parent="Viruses")
  gs_pop_add(gsbv_fs, v3, parent="Viruses")
  recompute(gsbv_fs)
  p<- ggcyto::ggcyto(gsbv_fs[[2]], aes(x = `SSC-H`, y = `FL1-H`), subset = "root") +
    geom_hex(bins = 300) +  
    theme_bw()+
    labs(x = "Side scatter (a.u.)", y = "Green Fluorescence (a.u.)")+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          strip.background = element_rect(colour="white", fill="white"),
          panel.border = element_rect(colour = "white")) +
    geom_gate(c("Viruses", "Bacteria", "V1", "V2", "V3", "HNA_Bacteria", "LNA_Bacteria")) +
    ggcyto_par_set(limits = list(x = c(0,3.8), y = c(0, 3.6))) + 
    geom_stats(type= c("percent", "count"), adjust = 0.5)
  print(p)
  
}


#### 3.0 Plots and Stats Output ####
#For Plots
{
  pdf("plots3.pdf",onefile = T)
  for (i in data) {
    try(read_transform_fs_bv(i)  %>%
      gatingset_bv_plots()) 
  }
  dev.off()
}


#For Stats

#writing an empty table to append later with data
x<-data.frame(file_name = "test_file_name", pop = "pop", count = "count")
x<-  x[-c(1),]
#View(x)  
write.table(x,file= "counts.csv", sep = ",", col.names = c("file_name", "pop", "count"))

{
  error_files_stats<- NULL
  error_files_stats<- c()

  for (i in data) {
    out<-  tryCatch(
      {
        read_transform_fs_bv(i)  %>%
          gatingset_bv_stats()
        },
      error = function(e){ 
        print(i) #you can make this to print(i, e) to get the error printed
        #on the console. Likewise, you'll have to also hide the error_files_stats
        #to not redirect the output
        
    }
    )
  
  error_files_stats<- c(error_files_stats, out)
}
}

#Check the error_files_stats to see if you have missed important files. 
#The files with "cannot allocate more than 8.0 Gb vector" are bacterial TE files,
#with issues with transformation

#Find the files lost (not counted)  in stats step
{
  library(readr)
  counts <- read_csv("counts.csv")
}

#Quality check for missing/unaccounted files
{
  print(paste("Length of data is", length(data)))
  print(paste("Length of number of files proccessed/counted is", 
              length(unique(counts$file_name))))
  print(paste("Length of error_files_stats is", length(error_files_stats)))
  if (length(error_files_stats) > 0) {
    print(paste( "Check error_files_stats to determine if any important",
                 "files were not processed.",
                 "Files in error_files_stats are as follows:"))
      print(error_files_stats)
  }
  
if (length(error_files_stats) + length(unique(counts$file_name)) == length(data)) {
  print(paste("All files are accounted for. Files are either in error_files_stats or",  
        "in the counts csv."))} else {
          if (length(setdiff(setdiff(data, paste0("./Data/", unique(counts$file_name))), 
                      error_files_stats) )> 0) {
            print("There are files missing/not accounted for. Here are the filenames:")
            print(setdiff(setdiff(data, paste0("./Data/", unique(counts$file_name))), 
                          error_files_stats))
          }
        }
  all_missing_files<- setdiff(data, paste0("./Data/", unique(counts$file_name)))
  
}
 
#Plot all the missing files to understand which ones are important

read_transform_fs_bv(all_missing_files[5])  %>%
  gatingset_bv_plots()
#Still can't allocate a vector of 8 Gb. 

for (missing_file in all_missing_files) {
tryCatch(read_transform_fs_bv(missing_file)  %>%
  gatingset_bv_plots(),
  error= function(e){
    print(missing_file)
    print(e)
  })
}

#append the missing file to csv if the file is important
for (missing_file in all_missing_files) {
  tryCatch({print(missing_file)
    read_transform_fs_bv(missing_file)  %>%
  gatingset_bv_stats()},
  error= function(e){
    print(e)
  })
}

#Import the csv once again. This is the final product of this script.
{
  library(readr)
  counts <- read_csv("counts.csv")
}
