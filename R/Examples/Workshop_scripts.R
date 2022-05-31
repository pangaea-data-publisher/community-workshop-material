# install.packages('pangaear')
# install.packages('dplyr')
library(pangaear) #see package details at https://github.com/ropensci/pangaear
library(dplyr)
#library(ggplot2)


#=============== 1. SEARCH (by PROJECT) =================

# website: https://www.pangaea.de/?q=project:label:PAGES_C-PEAT
# search with pg_search: maximum = 500 records (set with count, continue with offset)
PAGES <- pg_search("project:label:PAGES_C-PEAT", count = 100)

#PAGES <- pg_search("project:label:PAGES_C-PEAT", count = 500, offset = 500)
#like this one can download all 875 dataset citations
PAGES <- rbind(PAGES, pg_search("project:label:PAGES_C-PEAT", count = 500, offset = 500))

#=============== 2. GET DATA =================
#download single dataset - first list, then -> data frame
Joey_core12 <- pg_data(doi="10.1594/PANGAEA.890405")
Joey_core12 <- Joey_core12[[1]]$data

#create a folder for download
getwd()
dir.create(path="R/Files")
folderpath <- "R/Files/"

#download table as txt file
write.table(Joey_core12, file=paste(folderpath,"Joey_core12.txt", sep=""), row.names = FALSE, quote = FALSE, sep = "\t", na = "")

#=============== 3. FILTER SEARCH RESULTS =================
# restrict the search by geographical coordinates with an attribute bbox=c(minlon, minlat, maxlon, maxlat)
# datasets in northern Sweden
PAGES_Sweden <- pg_search("project:label:PAGES_C-PEAT", count = 1000, bbox=c(17.7, 67.7, 21, 69))
#filter only datasets with "Geochemistry" in title (column citation)
PAGES_Sweden <- filter(PAGES_Sweden, grepl("Geochemistry", citation))

#=============== 4. GET MULTIPLE DATA =================
# combine data into a single data frame
PAGES_Sweden_data <- data.frame()

for (i in 1:nrow(PAGES_Sweden)) {
  geochem <- pg_data(doi=PAGES_Sweden[i,2])
  geochem <- geochem[[1]]$data
  geochem$DOI <- as.character(PAGES_Sweden[i,2])
  geochem$citation <- as.character(PAGES_Sweden[i,5])
  PAGES_Sweden_data <- bind_rows(PAGES_Sweden_data, geochem)
}

#download table as txt file
write.table(PAGES_Sweden_data, file=paste(folderpath,"Sweden_geochem.txt", sep=""), row.names = FALSE, quote = FALSE, sep = "\t", na = "")


#=============== 5. DOWNLOAD DATA FILES VIA PANGAEAR CACHE ===================
#check and set cache path of pangaear
pg_cache$cache_path_get()
pg_cache$cache_path_set(full_path = "~/R/Files")
#now data files are downloaded into that folder when executing pg_data()
#does not work for multiple binary files

#=============== 6. DOWNLOAD MANY BINARY FILES ==================

# download the images from a single dataset https://doi.pangaea.de/10.1594/PANGAEA.919398
table <- pg_data(doi="10.1594/PANGAEA.919398")
table <- table[[1]]$data
View(table)

#download files listed in the column "IMAGE"
#set he prefix first (see .tab file) https://doi.pangaea.de/10.1594/PANGAEA.919398?format=textfile
prefix <- "https://download.pangaea.de/dataset/919398/files/"

#with a condition ("fauna" in Content)
for (i in (1:nrow(table))){
  if (grepl("fauna", table$'Content'[i]) == TRUE ) {
    download.file(paste(prefix, table$IMAGE[i], sep=""), destfile = paste(folderpath, table$'IMAGE'[i], sep=""))  
  }
}

#=============== 7. DOWNLOAD MANY FILES ==================
#this concerns datasets published before 2020 (in .tab file the full path is given, not just the file name)

list_of_files<-pg_data(doi="10.1594/PANGAEA.910179")
list_of_files<-list_of_files[[1]]$data

for (i in (1:nrow(list_of_files))){
  download.file(as.character(list_of_files$'URL file'[i]), destfile = paste0(folderpath, list_of_files$'File name'[i]))
}
