# install.packages('pangaear')
# install.packages('dplyr')
library(pangaear) # see package details at https://github.com/ropensci/pangaear
library(dplyr) # set of tools for data manipulation, see details at https://dplyr.tidyverse.org/

#=============== 1. SEARCH (by PROJECT) =================
# Documentation of PANGAEA search: https://wiki.pangaea.de/wiki/PANGAEA_search
# Website: https://www.pangaea.de/?q=project:label:PAGES_C-PEAT
# search with pg_search: maximum = 500 records (set with count, continue with offset)
PAGES <- pg_search("project:label:PAGES_C-PEAT", count = 1000)
PAGES1 <- pg_search("project:label:PAGES_C-PEAT", count = 500)
PAGES2 <- pg_search("project:label:PAGES_C-PEAT", count = 500, offset = 500)

PAGES_all <- rbind(PAGES1, PAGES2)

# PAGES <- pg_search("project:label:PAGES_C-PEAT", count = 500, offset = 500)
# like this one can download all 875 dataset citations:
# rbind function: joins 2 or more dataframes
PAGES <- rbind(PAGES, pg_search("project:label:PAGES_C-PEAT", count = 500, offset = 500))

#=============== 2. GET DATA =================
# download single dataset (randomly selected from the search result above)
# pg_data returns list, data table -> data frame
Joey_core12 <- pg_data(doi="10.1594/PANGAEA.890405")
Joey_core12 <- Joey_core12[[1]][["data"]]

# create a folder for download
getwd()
dir.create(path="R/Files")
folderpath <- "R/Files/"

# write table as txt file
# paste function: concatenate vectors by converting them into character (list of vectors, separator), paste0() - without separator
write.table(Joey_core12, file=paste0(folderpath,"Joey_core12.txt"), row.names = FALSE, quote = FALSE, sep = "\t", na = "")


#=============== 3. FILTER SEARCH RESULTS =================
# restrict the search by geographical coordinates with an attribute bbox=c(minlon, minlat, maxlon, maxlat)
# datasets in northern Sweden
PAGES_Sweden <- pg_search("project:label:PAGES_C-PEAT", count = 1000, bbox=c(17.7, 67.7, 21, 69))

# filter only datasets with "Geochemistry" in title (column citation)
# grepl function: looks for a given pattern in data
# filter function from dplyr package: subsetting data (arguments: data frame and logical condition specifying the rows that should be returned)
PAGES_Sweden <- filter(PAGES_Sweden, grepl("Geochemistry", citation))

#=============== 4. GET MULTIPLE DATA =================
# combine data into a single data frame

# initiate a data frame
PAGES_Sweden_data <- data.frame()

# loop over all filtered datastes

# function bind_rows from dplyr package: unlike for rbind, the number of columns of the dataframes doesn't need to be the same
for (i in 1:nrow(PAGES_Sweden)) {
  geochem <- pg_data(doi=PAGES_Sweden[i,2])
  # extract georeferencing
  latitude <- geochem[["doi"]][["metadata"]][["events"]][["LATITUDE"]]
  longitude <- geochem[["doi"]][["metadata"]][["events"]][["LONGITUDE"]]
  geochem <- geochem[[1]]$data
  # add columns identifying unique source of data for further attribution
  geochem$DOI <- as.character(PAGES_Sweden[i,2])
  geochem$citation <- as.character(PAGES_Sweden[i,5])
  # add columns with georeferencing
  geochem$latitude <- latitude
  geochem$longitude <- longitude
  # append table
  PAGES_Sweden_data <- bind_rows(PAGES_Sweden_data, geochem)
}

# write table as txt file
write.table(PAGES_Sweden_data, file=paste0(folderpath,"Sweden_geochem.txt"), row.names = FALSE, quote = FALSE, sep = "\t", na = "")


#=============== 5. DOWNLOAD DATA FILES VIA PANGAEAR CACHE ===================
# check and set cache path of pangaear
pg_cache$cache_path_get()
pg_cache$cache_path_set(full_path = "~/R/Files")
# now data files are downloaded into that folder when executing pg_data()
# does not work for multiple binary files

#=============== 6. DOWNLOAD MANY BINARY FILES ==================

# download the images from a single dataset https://doi.pangaea.de/10.1594/PANGAEA.919398
table <- pg_data(doi="10.1594/PANGAEA.919398")
table <- table[[1]]$data
View(table)

# download files listed in the column "IMAGE"
# set he prefix first (see .tab file) https://doi.pangaea.de/10.1594/PANGAEA.919398?format=textfile
prefix <- "https://download.pangaea.de/dataset/919398/files/"

# with a condition ("fauna" in Content)
for (i in (1:nrow(table))){
  if (grepl("fauna", table$'Content'[i]) == TRUE ) {
    download.file(paste0(prefix, table$IMAGE[i]), destfile = paste0(folderpath, table$'IMAGE'[i]))  
  }
}

#=============== 7. DOWNLOAD MANY FILES ==================
# this concerns datasets published before 2020 (in .tab file the full path is given, not just the file name)
# example dataset: https://doi.pangaea.de/10.1594/PANGAEA.910179?format=html#download with several netcdf files + a csv

list_of_files <- pg_data(doi="10.1594/PANGAEA.910179")
list_of_files <- list_of_files[[1]]$data
View(list_of_files)

for (i in (1:nrow(list_of_files))){
  download.file(as.character(list_of_files$'URL file'[i]), destfile = paste0(folderpath, list_of_files$'File name'[i]))
}
