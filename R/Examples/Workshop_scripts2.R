#install.packages("pangaear")
library(pangaear)

## Search

# Search by project, return up to 100 results
res1 <- pg_search(query = 'project:Sustainable Management of Mesopelagic Resources', count = 100)

# Write results to tab-delimited .txt file, working locally
#write.table(res1,file="C:/Users/awittmann/Documents/pangaear/SUMMER_datasets_220511.txt",quote = FALSE, sep = "\t", row.names=FALSE, col.names = TRUE, fileEncoding = "UTF-8")

# Write results to tab-delimited .txt file, relative path
write.table(res1,file="../../SUMMER_datasets_220511.txt",quote = FALSE, sep = "\t", row.names=FALSE, col.names = TRUE, fileEncoding = "UTF-8")

# Search for species 'Arenicola marina' using its unique semantic URI, see WoRMS database https://www.marinespecies.org/aphia.php?p=taxdetails&id=129868
res2 <- pg_search(query = 'urn:lsid:marinespecies.org:taxname:129868', count = 100)


## Download and compile data from search results

# Setup data frame
master <- data.frame(datetime = c(), latitude = c(), longitude = c(), 'A. marina [#/m**3]' = c())

# Download all referenced data if they contain all of the columns given in 
# if statement, and combine into one dataframe

pids <- res2$doi

for (pid in pids) {
  res2 <- pg_data(doi = pid, overwrite = FALSE, mssgs = FALSE)
  d <- as.data.frame(res2[[1]]$data)
  names(d)
  if (all(c("Date/Time", "Latitude", "Longitude", "A. marina [#/m**3]") %in% colnames(d))) {
    df <- data.frame(
      datetime = as.POSIXct(d[,"Date/Time"], format = "%Y-%m-%d"),
      latitude = d[,"Latitude"],
      longitude = d[,"Longitude"],
      'A. marina [#/m**3]' = d[,"A. marina [#/m**3]"])
    
    master = rbind(master, df)
    print(sprintf("Dataset %s included in compilation.", pid))
  } else {
    print(sprintf("Dataset %s does not contain required columns.", pid))
  }
}

View(master)

## Elasticsearch
res <- pg_search_es(query = 'physical oceanography', size = 100)

# Filter for open access datasets, which are coded with "1" in column `_source.sp-loginOption`
openaccessres <- res[res$`_source.sp-loginOption` %in% c("1"),]
View(openaccessres)

# Fetch source URIs:
URIs <- grep("https://doi.org/", openaccessres$`_source.URI`, value = TRUE)

# To get to the DOIs for each study, subtitute "https://doi.org/" with ""
pids2 <- gsub("https://doi.org/", "", URIs)

