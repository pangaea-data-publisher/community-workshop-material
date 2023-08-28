
# Import libraries --------------------------------------------------------
library(dplyr)
library(stringr)
library(lubridate)
# install.packages('raster')
# library(raster)
# install.packages("taxize")
library(taxize)
library(httr)

# Create practice data tables --------------------------------------------------------

# Function to create fake data
create_fake_df <- function() {
  # Add numeric features
  df <- data.frame(seq(from = 69.2, to = 89, by = 0.1),
                   seq(from = 11, to = 110, by = 0.5),
                   seq(from = 1, to = 100, by = 0.5),
                   runif(length(seq(from = 69.2, to = 89, by = 0.1)))
                   )
  names(df) <- c("latitude (deg)", "lngitude (deg)", "water depth (cm)", "water temperature (degC)" )

  
  # Add object feature
  df$Location <- "Bremerhaven, Germany"
  
  # Add geolocations in degrees and in a single field
  df$`lat/lon` <- "53°32'59.9964''N; 8°34'59.9988''E"
  
  # Create parameter with abbreviations
  threatened_status <- rep(c("LC", "NT", "EN", "EX"), length.out = nrow(df))
  df$`Threatened status` <- threatened_status
  
  # Add species column
  species <- rep(c("Stenella coeruleoalba", "Tursiops truncatus", "Dolphinus delphis"), length.out = nrow(df))
  df$species <- species
  
  # Add row with comments
  df[1, ] <- rep("Comment", ncol(df))
  
  # Add row at the end with "Total sum"
  df[nrow(df) + 1, ] <- rep("Total sum", ncol(df))
  
  # Add empty column
  df$Empty <- NA
  
  ## Date and time manipulations
  # Add date and time columns
  sdate <- as.Date("2010-03-22")   # start date
  edate <- as.Date("2021-04-09")   # end date
  # Generate date and time range
  date_time_range <- seq.POSIXt(from = as.POSIXct(sdate), to = as.POSIXct(edate) - 86400, by = "hour")
  # Sample random date
  df$date <- sample(as.Date(date_time_range), nrow(df), replace = TRUE)
  # Sample random time
  df$time <- format(sample(date_time_range, nrow(df), replace = TRUE), format = "%H:%M:%S")
  
  ## Manipulate values
  # Add leading and trailing white space to values
  df$Location <- sprintf("%3s", df$Location)
  df$Location <- sprintf("%-2s", df$Location)
  # Add double white spaces in between strings
  df$Location[3] <- "Bremerhaven,     Germany"
  
  # Add comma separated value to latitude
  df$`latitude (deg)` <- as.character(df$`latitude (deg)`)
  df$`latitude (deg)`[3] <- "69,4"
  
  # Add missing values in different formats
  df$`water depth (cm)` <- as.character(df$`water depth (cm)`)
  df$`water depth (cm)`[2] <- "-999.9"
  df$`water depth (cm)`[3] <- "n/a"
  
  # Sort columns
  df <- df[, c('date', 'time', 'Location', 'Threatened status', 'species', 'lat/lon', 'latitude (deg)',
               'lngitude (deg)', 'water depth (cm)', 'water temperature (degC)', 'Empty')]
  
  ## Save as csv file
  write.table(df, file = 'test_data.csv', row.names = FALSE, sep = ',')
  
  # Show data table
  View(df)
  
  return(df)
}

df <- create_fake_df()
head(df)

# Data Curation Checklist -------------------------------------------------

# Data structure ----------------------------------------------------------


# Check structure of data table
head(df, 5)

# Alternative way to see head and tail together
rbind(head(df, 4), tail(df, 4))

# Remove row with comments
df <- df[-1, ]

# Remove row with aggregated statistics
df <- df[-nrow(df), ]

# Concatenate head and tail of the data frame
rbind(head(df, 2), tail(df, 2))

# Data types ----------------------------------------------------------

# Check data types
str(df)


# Remove columns containing only NA
df <- df[, colSums(is.na(df)) != nrow(df)]


# Remove ambiguous missing values
df$`water depth (cm)` <- gsub("-999.9|n/a", "", df$`water depth (cm)`)

# Check for comma separated values
# List unique values
unique_values <- unique(df$`latitude (deg)`)
head(unique_values, 10)

# Replace commas with dots
df$`latitude (deg)` <- gsub(",", ".", df$`latitude (deg)`)

# Convert to numeric
# Convert specific column to numeric
df$`latitude (deg)` <- as.numeric(df$`latitude (deg)`)

# OR: Convert multiple columns of dataframe to numeric
df[, 7:ncol(df)] <- apply(df[, 7:ncol(df)], 2, as.numeric)

# Check numeric features for text entries
str(df)

# Remove leading and trailing white space
# Remove all leading and trailing white spaces in columns identified as characters
cols <- sapply(df, is.character)
df[cols] <- lapply(df[cols], str_trim)

# Optional: Remove double white spaces
cols <- sapply(df, is.character)
df[cols] <- lapply(df[cols], function(x) gsub("\\s+", " ", x))
df[cols] <- as.data.frame(lapply(df[cols], function(x) gsub("\\s+", " ", x)))


# Date formatting ---------------------------------------------------------

# Merge Date and Time and convert to PANGAEA format
# Join date and time
df$date <- paste(df$date, df$time)
df$date <- ymd_hms(df$date)

# Convert to pangaea standard time format
df$date <- format(df$date, "%Y-%m-%dT%H:%M:%S")

# Remove redundant time column
df <- subset(df, select = -c(time))

# Split cell by semicolon separator
df$lat <- sapply(strsplit(df$`lat/lon`, ";"), "[", 1)
df$lon <- sapply(strsplit(df$`lat/lon`, ";"), "[", 2)

# Remove old lat/lon column
df <- subset(df, select = -c(`lat/lon`))

dms2dec <- function(dms, separators = c("º", "°", "\'", "’", "’’", "\"", "\'\'", "\\?")) {
  
  # version 1.4 (2 Feb 2022) source("https://raw.githubusercontent.com/AMBarbosa/unpackaged/master/dms2dec", encoding = "UTF-8")
  # dms: a vector of latitude or longitude in degrees-minutes-seconds-hemisfere, e.g. 41° 34' 10.956" N (with or without spaces)
  # separators: the characters that are separating degrees, minutes and seconds in 'dms'; mind these are taken in the order in which they appear and not interpreted individually, i.e. 7'3º will be taken as 7 degrees, 3 minutes! input data are assumed to be properly formatted
  
  dms <- as.character(dms)
  dms <- gsub(pattern = " ", replacement = "", x = dms)
  for (s in separators) dms <- gsub(pattern = s, replacement = "_splitHere_", x = dms)
  
  splits <- strsplit(dms, split = "_splitHere_")
  n <- length(dms)
  deg <- min <- sec <- hem <- vector("character", n)
  
  for (i in 1:n) {
    deg[i] <- splits[[i]][1]
    min[i] <- splits[[i]][2]
    
    if (length(splits[[i]]) < 4) {
      hem[i] <- splits[[i]][3]
    } else {
      sec[i] <- splits[[i]][3]
      hem[i] <- splits[[i]][4]
    }
  }
  
  dec <- colSums(rbind(as.numeric(deg), (as.numeric(min) / 60), (as.numeric(sec) / 3600)), na.rm = TRUE)
  sign <- ifelse (hem %in% c("N", "E"), 1, -1)
  hem_miss <- which(is.na(hem))
  if (length(hem_miss) > 0) {
    warning("Hemisphere not specified at position(s) ", hem_miss, ", so the sign of the resulting coordinates may be wrong.")
  }
  dec <- sign * dec
  return(dec)
}  # end dms2dec function

# Convert latitude and longitude to decimal units
df$lat<- dms2dec(df$lat)
df$lon<- dms2dec(df$lon)

# Spelling ----------------------------------------------------------------

# Spell out abbreviations

# What are the abbreviations
unique_abbreviations <- unique(df$`Threatened status`)

# Create list with abbreviations
abbreviated <- c("LC", "NT", "EN", "EX")

# Create list with full names
spelled_out <- c("Least Concern", "Near Threatened", "Endangered", "Extinct in the Wild")

# Replace the values in 'Threatened status' column with the named vectors
df$`Threatened status` <- spelled_out[match(df$`Threatened status`, abbreviated)]

# Correct species names
# Create data frame with unique species names
species <- data.frame(name = unique(df$species))

# Save as tab-separated values (tsv) file for upload to WoRMS or ITIS
write.table(species, file = "Species.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# Print the species data frame
print(species)

# Using package taxize
matched_species <- tax_name(species$name, get = "species" )

# Correct misspelled species names
df$species <- gsub("Dolphinus delphis", "Delphinus delphis", df$species, ignore.case = TRUE)

# Get unique species names
unique_species <- unique(df$species)

# Parameter (header) naming -----------------------------------------------

params <-read.csv("https://www.pangaea.de/lists/parameter/all-byname", sep = "\t")
# Check table size
cat(paste("There are currently", nrow(params), "parameters available in PANGAEA\n"))

# Display the first few rows of the data frame
head(params)

# Find parameters containing your parameter name

# Find parameters with "Latitude" in the name (case-insensitive)
matching_params <- params[grepl("Latitude", params$Parameter, ignore.case = TRUE), ]

# Display the matching parameters
matching_params

# Find similar parameters for "latitude" (case-insensitive)
similar_params <- params[agrepl("latitude", params$Parameter, ignore.case = TRUE), ]

# Display the similar parameters
similar_params

# Rename parameters
colnames(df) <- c('DATE/TIME []', 'Location []', 'IUCN Red List status []', 'Species []', 'LATITUDE []', 'LONGITUDE []', 
                  'DEPTH, water [m]', 'Temperature, water [°C]', 'Latitude 2 []', 'Longitude 2 []')

head(df, 2)

# Conversions -------------------------------------------------------------
df$`DEPTH, water [m]` <- df$`DEPTH, water [m]` / 100


# URLs --------------------------------------------------------------------

df$'Uniform resource locator/link to reference []'<- "https://doi.org/10.1594/PANGAEA.945749"
# Function to check if URL is valid
check_url <- function(url) {
  response <- try(GET(url))
  return(response$status_code == 200)
}

# Apply the function to the URLs in the column
df$`url check` <- sapply(df$`Uniform resource locator/link to reference []`, check_url)

# Event -------------------------------------------------------------------
df$'Event []' <- "PS132/1-2"
 
# Save curated data -------------------------------------------------------
# with a slightly different column sequence
write.table(df[,c(12, 1:11)], file = "Curated_data.txt", sep = "\t", quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")

