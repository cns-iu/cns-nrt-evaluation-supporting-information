#### Environment Setup #### 
library(bib2df)       # Citation importing
library(magrittr)     # Piping
library(dplyr)        # Date prep
library(stringr)      # String manipulation

# Path setting
getwd()
path <- ("./data/publications/")
# Load data
# Prepared publication data - with cleaned author
data <- bib2df(paste0(path,"CNS_NRT-2024.08.14-publications-wos_gs.bib"))
names(data) <- str_to_lower(names(data))

# Select variables
data <-
  data %>%
  select(bibtexkey, year, title, author, editor, journal, booktitle,
         volume, number, issn, isbn, doi, url, keywords, type, category)

# Update WOS URL
data$url <- str_replace(data$url, "<Go to ISI>://","")

# Clean up variable names
names(data)[13] <- "wosid"
names(data)[7] <- "proceeding_title"

# prepare keywords
data[data$keywords=="" & !is.na(data$keywords),]$keywords <- NA

# Prepare author data
tmp <- data.frame(author=as.character())
for(i in 1:nrow(data)){
  tmp2 <- data.frame(author=paste(unlist(data[i,]$author), collapse=";"))
  tmp <- rbind(tmp,tmp2)
}
data$author <- tmp$author

# Prepare editors data
tmp <- data.frame(editor=as.character())
for(i in 1:nrow(data)){
  tmp2 <- data.frame(editor=paste(unlist(data[i,]$editor), collapse=";"))
  tmp <- rbind(tmp,tmp2)
}
data$editor <- tmp$editor
data[data$editor=="NA",]$editor <- NA
rm(tmp, tmp2, i)

# Prepare years
data$year <- as.integer(data$year)

# Prepare Titles
data$title <- str_to_title(data$title)
data$proceeding_title <- str_to_title(data$proceeding_title)

# Save results
write.csv(data, paste0(path,"CNS_NRT-publications_wos_gs.csv"),
          row.names = F, fileEncoding = "UTF-8" )
