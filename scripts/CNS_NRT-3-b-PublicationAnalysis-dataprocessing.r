#### Environment Setup #### 
library(tidyr)        # Tidyverse functions
library(magrittr)     # Piping
library(plyr)         # Data prep
library(dplyr)        # Date prep
library(stringr)      # String manipulation

# Path setting
#getwd()
path <- ("./data/")

#### Load Publication data ####
# Prepared publication data - with cleaned author
data <-
  read.csv(file=paste0(path,"publications/CNS_NRT-2024.08.14-publications_wos_gs-prepared.csv"),
           header = T, sep=",")

#### Prepare Publication Data ####
# Arrange by year
data <- 
  data %>%
  arrange(year)

# List of Author Names and PubIds (BibTexKey)
pub_authors_long <-
  data %>%
  select(bibtexkey,year,author_prepared) %>%
  separate_rows(c(author_prepared), sep=";", convert=T) %>%
  arrange(year)

# Identifies publications to drop due to excessive authorship
pub_drop <-
  pub_authors_long %>%
  ddply(.(bibtexkey), summarise,
        author_ct = length(author_prepared)) %>%
  filter(author_ct > 75)
write.csv(pub_drop, file=paste0(path,"publications/CNS_NRT-publications-pubid-drop_author_ct.csv"),
          row.names = F)

# Removes dropped pub from author publication list
pub_authors_long <-
  data %>%
  select(bibtexkey,year,author_prepared) %>%
  separate_rows(c(author_prepared), sep=";", convert=T) %>%
  arrange(year) %>%
  filter(bibtexkey!=pub_drop$bibtexkey)

#### Author Publication Stats ####
# CNS NRT Authorship and Membership Identification
# Generates a list of unique authors, pub counts and first NRT Paper
# This data is compared to the roster of NRT Members (faculty, fellow, & affiliates)
# to determine an author's group in the CNS NRT program.
authors <-
  pub_authors_long %>%
  plyr::rename(c("author_prepared" = "author")) %>%
  tidyr::separate(author, into=c("last_name","first_name"),
                  sep=",", remove=FALSE) %>%
  select(author,last_name, first_name, year,bibtexkey) %>%
  ddply(.(author, last_name, first_name), summarise,
        pubs = length(bibtexkey),
        first_year = min(year)) %>%
  arrange(last_name)

#### Author Name Clean-Up ####
# Special cases
authors[authors$last_name=="Börner",]$last_name <- "Borner"
authors[authors$last_name=="Milojević",]$last_name <- "Milojevic"

#### Author Group Matching ####
# CNS NRT Member List
nrt_members <-
  read.csv(file=paste0(path,"membership/CNS_NRT-Membership-2024.csv"),
           header=T)[c(1:3,5:7,11)]
names(nrt_members) <- str_to_lower(names(nrt_members))
names(nrt_members)[1:2] <- c("last_name","first_name")

# Matches CNS NRT authors with CNS NRT Members by last name.
# This will generate bad matches that need to be reviewed.
authors <-
  left_join(authors, nrt_members, by="last_name", 
            relationship="many-to-many") %>%
  mutate(match=0, verified=1)
authors[!is.na(authors$name),]$match <- 1
authors[!is.na(authors$name),]$verified <- 0
names(authors)[c(3,6)] <- c("first_name","matched_first_name")

# A review of the results indicate 9 author mis-matches
# Clean-up bad matches 
authors[c(105,164,222,223,224,226,227,239,240),] <-
  authors[c(105,164,222,223,224,226,227,239,240),] %>%
  mutate(matched_first_name=NA,
         name=NA,
         group=NA,
         role=NA,
         cohort=NA,
         exclude=NA,
         match=0,
         verified=1)

# Remove duplicates added in matching step.
authors <- 
  authors %>%
  distinct()
# Drop bad matches remaining duplicate (Wang, Xuan)
authors <- authors[-c(223),]

# Remaining matches are accurate and converted to 1
authors[authors$match==1,]$verified <- 1

# Clean up cohorts for faculty to NA
authors[authors$group %in% c("Faculty") & authors$cohort == "",]$cohort <- NA

# Clean up group NAs 
authors[is.na(authors$group),]$group <- "Collaborator"

#### Add author roles to pub_authors_long data ####
names(pub_authors_long) <- c("id","year","author")
pub_authors_long <- left_join(pub_authors_long, authors[,c(1,8)], by="author", 
            relationship="many-to-many")
names(pub_authors_long) <- c("Id","Year","Author","Group")

# Output data used to generate edge list.
write.csv(pub_authors_long,
          file=paste0(path,"publications/CNS_NRT-publications-pubid_year_author-list.csv"),
          row.names = F)

#### Create Author Node List ####
# Update author data frame
authors <- 
  authors %>%
  arrange(author) %>%
  mutate(first_name = str_trim(first_name, side="both"))

authors <-
  authors %>%
  mutate(id = as.numeric(row.names(authors))-1) %>%
  plyr::rename(c("name"="match_name","author"="name")) %>%
  select(id, name, last_name, first_name, match_name, match, verified, 
         group, cohort, pubs, first_year)


# Output data used to generate node list
# Save the matched and verified data to publications.
authors %>%
  write.csv(file=paste0(path,"publications/CNS_NRT-publications-author_names-match-verified.csv"),
            row.names = F, fileEncoding = "UTF-8")

# Clean up environment
rm(nrt_members)