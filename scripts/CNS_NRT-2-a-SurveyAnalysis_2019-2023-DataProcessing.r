#### Response Preparation Script ####
# This script is designed to process Qualtrics survey into usable data sets
# 1) Collect and clean sets of question metadata by respondent groups 
#    (text and variable name)
# 2) Prepare categorically scaled data (i.e., Likert, y/n).
# 3) Prepare subset of questions by audience and question types.

#### Environment Setup#### 
rm(list=ls())
path_s <- c("./data/survey/")
#getwd()

##### Select Packages#####
library(tidyr)
library(dplyr)
library(plyr)
library(stringr)
library(magrittr)
library(reshape2)

#### Load Data ####
data <- read.csv(file=paste0(path_s,"CNS_NRT-SurveyResponses_2019-2023.csv"), 
                 header = T, sep=",")

data$Role <- str_replace(data$Role, "NRT ","")
data[data$Role=="Student",]$Role <- "Fellow"

# Clean up role labels
#### Extract Survey Question Metadata ####
##### Metadata for questions asked of all participants.#####
d_all_m <-
  data %>% 
  select("Year","Role","RecordedDate","ResponseId","Progress","Remove",
         "Q123","Q2","Q1","Q120","Q158","Q155","Q160","Q162","Q107") %>%
  slice(1:2) %>%
  t()
d_all_m <- cbind(rownames(d_all_m),d_all_m)
row.names(d_all_m) <- 1:nrow(d_all_m)   
colnames(d_all_m) <- c("Variable","Label","ImportJSON")
d_all_m <- as.data.frame(d_all_m)
# Qualitative text clean up.
d_all_m[d_all_m$Variable=="Q107",]$Label <- 
  str_replace(d_all_m[d_all_m$Variable=="Q107",]$Label,":",".")
d_all_m[d_all_m$Variable=="Q107",]$Label <- 
  str_replace(d_all_m[d_all_m$Variable=="Q107",]$Label,"other ","")

d_cont_m <-
  data %>% 
  select("Year","Role","RecordedDate","ResponseId","Progress","Remove",
         "Q159_0_GROUP","Q159_1_GROUP","Q159_2_GROUP",
         "Q159_0_1_RANK","Q159_0_2_RANK","Q159_0_3_RANK",
         "Q159_0_4_RANK","Q159_0_5_RANK","Q159_0_6_RANK",
         "Q159_0_7_RANK","Q159_1_1_RANK","Q159_1_2_RANK",
         "Q159_1_3_RANK","Q159_1_4_RANK","Q159_1_5_RANK",
         "Q159_1_6_RANK","Q159_1_7_RANK","Q159_2_1_RANK",
         "Q159_2_2_RANK","Q159_2_3_RANK","Q159_2_4_RANK",
         "Q159_2_5_RANK","Q159_2_6_RANK","Q159_2_7_RANK") %>%
  slice(1:2) %>%
  t()
d_cont_m <- cbind(rownames(d_cont_m),d_cont_m)
row.names(d_cont_m) <- 1:nrow(d_cont_m)   
colnames(d_cont_m) <- c("Variable","Label","ImportJSON")
d_cont_m <- as.data.frame(d_cont_m)

##### Metadata for questions asked of faculty.#####
d_fac_m <-
  data %>% 
  select("Year","Role","RecordedDate","ResponseId","Progress","Remove",
         "Q149","Q126_1","Q126_2","Q126_3","Q126_4","Q36","Q36T","Q38") %>%
  slice(1:2) %>%
  t()
d_fac_m <- cbind(rownames(d_fac_m),d_fac_m)
row.names(d_fac_m) <- 1:nrow(d_fac_m)   
colnames(d_fac_m) <- c("Variable","Label","ImportJSON")
d_fac_m <- as.data.frame(d_fac_m)
names(d_fac_m) <- c("Variable","Label","ImportJSON")

# Question text clean up
for(i in 1:nrow(d_fac_m)){
  # Fixes apostrophe
  if(is.na(d_fac_m[i,]$Label)==F){
    d_fac_m[i,]$Label <- stringr::str_replace(d_fac_m[i,]$Label,"â€™","'")
    # Fixes extra text for question
    if(length(unlist(str_split(d_fac_m[i,]$Label,"\\.\\n",n=2))) == 2){
      d_fac_m[i,]$Label <- str_split(d_fac_m[i,]$Label,"\\.\\n",n=2)[[1]][2]
    }
  } 
}
#Faculty qualitative text clean up
d_fac_m[d_fac_m$Variable=="Q36T",]$Label <- 
  paste0("How ",tolower(d_fac_m[d_fac_m$Variable=="Q36",]$Label)) 

##### Metadata for questions asked of doctoral fellows (students). #####
d_std_m <- data[c(1,2), c(103,8,9,5,32:98)]
d_std_m <-
  data %>% 
  select("Year","Role","RecordedDate","ResponseId","Progress","Remove",
         "Q129_1","Q129_2","Q129_3","Q129_4","Q129_5",
         "Q137_1","Q137_2","Q137_3","Q137_4","Q137_5",
         "Q133","Q48","Q49","Q50","Q148","Q51","Q53","Q53a","Q53b",
         "Q56","Q60","Q59","Q62","Q62a","Q64",
         "Q124_1","Q124_2","Q124_3","Q124_4","Q124_5","Q124_6",
         "Q125_1","Q125_2","Q125_3","Q125_4","Q125_5","Q125_6",
         "Q125_7","Q125_8","Q125_9","Q125_10","Q125_11",
         "Q27","Q27T","Q29",
         "Q138_1","Q138_2","Q138_3","Q138_4","Q138_5","Q138_6",
         "Q140","Q141","Q142","Q143","Q144","Q145","Q137") %>%
  slice(1:2) %>%
  t()
d_std_m <- cbind(rownames(d_std_m),d_std_m)
row.names(d_std_m) <- 1:nrow(d_std_m)   
colnames(d_std_m) <- c("Variable","Label","ImportJSON")
d_std_m <- as.data.frame(d_std_m)
names(d_std_m) <- c("Variable","Label","ImportJSON")

# Loop cleans up text of questions 137_...
for(i in 1:nrow(d_std_m)){
  # Fixes apostrophe
  if(is.na(d_std_m[i,]$Label)==F){
    if(length(unlist(str_split(d_std_m[i,]$Label,"m\\. \\- ",n=2))) == 2){
      d_std_m[i,]$Label <- str_split(d_std_m[i,]$Label,"m\\. \\- ",n=2)[[1]][2]
    }
  } 
}

# Student qualitative text clean up
d_std_m[d_std_m$Variable == "Q133",]$Label <- "If you are not satisfied with the CNS NRT, please feel free to add any comments."
d_std_m[d_std_m$Variable == "Q53a",]$Label <- paste0("In what ways ",
                                                     str_replace(d_std_m[d_std_m$Variable == "Q53",]$Label,
                                                                                 "Do ","do "))
d_std_m[d_std_m$Variable == "Q62a",]$Label <- paste0("In what ways could ",
                                                     str_replace(d_std_m[d_std_m$Variable == "Q62",]$Label,
                                                                                       "Are there ways that ",""))

d_std_m[d_std_m$Variable == "Q27T",]$Label <- paste0("What are ",
                                                     str_replace(d_std_m[d_std_m$Variable == "Q27",]$Label,
                                                                 "Are there ",""))
d_std_m[d_std_m$Variable == "Q29",]$Label <- "Please use the space below to share thoughts or comments about NRT mentorships."

#Qualitative question metadata
d_qual_m <- d_all_m[d_all_m$Variable %in% c("Q155","Q160","Q162","Q107"),]
d_qual_m <- rbind(d_qual_m, d_fac_m[d_fac_m$Variable %in% c("Q36T","Q38"),])
d_qual_m <- rbind(d_qual_m, d_std_m[d_std_m$Variable %in% 
                                      c("Q133","Q53a","Q53b","Q62a","Q64",
                                        "Q27T","Q29","Q147"),])
rownames(d_qual_m) <- 1:nrow(d_qual_m)

#### Prepare Responses for Analysis ####
# Remove metadata rows
data <- data[-c(1,2),]
# Update Progress & Year to variable to numeric
data$Progress <- as.numeric(data$Progress)
data$Year <- as.numeric(data$Year)

##### Process Likert scale variables #####
# Likert Questions
vals1 <- c("Q123","Q2","Q1","Q120","Q158","Q126_1","Q126_2",
           "Q126_3","Q126_4","Q129_1","Q129_2","Q129_3",
           "Q129_4","Q129_5","Q137_1","Q137_2","Q137_3",
           "Q137_4","Q137_5","Q48","Q49","Q50","Q60","Q59",
           "Q124_1","Q124_2","Q124_3","Q124_4","Q124_5",
           "Q124_6","Q125_1","Q125_2","Q125_3","Q125_4",
           "Q125_5","Q125_6","Q125_7","Q125_8","Q125_9",
           "Q125_10","Q125_11","Q138_1","Q138_2","Q138_3",
           "Q138_4","Q138_5","Q138_6","Q140","Q141","Q142",
           "Q143","Q144","Q145","Q137","Q151_1","Q151_2",
           "Q151_3","Q151_4","Q151_5","Q151_6")
# Likert Scale Loop
for(i in 1:length(vals1)){
  data[data[,vals1[i]]=="" & !is.na(data[,vals1[i]]), vals1[i]] <- NA
}

# Y/N/NA questions
vals2 <- c("Q149","Q36","Q148","Q51","Q53","Q62","Q27","Q153")
# Yes / No scale loop
for(i in 1:length(vals2)){
  data[data[,vals2[i]]=="" & !is.na(data[,vals2[i]]), vals2[i]] <- NA
}

# Remove objects
rm(i, vals1, vals2)

#### Subset Data into Groups of Related questions.####
##### Scaled Questions asked of all participants.#####
d_all <-
  data %>% 
  select("Year","Role","RecordedDate","ResponseId","Progress","Remove",
         "Q123","Q2","Q1","Q120","Q158") %>%
  #Removes participants low progress.
  filter(Remove!=1) %>%
  melt(id.vars=c("Year","Role","ResponseId"),
       measure.vars=c("Q123","Q2","Q1","Q120","Q158")) %>%
  dplyr::rename(Variable = variable, Value = value) %>%
  # Removes responses from years a question was not asked.
  filter(Year!=2019 | Variable!="Q158") %>%
  filter(Year!=2020 | Variable!="Q158")

# Clean up NA and Not Applicable values.
d_all[is.na(d_all$Value),]$Value <- "Not Applicable"

# Add Item Labels and Group Name
d_all$Item <- d_all$Group <- NA
d_all$Group <- "NRT Goals"
d_all[d_all$Variable=="Q158",]$Group <- "COVID"
d_all[d_all$Variable=="Q123",]$Item <- "Overall Goals" 
d_all[d_all$Variable=="Q2",]$Item <- "Disseminate Research" 
d_all[d_all$Variable=="Q1",]$Item <- "Local Awareness" 
d_all[d_all$Variable=="Q120",]$Item <- "National Awareness"
d_all[d_all$Variable=="Q158",]$Item <- "COVID-19"

#####Qualitative questions  asked of all participants.#####
d_all_qual <- 
  data %>% 
    select("Year","Role","RecordedDate","ResponseId","Progress","Remove",
           "Q155","Q160","Q162","Q107") %>%
    #Removes participants low progress.
    filter(Remove!=1) %>%
    melt(id.vars=c("Year","Role","ResponseId"),
         measure.vars=c("Q155","Q160","Q162","Q107")) %>%
    dplyr::rename(Variable = variable, Value = value) %>%
    # Removes responses from years a question was not asked.
    filter(Year!=2019 | Variable!="Q155") %>%
    filter(Year!=2019 | Variable!="Q160") %>%
    filter(Year!=2019 | Variable!="Q162") %>%
    filter(Year!=2019 | Variable!="Q107") %>%
    filter(Year!=2020 | Variable!="Q155") %>%
    filter(Year!=2020 | Variable!="Q160") %>%
    filter(Year!=2020 | Variable!="Q162") %>%
    filter(Year!=2021 | Variable!="Q160") %>%
    filter(Year!=2021 | Variable!="Q162") %>%
    # Removes empty qualitative responses (i.e., "" or NA )
    filter(Variable!="Q107" | Value!="") %>%
    filter(Variable!="Q107" | Value!="N/A") %>%
    filter(Variable!="Q155" | Value!="") %>%
    filter(Variable!="Q160" | Value!="") %>%
    filter(Variable!="Q162" | Value!="")

# Add Q Item and Group Labels
d_all_qual$Item <- d_all_qual$Group <- NA
d_all_qual[d_all_qual$Variable=="Q107",]$Group <- "Other"
d_all_qual[d_all_qual$Variable=="Q155",]$Group <- "COVID" 
d_all_qual[d_all_qual$Variable=="Q160",]$Group <- "ContinueNRT"
d_all_qual[d_all_qual$Variable=="Q162",]$Group <- "ContinueNRT"
d_all_qual[d_all_qual$Variable=="Q107",]$Item <- "Comments" 
d_all_qual[d_all_qual$Variable=="Q155",]$Item <- "Comments" 
d_all_qual[d_all_qual$Variable=="Q160",]$Item <- "Explain" 
d_all_qual[d_all_qual$Variable=="Q162",]$Item <- "Comments"

#####Group and ranking questions asked of all participants.#####
# Questions related to continuation of NRT activities after program funding ends.
d_cont <-
  data %>% 
  select("Year","Role","RecordedDate","ResponseId","Progress","Remove",
         "Q159_0_GROUP","Q159_1_GROUP","Q159_2_GROUP",
         "Q159_0_1_RANK","Q159_0_2_RANK","Q159_0_3_RANK",
         "Q159_0_4_RANK","Q159_0_5_RANK","Q159_0_6_RANK",
         "Q159_0_7_RANK","Q159_1_1_RANK","Q159_1_2_RANK",
         "Q159_1_3_RANK","Q159_1_4_RANK","Q159_1_5_RANK",
         "Q159_1_6_RANK","Q159_1_7_RANK","Q159_2_1_RANK",
         "Q159_2_2_RANK","Q159_2_3_RANK","Q159_2_4_RANK",
         "Q159_2_5_RANK","Q159_2_6_RANK","Q159_2_7_RANK") %>%
  #Removes participants low progress, years where questions are not asked.
  filter(Remove!=1) %>%
  filter(Year %in% c(2022,2023)) %>%
  melt(id.vars=c("Year","Role","ResponseId"),
       measure.vars=c("Q159_0_GROUP","Q159_1_GROUP","Q159_2_GROUP",
                      "Q159_0_1_RANK","Q159_0_2_RANK","Q159_0_3_RANK",
                      "Q159_0_4_RANK","Q159_0_5_RANK","Q159_0_6_RANK",
                      "Q159_0_7_RANK","Q159_1_1_RANK","Q159_1_2_RANK",
                      "Q159_1_3_RANK","Q159_1_4_RANK","Q159_1_5_RANK",
                      "Q159_1_6_RANK","Q159_1_7_RANK","Q159_2_1_RANK",
                      "Q159_2_2_RANK","Q159_2_3_RANK","Q159_2_4_RANK",
                      "Q159_2_5_RANK","Q159_2_6_RANK","Q159_2_7_RANK")) %>%
  dplyr::rename(Variable = variable, Value = value) %>%
  # Removes responses from years a question was not asked.
  filter(Value!="") %>%
  arrange(Year,ResponseId,Variable)

# Add Q Item and Group Labels
d_cont$Item <- d_cont$Group <- NA
# Grouping questions
d_cont[d_cont$Variable %in% c("Q159_0_GROUP","Q159_1_GROUP",
                              "Q159_2_GROUP"),]$Group <- "Group"
d_cont[d_cont$Variable=="Q159_0_GROUP",]$Item <- "As Is"
d_cont[d_cont$Variable=="Q159_1_GROUP",]$Item <- "Adapt"
d_cont[d_cont$Variable=="Q159_2_GROUP",]$Item <- "Drop"
# Ranking Questions
d_cont[d_cont$Variable %in% c("Q159_0_1_RANK","Q159_0_2_RANK",
                              "Q159_0_3_RANK","Q159_0_4_RANK",
                              "Q159_0_5_RANK","Q159_0_6_RANK",
                              "Q159_0_7_RANK"),]$Group <- "Rank-As Is"
d_cont[d_cont$Variable %in% c("Q159_1_1_RANK","Q159_1_2_RANK",
                              "Q159_1_3_RANK","Q159_1_4_RANK",
                              "Q159_1_5_RANK","Q159_1_6_RANK",
                              "Q159_1_7_RANK"),]$Group <- "Rank-Adapt" 
d_cont[d_cont$Variable %in% c("Q159_2_1_RANK","Q159_2_2_RANK",
                              "Q159_2_3_RANK","Q159_2_4_RANK",
                              "Q159_2_5_RANK","Q159_2_6_RANK",
                              "Q159_2_7_RANK"),]$Group <- "Rank-Drop"
d_cont[d_cont$Variable %in% c("Q159_0_1_RANK","Q159_1_1_RANK",
                              "Q159_2_1_RANK"),]$Item <- "Colloquium"
d_cont[d_cont$Variable %in% c("Q159_0_2_RANK","Q159_1_2_RANK",
                              "Q159_2_2_RANK"),]$Item <- "Research Showcase"
d_cont[d_cont$Variable %in% c("Q159_0_3_RANK","Q159_1_3_RANK",
                              "Q159_2_3_RANK"),]$Item <- "Affiliate Research"
d_cont[d_cont$Variable %in% c("Q159_0_4_RANK","Q159_1_4_RANK",
                              "Q159_2_4_RANK"),]$Item <- "CNS Dual PhD Program"
d_cont[d_cont$Variable %in% c("Q159_0_5_RANK","Q159_1_5_RANK",
                              "Q159_2_5_RANK"),]$Item <- "Interdisciplinary Mentorships"
d_cont[d_cont$Variable %in% c("Q159_0_6_RANK","Q159_1_6_RANK",
                              "Q159_2_6_RANK"),]$Item <- "Interdisciplinary Skills Training"
d_cont[d_cont$Variable %in% c("Q159_0_7_RANK","Q159_1_7_RANK",
                              "Q159_2_7_RANK"),]$Item <- "CNS Research Skills Training"

#####Scaled questions asked of NRT Faculty.#####
d_fac <- 
  data %>% 
  select("Year","Role","RecordedDate","ResponseId","Progress","Remove",
         "Q149","Q126_1","Q126_2",
         "Q126_3","Q126_4") %>%
  #Selects Faculty Participants, High Progress (40%), and has a mentee.
  filter(Role=="Faculty" |
           is.na(Role)) %>%
  filter(Remove!=1) %>%
  filter(Q149=="Yes") %>%
  melt(id.vars=c("Year","Role","ResponseId"),
       measure.vars=c("Q126_1","Q126_2",
                      "Q126_3","Q126_4")) %>%
  dplyr::rename(Variable = variable, Value = value)

# Clean up NA
d_fac[is.na(d_fac$Value),]$Value <- "Not Applicable"

# Add Q Item and Group Labels
d_fac$Item <- d_fac$Group <- NA
d_fac$Group <- "Faculty Mentorship"
d_fac[d_fac$Variable=="Q126_1",]$Item <- "Presentations" 
d_fac[d_fac$Variable=="Q126_2",]$Item <- "Publications" 
d_fac[d_fac$Variable=="Q126_3",]$Item <- "Grants" 
d_fac[d_fac$Variable=="Q126_4",]$Item <- "Research"

#####Qualitative questions asked of NRT Faculty.#####
d_fac_qual <-
  data %>% 
  select("Year","Role","RecordedDate","ResponseId","Progress","Remove",
         "Q36","Q36T","Q38") %>%
  #Selects Faculty Participants, High Progress (40%), and has a mentee.
  filter(Role=="Faculty" |
           is.na(Role)) %>%
  filter(Remove!=1) %>%
  filter(Q36=="Yes") %>%
  melt(id.vars=c("Year","Role","ResponseId"),
       measure.vars=c("Q36T","Q38")) %>%
  filter(value!="") %>%
  dplyr::rename(Variable=variable, Value=value)

# Add Q Item and Group Labels
d_fac_qual$Item <- d_fac_qual$Group <-NA
d_fac_qual$Group <- "Faculty Mentorship"
d_fac_qual[d_fac_qual$Variable=="Q36T",]$Item <- "Help" 
d_fac_qual[d_fac_qual$Variable=="Q38",]$Item <- "Comments"

#####Scale Questions asked of NRT Doctoral Fellow students.#####
d_std <- 
  data %>% 
  select("Year","Role","RecordedDate","ResponseId","Progress","Remove",
         "Q129_1","Q129_2","Q129_3","Q129_4","Q129_5",
         "Q137_1","Q137_2","Q137_3","Q137_4","Q137_5",
         "Q48","Q49","Q50","Q148","Q51","Q53","Q60","Q59","Q62",
         "Q124_1","Q124_2","Q124_3","Q124_4","Q124_5","Q124_6",
         "Q125_1","Q125_2","Q125_3","Q125_4","Q125_5","Q125_6",
         "Q125_7","Q125_8","Q125_9","Q125_10","Q125_11","Q27",
         "Q138_1","Q138_2","Q138_3","Q138_4","Q138_5","Q138_6",
         "Q140","Q141","Q142","Q143","Q144","Q145","Q137") %>%
  # Filter by role, progress (>40%), 
  filter(Role=="Fellow" |
           is.na(Role)) %>%
  filter(Remove!=1) %>%
  melt(id.vars=c("Year","Role","ResponseId"),
       measure.vars=c("Q129_1","Q129_2","Q129_3","Q129_4","Q129_5",
                      "Q137_1","Q137_2","Q137_3","Q137_4","Q137_5",
                      "Q48","Q49","Q50","Q148","Q51","Q53","Q60","Q59","Q62",
                      "Q124_1","Q124_2","Q124_3","Q124_4","Q124_5","Q124_6",
                      "Q125_1","Q125_2","Q125_3","Q125_4","Q125_5","Q125_6",
                      "Q125_7","Q125_8","Q125_9","Q125_10","Q125_11","Q27",
                      "Q138_1","Q138_2","Q138_3","Q138_4","Q138_5","Q138_6",
                      "Q140","Q141","Q142","Q143","Q144","Q145","Q137")) %>%
  filter(value!="") %>%
  dplyr::rename(Variable=variable, Value=value)

# Add Q Item and Group Labels
d_std$Item <- d_std$Group <- NA

######Student satisfaction questions######
d_std[d_std$Variable %in% 
        c("Q137_2","Q137_5","Q137_3",
          "Q137_1","Q137_4"),]$Group <- c("Student Satisfaction")
d_std[d_std$Variable=="Q137_1",]$Item <- c("Community")
d_std[d_std$Variable=="Q137_2",]$Item <- c("Milestone Progress")
d_std[d_std$Variable=="Q137_3",]$Item <- c("2nd PhD Program")
d_std[d_std$Variable=="Q137_4",]$Item <- c("Mentoring")
d_std[d_std$Variable=="Q137_5",]$Item <- c("CNS PhD Program")

######Program & Mentorship Impact on Research Skills######
## Program Impact
d_std[d_std$Variable %in% 
        c("Q129_1","Q129_2","Q129_5",
          "Q129_4","Q129_3"),]$Group <- "Program Impact" 
d_std[d_std$Variable=="Q129_1",]$Item <- "Presentations" 
d_std[d_std$Variable=="Q129_2",]$Item <- "Writing"
d_std[d_std$Variable=="Q129_3",]$Item <- "Networking"
d_std[d_std$Variable=="Q129_5",]$Item <- "Grant Writing"
d_std[d_std$Variable=="Q129_4",]$Item <- "Technical Skills" 

## Mentorship Impact
d_std[d_std$Variable %in% 
        c("Q124_1","Q124_2","Q124_3",
          "Q124_4","Q124_5","Q124_6"),]$Group <- "Mentorship Impact"
d_std[d_std$Variable=="Q124_1",]$Item <- "Presentations" 
d_std[d_std$Variable=="Q124_2",]$Item <- "Publications" 
d_std[d_std$Variable=="Q124_3",]$Item <- "Grants"
d_std[d_std$Variable=="Q124_4",]$Item <- "Research" 
d_std[d_std$Variable=="Q124_5",]$Item <- "Interdisciplinary"
d_std[d_std$Variable=="Q124_6",]$Item <- "Collaboration"

# Mentor Quality
d_std[d_std$Variable %in% 
        c("Q125_1","Q125_2","Q125_3","Q125_4",
          "Q125_5","Q125_6","Q125_7","Q125_8",
          "Q125_9","Q125_10","Q125_11"),]$Group <- "Mentor Quality"
d_std[d_std$Variable=="Q125_1",]$Item <- "Accessibility" 
d_std[d_std$Variable=="Q125_2",]$Item <- "Integrity" 
d_std[d_std$Variable=="Q125_3",]$Item <- "Expertise"
d_std[d_std$Variable=="Q125_4",]$Item <- "Approach" 
d_std[d_std$Variable=="Q125_5",]$Item <- "Supportive"
d_std[d_std$Variable=="Q125_6",]$Item <- "Constructive"
d_std[d_std$Variable=="Q125_7",]$Item <- "Guidance" 
d_std[d_std$Variable=="Q125_8",]$Item <- "Responsive"
d_std[d_std$Variable=="Q125_9",]$Item <- "Acknowledgment" 
d_std[d_std$Variable=="Q125_10",]$Item <- "Resources"
d_std[d_std$Variable=="Q125_11",]$Item <- "Challenge"

# Mentors Improvement
d_std[d_std$Variable=="Q27",]$Group <- "Mentorship"
d_std[d_std$Variable=="Q27",]$Item <- "Improve"

######Interdisciplinary######
d_std[d_std$Variable %in% c("Q137"),]$Group <- "CNS Program"
d_std[d_std$Variable %in% 
       c("Q138_1","Q138_2","Q138_3","Q138_4","Q138_5","Q138_6"),]$Group <- "Academic Research"
d_std[d_std$Variable %in% 
       c("Q140","Q141","Q142","Q143","Q144","Q145"),]$Group <- "Personal Values"
d_std[d_std$Variable=="Q137",]$Item <- "CNS Program" 
d_std[d_std$Variable=="Q138_1",]$Item <- "Openness" 
d_std[d_std$Variable=="Q138_2",]$Item <- "Learning" 
d_std[d_std$Variable=="Q138_3",]$Item <- "New Methods"
d_std[d_std$Variable=="Q138_4",]$Item <- "Complex Problems" 
d_std[d_std$Variable=="Q138_5",]$Item <- "Frameworks"
d_std[d_std$Variable=="Q138_6",]$Item <- "Thinking"
d_std[d_std$Variable=="Q140",]$Item <- "Collaboration" 
d_std[d_std$Variable=="Q141",]$Item <- "Time Investment"
d_std[d_std$Variable=="Q142",]$Item <- "Challenges" 
d_std[d_std$Variable=="Q143",]$Item <- "Perspectives"
d_std[d_std$Variable=="Q144",]$Item <- "Fields"
d_std[d_std$Variable=="Q145",]$Item <- "Methods"

######Program Activities######
d_std[d_std$Variable %in% 
        c("Q48","Q49","Q50",
          "Q148","Q51","Q53"),]$Group <- "Showcase"
d_std[d_std$Variable %in% 
        c("Q60","Q59","Q62"),]$Group <- "Colloquium"
d_std[d_std$Variable=="Q48",]$Item <- "Quality of Work" 
d_std[d_std$Variable=="Q49",]$Item <- "Research Skills" 
d_std[d_std$Variable=="Q50",]$Item <- "Collaboration" 
d_std[d_std$Variable=="Q148",]$Item <- "Publications"
d_std[d_std$Variable=="Q51",]$Item <- "Proposals"
d_std[d_std$Variable=="Q53",]$Item <- "Research Skills" 
d_std[d_std$Variable=="Q60",]$Item <- "Research Skills"
d_std[d_std$Variable=="Q59",]$Item <- "Collaboration"
d_std[d_std$Variable=="Q62",]$Item <- "Improved" 

#####Qualitative Questions asked of NRT Doctoral Fellow students.#####
d_std_qual <- 
  data %>% 
  melt(id.vars=c("Year","Role","ResponseId"),
       measure.vars=c("Q133","Q53a","Q53b","Q62a","Q64",
                      "Q27T","Q29","Q147")) %>%
  filter(value!="", !is.na(value)) %>%
  dplyr::rename(Variable=variable, Value=value)

# Add Q Item and Group Labels
d_std_qual$Item <- d_std_qual$Group <- NA
d_std_qual[d_std_qual$Variable=="Q133",]$Group <- "Student Satisfaction"
d_std_qual[d_std_qual$Variable=="Q53a",]$Group <- "Showcase"
d_std_qual[d_std_qual$Variable=="Q53b",]$Group <- "Showcase"
d_std_qual[d_std_qual$Variable=="Q62a",]$Group <- "Colloquium"
d_std_qual[d_std_qual$Variable=="Q64",]$Group <- "Colloquium"
d_std_qual[d_std_qual$Variable=="Q27T",]$Group <- "Mentorship"
d_std_qual[d_std_qual$Variable=="Q29",]$Group <- "Mentorship"
d_std_qual[d_std_qual$Variable=="Q147",]$Group <- "Interdisciplinary"
d_std_qual[d_std_qual$Variable=="Q133",]$Item<- "Comments"
d_std_qual[d_std_qual$Variable=="Q53a",]$Item <- "Advance Work"
d_std_qual[d_std_qual$Variable=="Q53b",]$Item <- "Advance Skills"
d_std_qual[d_std_qual$Variable=="Q62a",]$Item <- "Improve"
d_std_qual[d_std_qual$Variable=="Q64",]$Item <- "Comments"
d_std_qual[d_std_qual$Variable=="Q27T",]$Item <- "Improve"
d_std_qual[d_std_qual$Variable=="Q29",]$Item <- "Comments"
d_std_qual[d_std_qual$Variable=="Q147",]$Item <- "Comments"

####Qualitative Questions Set####
d_qual <- rbind(d_all_qual,d_fac_qual,d_std_qual)
rm(d_all_qual,d_fac_qual,d_std_qual)

#### Export results ####
# Prepared Survey response data
write.csv(data, paste0(path_s,"CNS_NRT-SurveyResponses_2019-2023-2025prep.csv"),
          row.names = F)
# All
write.csv(d_all, paste0(path_s,"CNS_NRT-Survey_2019-2023_general.csv"),
          row.names=F, na="NA")
write.csv(d_all_m, paste0(path_s,"CNS_NRT-Survey_2019-2023_general_meta.csv"),
          row.names=F, na="NA")
write.csv(d_cont, paste0(path_s,"CNS_NRT-Survey_2019-2023_continue.csv"),
          row.names=F, na="NA")
write.csv(d_cont_m, paste0(path_s,"CNS_NRT-Survey_2019-2023_continue_meta.csv"),
          row.names=F, na="NA")
# Faculty
write.csv(d_fac, paste0(path_s,"CNS_NRT-Survey_2019-2023_faculty.csv"),
          row.names=F, na="NA")
write.csv(d_fac_m, paste0(path_s,"CNS_NRT-Survey_2019-2023_faculty_meta.csv"),
          row.names=F, na="NA")
# Students
write.csv(d_std, paste0(path_s,"CNS_NRT-Survey_2019-2023_students.csv"),
          row.names=F, na="NA")
write.csv(d_std_m, paste0(path_s,"CNS_NRT-Survey_2019-2023_students_meta.csv"),
          row.names=F, na="NA")
# Qualitative
write.csv(d_qual, paste0(path_s,"CNS_NRT-Survey_2019-2023_qual.csv"),
          row.names=F, na="NA")
write.csv(d_qual_m, paste0(path_s,"CNS_NRT-Survey_2019-2023_qual_meta.csv"),
          row.names=F, na="NA")
