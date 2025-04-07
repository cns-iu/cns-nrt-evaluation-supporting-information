#### Setup Environment ####
#rm(list=ls())
# Libraries
library(tidyr)        # Tidyverse 
library(reshape2)     # Data Shaping
library(plyr)         # Data shaping
library(dplyr)        # Data shaping
library(forcats)      # Factor manipulation
library(stringr)      # String manipulation
library(magrittr)     # Piping
library(ggplot2)      # Figures
library(patchwork)    # Grammar for combining figures
library(grid)         # Figure layouts
library(gridExtra)    # Figure layouts

# Set CNS-NRT figure theme
theme_nrt <- theme_grey()
theme_set(theme_nrt)
theme_update(
  plot.background = element_rect(fill="White"),
  plot.caption.position = "plot",
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.background = element_rect(fill="White"),
  panel.border = element_rect(fill=NA, 
                              color="black",
                              linetype="solid"))

#### Set Paths ####
path_s <- paste0(getwd(),"/data/survey/")
path_t <- paste0(getwd(),"/tables/")
path_f <- paste0(getwd(),"/figures/")
path_si <- paste0(getwd(),"/supporting_information/")

#### Load Survey Responses ####
data <- read.csv(file=paste0(path_s,"CNS_NRT-SurveyResponses_2019-2023-prepared.csv"), 
                 header = T, sep=",") 
data$Progress <- as.numeric(data$Progress)
# All NRT question responses
d_all_m <- read.csv(file=paste0(path_s,"CNS_NRT-Survey_2019-2023_general_meta.csv"), 
                    header = T, sep=",")
d_all   <- read.csv(file=paste0(path_s,"CNS_NRT-Survey_2019-2023_general.csv"),
                    header = T, sep=",")

# NRT Faculty question responses
d_fac   <- read.csv(file=paste0(path_s,"CNS_NRT-Survey_2019-2023_faculty.csv"),
                    header = T, sep=",")
d_fac_m <- read.csv(file=paste0(path_s,"CNS_NRT-Survey_2019-2023_faculty_meta.csv"),
                    header = T, sep=",")
# NRT Student question responses
d_std_m <- read.csv(file=paste0(path_s,"CNS_NRT-Survey_2019-2023_students_meta.csv"),
                    header = T, sep=",")
d_std   <- read.csv(file=paste0(path_s,"CNS_NRT-Survey_2019-2023_students.csv"),
                    header = T, sep=",")
# NRT Student question responses
d_cont_m <- read.csv(file=paste0(path_s,"CNS_NRT-Survey_2019-2023_continue_meta.csv"),
                     header = T, sep=",")
d_cont  <- read.csv(file=paste0(path_s,"CNS_NRT-Survey_2019-2023_continue.csv"),
                    header = T, sep=",")
# NRT Qualitative responses
d_qual_m <- read.csv(file=paste0(path_s,"CNS_NRT-Survey_2019-2023_qual_meta.csv"), 
                     header = T, sep=",")
d_qual   <- read.csv(file=paste0(path_s,"CNS_NRT-Survey_2019-2023_qual.csv"), 
                     header = T, sep=",")
# Load Response Summary Statistics
rep <- read.csv(file=paste0(path_s,"CNS_NRT-SurveyResponseRates_2019-2023.csv"), 
                header = T, sep=",") 

#### Set factor variables ####
# Factor scales
roles <- c("Faculty","Fellow","Affiliate","Admin")
likerts <- c("Strongly Disagree", "Disagree", "Somewhat Disagree",
             "Somewhat Agree", "Agree", "Strongly Agree","Not Applicable")
yn <- c("Not applicable","No","Yes")

# General Question - All Participants 
d_all$Role <- ordered(d_all$Role, levels=roles)
d_all$Group <- factor(d_all$Group)
d_all$Item <- ordered(d_all$Item, 
                      levels=c("Overall Goals","Disseminate Research",
                               "Local Awareness","National Awareness",
                               "COVID-19"))
d_all <- d_all %>% filter(Value!="Not Applicable")
d_all$Value <- ordered(d_all$Value, levels=likerts[c(1:6)])

# Continuation of CNS NRT Activities
d_cont$Role <- ordered(d_cont$Role, levels=roles)
d_cont$Group <- factor(d_cont$Group)
d_cont$Item <- factor(d_cont$Item)

# Faculty Questions
d_fac$Role <- ordered(d_fac$Role, levels=roles)
d_fac$Group <- factor(d_fac$Group)
d_fac$Item <- factor(d_fac$Item)
d_fac <- d_fac %>% filter(Value!="Not Applicable")
d_fac$Value <- ordered(d_fac$Value, levels=likerts[c(1:6)])

# Doctoral Fellow Questions - Binary - Yes & No
d_std_b <- d_std[d_std$Value %in% c("Yes","No"),]
d_std_b$Role <- ordered(d_std_b$Role, levels=roles)
d_std_b$Group <- factor(d_std_b$Group)
d_std_b$Value <- ordered(d_std_b$Value, levels=yn[1:3])

# Doctoral Fellow Questions  - Likert Scales
d_std <- 
  d_std %>% 
  filter(Value!="Yes" | Value!="No") %>%
  filter(Value!= "Not Applicable")
d_std$Role <- ordered(d_std$Role, levels=roles)
d_std$Group <- factor(d_std$Group)
d_std$Value <- ordered(d_std$Value, levels=likerts[1:6])

# Qualitative Questions
d_qual$Role <- ordered(d_qual$Role, levels=roles)
d_qual$Group <- factor(d_qual$Group)
d_qual$Item <- factor(d_qual$Item)

#### Tables ####
##### Table 5: Survey Response Statistics #####
# Response statistics
names(rep)[2] <- "Role"
# Overall
rep_overall <- rep[rep$Role == "Overall",]
rep_overall <- rep_overall[,c(1,3:4,7,6,9)]
row.names(rep_overall) <- 1:nrow(rep_overall)
rep_overall[,c(4,6)] <- rep_overall[,c(4,6)]*100
rep_overall <- 
  rbind(rep_overall,
        c("2019-2023",
          sum(rep_overall$Invitation),
          sum(rep_overall$Responses),
          round(sum(rep_overall$Responses)/sum(rep_overall$Invitation)*100,1),
          sum(rep_overall$Responses.Used),
          round(sum(rep_overall$Responses.Used)/sum(rep_overall$Invitation)*100,1)))
rep_overall[,c(2,3,5)] <- as.integer(unlist(rep_overall[,c(2,3,5)]))
rep_overall[,c(4,6)] <- as.numeric(unlist(rep_overall[,c(4,6)]))
write.csv(rep_overall, paste0(path_t,"table_5-CNS_NRT-Survey_2019-2023-ResponseRates-overall.csv"),
          row.names = F)
names(rep_overall) <- c("Year(s)","Invitations","Total Responses",
                        "Total Resp.\nRate (%)",
                        "Completed (>40%)\nResponses",
                        "Completed Resp.\nRate (%)")

##### Table 6: Survey Response by NRT Roles #####
# By Role
rep <- rep[rep$Role != "Overall",]
row.names(rep) <- 1:nrow(rep)
rep$Role <- ordered(rep$Role, levels=roles)
rep <- rep[,c(1:4,7,6,9)]
write.csv(rep, paste0(path_t,"table_6-CNS_NRT-Survey_2019-2023-ResponseRates-role.csv"),
          row.names = F)
names(rep) <- c("Year","NRT Role","Invitations","Total Responses",
                "Total Resp.\nRate (%)",
                "Completed (>40%)\nResponses","Completed Resp.\nRate (%)")
rep[,c(5,7)] <- rep[,c(5,7)]*100

##### Table 7: Student satisfaction questions ######
tmp <- 
  d_std %>%
  filter(Group == "Student Satisfaction") %>%
  mutate(Item = ordered(Item, 
                        levels=c("Milestone Progress", "CNS PhD Program",
                                 "2nd PhD Program","Community","Mentoring"))) %>%  
  group_by(Item, Year) %>%
  dplyr::summarise(Responses = n(),
                   Median = round(median(as.numeric(Value)),1),
                   Mean = round(mean(as.numeric(Value)),1),
                   SD = round(sd(as.numeric(Value)),3))
write.csv(tmp,paste0(path_t,"table_7-CNS_NRT-Survey_2019-2023-DoctoralFellow_Satisfaction.csv"),
          row.names = FALSE)
rm(tmp)

##### Table 8: CNS NRT Academic Program Impact on Research Skills ####
tmp <-
  d_std %>% 
  filter(Group=="Program Impact") %>%
  mutate(Item==ordered(Item, 
                       levels= c("Presentations","Writing","Grant Writing",
                                 "Technical Skills","Networking"))) %>%
  group_by(Item, Year) %>%
  dplyr::summarise(Responses = n(),
                   Median = round(median(as.numeric(Value)),1),
                   Mean = round(mean(as.numeric(Value)),1),
                   SD = round(sd(as.numeric(Value)),3))
write.csv(tmp,paste0(path_t,"table_8-CNS_NRT-Survey_2019-2023-DoctoralFellow_ProgramImpact.csv"),
          row.names = FALSE)
rm(tmp)

##### Table 9: CNS NRT Faculty Agreement Mentorship Improves Research Skills #####
tmp <-
  d_fac %>%
  filter(Value!="Not Applicable") %>%
  group_by(Item) %>%
  dplyr::summarise(Responses = n(),
                   Median = round(median(as.numeric(Value)),1),
                   Mean = round(mean(as.numeric(Value)),1),
                   SD = round(sd(as.numeric(Value)),3))
write.csv(tmp,paste0(path_t,"table_9-CNS_NRT-Survey_2019-2023-Faculty_MentorshipImpact.csv"),
          row.names = FALSE)
rm(tmp)

##### Table 10: Mentorship Impact on Research Skills #####
tmp <-
  d_std %>% 
  filter(Group == "Mentorship Impact") %>%
  mutate(Item == ordered(Item, 
                         levels=c("Grants","Collaboration",
                                  "Interdisciplinary","Publications",
                                  "Presentations","Research"))) %>%
  group_by(Item) %>%
  dplyr::summarise(Responses = n(),
                   Median = round(median(as.numeric(Value)),1),
                   Mean = round(mean(as.numeric(Value)),1),
                   SD = round(sd(as.numeric(Value)),3)) %>%
  arrange(desc(Item))
write.csv(tmp,paste0(path_t,"table_10-CNS_NRT-Survey_2019-2023-DoctoralFellow_MentorshipImpact.csv"),
          row.names = FALSE)
rm(tmp)

##### Table 11: Quality of Mentor #####
tmp <-
  d_std %>%  
  filter(Group=="Mentor Quality") %>%
  group_by(Item) %>%
  dplyr::summarise(Responses = n(),
                   Median = round(median(as.numeric(Value)),1),
                   Mean = round(mean(as.numeric(Value)),1),
                   SD = round(sd(as.numeric(Value)),3))
write.csv(tmp,paste0(path_t,"/table_11-CNS_NRT-Survey_2019-2023-DoctoralFellow_MentorshipQuality.csv"),
          row.names = FALSE)
rm(tmp)

#### Figures ####
##### Figure 4: CNS NRT Goal Achievement #####
# Annual count of distinct participants for each year
participant_ct <- 
  d_all %>% 
  filter(Group!="COVID") %>%
  select(Year, ResponseId) %>% 
  distinct() %>%
  group_by(Year) %>% 
  dplyr::summarise(Participants = n())
# Annual counts of Item responses, by Likert Value
answer_ct <- 
  d_all %>%
  filter(Group!="COVID") %>%
  filter(Value!="Not Applicable") %>%
  group_by(Year, Item, Value) %>% 
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  complete(Item,Year,Value, fill=list(Count=0)) %>%
  filter(Item!="COVID-19")
# Combine Annual Participant and Answer Counts
# Calculate Percentage of Answers for each Item and Likert Value
answer_ct <- 
  plyr::join(participant_ct, answer_ct, 
             by=c("Year")) %>%
  select(Year,Item,Value,Participants,Count) %>%
  mutate(Percent=round(Count/Participants*100,2)) %>%
  complete(Item,Value, fill=list(Participants=0,Count=0,Percent=0)) %>%
  filter(Item!="COVID-19") %>%
  filter(!is.na(Value)) %>%
  arrange(Item,Year,Item,desc(Value))
answer_ct[answer_ct$Percent==0,]$Percent <- NA
# Generate figure
p1 <- 
  answer_ct %>%
  mutate(Year=factor(Year)) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", drop=F) +
  facet_wrap(~Item, ncol=4) +
  scale_fill_steps(low="#EBF7FF", high="#005B94", na.value = "White",
                   limits=c(1,52)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(y=NULL, x="Year") +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(vjust = -10, size=8),
        axis.text = element_text(size=6.5),
        strip.placement.y = "outside",
        strip.background = element_rect(fill="White"),
        strip.text.y.left = element_text(size=8.5),
        strip.text.x = element_text(size=8.5), 
        legend.title = element_text(size=8),
        legend.text=element_text(vjust=.5, size=6),
        plot.margin = margin(,.1,.1,.1, "cm"))
# Tiff
tiff(filename = paste0(path_f,"/Fig4.tiff"),
     width = 2238, height = 560, units = "px", pointsize = 12,
     compression = c("lzw"), bg = "white", res = 300, , restoreConsole = TRUE,
     type = c("windows"), symbolfamily="default")
  p1
dev.off()
# Png
png(filename = paste0(path_f,"/png/Fig4.png"),
     width = 2238, height = 560, units = "px", pointsize = 12,
     bg = "white", res = 300, , restoreConsole = TRUE,
     type = c("windows"), symbolfamily="default")
p1
dev.off()

##### Figure 5: CNS NRT Goal Achievement - By Respondent Roles #####
# Annual count of distinct participants, by Role
participant_ct <- 
  d_all %>% 
  filter(Group!="COVID") %>%
  select(Year, Role, ResponseId) %>% 
  distinct() %>%
  group_by(Year, Role) %>% 
  dplyr::summarise(Participants = n()) %>%
  complete(Role, fill = list(Year=2019, Participants = 0))
# Annual counts of Item responses, by Role, and Likert Value
answer_ct <- 
  d_all %>%
  filter(Group!="COVID") %>%
  filter(Value!="Not Applicable") %>%
  group_by(Year, Role, Item, Value) %>% 
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  complete(Role,Item,Year,fill=list(Count=0)) %>%
  filter(Item!="COVID-19")
# Combine Annual Participant and Answer Counts, by Role
# Calculate Percentage of Answers for each Item and Likert Value
answer_ct <- 
  plyr::join(participant_ct, answer_ct, 
             by=c("Year", "Role")) %>%
  select(Year,Role,Item,Value,Participants,Count) %>%
  mutate(Percent=round(Count/Participants*100,2)) %>%
  complete(Role,Item,Value, fill=list(Participants=0,Count=0,Percent=0)) %>%
  filter(Item!="COVID-19") %>%
  filter(!is.na(Value)) %>%
  arrange(Item,Role,Year,Item,desc(Value))
answer_ct[answer_ct$Percent==0,]$Percent <- NA
# Generate Figure Parts
p1 <- 
  answer_ct %>%
  filter(Role=="Faculty") %>%
  mutate(Year=factor(Year)) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", bins=13, binwidth=4, drop=F) +
  facet_grid(Role~Item, switch="y") +
  scale_fill_steps(low="#EBF7FF",high="#005B94",na.value = "White",
                   limits=c(1,100),n.breaks=6) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(y=NULL, x=NULL) +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(vjust = -10),
        axis.text.x = element_text(size=6.5),
        axis.text.y = element_text(size=7.5),
        strip.placement = "outside",
        strip.background = element_rect(fill="White"),
        strip.text = element_text(size=8.5),
        legend.title = element_text(size=8.5),
        legend.text=element_text(vjust=1.5, size=7))
#Doctoral Students
p2 <- 
  answer_ct %>%
  filter(Role=="Fellow") %>%
  mutate(Year=factor(Year)) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", bins=13, binwidth=4, drop=F) +
  scale_fill_steps(low="#E0F7F4",high="#009480",na.value = "White",
                   limits=c(1,100),n.breaks=6) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_grid(Role~Item, switch="y") +
  labs(x=NULL, y=NULL) +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(vjust = -10),
        axis.text.x = element_text(size=6.5),
        axis.text.y = element_text(size=7.5),
        strip.placement = "outside",
        strip.text.x = element_blank(),
        strip.text.y = element_text(size=8.5),
        strip.background = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(vjust=1.5, size=7))
#Affiliates
p3 <- 
  answer_ct %>%
  filter(Role=="Affiliate") %>%
  mutate(Year=factor(Year)) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", bins=13, binwidth=4, drop=F) +
  scale_fill_steps(low="#FFEAD1",high="#FEC148",na.value = "White",
                   limits=c(1,100),n.breaks=6) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_grid(Role~Item, switch="y") +
  labs(x=NULL, y=NULL) +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(vjust = -10),
        axis.text.x = element_text(size=6.5),
        axis.text.y = element_text(size=7.5),
        strip.placement = "outside",
        strip.text.x = element_blank(),
        strip.text.y = element_text(size=8.5),
        strip.background = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(vjust=1.5, size=7))
#Administrators
p4 <- 
  answer_ct %>%
  filter(Role=="Admin") %>%
  mutate(Year=factor(Year)) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", bins=13, binwidth=4, drop=F) +
  scale_fill_steps(low="#F1E4EF",high="#5c3354",na.value = "White",
                   limits=c(1,100),n.breaks=6) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_grid(Role~Item, switch="y") +
  labs(x="Years", y=NULL) +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(vjust = -10),
        axis.text.x = element_text(size=6.5),
        axis.text.y = element_text(size=7.5),
        strip.placement = "outside",
        strip.text.x = element_blank(),
        strip.text.y = element_text(size=8.5),
        strip.background = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(vjust=1.5, size=7))
# Tiff 
tiff(filename = paste0(path_f,"/Fig5.tiff"),
     width = 2250, height = 1800, units = "px", pointsize = 12,
     compression = c("lzw"), bg = "white", res = 300, , restoreConsole = TRUE,
     type = c("windows"), symbolfamily="default")
  (p1/p2/p3/p4)
dev.off()

# PNG
png(filename = paste0(path_f,"/png/Fig5.png"),
    width = 2250, height = 1800, units = "px", pointsize = 12,
    bg = "white", res = 300, , restoreConsole = TRUE,
    type = c("windows"), symbolfamily="default")
  (p1/p2/p3/p4)
dev.off()
rm(p1,p2,p3,p4)

##### Figure 6: CNS NRT Doctoral Fellow Student Satisfaction #####
# Student Participants - Satisfaction Questions
participant_ct <- 
  d_std %>% 
  filter(Group=="Student Satisfaction") %>%
  select(Year, Role, ResponseId) %>% 
  distinct() %>%
  group_by(Year, Role) %>% 
  dplyr::summarise(Participants = n())
# Annual counts of Item responses, by Role, and Likert Value
answer_ct <- 
  d_std %>%
  select(Year,Variable,Value,Group,Item) %>%
  filter(Group=="Student Satisfaction") %>%
  filter(Value!="Not Applicable") %>%
  group_by(Year, Item, Value) %>% 
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  complete(Item,Year,Value,fill=list(Count=0))
# Combine Annual Participant and Answer Counts, by Role
# Calculate Percentage of Answers for each Item and Likert Value
answer_ct <- 
  plyr::join(participant_ct, answer_ct, 
             by=c("Year")) %>%
  select(Year,Role,Item,Value,Participants,Count) %>%
  mutate(Percent=round(Count/Participants*100,2)) %>%
  filter(!is.na(Value)) %>%
  arrange(Item,Role,Year,Item,desc(Value))
answer_ct[answer_ct$Percent==0,]$Percent <- NA
# Generate Figure
p1 <-
  answer_ct %>%
  mutate(Year=factor(Year)) %>%
  mutate(Item=ordered(Item, levels=c("Milestone Progress","CNS PhD Program",
                                     "2nd PhD Program","Mentoring",
                                     "Community"))) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", drop=F) +
  facet_wrap(~Item) +
  scale_fill_steps(low="#F1E4EF",high="#5c3354",na.value = "White",
                   limits=c(1,70),n.break=7) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        axis.title.x = element_text(size=7),
        axis.title.y=element_blank(),
        axis.text=element_text(size=6),
        strip.background = element_blank(),
        strip.text.x = element_text(size=7),
        legend.title=element_text(vjust=.5, size=7),
        legend.text=element_text(vjust=1.5, size=6.5))
# Tiff
tiff(filename = paste0(path_f,"/Fig6.tiff"),
     width = 1550, height = 925, units = "px", pointsize = 12,
     compression = c("lzw"), bg = "white", res = 300, , restoreConsole = TRUE,
     type = c("windows"), symbolfamily="default")
  p1
dev.off()
# PNG
png(filename = paste0(path_f,"/png/Fig6.png"),
    width = 1500, height = 900, units = "px", pointsize = 12,
    bg = "white", res = 300, , restoreConsole = TRUE,
    type = c("windows"), symbolfamily="default")
  p1
dev.off()
rm(p1)

##### Figure 7: Program Impact on Doctoral Fellow Research Skills #####
# Doctoral Fellow Participants - Research Skills
participant_ct <- 
  d_std %>% 
  filter(Group=="Program Impact") %>%
  select(Year, Role, ResponseId) %>% 
  distinct() %>%
  group_by(Year, Role) %>% 
  dplyr::summarise(Participants = n())
# Annual counts of Item responses, by Role, and Likert Value
answer_ct <- 
  d_std %>%
  select(Year,Variable,Value,Group,Item) %>%
  filter(Group=="Program Impact") %>%
  filter(Value!="Not Applicable") %>%
  group_by(Year, Item, Value) %>% 
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  complete(Item,Year,Value,fill=list(Count=0))
# Combine Annual Participant and Answer Counts, by Role
# Calculate Percentage of Answers for each Item and Likert Value
answer_ct <- 
  plyr::join(participant_ct, answer_ct, 
             by=c("Year")) %>%
  select(Year,Role,Item,Value,Participants,Count) %>%
  mutate(Percent=round(Count/Participants*100,2)) %>%
  filter(!is.na(Value)) %>%
  arrange(Item,Role,Year,Item,desc(Value))
answer_ct[answer_ct$Percent==0,]$Percent <- NA
# Generate Figure
p1 <- 
  answer_ct %>%
  mutate(Year=factor(Year)) %>%
  mutate(Item=ordered(Item, levels=c("Writing","Presentations",
                                     "Technical Skills","Grant Writing",
                                     "Networking"))) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", drop=F) +
  facet_wrap(~Item) +
  scale_fill_steps(low="#F1E4EF", high="#5c3354",na.value = "White",
                   limits=c(1,60), n.breaks=7) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        axis.title.x = element_text(size=7),
        axis.title.y=element_blank(),
        axis.text=element_text(size=6),
        strip.background = element_blank(),
        strip.text.x = element_text(size=7),
        legend.title=element_text(vjust=.5, size=7),
        legend.text=element_text(vjust=1.5, size=6.5))
# Tiff
tiff(filename = paste0(path_f,"/Fig7.tiff"),
     width = 1500, height = 900, units = "px", pointsize = 12,
     compression = c("lzw"), bg = "white", res = 300, , restoreConsole = TRUE,
     type = c("windows"), symbolfamily="default")
  p1
dev.off()
# PNG
png(filename = paste0(path_f,"/png/Fig7.png"),
    width = 1500, height = 900, units = "px", pointsize = 12,
    bg = "white", res = 300, , restoreConsole = TRUE,
    type = c("windows"), symbolfamily="default")
  p1
dev.off()
rm(p1)

##### Figure 8: Program Impact on Doctoral Fellow Sense of Interdisciplinary #####
# Fellows Participants - Program Impact on CNS Fellow Interdisciplinary
participant_ct <-
  d_std %>% 
  filter(Group %in% c("CNS Program")) %>%
  select(Year,Role,ResponseId) %>%
  distinct() %>%
  group_by(Year, Role) %>% 
  dplyr::summarise(Participants = n())
# Annual counts of Item responses, by Role, and Likert Value
answer_ct <-
  d_std %>% 
  filter(Group %in% c("CNS Program")) %>%
  filter(!is.na(Value)) %>%
  group_by(Year, Item, Value) %>%
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  complete(Item,Value,Year,fill=list(Count=0))
# Combine Annual Participant and Answer Counts, by Role
# Calculate Percentage of Answers for each Item and Likert Value
answer_ct <- 
  plyr::join(participant_ct, answer_ct, 
             by=c("Year")) %>%
  select(Year,Role,Item,Value,Participants,Count) %>%
  mutate(Percent=round(Count/Participants*100,2)) %>%
  filter(!is.na(Value)) %>%
  arrange(Item,Role,Year,Item,desc(Value)) %>%
  filter(Year!=2019)
# Generate Figure
p1 <- 
  answer_ct %>%
  mutate(Year=factor(Year)) %>%
  ggplot(aes(y=Value, weight=Percent)) +
  geom_bar(fill ="#5c3354") +
  facet_wrap(~Year,nrow=1) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y=NULL,x="Percent") +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        strip.background = element_blank(),
        strip.text.x = element_text(size=7),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7),
        legend.title=element_text(vjust=.5, size=7),
        legend.text=element_text(vjust=1.5, size=8))
# Tiff
tiff(filename = paste0(path_f,"/Fig8.tiff"),
     width = 1600, height = 500, units = "px", pointsize = 12,
     compression = c("lzw"), bg = "white", res = 300, , restoreConsole = TRUE,
     type = c("windows"), symbolfamily="default")
p1
dev.off()
# PNG
png(filename = paste0(path_f,"/png/Fig8.png"),
    width = 1600, height = 500, units = "px", pointsize = 12,
    bg = "white", res = 300, , restoreConsole = TRUE,
    type = c("windows"), symbolfamily="default")
p1
dev.off()
rm(p1)

##### Figure 9: Doctoral Fellows Interdisciplinary Values and Impact on Research Skills #####
# Fellows Participants - Interdisciplinary Questions
participant_ct <- 
  d_std %>% 
  filter(Group %in% c("Academic Research","Personal Values")) %>%
  select(Group, Year, Role, ResponseId) %>% 
  distinct() %>%
  group_by(Group, Year, Role) %>% 
  dplyr::summarise(Participants = n())
# Annual counts of Item responses, by Role, and Likert Value
answer_ct <- 
  d_std %>% 
  filter(Group %in% c("Academic Research","Personal Values")) %>%
  select(Group,Item,Year,Variable,Value) %>%
  filter(Value!="Not Applicable") %>%
  mutate(Group=ordered(Group, levels=c("Academic Research",
                                       "Personal Values")),
         Item=ordered(Item, 
                      levels=c("Openness","Learning","Thinking","Frameworks","New Methods","Complex Problems",
                               "Challenges","Perspectives","Collaboration","Fields","Methods","Time Investment"))) %>%
  group_by(Group, Item, Year, Value) %>% 
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  complete(Group,Item,Year,Value,fill=list(Count=0))
# Combine Annual Participant and Answer Counts, by Role
# Calculate Percentage of Answers for each Item and Likert Value
answer_ct <- 
  plyr::join(participant_ct, answer_ct, 
             by=c("Year","Group")) %>%
  select(Group,Item,Value,Year,Participants,Count) %>%
  mutate(Percent=round(Count/Participants*100,2)) %>%
  filter(!is.na(Value)) %>%
  arrange(Group,Item,Year,desc(Value))
answer_ct[answer_ct$Percent==0,]$Percent <- NA
# Generate Figure Parts
#Interdisciplinary Impact on Academic Research
p1 <- 
  answer_ct %>%
  filter(Group=="Academic Research") %>% 
  filter(Item %in% c("Openness","Learning","Thinking","Frameworks",
                     "New Methods","Complex Problems")) %>% 
  mutate(Year=factor(Year)) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", drop=F) +
  facet_wrap(~Item) +
  scale_fill_steps(low="#F1E4EF", high="#5c3354",na.value = "White",
                   limits=c(1,60), n.breaks=7) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(y=NULL,tag="A") +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        axis.title.x = element_text(size=9),
        axis.text=element_text(size=6.5),
        strip.background = element_blank(),
        strip.text.x = element_text(size=9),
        legend.title=element_text(vjust=.5, size=9),
        legend.text=element_text(vjust=1.5, size=6.5))
#Interdisciplinary Impact on Personal Values
p2 <- 
  answer_ct %>%
  filter(Group=="Personal Values") %>%
  filter(Item %in% c("Challenges","Perspectives","Collaboration",
                     "Fields","Methods","Time Investment")) %>% 
  mutate(Year=factor(Year)) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", drop=F) +
  facet_wrap(~Item) +
  scale_fill_steps(low="#F1E4EF", high="#5c3354",na.value = "White",
                   limits=c(1,60), n.breaks=7) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(y=NULL, tag="B") +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        axis.title.x = element_text(size=9),
        axis.text=element_text(size=6.5),
        strip.background = element_blank(),
        strip.text.x = element_text(size=9),
        legend.title=element_text(vjust=.5, size=9),
        legend.text=element_text(vjust=1.5, size=6.5))
# Tiff
tiff(filename = paste0(path_f,"/Fig9.tiff"),
     width = 1850, height = 2400, units = "px", pointsize = 12,
     compression = c("lzw"), bg = "white", res = 300, , restoreConsole = TRUE,
     type = c("windows"), symbolfamily="default")
(p1/p2)
dev.off()
# PNG
png(filename = paste0(path_f,"/png/Fig9.png"),
    width = 1850, height = 2400, units = "px", pointsize = 12,
    bg = "white", res = 300, , restoreConsole = TRUE,
    type = c("windows"), symbolfamily="default")
(p1/p2)
dev.off()
rm(p1, p2)

##### Figure 10: CNS NRT Faculty Participants - Mentorships Impact on Research Skills #####
participant_ct <- 
  d_fac %>% 
  select(Year, Role, ResponseId) %>% 
  distinct() %>%
  group_by(Year, Role) %>% 
  dplyr::summarise(Participants = n())
# Annual counts of Item responses, by Role, and Likert Value
answer_ct <- 
  d_fac %>%
  select(Year,Variable,Value,Group,Item) %>%
  filter(Value!="Not Applicable") %>%
  group_by(Year, Item, Value) %>% 
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  complete(Item,Year,Value,fill=list(Count=0))
# Combine Annual Participant and Answer Counts, by Role
# Calculate Percentage of Answers for each Item and Likert Value
answer_ct <- 
  plyr::join(participant_ct, answer_ct, 
             by=c("Year")) %>%
  select(Year,Role,Item,Value,Participants,Count) %>%
  mutate(Percent=round(Count/Participants*100,2)) %>%
  filter(!is.na(Value)) %>%
  arrange(Item,Role,Year,Item,desc(Value))
answer_ct[answer_ct$Percent==0,]$Percent <- NA
# Generate Figure Parts
p1 <- 
  answer_ct %>%
  mutate(Year=factor(Year)) %>%
  mutate(Item=ordered(Item, levels=c("Research","Presentations",
                                     "Publications","Grants"))) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", drop=F) +
  facet_wrap(~Item) +
  scale_fill_steps(low="#E0F7F4", high="#009480",na.value = "White",
                   limits=c(1,100),n.breaks=6) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        axis.title.x = element_text(size=7),
        axis.title.y=element_blank(),
        axis.text=element_text(size=6),
        strip.background = element_blank(),
        strip.text.x = element_text(size=7),
        legend.title=element_text(vjust=.5, size=7),
        legend.text=element_text(vjust=1.5, size=6.5))
# Tiff
tiff(filename = paste0(path_f,"/Fig10.tiff"),
     width = 1200, height = 900, units = "px", pointsize = 12,
     compression = c("lzw"), bg = "white", res = 300, , restoreConsole = TRUE,
     type = c("windows"), symbolfamily="default")
  p1
dev.off()
# PNG
png(filename = paste0(path_f,"/png/Fig10.png"),
    width = 1200, height = 900, units = "px", pointsize = 12,
    bg = "white", res = 300, , restoreConsole = TRUE,
    type = c("windows"), symbolfamily="default")
  p1
dev.off()
rm(p1)

##### Figure 11: Mentorships Impact on CNS NRT Doctoral Fellows #####
# Fellows Participants - Mentorship Impact on Research Skills
participant_ct <- 
  d_std %>% 
  select(Year, Role, ResponseId) %>% 
  distinct() %>%
  group_by(Year, Role) %>% 
  dplyr::summarise(Participants = n())
# Annual counts of Item responses, by Role, and Likert Value
answer_ct <- 
  d_std %>%
  filter(Group == "Mentorship Impact") %>%
  select(Year,Variable,Value,Group,Item) %>%
  filter(Value!="Not Applicable") %>%
  mutate(Item == ordered(Item, 
                         levels=c("Grants","Collaboration",
                                  "Interdisciplinary","Publications",
                                  "Presentations","Research"))) %>%
  group_by(Year, Item, Value) %>% 
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  complete(Item,Year,Value,fill=list(Count=0))
# Combine Annual Participant and Answer Counts, by Role
# Calculate Percentage of Answers for each Item and Likert Value
answer_ct <- 
  plyr::join(participant_ct, answer_ct, 
             by=c("Year")) %>%
  select(Year,Role,Item,Value,Participants,Count) %>%
  mutate(Percent=round(Count/Participants*100,2)) %>%
  filter(!is.na(Value)) %>%
  arrange(Item,Role,Year,Item,desc(Value))
answer_ct[answer_ct$Percent==0,]$Percent <- NA
# Generate Figure
p1 <- 
  answer_ct %>%
  mutate(Year=factor(Year)) %>%
  mutate(Item=ordered(Item, levels=c("Research","Publications","Presentations",
                                     "Grants","Collaboration","Interdisciplinary"))) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", drop=F) +
  facet_wrap(~Item) +
  scale_fill_steps(low="#E0F7F4", high="#009480",na.value = "White",
                   limits=c(1,60), n.breaks=5) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        axis.title.x = element_text(size=7),
        axis.title.y=element_blank(),
        axis.text=element_text(size=6),
        strip.background = element_blank(),
        strip.text.x = element_text(size=7),
        legend.title=element_text(vjust=.5, size=7),
        legend.text=element_text(vjust=1.5, size=6.5))
# Tiff
tiff(filename = paste0(path_f,"/Fig11.tiff"),
     width = 1500, height = 900, units = "px", pointsize = 12,
     compression = c("lzw"), bg = "white", res = 300, , restoreConsole = TRUE,
     type = c("windows"), symbolfamily="default")
p1
dev.off()
# PNG
png(filename = paste0(path_f,"/png/Fig11.png"),
    width = 1500, height = 900, units = "px", pointsize = 12,
    bg = "white", res = 300, , restoreConsole = TRUE,
    type = c("windows"), symbolfamily="default")
p1
dev.off()
rm(p1)

##### Figure 12: Quality with Mentorship for CNS NRT Doctoral #####
# Fellows Participants - Mentorship Impact on Research Skills
participant_ct <- 
  d_std %>% 
  select(Year, Role, ResponseId) %>% 
  distinct() %>%
  group_by(Year, Role) %>% 
  dplyr::summarise(Participants = n())
# Annual counts of Item responses, by Role, and Likert Value
answer_ct <- 
  d_std %>%
  filter(Group == "Mentor Quality") %>%
  select(Year,Variable,Value,Group,Item) %>%
  filter(Value!="Not Applicable") %>%
  mutate(Item == ordered(Item, 
                         levels=c("Grants","Collaboration",
                                  "Interdisciplinary","Publications",
                                  "Presentations","Research"))) %>%
  group_by(Year, Item, Value) %>% 
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  complete(Item,Year,Value,fill=list(Count=0))
# Combine Annual Participant and Answer Counts, by Role
# Calculate Percentage of Answers for each Item and Likert Value
answer_ct <- 
  plyr::join(participant_ct, answer_ct, 
             by=c("Year")) %>%
  select(Year,Role,Item,Value,Participants,Count) %>%
  mutate(Percent=round(Count/Participants*100,2)) %>%
  filter(!is.na(Value)) %>%
  arrange(Item,Role,Year,Item,desc(Value))
answer_ct[answer_ct$Percent==0,]$Percent <- NA
# Generate Figure
p1 <- 
  answer_ct %>%
  mutate(Year=factor(Year)) %>%
  ggplot(aes(y=Value, x=Year)) +
  stat_bin_2d(aes(fill=Percent), color="#D3D3D3", drop=F) +
  facet_wrap(~Item) +
  scale_fill_steps(low="#E0F7F4", high="#009480",na.value = "White",
                   limits=c(1,60),n.breaks=7) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(y=NULL) +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
      axis.title.x = element_text(size=7),
      axis.title.y=element_blank(),
      axis.text=element_text(size=6),
      strip.background = element_blank(),
      strip.text.x = element_text(size=7),
      legend.title=element_text(vjust=.5, size=7),
      legend.text=element_text(vjust=1.5, size=6.5))
# Tiff
tiff(filename = paste0(path_f,"/Fig12.tiff"),
     width = 1775, height = 1200, units = "px", pointsize = 12,
     compression = c("lzw"), bg = "white", res = 300, , restoreConsole = TRUE,
     type = c("windows"), symbolfamily="default")
  p1
dev.off()
# PNG
png(filename = paste0(path_f,"/png/Fig12.png"),
    width = 1775, height = 1200, units = "px", pointsize = 12,
    bg = "white", res = 300, , restoreConsole = TRUE,
    type = c("windows"), symbolfamily="default")
  p1
dev.off()
rm(p1)

##### Figure 13: COVID Impact on CNS NRT Program ####
# Annual count of distinct participants for each year
participant_ct <- 
  d_all %>% 
  filter(Group=="COVID") %>%
  select(Year, Role, ResponseId) %>% 
  distinct() %>%
  group_by(Year,Role) %>% 
  dplyr::summarise(Participants = n())
# Annual counts of Item responses, by Likert Value
answer_ct <- 
  d_all %>%
  filter(Group=="COVID") %>%
  filter(Value!="Not Applicable") %>%
  group_by(Year, Item, Role, Value) %>% 
  dplyr::summarise(Count = n()) %>%
  ungroup() %>%
  complete(Item,Year,Value,Role, fill=list(Count=0)) %>%
  filter(Item=="COVID-19")
# Combine Annual Participant and Answer Counts
# Calculate Percentage of Answers for each Item and Likert Value
answer_ct <- 
  plyr::join(participant_ct, answer_ct, 
             by=c("Year","Role")) %>%
  select(Year,Item,Role,Value,Participants,Count) %>%
  mutate(Percent=round(Count/Participants*100,2)) %>%
  arrange(Item,Role,Year,Item,desc(Value))
answer_ct[answer_ct$Percent==0,]$Percent <- NA

# Generate Figure
p1 <-
  answer_ct %>%
  mutate(Year=factor(Year)) %>%
  ggplot(aes(y=Value, weight=Percent)) +
  geom_bar(fill ="#a90533") +
  facet_grid(Role~Year, switch = "y") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y=NULL,x="Percent") +
  theme(panel.grid.major.x = element_line(color="#e7e9eb"),
        panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(vjust = -10),
        axis.title.x = element_text(size=9),
        axis.text=element_text(size=6.5),
        strip.placement.y = "outside",
        strip.background = element_rect(fill="White"),
        strip.text.y.left = element_text(size=9),
        strip.text.x = element_text(size=9), 
        plot.margin = margin(,.4, , , "cm"))
# Tiff 
tiff(filename = paste0(path_f,"/Fig13.tiff"),
     width = 1200, height = 1050, units = "px", pointsize = 12,
     compression = c("lzw"), bg = "white", res = 300, , restoreConsole = TRUE,
     type = c("windows"), symbolfamily="default")
  p1
dev.off()
# PNG
png(filename = paste0(path_f,"/png/Fig13.png"),
    width = 1200, height = 1050, units = "px", pointsize = 12,
    bg = "white", res = 300, , restoreConsole = TRUE,
    type = c("windows"), symbolfamily="default")
  p1
dev.off()
rm(p1)

#### Appendices #### 
# CNS NRT Goal Achievement and Satisfaction 
# appendix_1 <- 
#   d_all %>% 
#   filter(Group!="COVID") %>%
#   filter(Value!="Not Applicable") %>%
#   group_by(Item,Role) %>% 
#   dplyr::summarise(Responses = n(),
#                    Median = round(median(as.numeric(Value)),1),
#                    Mean = round(mean(as.numeric(Value)),1),
#                    SD = round(sd(as.numeric(Value)),2))
# write.csv(tmp,paste0(path_si,"Appendix-CNS_NRT-Survey_2019-2023-AchieveProgramGoals.csv"),
#           row.names = FALSE)
# CNS NRT Covid Impact
# appendix_2 <-
#   d_all %>%
#   filter(Group=="COVID") %>%
#   filter(Value!="Not Applicable") %>%
#   group_by(Role,Year) %>%
#   dplyr::summarise(Responses = n(),
#                    Median = round(median(as.numeric(Value)),1),
#                    Mean = round(mean(as.numeric(Value)),1),
#                    SD = round(sd(as.numeric(Value)),2))
# write.csv(tmp,paste0(path_si,"Appendix-CNS_NRT-Survey_2019-2023-COVID.csv"),
#           row.names = FALSE)