#### Setup Environment ###
library(tidyr)        # Data preparation
library(plyr)         # Data preparation
library(dplyr)        # Data preparation
library(broom)        # Clean up summary tables

#### Set Paths ####
path_c <- paste0(getwd(),"/data/membership/")
path_t <- paste0(getwd(),"/tables/")
path_si <- paste0(getwd(),"/supporting_information/")

#### Load Data ####
# Program Demographics - Students
demo_s <- 
  read.csv(paste0(path_c,"CNS_NRT-DoctoralFellows-Demographics-2024.csv"), 
           header=T, encoding = "UTF8")
# Program Demographics - Faculty
demo_f <- 
  read.csv(paste0(path_c,"CNS_NRT-Faculty-Affiliations.csv"), 
           header=T, encoding = "UTF8")

# Factor demographic variables
demo_s$Admit.Term <- factor(demo_s$Admit.Term, 
                            levels=c("Fall 2015-2017","Fall 2018","Fall 2019",
                                     "Fall 2020","Spring 2020","Fall 2021"))
demo_s[is.na(demo_s$Ethnicity),]$Ethnicity <- "Not Provided"
demo_s$Ethnicity <- factor(demo_s$Ethnicity,
                           levels=c("Asian","Black/African American",
                                    "White","Not Provided"))
demo_s$Gender <- factor(demo_s$Gender, levels=c("F","M"), 
                        labels=c("Female","Male"))
demo_s$Aff.Lab <- as.factor(demo_s$Aff.Lab)
demo_s$Affiliation <- as.factor(demo_s$Affiliation)

#### Doctoral Fellow Demographics ####
##### Ethnicity #####
# Pivot table of counts of student ethnicity broken out by admission term
eth1 <- demo_s %>%
  select(Id, Ethnicity, Admit.Term) %>%
  group_by(Ethnicity, Admit.Term) %>% 
  dplyr::summarise(n=n()) %>%
  pivot_wider(names_from = Admit.Term, 
              values_from = n, values_fill=0) %>% 
  select(Ethnicity,'Fall 2015-2017','Fall 2018','Fall 2019',
         'Fall 2020',"Spring 2020","Fall 2021") 
eth1$Total <- rowSums(eth1[,2:ncol(eth1)])
eth1 <- eth1 %>% arrange(desc(Total))
eth2 <- as.data.frame(t(c("Total", colSums(eth1[,2:ncol(eth1)]))))
eth2[,2:ncol(eth2)] <- as.numeric(eth2[,2:ncol(eth2)])
names(eth2)[1]<- "Ethnicity"
eth1 <- rbind(eth1,eth2)
rm(eth2)
# Export Results
eth1 %>% 
  write.csv(file=paste0(path_t,"table_1-CNS_NRT-DoctoralFellow-EthnicityCounts.csv"),
            row.names = F)

##### Gender #####
# Pivot table of counts of student gender broken out by admission term
gen1 <- demo_s %>%
  select(Id, Gender, Admit.Term) %>%
  group_by(Gender, Admit.Term) %>% 
  dplyr::summarise(n=n()) %>%
  pivot_wider(names_from = Admit.Term, values_from = n, values_fill=0) %>% 
  select(Gender,'Fall 2015-2017','Fall 2018','Fall 2019',
         'Fall 2020',"Spring 2020","Fall 2021") 
gen1$Total <- rowSums(gen1[,2:ncol(gen1)])
gen1 <- gen1 %>% arrange(desc(Total))
gen2 <- as.data.frame(t(c("Total", colSums(gen1[,2:ncol(gen1)]))))
gen2[,2:ncol(gen2)] <- as.numeric(gen2[,2:ncol(gen2)])
names(gen2)[1]<- "Gender"
gen1 <- rbind(gen1,gen2)
rm(gen2)

# Export Results
gen1 %>% 
  write.csv(file=paste0(path_t,"table_2-CNS_NRT-DoctoralFellow-GenderCounts.csv"),
            row.names = F)

#### Education ####
##### Prior Education #####
# Pivot table of counts of student prior education
edu1 <- 
  demo_s %>%
  mutate(Role = "Students") %>%
  select(Role,BA,BS,MA,MS) %>% 
  ddply(.(Role),summarise,
        BA=sum(BA),
        BS=sum(BS),
        MA=sum(MA),
        MS=sum(MS),
        Total=length(Role)) %>%
  t() %>%
  data.frame()

# Clean up dataframe
names(edu1) <- "Students"
edu1$Degree <- row.names(edu1)
edu1 <- edu1[-1,c(2,1)]
row.names(edu1) <- 1:nrow(edu1)    

# Calculate percentage 
edu1 <-
  edu1 %>%
  mutate(Students = as.numeric(Students))%>%
  mutate(Percent = round((Students/max(Students)*100),1))

# Export results
edu1 %>% 
  write.csv(file=paste0(path_t,"table_3-CNS_NRT-DoctoralFellow-PriorEducation.csv"),
            row.names = F)

##### Secondary Doctoral Degree #####
# Pivot table of counts of second doctoral degree program
edu2 <- 
  demo_s %>%
  select(Affiliation, Aff.Lab) %>%
  mutate(Affiliation = paste0(Affiliation," (",Aff.Lab,")")) %>%
  group_by(Affiliation) %>% 
  dplyr::summarise(Count=n()) %>%
  arrange(desc(Count))

# Export results
edu2 %>% 
  write.csv(file=paste0(path_t,"table_4-CNS_NRT-DoctoralFellow-SecondProgram.csv"),
            row.names = F)

#### CNS NRT Faculty Academic Disciplines ####
# # Pivot table of counts of faculty discipline
#fac1 <- 
#  demo_f %>%
#  select(Affiliation, Aff_Abbr) %>%
#  mutate("Affiliation" = paste0(Affiliation," (",Aff_Abbr,")")) %>%
#  group_by(Affiliation) %>% 
#  dplyr::summarise(Count=n()) %>%
#  arrange(desc(Count)) 
#
# # Export results
#fac1 %>% 
#  write.csv(file=paste0(path_si,
#            "S1_Dataset_CNS_NRT_core_faculty_data/S1B_Table_CNS_NRT_department_affiliation_counts.csv"),
#            row.names = F)
