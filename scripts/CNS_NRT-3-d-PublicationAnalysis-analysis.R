#### Setup Environment #### 
#rm(list=ls())

library(tidyr)        # Tidyverse functions
library(magrittr)     # Piping
library(plyr)         # Data prep
library(dplyr)        # Date prep
library(igraph)       # Network graph creation
library(tidygraph)    # Tidyverse graph data structure
library(ggplot2)      # Tidyverse visualization base
library(ggraph)       # Graph visualization
library(graphlayouts) # Graph layouts for visualization
library(cowplot)      # Graph layouts for multiple plots
library(grid)         # Graph layouts for visualization
library(gridExtra)    # Graph layouts for visualization

#### Set Paths ####
path_p <- paste0(getwd(),"/data/publications/")
path_m <- paste0(getwd(),"/data/membership/")
path_a <- paste0(getwd(),"/analysis/")
path_t <- paste0(getwd(),"/tables/")
path_f <- paste0(getwd(),"/figures/")
path_si <- paste0(getwd(),"/supporting_information/")

#### Set Figure theme ####
theme_nrt <- theme_grey()
theme_set(theme_nrt)
theme_update(
  plot.background=element_rect(fill="White"),
  plot.caption.position="plot",
  panel.grid.major.x=element_blank(),
  panel.grid.major.y=element_blank(),
  panel.grid.minor.x=element_blank(),
  panel.grid.minor.y=element_blank(),
  panel.background=element_rect(fill="White"),
  panel.border=element_rect(fill=NA, 
                              color="black",
                              linetype="solid"))
#### Load Data ####
##### Publication Data #####
# Publications
data <- read.csv(file=paste0(path_p,
                             "CNS_NRT-2024.08.14-publications_wos_gs-prepared.csv"), 
                 header=T, sep=",") 
# Author List 
author_pub_list <- 
  read.csv(file=paste0(path_p,
                       "CNS_NRT-publications-pubid_year_author-list.csv"),
           header=T, sep=",")

##### CNS NRT Role Descriptive Statistics ####
# CNS NRT Membership Group 
cns_group_members <- 
  read.csv(file=paste0(path_m,
                       "CNS_NRT-Membership-2024.csv"),
           header=T, sep=",")[c(1,2)]
#Annual Author and Publication Counts
role_pub_ct <- 
  read.csv(file=paste0(path_a,"publications/group_analysis/",
                       "CNS_NRT-publications-group+year_pub_count-long.csv"), 
           header=T, sep=",")
role_authors_ct <- 
  read.csv(file=paste0(path_a,"publications/group_analysis/",
                       "CNS_NRT-publications-group+year_author_count-long.csv"), 
           header=T, sep=",")
role_counts <- rbind(role_pub_ct,role_authors_ct)
rm(role_pub_ct,role_authors_ct)

##### Network Data #####
# Node List
nodes <-
  read.csv(file=paste0(path_a,"publications/network_analysis/",
                       "CNS_NRT-publications-coauthorship_nodes.csv"),
           header=T, sep=",")
# Edge List
edges <- 
  read.csv(file=paste0(path_a,"publications/network_analysis/",
                       "CNS_NRT-publications-coauthorship_edges.csv"),
           header=T, sep=",")
# Node and edge coordinates
coordinates <-
  read.csv(file=paste0(path_a,"publications/network_analysis/",
                       "CNS_NRT-publications-coauthorship_coordinates.csv"),
           header=T, sep=",")

#### Common factor variables ####
# Factor scales
role <- c("Faculty","Doctoral Fellow","Affiliate Student","Collaborator")

#### Data Processing #####
##### Publication Data #####
# Field names 
names(data) <- c("Id","Year","Title","Authors","Authors-Prepared","Editor",
                 "Journal","Proceeding_Title","Volume","Number","ISSN","ISBN",
                 "DOI","WOS_Id","Keywords","Type","Category")
# Arrange data frames by Year
data <- 
  data %>%
  plyr::arrange(Year)
# Clean up Publication Type variable
data[data$Type=="Book Section",]$Type <- "Book"
data[data$Type=="Conference Proceedings",]$Type <- "Conference\nProceedings"

##### CNS NRT Group Membership ####
cns_group_members <- 
  cns_group_members %>% 
  filter(Group!=c("Administrator")) %>%
  mutate(Group=revalue(Group, 
                         c("Faculty"="Faculty",
                           "Fellow"="Doctoral Fellow",
                           "Affiliate"="Affiliate Student"))) %>%
  mutate(Group=ordered(Group, levels=role)) %>%
  group_by(Group) %>%
  dplyr::summarise(Count=n_distinct(unique(Id))) %>%
  complete(Group, fill=list(Count=NA)) 

##### Author Publication List #####
# Arrange author publication list by year, id and author
author_pub_list <- 
  author_pub_list %>% 
  plyr::arrange(Year,Id,Author)

author_pub_list <- 
  author_pub_list %>%
  mutate(Group=revalue(Group, 
                         c("Faculty"="Faculty",
                           "Fellow"="Doctoral Fellow",
                           "Affiliate"="Affiliate Student",
                           "Collaborator"="Collaborator"))) %>%
  mutate(Group=ordered(Group, levels=role))

##### Role Count Statistics #####
role_counts <-
  role_counts %>%
  mutate(Group=revalue(Group, 
                         c("Faculty"="Faculty",
                           "Fellow"="Doctoral Fellow",
                           "Affiliate"="Affiliate Student",
                           "Collaborator"="Collaborator"))) %>%
  mutate(Group=ordered(Group, levels=role))
role_counts$Var <- factor(role_counts$Var)
role_counts <-
  role_counts %>%
  group_by(Group, Var) %>%
  mutate(Cum_Sum=cumsum(Count)) %>%
  select(Var, Group, Year, Count, Cum_Sum)

##### Co-authorship Network #####
# 2018-2024 - All years
nodes$Group <- 
  ordered(nodes$Group, levels=rev(role))
# Update co-author node label set
nodes$Label2 <- ""
nodes[nodes$Pubs>=4,]$Label2 <- 
  nodes[nodes$Pubs>=4,]$Label
# Factor edge categories
edges$Group <-
  factor(edges$Group, 
         levels=c("Faculty-Faculty",
                  "Faculty-Doctoral Fellow",
                  "Faculty-Affiliate Student",
                  "Faculty-Collaborator",
                  "Doctoral Fellow-Doctoral Fellow",
                  "Doctoral Fellow-Affiliate Student",
                  "Doctoral Fellow-Collaborator",
                  "Affiliate Student-Affiliate Student",
                  "Affiliate Student-Collaborator",
                  "Collaborator-Collaborator"))
# Generate igraph network data
net <- igraph::graph_from_data_frame(d=edges, vertices=nodes, directed=F) %>% 
  as_tbl_graph()
# Component size
comp <- as.data.frame(components(net, mode = c("weak"))$csize)
names(comp) <- "c"

# 2018-2021 - First half of NRT
edges_2021 <- 
  edges %>%
  filter(Weight_2021>0) %>%
  mutate(Weight = as.integer(Weight_2021)) %>%
  select(From,To,Group,Weight)
nodes_2021 <- 
  nodes %>%
  filter(Cum_Pubs_2021>0) %>%
  mutate(Pubs = Cum_Pubs_2021) %>%
  select(Id, Label, Label2, Group, Cohort, First_year, Pubs)
coords_2021 <- coordinates[coordinates$Id %in% nodes_2021$Id,]
row.names(coords_2021) <- 1: nrow(coords_2021)
# Save 2018-2021 subgraph
write.csv(edges_2021, 
          file = paste0(path_a,"/publications/network_analysis/",
                        "CNS_NRT-publications-coauthorship_edges_2018-2021.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)
write.csv(nodes_2021, 
          file = paste0(path_a,"publications/network_analysis/",
                        "CNS_NRT-publications-coauthorship_nodes_2018-2021.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)
write.csv(coords_2021,
          file = paste0(path_a,"publications/network_analysis/",
                        "CNS_NRT-publications-coauthorship_coordinates_2018-2021.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)
# Create network
net2 <- 
  igraph::graph_from_data_frame(d=edges_2021, vertices=nodes_2021, directed=F) %>% 
  as_tbl_graph()
# Network Density calculations
edge_density(igraph::graph_from_data_frame(d=edges_2021, vertices=nodes_2021, directed=F))
edge_density(igraph::graph_from_data_frame(d=edges, vertices=nodes, directed=F))
# Component size
comp2 <- as.data.frame(components(net2, mode = c("weak"))$csize)
names(comp2) <- "c"

#### Create Tables and Figures ####
##### Tables #####
###### Table 12: CNS NRT Group Publications Stats, 2018-2024 #####
# Descriptive counts - authors and associated paper counts
auth_t1 <- 
  author_pub_list %>% 
  group_by(Group) %>% 
  dplyr::summarise(
    Publications=n_distinct(unique(Id)),
    Authors=n_distinct(unique(Author))
  ) 
# Calculates Percent of NRT Members (by Role) with publications - 2018-2024
auth_t1 <- 
  join(auth_t1,cns_group_members, by="Group") %>%
  mutate(Percent=round(Authors/Count*100,1)) %>%
  select(Group, Count, Authors, Percent, Publications) %>%
  plyr::rename(c("Group"="NRT Role",
                 "Count"="Membership"))
names(auth_t1)[4] <- "Percent with Publications"
# Save results
write.csv(auth_t1, 
          file=paste0(path_t,"Table12-CNS_NRT-Publications_2018-2024-Group_Stats.csv"),
          fileEncoding="UTF-8", row.names=FALSE)

###### Table 14: CNS NRT Count Co-authorship Relationships, by Role #####
# Count Relationship (edges) by Group
rels <- edges[,c(1:2,5:6,10)]
rels$Group2<- NA
rels[grepl("Faculty",rels$Group)==T,]$Group2 <- "Faculty"
# Faculty
fac <- 
  rels %>%
  filter(Group2=="Faculty") %>%
  mutate(Weight = ifelse(Weight >= 1, 1, 0),
         Weight_2021 = ifelse(Weight_2021 >= 1, 1, 0),) %>%
  ddply(.(Group2), summarise, 
        count_21 = sum(Weight_2021),
        count_24 = sum(Weight))
rels[grepl("Doctoral",rels$Group)==T,]$Group2 <- "Doctoral Fellow"
# Fellows
doc <- 
  rels %>%
  filter(Group2=="Doctoral Fellow") %>%
  mutate(Weight = ifelse(Weight >= 1, 1, 0),
         Weight_2021 = ifelse(Weight_2021 >= 1, 1, 0),) %>%
  ddply(.(Group2), summarise, 
        count_21 = sum(Weight_2021),
        count_24 = sum(Weight))
rels[grepl("Affiliate",rels$Group)==T,]$Group2 <- "Affiliate"
# Affiliaties
aff <- 
  rels %>%
  filter(Group2=="Affiliate") %>%
  mutate(Weight = ifelse(Weight >= 1, 1, 0),
         Weight_2021 = ifelse(Weight_2021 >= 1, 1, 0),) %>%
  ddply(.(Group2), summarise, 
        count_21 = sum(Weight_2021),
        count_24 = sum(Weight))
rels[grepl("Collaborator",rels$Group)==T,]$Group2 <- "Collaborator"
# Collaborators
coll <- 
  rels %>%
  filter(Group2=="Collaborator") %>%
  mutate(Weight = ifelse(Weight >= 1, 1, 0),
         Weight_2021 = ifelse(Weight_2021 >= 1, 1, 0),) %>%
  ddply(.(Group2), summarise, 
        count_21 = sum(Weight_2021),
        count_24 = sum(Weight))
rels <- rbind(fac,doc,aff,coll)
rm(fac,doc,aff,coll)
names(rels) <- c("Group","Relationships_21","Relationships_24")
rels$Percent_21 <- round(rels$Relationships_21/466*100,2)
rels$Percent_24 <- round(rels$Relationships_24/806*100,2)
rels$Growth <- round((rels$Relationships_24-rels$Relationships_21)/rels$Relationships_21*100,2)
rels <- rels[,c(1,3,5,2,4,6)]
# Save results
write.csv(rels,
          file=paste0(path_t,"Table14-CNS_NRT-Publications-Coauthorship_Relationships_CT.csv"),
          fileEncoding="UTF-8", row.names=FALSE)

###### Table 15: Count of Co-authorship Relationship Pairs #####
# Count relationship pairs of NRT Roles
# 2018-2024
rel_pairs <- 
  edges %>%
  select(Group, Weight) %>%
  ddply(.(Group), summarize,
        c2024 = length(Weight),
        p2024 = round(length(Weight)/806*100,2))
# 2018-2021
rel_pairs2 <- 
  edges_2021 %>%
  select(Group, Weight) %>%
  ddply(.(Group), summarise,
        c2021 = length(Weight),
        p2021 = round(length(Weight)/466*100,2))
# Join relationship type calculation tables by group
rel_pairs <- join(rel_pairs2, rel_pairs, by="Group")
# Change Growth
rel_pairs$change <- round(((rel_pairs$c2024 - rel_pairs$c2021)/rel_pairs$c2021)*100,2)
names(rel_pairs) <- c("Co-Authorship Relationship Type","Count (2021)","Percent (2021)",
                      "Count (2024)","Percent (2024)","Growth Rate")
# Save results
write.csv(rel_pairs,
          file=paste0(path_t,"Table15-CNS_NRT-Publications-Coauthorship_RelationshipPairs_CT.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)

##### Figures #####
###### Figure 14: Annual Publications, by Type #####
p1 <-
  data %>% 
  select(Year, Type, Id) %>%
  mutate(Year=factor(Year)) %>%
  ggplot(aes(x=Year)) +
  geom_bar(aes(fill=Type)) +
  labs(y="Count", x="Year") +
  scale_fill_manual(values=c("#fec553","#009480","#005b94")) +
  scale_y_continuous(limits=c(0,21), 
                     breaks=seq(0,20,5), 
                     expand=c(0, 0)) +
  guides(fill=guide_legend(title="Publication\nType")) +          
  theme(axis.title=element_text(size=8),
        axis.text=element_text(size=6.5),
        legend.title=element_text(vjust=.5, size=8),
        legend.text=element_text(size=6.5),
        legend.position="right",
        legend.justification="left",
        legend.text.position="right")
# Tiff 
tiff(filename=paste0(path_f,"/Fig14.tiff"),
     width=1400, height=800, units="px", pointsize=12,
     compression=c("lzw"), bg="white", res=300, , restoreConsole=TRUE,
     type=c("windows"), symbolfamily="default")
  p1
dev.off()
# PNG
png(filename=paste0(path_f,"/png/Fig14.png"),
    width=1400, height=800, units="px", pointsize=12,
    bg="white", res=300, , restoreConsole=TRUE,
    type=c("windows"), symbolfamily="default")
  p1
dev.off()
rm(p1)

###### Figure 15: Annual Publication Counts by Role ####
p1 <- 
  role_counts %>% 
  filter(Var=="Authors",
         Group != "Collaborator") %>%
  ggplot(aes(x=Year, color=Group)) +
  geom_line(aes(y=Count), linewidth=1) +
  geom_point(aes(y=Count), size=3) +
  scale_x_continuous(breaks=seq(min(role_counts$Year),
                                max(role_counts$Year), 1)) +
  scale_y_continuous(limits=c(0,17), 
                     breaks=seq(0,15,3), 
                     expand=c(0,0)) +
  scale_color_manual(values=c("#009480","#005b94",
                              "#fec553")) +
  labs(x="Year", y="Authors (Count)", 
       colour="NRT Role", tag="A") +
  theme(axis.title=element_text(size=8),
        axis.text=element_text(size=6),
        legend.title=element_text(vjust=.5, size=8),
        legend.text=element_text(size=6.5),
        legend.position="bottom")
# Annual count of publications 
p2 <- role_counts %>% 
  filter(Var=="Pubs",
         Group != "Collaborator") %>%
  ggplot(aes(x=Year, color=Group)) +
  geom_line(aes(y=Count), linewidth=1) +
  geom_point(aes(y=Count), size=3) +
  scale_x_continuous(breaks=seq(min(role_counts$Year),
                                max(role_counts$Year), 1)) +
  scale_y_continuous(limits=c(0,17), 
                     breaks=seq(0,15,3),
                     expand=c(0,0)) +
  scale_color_manual(values=c("#009480","#005b94",
                              "#fec553")) +
  labs(x="Year", y="Pubs. (Count)", tag="B") +
  guides(color=guide_legend(title="NRT Group")) + 
  theme(axis.title=element_text(size=8),
        axis.text=element_text(size=6),
        legend.position="none")
# Combine Figure elements
legend <- cowplot::get_plot_component(p1, 'guide-box-bottom', return_all=TRUE)
p_grid <- cowplot::plot_grid(plotlist=list(p1 + theme(legend.position="none"),
                                             p2), ncol=2)
p3 <- cowplot::plot_grid(p_grid, legend, nrow=2, rel_heights=c(5,.5))
# Tiff 
tiff(filename=paste0(path_f,"/Fig15.tiff"),
     width=1800, height=800, units="px", pointsize=12,
     compression=c("lzw"), bg="white", res=300, , restoreConsole=TRUE,
     type=c("windows"), symbolfamily="default")
  p3
dev.off()
# PNG
png(filename=paste0(path_f,"/png/Fig15.png"),
    width=1800, height=800, units="px", pointsize=12,
    bg="white", res=300, , restoreConsole=TRUE,
    type=c("windows"), symbolfamily="default")
  p3
dev.off()
rm(p1,p2,p3)

###### Figure 16: Co-authorship network visualization 2019-2021 ####
# Co-authorship network visualization 2019-2024
net2 <- 
  ggraph(net2, layout=coords_2021) +
  geom_edge_link(aes(edge_colour=Weight),
                 lineend = "round",
                 linejoin = "bevel",
                 linemitre = 2,
                 start_cap=circle(radius=2,"mm"),
                 end_cap=circle(radius=2,"mm"),
                 alpha=.9,
                 width=.75,
                 check_overlap=T) +
  geom_node_point(aes(shape=Group,
                      fill=Group,
                      size=Pubs),
                  color=alpha("#493828",.5),
                  alpha=.825) + 
  geom_node_text(aes(label=Label2),
                 size=3.5,
                 repel=T,
                 check_overlap=T) +
  scale_edge_color_continuous(name="Collaborations",
                              low="#c9c5c3", high="#5c3354", 
                              breaks=c(1,3,6)) +
  scale_fill_manual(name="NRT Group",
                    values=c("Faculty"="#005b94",
                             "Doctoral Fellow"="#fec553",
                             "Affiliate Student"="#a90533",
                             "Collaborator"="#009480"),
                    breaks=role) +
  scale_shape_manual(name="NRT Group",
                     values=c("Faculty"=21,
                              "Doctoral Fellow"=24,
                              "Affiliate Student"=25,
                              "Collaborator"=22),
                     breaks=role) +
  scale_size_continuous(name="Pub. Count",
                        limit=c(1,10),
                        range=c(2,6),
                        breaks=c(1,5,10))  +
  guides(fill=guide_legend(order=1,
                           override.aes = list(size = 4)),
         shape=guide_legend(order=1,
                            override.aes = list(size = 4)),
         size=guide_legend(order=2),
         edge_colour=guide_legend(order=3)) +
  theme_graph(base_family = 'Helvetica') +
  theme(legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.justification = "center")
# Tiff 
tiff(filename=paste0(path_f,"/Fig16.tiff"),
     width=2250, height=2250, units="px", pointsize=10,
     compression=c("lzw"), bg="white", res=300, , restoreConsole=TRUE,
     type=c("windows"), symbolfamily="default")
net2
dev.off()
# PNG
png(filename=paste0(path_f,"/png/Fig16.png"),
    width=2250, height=2250, units="px", pointsize=12,
    bg="white", res=300, , restoreConsole=TRUE,
    type=c("windows"), symbolfamily="default")
net2
dev.off()

###### Figure 17: Co-authorship network visualization 2019-2024 ####
net1 <- 
  ggraph(net, layout=coordinates) +
  geom_edge_link(aes(edge_colour=Weight),
                 lineend = "round",
                 linejoin = "bevel",
                 linemitre = 2,
                 start_cap=circle(radius=2,"mm"),
                 end_cap=circle(radius=2,"mm"),
                 alpha=.9,
                 width=.75,
                 check_overlap=T)+
  geom_node_point(aes(shape=Group,
                      fill=Group,
                      size=Pubs),
                  color=alpha("#493828",.5),
                  alpha=.825) + 
  geom_node_text(aes(label=Label2),
                 size=3.5,
                 repel=T,
                 check_overlap=T) +
  scale_edge_color_continuous(name="Collaborations",
                              low="#c9c5c3", high="#5c3354", 
                              breaks=c(1,3,6,9)) +
  scale_fill_manual(name="NRT Group", 
                    values=c("Faculty"="#005b94",
                             "Doctoral Fellow"="#fec553",
                             "Affiliate Student"="#a90533",
                             "Collaborator"="#009480"),
                    breaks=role) +
  scale_shape_manual(name="NRT Group",
                     values=c("Faculty"=21,
                              "Doctoral Fellow"=24,
                              "Affiliate Student"=25,
                              "Collaborator"=22),
                     breaks=role) +
  scale_size_continuous(name="Pub. Count",
                        limit=c(1,21),
                        range=c(2,9),
                        breaks=c(1,5,15)) +
  guides(fill=guide_legend(order=1,
                           override.aes = list(size = 4)),
         shape=guide_legend(order=1,
                            override.aes = list(size = 4)),
         size=guide_legend(order=2),
         edge_colour=guide_legend(order=3)) +
  theme_graph(base_family = 'Helvetica') +
  theme(legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.justification = "center")
# Tiff 
tiff(filename=paste0(path_f,"/Fig17.tiff"),
     width=2250, height=2250, units="px", pointsize=10,
     compression=c("lzw"), bg="white", res=300, , restoreConsole=TRUE,
     type=c("windows"), symbolfamily="default")
  net1
dev.off()
# PNG
png(filename=paste0(path_f,"/png/Fig17.png"),
    width=2250, height=2250, units="px", pointsize=12,
    bg="white", res=300, , restoreConsole=TRUE,
    type=c("windows"), symbolfamily="default")
  net1
dev.off()

#### Appendices S5 - not used ####
# # Calculates Percent of NRT Members (by Role) with publications - 2018-2021
# auth_t2 <- 
#   author_pub_list %>% 
#   filter(Year <= 2021) %>%
#   group_by(Group) %>% 
#   dplyr::summarise(
#     Publications=n_distinct(unique(Id)),
#     Authors=n_distinct(unique(Author))
#   ) 
# auth_t2 <- 
#   join(auth_t2,cns_group_members,by="Group") %>%
#   mutate(Percent=round(Authors/Count*100,1)) %>%
#   select(Group, Count, Authors, Percent, Publications) %>%
#   plyr::rename(c("Group"="NRT Role",
#                  "Count"="Membership"))
# names(auth_t2)[4] <- "Percent with Publications"
# # Save results
# write.csv(auth_t2, 
#           file=paste0(path_si,"Appendix_S5/CNS_NRT-Publications_2018-2021-Group_Stats.csv"),
#           fileEncoding="UTF8", row.names=FALSE)
#
# # Calculates Change in NRT Members (by Role) with publications for 2018-2021 
# # and 2018 - 2024 - count and percents of authors
# auth_t3 <- join(auth_t1, auth_t2[,c(1,3,4,5)], by=c("NRT Role"))
# names(auth_t3)[3:8] <- c("Authors_24","Percent_24","Publications_24",
#                          "Authors_21","Percent_21","Publications_21")
# auth_t3$Authors_Change <- auth_t3$Authors_24-auth_t3$Authors_21 
# auth_t3$Percent_Change <- auth_t3$Percent_24-auth_t3$Percent_21 
# auth_t3$Publication_Change <- auth_t3$Publications_24-auth_t3$Publications_21 
# auth_t3 <- 
#   auth_t3[,c(1,2,6,3,9,7,4,10,8,5,11)]
# # Save results
# write.csv(auth_t3,
#           file=paste0(path_si,"Appendix_S5/CNS_NRT-Publications_2018-2024-Group+Change_Stats.csv"),
#           fileEncoding="UTF-8", row.names=FALSE)
#
# # Calculate the number of publications by journal title.
# jour <- 
#   data %>% 
#   select(Id, Journal) %>%
#   filter(Journal !="") %>%
#   group_by(Journal) %>%
#   dplyr::summarize(Publications=length(Id)) %>%
#   dplyr::rename("Venue"=Journal) %>%
#   arrange(desc(Publications))
# write.csv(jour,
#           file=paste0(path_si,"Appendix_S6-CNS_NRT-Journals_Publication_CT.csv"),
#           fileEncoding = "UTF-8", row.names = FALSE)
