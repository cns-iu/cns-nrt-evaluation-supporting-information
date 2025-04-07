#### Environment Setup #### 
library(tidyr)        # Tidyverse functions
library(magrittr)     # Piping
library(plyr)         # Data prep
library(dplyr)        # Date prep
library(reshape2)     # Date prep
library(igraph)       # Network graph creation

# Path setting
#getwd()
path <- ("./data/")
path_analysis <- ("./analysis/")

#### Load Author and Publication data ####
# Load data - publications to drop from analysis
pub_drop <- 
  read.csv(file=paste0(path,"publications/CNS_NRT-publications-pubid-drop_author_ct.csv"), 
           header = T, sep=",")

# Load data - Author Publication Year list data
pub_authors_long <-
  read.csv(file=paste0(path,"publications/CNS_NRT-publications-pubid_year_author-list.csv"),
           header = T, sep=",")

# Load data - Author List with stats - with matched and verified NRT groups
authors <- 
  read.csv(file=paste0(path,"publications/CNS_NRT-publications-author_names-match-verified.csv"), 
           header = T, sep=",")

#### Prepare Author Node List ####
##### Update Author Groups #####
authors$group <- factor(authors$group, 
                        levels=c("Faculty","Fellow",
                                 "Affiliate","Collaborator"), 
                        ordered=T)
authors$group <- revalue(authors$group, 
                         c("Faculty"="Faculty",
                           "Fellow"="Doctoral Fellow",
                           "Affiliate"="Affiliate Student",
                           "Collaborator"="Collaborator"))
##### Author label #####
# Label is determined by authors' group, year of first pub, and name.
authors <- 
  authors %>%
  arrange(group, first_year, name) %>%
  ddply(.(group), mutate, lab_id = seq_along(name)) %>%
  mutate(label = paste(group, lab_id)) %>%
  arrange(id) %>%
  select(-c(lab_id, match_name, match, verified))

##### Calculate author publication counts #####
# Annual and cumulative
author_pub_cts_long <- 
  pub_authors_long %>%
  dplyr::mutate(Weight = 1) %>%
  pivot_wider(id_cols = c(Author, Group), names_from = Year,
              values_from = Weight, values_fn=sum, values_fill = 0) %>%
  pivot_longer(cols=c(as.character(2018:2024)), names_to = "Year", 
               values_to="Ann_Pubs") %>%
  group_by(Author) %>%
  dplyr::mutate(Cum_Pubs = cumsum(Ann_Pubs)) %>%
  ungroup()

# Creates a pivot table with annual cumulative pub counts
author_pub_cts  <-
  author_pub_cts_long %>%
  pivot_wider(id_cols = c(Author, Group), names_from = Year, names_prefix = "Cum_Pubs_",
              values_from = c(Cum_Pubs),
              values_fn = sum, values_fill=0) %>%
  ungroup()
# Join authors' annual cumulative publication counts to author df.
names(authors) <- str_to_title(names(authors))
authors <- 
  left_join(authors,
            author_pub_cts[,c(1,3:ncol(author_pub_cts))],
            by=c("Name"="Author")) 

authors <- authors[,c(1,9,2:6,8,7,10:ncol(authors))]

##### Set author node list #####
# DF of nodes structured for in Graph creation
author_nodes <- authors[,c(1,3,2,6:ncol(authors))]

#### Edge List Preparation ####
years <- unique(pub_authors_long$Year)
tmp <- data.frame(From=as.character(),
                  To=as.character(),
                  Year=as.integer(),
                  Weight=as.integer())

# Loop is set up to calculate cumulative annual counts for
# author relationships
for(i in 1:length(years)){
  adj_matrix <- 
    pub_authors_long %>%
    # Filter author list by years (cumulative)
    filter(Id!=pub_drop$bibtexkey,
           Year<=years[i]) %>%
    # Group by author and pub to get count spread
    group_by(Author, Id) %>%
    tally() %>%
    spread(Id, n, drop=FALSE, fill=0)
  
  # Convert tibble to data frame
  adj_matrix <- as.data.frame(adj_matrix) 
  # Convert row names to author names
  row.names(adj_matrix) <- adj_matrix$Author
  # Remove author column from df
  adj_matrix <- adj_matrix[,-1]
  # Convert data frame to matrix
  adj_matrix <- as.matrix(adj_matrix) 
  # Multiply author-pub matrix to create adjacency matrix for authors.
  adj_matrix <- adj_matrix %*% t(adj_matrix)
  
  ## Convert adjacency to edge list (from to pairs)
  tmp2 <- 
    reshape2::melt(adj_matrix) %>%
    filter(value != 0) %>%
    plyr::rename(c("Var1" = "From", "Var2" = "To", "value"="Weight")) %>%
    ddply(.(From,To), summarise,
          Weight = sum(Weight)) %>%
    filter(From != To) %>%
    mutate(Year=years[i]) %>%
    arrange(desc(Weight)) %>%
    select(From, To, Year, Weight)
  
  ## Combine results to others
  tmp <- rbind(tmp,tmp2)
}

# Pivot results to a wide table of annual cum co-authorship weights
edges <- 
  tmp %>% 
  mutate(Year = factor(as.character(Year))) %>%
  pivot_wider(id_cols = c(From, To), names_from = Year, 
              names_prefix = "Weight_", values_from = Weight,
              values_fn = sum, values_fill = 0) %>%
  mutate(Weight = as.integer(Weight_2024))
rm(years, tmp, tmp2, i, adj_matrix)
edges2 <- edges[,c(1,2,ncol(edges))]

##### Create Co-Author Network Graph #####
## Generate network for combined network with all projects.
g <- graph_from_data_frame(d=edges2, directed=F,
                           vertices=author_nodes[,-c(1)])
# Remove parallel edge in graph
E(g)$multi <- count_multiple(g)
g <- simplify(g, edge.attr.comb=list(Weight = "min"))
# Create edge list df for overall graph
edges2 <- g %>% igraph::as_data_frame()
edges2 <- edges2[,c(1:3)]

##### Update Edge List #####
# This selects the preserved edges and edge data from graph simplification.
author_relationships <- 
  left_join(edges2,
            edges[,c(1:ncol(edges)-1)],
            by=c("from"="From","to"="To"))
names(author_relationships)[1:2] <- c("From","To")

# Adding author group to edges
author_relationships <-
  left_join(author_relationships, 
            author_nodes[,c(2,4)],
            by=c("From"="Name")) %>%
  plyr::rename(c("Group"="Source_Group"))
author_relationships <-
  left_join(author_relationships, 
            author_nodes[,c(2,4)],
            by=c("To"="Name")) %>%
  plyr::rename(c("Group"="Target_Group"))
author_relationships <-
  author_relationships %>%
  mutate(Group = paste0(Source_Group,"-",Target_Group))
author_relationships <- 
  author_relationships %>%
  select(-c(Source_Group,Target_Group)) %>%
  mutate(Group = factor(Group))

# Order relationship pair factors.
author_relationships$Group <- factor(author_relationships$Group)
author_relationships$Group <-
  revalue(author_relationships$Group,
          c("Doctoral Fellow-Faculty"="Faculty-Doctoral Fellow",
            "Affiliate Student-Faculty"="Faculty-Affiliate Student",
            "Collaborator-Faculty"="Faculty-Collaborator",
            "Affiliate Student-Doctoral Fellow"="Doctoral Fellow-Affiliate Student",
            "Collaborator-Doctoral Fellow"="Doctoral Fellow-Collaborator",
            "Collaborator-Affiliate Student"="Affiliate Student-Collaborator"))

# Reorganize edge list variables
author_relationships <- 
  author_relationships[,c(1:2,ncol(author_relationships),
                        3:(ncol(author_relationships)-1))] 

# Add source and target variables to edge list based on author node list
# from to are used when 'name' is node list ID.
# source and target are used when "id" is node list ID.
author_relationships <- 
  left_join(author_relationships,author_nodes[,c(2,1)],
            by=c("From"="Name")) %>%
  plyr::rename(c("Id"="Source"))
author_relationships <- 
  left_join(author_relationships,author_nodes[,c(2,1)],
            by=c("To"="Name")) %>%
  plyr::rename(c("Id"="Target"))
# Re order columns
author_relationships <- 
  author_relationships[,c((ncol(author_relationships)-1),
                          ncol(author_relationships),
                          1:(ncol(author_relationships)-2))]

#### Export network files ####
# Save author node list with publication stats
authors %>%
  select(-c(Name, Last_name, First_name)) %>%
  write.csv(file=paste0(path_analysis,"publications/author_analysis/CNS_NRT-publications-author_stats.csv"),
            row.names = FALSE, fileEncoding = "UTF-8") 
authors %>%
  select(-c(Name, Last_name, First_name)) %>%
  write.csv(file=paste0(path_analysis,"publications/network_analysis/CNS_NRT-publications-coauthorship_nodes.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

# Save final edge list
author_relationships %>%
  mutate(From =  as.integer(Source),
         To =  as.integer(Target)) %>% 
  write.csv(file=paste0(path_analysis,"publications/author_analysis/CNS_NRT-publications-author_relationships_stats.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
author_relationships %>%
  mutate(From =  as.integer(Source),
         To =  as.integer(Target)) %>% 
  write.csv(file=paste0(path_analysis,"publications/network_analysis/CNS_NRT-publications-coauthorship_edges.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

# Clean up environment
rm(edges, edges2, author_pub_cts)

#### Group Level Statistics ####
##### Group+Year~ Author Counts #####
# Group~Author Counts
group_author_cts <-
  authors %>%
  select(Name, Group) %>%
  ddply(.(Group), summarise, 
        Authors = length(Name))

# Save group~author counts for visualization
write.csv(group_author_cts,
          file=paste0(path_analysis,
                      "publications/group_analysis/CNS_NRT-publications-group_author_count.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Year+Group~Author Counts
group_author_annual_long <-
  author_pub_cts_long %>%
  filter(Ann_Pubs > 0) %>%
  ddply(.(Group, Year), summarise,
        Count = length(Author)) %>%
  mutate(Var="Authors")

# Save annual+group~author counts for visualization
write.csv(group_author_annual_long,
          file=paste0(path_analysis,
                      "publications/group_analysis/CNS_NRT-publications-group+year_author_count-long.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Group~Year->Author - Wide
group_author_annual_wide <-
  group_author_annual_long %>%
  pivot_wider(id_cols=Group, names_from = Year, names_prefix = "Authors_",
              values_from = Count,
              values_fill = 0, values_fn = sum )

# Matching factors to group_authors_cts data frame.
group_author_annual_wide$Group <- 
  factor(group_author_annual_wide$Group, 
         levels=c("Faculty","Fellow",
                  "Affiliate","Collaborator"), 
         ordered=T)
group_author_annual_wide$Group <- 
  revalue(group_author_annual_wide$Group, 
          c("Faculty"="Faculty",
            "Fellow"="Doctoral Fellow",
            "Affiliate"="Affiliate Student",
            "Collaborator"="Collaborator"))

# Combine overall and annual group author counts
group_author_annual_wide <- 
  left_join(group_author_cts, group_author_annual_wide, by="Group")

# Save combined overall and annual group author counts
write.csv(group_author_annual_wide,
          file=paste0(path_analysis,
                      "publications/group_analysis/CNS_NRT-publications-group+year_author_count-wide.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

##### Group+Year~Publication Count #####
# Calculated by subset pub_authors_long to select distinct group-pub_id-year data.
group_pubs_annual_long <- 
  pub_authors_long %>%
  select(Id,Year,Group) %>%
  distinct() %>%
  ddply(.(Group,Year),summarise,
        Count = length(Id)) %>%
  mutate(Var="Pubs")

# Save results
write.csv(group_pubs_annual_long, 
          file=paste0(path_analysis,"publications/group_analysis/CNS_NRT-publications-group+year_pub_count-long.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Convert annual group publication long to wide data with pivot
group_pubs_annual_wide <- 
  group_pubs_annual_long %>%
  pivot_wider(id_cols=Group, names_from=Year, names_prefix="Pubs_",
              values_from=Count, values_fn=sum, values_fill=0) %>%
  mutate(Pubs = as.integer(Pubs_2024)) 

# Add total pub count
group_pubs_annual_wide <-
  group_pubs_annual_wide[,c(1,ncol(group_pubs_annual_wide),
                            2:(ncol(group_pubs_annual_wide)-1))]
# Save results
write.csv(group_pubs_annual_wide, 
          file=paste0(path_analysis,"publications/group_analysis/CNS_NRT-publications-group+year_pub_count-wide.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
