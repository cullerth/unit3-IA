## 1. PREPARE
library(readxl)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(statnet)

# Background: 
#   - About the data
#   - Data collection
#
# Summary of some previous analyses:
#   
# Research Questions: 
#   - 
# 
# Libraries used: 
#   - 
  
## 2. WRANGLE

# Data Import: 
leader_nodes <- read_excel("data/School Leaders Data Chapter 9_e.xlsx", 
                    col_types = c("text", "numeric", "numeric", "numeric", "numeric"))

leader_matrix <- read_excel("data/School Leaders Data Chapter 9_d.xlsx", 
                            col_names = FALSE)

leader_matrix_1 <- read_excel("data/School Leaders Data Chapter 9_c.xlsx", 
                              col_names = FALSE)

# Dichotomize the matrix: 

#yr 3
leader_matrix <- leader_matrix %>%
  as.matrix()

class(leader_matrix)

leader_matrix[leader_matrix <= 2] <- 0

leader_matrix[leader_matrix >= 3] <- 1

rownames(leader_matrix) <- leader_nodes$ID
colnames(leader_matrix) <- leader_nodes$ID

#yr 1
leader_matrix_1 <- leader_matrix_1 %>%
  as.matrix()

class(leader_matrix_1)

leader_matrix_1[leader_matrix_1 <= 2] <- 0

leader_matrix_1[leader_matrix_1 >= 3] <- 1

rownames(leader_matrix_1) <- leader_nodes$ID
colnames(leader_matrix_1) <- leader_nodes$ID


# Create graph with attributes 

# yr 3
adjacency_matrix <- graph.adjacency(leader_matrix,
                                    diag = FALSE)

class(adjacency_matrix)


leader_edges <- get.data.frame(adjacency_matrix)

leader_edges

leader_graph <- tbl_graph(edges = leader_edges,
                          nodes = leader_nodes,
                          directed = TRUE)
leader_graph

# yr 1
adjacency_matrix_1 <- graph.adjacency(leader_matrix_1,
                                    diag = FALSE)

class(adjacency_matrix_1)


leader_edges_1 <- get.data.frame(adjacency_matrix_1)

leader_edges_1

leader_graph_1 <- tbl_graph(edges = leader_edges_1,
                          nodes = leader_nodes,
                          directed = TRUE)
leader_graph_1

## 3. EXPLORE

# Examine basic descriptives

# in vs out degree

# yr 3
leader_measures <- leader_graph %>%
  activate(nodes) %>%
  mutate(in_degree = centrality_degree(mode = "in")) %>%
  mutate(out_degree = centrality_degree(mode = "out"))

leader_measures

# yr 1
leader_measures_1 <- leader_graph_1 %>%
  activate(nodes) %>%
  mutate(in_degree = centrality_degree(mode = "in")) %>%
  mutate(out_degree = centrality_degree(mode = "out"))

leader_measures_1

# idk what this is something about looking at nodes individually

# yr 3
node_measures <- leader_measures %>% 
  activate(nodes) %>%
  data.frame()

summary(node_measures)

# yr 1
node_measures_1 <- leader_measures_1 %>% 
  activate(nodes) %>%
  data.frame()

summary(node_measures_1)

# basic stats

# yr 3
node_measures %>%
  group_by(DISTRICT.SITE) %>%
  summarise(n = n(),
            mean = mean(in_degree), 
            sd = sd(in_degree)
  )

# yr 1
node_measures_1 %>%
  group_by(DISTRICT.SITE) %>%
  summarise(n = n(),
            mean = mean(in_degree), 
            sd = sd(in_degree)
  )

# Create a sociogram

# yr 3
ggraph(leader_measures, layout = "kk") + 
  geom_node_point(aes()) +
  geom_node_text(aes(label = ID), 
                 repel=TRUE) +
  geom_edge_link() + 
  geom_edge_fan() +
  theme_graph()

# yr 1
ggraph(leader_measures_1, layout = "kk") + 
  geom_node_point(aes()) +
  geom_node_text(aes(label = ID), 
                 repel=TRUE) +
  geom_edge_link() + 
  geom_edge_fan() +
  theme_graph()

## 4. MODEL

# explain ERGMs

# loading network data: 

# yr 3
leader_network <- as.network(leader_edges,
                             vertices = leader_nodes)

leader_network

class(leader_network)

# yr 1
leader_network_1 <- as.network(leader_edges,
                             vertices = leader_nodes)

leader_network_1

class(leader_network_1)

# network structure parameters

# yr 3
summary(leader_network ~ edges + mutual)

set.seed(589)

ergm_mod_1 <-ergm(leader_network ~ edges + mutual)

summary(ergm_mod_1)

summary(leader_network ~ edges + 
          mutual +
          transitive +
          gwesp(0.25, fixed=T))

ergm_mod_2 <-ergm(leader_network ~ edges + 
                    mutual +
                    gwesp(0.25, fixed=T))
summary(ergm_mod_2)

# yr 1
summary(leader_network_1 ~ edges + mutual)

set.seed(589)

ergm_mod_1_1 <-ergm(leader_network_1 ~ edges + mutual)

summary(ergm_mod_1_1)

summary(leader_network_1 ~ edges + 
          mutual +
          transitive +
          gwesp(0.25, fixed=T))

ergm_mod_2_1 <-ergm(leader_network_1 ~ edges + 
                    mutual +
                    gwesp(0.25, fixed=T))
summary(ergm_mod_2_1)

# actor structure parameters

# yr 3
ergm_3 <- ergm(leader_network ~ edges +
                 mutual +
                 gwesp(0.25, fixed=T) +
                 nodefactor('MALE') +
                 nodecov('EFFICACY')
)


summary(ergm_3)

# yr 1
ergm_3_1 <- ergm(leader_network_1 ~ edges +
                 mutual +
                 gwesp(0.25, fixed=T) +
                 nodefactor('MALE') +
                 nodecov('EFFICACY')
)


summary(ergm_3_1)

# check model fit 
# GOF function

# yr 3
ergm_3_gof <- gof(ergm_3)

plot(ergm_3_gof)

# yr 1
ergm_3_gof_1 <- gof(ergm_3_1)

plot(ergm_3_gof_1)

# MCMC stats

# yr 3
mcmc.diagnostics(ergm_3)

# yr 1
mcmc.diagnostics(ergm_3_1)

## 5. COMMUNICATE
# Purpose, methods, findings, discussion
