####################################################
## ES Trophic level #############################
# Written by: Aislyn Keyes #########################
# Last edited: 29 Nov 2020    ####################

# Install pacakges and load libraries
#install.packages("NetIndices")
#install.packages("ggraph")
#library(bipartite)
library(igraph)
library(NetIndices)
library(dplyr)

# Load data and convert to graph object
nodes <- read.csv("Web_Data_Files/Final_STM_Nodes_ES.csv") # nodes
edges <- read.csv("Web_Data_Files/Final.STM.edges.csv") # edges

net.g <- graph_from_data_frame(edges,
                               directed = T,
                               vertices = nodes$SpeciesID) 

net <- simplify(net.g, remove.loops = TRUE)

adjmat <- get.adjacency(net, sparse = FALSE, attr = NULL) # convert to adj mat

t1 <- TrophInd(adjmat) # make dataframe with trophic levels per ndoe
t1$from <- rownames(t1) # pull node IDs

# mean and sd summary function
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


# ES providers TL
esp <- subset(edges,type=="ES")
esp2 <- merge(esp,t1,by="from",all.x=T)
TL <- data_summary(esp2,varname="TL",groupnames="to")





