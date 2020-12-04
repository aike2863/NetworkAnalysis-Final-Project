######################
## FluxWeb pracitce
## Written by: A Keyes, following FluxWeb vignette 
## 30 OCT 2020

library(fluxweb)
library(igraph)

# load data
nodes <- read.csv("Web_Data_Files/Final_BSQ_Nodes_ES.csv") # node list
links <- read.csv("Web_Data_Files/Final.BSQ.edges.filtered.csv") # edge list 
bio <- read.csv("Biomass_Abun_Dat/BSQBiomass.Estimates.csv")

nodes.spp <- subset(nodes, nodes$NodeType != "ES") # exclude ES from node list 
links.spp <- subset(links, links$Type == "Feeding") # subset interactions to remove all ES links

# create network object with services
net <- graph.data.frame(links.spp,
                        directed = T,
                        vertices = nodes.spp) 

net <- simplify(net, remove.loops=T) # remove cannibalism
mat <- get.adjacency(net, sparse=FALSE, attr = NULL) # convert to adj mat

dat <- data.frame(Species=nodes.spp$SpeciesID, org=nodes.spp$OrganismalGroup, org2=NA)
dat$org2[c(1:14)] <- "plant"
dat$org2[15:116] <- "animal"

bodymasses <- nodes.spp$BodySize.g.
bodymasses[is.na(bodymasses)] <- 1
losses <- 0.71 * bodymasses^(-0.25)
bio[is.na(bio)] <- 1
efficiencies <- rep(NA,nrow(dat))
efficiencies[dat$org2=="animal"] <- 0.906
efficiencies[dat$org2=="plant"] <- 0.545


mat.fluxes <- fluxing(mat,bio$all.biomass,losses,efficiencies,)

g <- graph_from_adjacency_matrix(adjmatrix=mat.fluxes,mode="directed",
                                 weighted=T)
plot(g)

write.csv(mat.fluxes,"Web_Data_Files/Weighted_BSQ_Mat.csv")

d <- get.data.frame(g)
write.csv(d,"Web_Data_Files/Weighted_BSQ_Edges.csv")



