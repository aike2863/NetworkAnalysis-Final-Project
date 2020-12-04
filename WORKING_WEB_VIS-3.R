
library(igraph)
library(dplyr)

spp <- read.csv("Web_Data_Files/Final_YTH_Nodes_ES.csv") 
spp <- spp[,c(1,9,10)]

links <- read.csv("Web_Data_Files/Final.YTH.edges.csv")
links <- links[!links$from==links$to,] # get rid of cannibalism

link_type <- as.character(c(rep("Species", times=nrow(filter(links,type=="feeding"))), 
                            rep("Ecosystem Service", times=nrow(filter(links,type=="ES")))))
links <- mutate(links, type=link_type)
links <- mutate(links, col=c(rep("gray75", times=nrow(filter(links,type=="Species"))), 
                             rep("black", times=nrow(filter(links,type=="Ecosystem Service")))))

spp_type <- as.character(c(rep("Species", times=87),rep("Ecosystem Serivce", times=4)))
spp <- mutate(spp, id = spp_type)
spp <- mutate(spp, col = c(rep("#C71585", times=87),rep("deep sky blue", times=4)))
spp <- mutate(spp, shape = c(rep("circle", times=87),rep("square", times=4)))


gr <- graph_from_data_frame(links, directed=TRUE, spp$SpeciesID)


layt <- layout.fruchterman.reingold(gr)

adjmat <- get.adjacency(gr, sparse = FALSE, attr = NULL) # convert to adj mat

t1 <- TrophInd(adjmat)
layt[,2] <- t1$TL # ensures that nodes will be arranged by trophic level from top to bottom



plot(gr,
     layout=layt,
     vertex.size = 5,
     vertex.label = NA,
     edge.arrow.size=0.4,
     vertex.color=spp$col,
     vertex.shape=spp$shape,
     edge.color=links$col,
     main="Ythan Estuary")

legend(locator(n=1),
       legend=c("Species","Ecosystem Service","Species interaction","Service provisioning"),
       col=c("#C71585","deep sky blue","grey","black"),
       pch=c(19,15,NA,NA),lty=c(NA,NA,1,1),cex=1, lwd=3,bty="n",
       ncol=2)






adjmat <- get.adjacency(gr, sparse = FALSE, attr = NULL) # convert to adj mat
t1 <- TrophInd(adjmat)

lay <- create_layout(gr,"fr")
lay$y <- t1$TL





ggraph(lay) +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)), 
                 arrow = arrow(type = "closed", length = unit(2, 'mm'))) +
  geom_node_point(aes(shape=id, color=id), size=4) +
  theme_graph()
