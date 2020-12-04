################################################
### Sequential Rank Agreement
## Written by: Aislyn Keyes
## Last edited: 25 Nov 2020

# load libraries
library(SuperRanker)
library(dplyr)
library(ggplot2)
library(tidyr)

# function to combine ranked lists of varying lengths
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

## load ranked lists
# Eco Serv ranks
indirect <- read.csv("Species-Rank/Unweighted/Indirect-Rank-by-PageRank/PageRank-Scores-Combined.csv")

# pull and process direct and indirect by system and average for all services
support <- indirect %>%
  filter(System=="STM") # change this to match the system 
support <- support[,c(2,3)]  

# build matrix - STM
c.fish <- support %>% filter(EcoServ=="CommFish") %>%
  select(SpeciesID)
c.fish <- data.frame(c.fish=c.fish$SpeciesID)

r.fish <- support %>% filter(EcoServ=="RecFish") %>%
  select(SpeciesID)
r.fish <- data.frame(r.fish=r.fish$SpeciesID)

mat <- cbind.fill(c.fish,r.fish)

# start comparing the rank of each unique combination of ES
# commercial and rec fishery
a <- sra(list(mat[,1],mat[,2]))
a.df <- data.frame(sra=a[1:140],depth=1:140)
mean(a[1:10])

t <- random_list_sra(mat[,c(1,2)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:140,times=500))

# now extract point-wise quantiles according to confidence level
test_sra(a,t)

ribbon <- data.frame(min=aggregate(t.df,by=list(t.df$depth),min),
                     max=aggregate(t.df,by=list(t.df$depth),max))
ribbon <- ribbon %>% select(min.sra,max.sra)
ribbon$depth <- 1:nrow(ribbon)

ggplot() +
  geom_ribbon(data=ribbon,aes(x=depth,ymin=min.sra,ymax=max.sra),
              fill="grey70") +
  geom_line(data=a.df,aes(x=depth,y=sra), lwd=1.25,color="blue") +
  ggtitle("Commercial and Recreational Fisheries, STM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

