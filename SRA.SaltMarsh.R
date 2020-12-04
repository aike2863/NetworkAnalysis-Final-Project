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
  filter(System=="CSM") # change this to match the system 
support <- support[,c(2,3)]  

# build matrix - salt marshes
bird <- support %>% filter(EcoServ=="Birdwatch") %>%
  select(SpeciesID)
bird <- data.frame(bird=bird$SpeciesID)
carbon <- support %>% filter(EcoServ=="CarbonSeq") %>%
  select(SpeciesID)
carbon <- data.frame(carbon1=carbon$SpeciesID)
fish <- support %>% filter(EcoServ=="Fishery") %>%
  select(SpeciesID)
fish <- data.frame(fish=fish$SpeciesID)
filt <- support %>% filter(EcoServ=="WaterFilt") %>%
  select(SpeciesID)
filt <- data.frame(filt=filt$SpeciesID)
hunt <- support %>% filter(EcoServ=="WaterfowlHunt") %>%
  select(SpeciesID)
hunt <- data.frame(hunt=hunt$SpeciesID)


mat <- cbind.fill(bird,carbon,fish,filt,hunt)

# start comparing the rank of each unique combination of ES
# bird watching and carbon
a <- sra(list(mat[,1],mat[,2]))
a.df <- data.frame(sra=a[1:107],depth=1:107)
mean(a)
mean(a[1:10])

t <- random_list_sra(mat[,c(1,2)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:107,times=500))

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
  ggtitle("Birdwatching and Carbon Storage, CSM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# bird watching and fishery
a <- sra(list(mat[,1],mat[,3]))
a.df <- data.frame(sra=a[1:107],depth=1:107)
mean(a[1:10])

t <- random_list_sra(mat[,c(1,3)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:107,times=500))
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
  ggtitle("Birdwatching and Fishery, CSM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# bird watching and filtration
a <- sra(list(mat[,1],mat[,4]))
a.df <- data.frame(sra=a[1:107],depth=1:107)
mean(a[1:10])

t <- random_list_sra(mat[,c(1,4)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:107,times=500))

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
  ggtitle("Birdwatching and Water Filtration, CSM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# bird watching and waterfowl hunting
a <- sra(list(mat[,1],mat[,5]))
a.df <- data.frame(sra=a[1:107],depth=1:107)
mean(a[1:10])

t <- random_list_sra(mat[,c(1,5)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:107,times=500))

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
  ggtitle("Birdwatching and Waterfowl Hunting, CSM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# Carbon and fishery
a <- sra(list(mat[,2],mat[,3]))
a.df <- data.frame(sra=a[1:107],depth=1:107)
mean(a[1:10])

t <- random_list_sra(mat[,c(2,3)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:107,times=500))

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
  ggtitle("Carbon storage and Fishery, CSM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# Carbon and filtration
a <- sra(list(mat[,2],mat[,4]))
a.df <- data.frame(sra=a[1:107],depth=1:107)
mean(a[1:10])

t <- random_list_sra(mat[,c(2,4)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:113,times=500))
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
  ggtitle("Carbon storage and Water Filtration, CSM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# Carbon and hunting
a <- sra(list(mat[,2],mat[,5]))
a.df <- data.frame(sra=a[1:107],depth=1:107)
mean(a[1:10])

t <- random_list_sra(mat[,c(2,5)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:107,times=500))

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
  ggtitle("Carbon storage and Waterfowl Hunting, CSM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# Fishery and filtration
a <- sra(list(mat[,3],mat[,4]))
a.df <- data.frame(sra=a[1:107],depth=1:107)
mean(a[1:10])

t <- random_list_sra(mat[,c(3,4)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:113,times=500))
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
  ggtitle("Fishery and Water Filtration, CSM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# Fishery and hunting
a <- sra(list(mat[,3],mat[,5]))
a.df <- data.frame(sra=a[1:107],depth=1:107)
mean(a[1:10])

t <- random_list_sra(mat[,c(3,5)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:107,times=500))

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
  ggtitle("Fishery and Waterfowl Hunting, CSM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# filtration and hunting
a <- sra(list(mat[,4],mat[,5]))
a.df <- data.frame(sra=a[1:107],depth=1:107)
mean(a[1:10])


t <- random_list_sra(mat[,c(4,5)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:107,times=500))

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
  ggtitle("Water Filtration and Waterfowl Hunting, CSM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))



# ALL ES
a <- sra(list(mat[,1],mat[,2],mat[,3],mat[,4],mat[,5]))
a.df <- data.frame(sra=a[1:107],depth=1:107)

t <- random_list_sra(mat[,c(1:5)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:107,times=500))

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
  ggtitle("All services, CSM") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))



