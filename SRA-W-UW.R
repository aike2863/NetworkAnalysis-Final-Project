################################################
### Sequential Rank Agreement - weighted and unweighted
## Written by: Aislyn Keyes
## Last edited: 4 December 2020

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
w.df <- read.csv("Species-Rank/Weighted/Indirect-Rank-by-PageRank/PageRank-Scores-Combined-W.csv")
uw.df <- read.csv("Species-Rank/Unweighted/Indirect-Rank-by-PageRank/PageRank-Scores-Combined.csv")


# pull and process direct and indirect by system and average for all services
supp.w <- w.df %>%
  filter(System=="BSQ") # change this to match the system 
supp.w <- supp.w[,c(2,3)]  

supp.uw <- uw.df %>%
  filter(System=="BSQ") # change this to match the system 
supp.uw <- supp.uw[,c(2,3)]

# build matrix -=
# weighted
bird.w <- supp.w %>% filter(EcoServ=="Birdwatch") %>%
  select(SpeciesID)
bird.w <- data.frame(bird.w=bird.w$SpeciesID)
carbon.w <- supp.w %>% filter(EcoServ=="CarbonSeq") %>%
  select(SpeciesID)
carbon.w <- data.frame(carbon.w=carbon.w$SpeciesID)
fish.w <- supp.w %>% filter(EcoServ=="Fishery") %>%
  select(SpeciesID)
fish.w <- data.frame(fish.w=fish.w$SpeciesID)
filt.w <- supp.w %>% filter(EcoServ=="WaterFilt") %>%
  select(SpeciesID)
filt.w <- data.frame(filt.w=filt.w$SpeciesID)
hunt.w <- supp.w %>% filter(EcoServ=="WaterfowlHunt") %>%
  select(SpeciesID)
hunt.w <- data.frame(hunt.w=hunt.w$SpeciesID)

#unweighted
bird.uw <- supp.uw %>% filter(EcoServ=="Birdwatch") %>%
  select(SpeciesID)
bird.uw <- data.frame(bird.uw=bird.uw$SpeciesID)
carbon.uw <- supp.w %>% filter(EcoServ=="CarbonSeq") %>%
  select(SpeciesID)
carbon.uw <- data.frame(carbon.uw=carbon.uw$SpeciesID)
fish.uw <- supp.uw %>% filter(EcoServ=="Fishery") %>%
  select(SpeciesID)
fish.uw <- data.frame(fish.uw=fish.uw$SpeciesID)
filt.uw <- supp.w %>% filter(EcoServ=="WaterFilt") %>%
  select(SpeciesID)
filt.uw <- data.frame(filt.uw=filt.uw$SpeciesID)
hunt.uw <- supp.w %>% filter(EcoServ=="WaterfowlHunt") %>%
  select(SpeciesID)
hunt.uw <- data.frame(hunt.uw=hunt.uw$SpeciesID)

mat <- cbind.fill(bird.w,bird.uw,carbon.w,carbon.uw,fish.w,
                  fish.uw,filt.w,filt.uw,hunt.w,hunt.uw)

# start comparing the rank of each ES - weighted and unweighted
# bird watching 
a <- sra(list(mat[,1],mat[,2]))
a.df <- data.frame(sra=a[1:116],depth=1:116)
mean(a)
mean(a[1:10])

t <- random_list_sra(mat[,c(1,2)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:116,times=500))

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
  ggtitle("Birdwatching, BSQ") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# carbon 
a <- sra(list(mat[,3],mat[,4]))
a.df <- data.frame(sra=a[1:116],depth=1:116)
mean(a)
mean(a[1:10])

t <- random_list_sra(mat[,c(3,4)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:116,times=500))

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
  ggtitle("Carbon storage, BSQ") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# fishery 
a <- sra(list(mat[,5],mat[,6]))
a.df <- data.frame(sra=a[1:116],depth=1:116)
mean(a)
mean(a[1:10])

t <- random_list_sra(mat[,c(5,6)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:116,times=500))

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
  ggtitle("Fishery, BSQ") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# Filtration 
a <- sra(list(mat[,7],mat[,8]))
a.df <- data.frame(sra=a[1:116],depth=1:116)
mean(a)
mean(a[1:10])

t <- random_list_sra(mat[,c(7,8)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:116,times=500))

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
  ggtitle("Water Filtration, BSQ") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))


# Hunting 
a <- sra(list(mat[,9],mat[,10]))
a.df <- data.frame(sra=a[1:116],depth=1:116)
mean(a)
mean(a[1:10])

t <- random_list_sra(mat[,c(9,10)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:116,times=500))

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
  ggtitle("Waterfowl Hunting, BSQ") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

