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
  filter(System=="YTH") # change this to match the system 
support <- support[,c(2,3)]  

# build matrix - salt marshes
bird <- support %>% filter(EcoServ=="Birdwatch") %>%
  select(SpeciesID)
bird <- data.frame(bird=bird$SpeciesID)
c.fish <- support %>% filter(EcoServ=="CommFish") %>%
  select(SpeciesID)
c.fish <- data.frame(c.fish=c.fish$SpeciesID)
r.fish <- support %>% filter(EcoServ=="RecFish") %>%
  select(SpeciesID)
r.fish <- data.frame(r.fish=r.fish$SpeciesID)
hunt <- support %>% filter(EcoServ=="WaterfowlHunt") %>%
  select(SpeciesID)
hunt <- data.frame(hunt=hunt$SpeciesID)

mat <- cbind.fill(bird,c.fish,r.fish,hunt)

# start comparing the rank of each unique combination of ES
# bird watching and commercial fishery
a <- sra(list(mat[,1],mat[,2]))
a.df <- data.frame(sra=a[1:87],depth=1:87)
mean(a[1:10])

t <- random_list_sra(mat[,c(1,2)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:87,times=500))

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
  ggtitle("Birdwatching and Commercial Fishery, YTH") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# bird watching and rec fishery
a <- sra(list(mat[,1],mat[,3]))
a.df <- data.frame(sra=a[1:87],depth=1:87)
mean(a[1:10])

t <- random_list_sra(mat[,c(1,3)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:87,times=500))
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
  ggtitle("Birdwatching and Recreational Fishery, YTH") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# bird watching and hunting
a <- sra(list(mat[,1],mat[,4]))
a.df <- data.frame(sra=a[1:87],depth=1:87)
mean(a[1:10])
t <- random_list_sra(mat[,c(1,4)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:87,times=500))

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
  ggtitle("Birdwatching and Waterfowl Hunting, YTH") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# Comm and rec fishery
a <- sra(list(mat[,2],mat[,3]))
a.df <- data.frame(sra=a[1:86],depth=1:86)
mean(a[1:10])

t <- random_list_sra(mat[,c(2,3)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:86,times=500))

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
  ggtitle("Commercial and Recreational Fishery, YTH") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# commercial fish and hunting
a <- sra(list(mat[,2],mat[,4]))
a.df <- data.frame(sra=a[1:87],depth=1:87)
mean(a[1:10])

t <- random_list_sra(mat[,c(2,4)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:87,times=500))
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
  ggtitle("Commercial fish and waterfowl hunting, YTH") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))


# rec Fishery and hunting
a <- sra(list(mat[,3],mat[,4]))
a.df <- data.frame(sra=a[1:87],depth=1:87)
mean(a[1:10])

t <- random_list_sra(mat[,c(3,4)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:87,times=500))
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
  ggtitle("Rec Fishery and Waterfowl hunting, YTH") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))

# ALL ES
a <- sra(list(mat[,1],mat[,2],mat[,3],mat[,4]))
a.df <- data.frame(sra=a[1:87],depth=1:87)

t <- random_list_sra(mat[,c(1:4)],n=500)
t.df <- data.frame(sra=c(rbind(t[,1:500])),
                   depth=rep(1:87,times=500))

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
  ggtitle("All services, YTH") +
  theme_classic() +
  xlab("List Depth") +
  ylab("Sequential Rank Agreement") +
  scale_size_manual(values=c(2,1.25)) +
  scale_color_manual(values=c("grey","blue")) +
  xlim(0,nrow(a.df)) +
  ylim(0,max(t.df$sra))



