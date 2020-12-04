################################################
### Plotting and regressions
## Written by: Aislyn Keyes
## Last edited: 4 Dec 2020

# load libraries
library(lme4)
library(ggplot2)
library(dplyr)

# Read in the data:  
dat <- read.csv("sra.factors-final.csv")
duw <- dat %>% subset(comp.type=="unweighted")
duw$system <- factor(duw$system) 
dw <- dat %>% subset(comp.type=="weighted")
dw$system <- factor(dw$system) 

# ## Fit linear mixed effects model:  
# unweighted
lmerExT <- lmer(avg.sra10~n.esp.diff*tl.diff+(1|system), data=duw)
summary(lmerExT)

# check the diagnostics 
plot(lmerExT) # residuals vs. fitted
qqnorm(residuals(lmerExT)) # normal q-q plot
plot(lmerExT,sqrt(abs(residuals(.))) ~ fitted(.),type=c("p","smooth")) # scale-location
# histogram of the residuals
r <- resid(lmerExT)
x <- seq(min(r),max(r),length.out=100)
y <- dnorm(x,mean(r),sd(r))
res_df <- data.frame(residuals=r)
norm_df <- data.frame(x=x,y=y)
rm(r,x,y)
ggplot() +
  geom_histogram(data=res_df,mapping=aes(x=residuals,y=stat(density)),bins=20) +
  geom_line(data=norm_df,mapping=aes(x=x,y=y),col="red")

#weighted
lmerExT <- lmer(avg.sra10~n.esp.diff*tl.diff+(1|system), data=dw)
summary(lmerExT)

# check the diagnostics 
plot(lmerExT) # residuals vs. fitted
qqnorm(residuals(lmerExT)) # normal q-q plot
plot(lmerExT,sqrt(abs(residuals(.))) ~ fitted(.),type=c("p","smooth")) # scale-location
# histogram of the residuals
r <- resid(lmerExT)
x <- seq(min(r),max(r),length.out=100)
y <- dnorm(x,mean(r),sd(r))
res_df <- data.frame(residuals=r)
norm_df <- data.frame(x=x,y=y)
rm(r,x,y)
ggplot() +
  geom_histogram(data=res_df,mapping=aes(x=residuals,y=stat(density)),bins=20) +
  geom_line(data=norm_df,mapping=aes(x=x,y=y),col="red")


# compare weighted to unweighted for CSM, BSQ, EPB
duw2 <- duw[1:26,] # pull csm, bsq, epb from unweighted

wilcox.test(duw2$avg.sra,dw$avg.sra, paired=TRUE) # wilcox test

#plot SRA value box plot with weighted vs. unweighted
dat <- rbind(dw,duw2)
ggplot(dat,aes(x=comp.type,y=avg.sra)) +
geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE) +
  theme_classic(base_size=15) +
  xlab("Personalized PageRank Calculation") +
  ylab("Average Overlap (SRA, full list)")



# plot SRA values by ecosystem service - weighted vs. unweighted
ggplot(comp,aes(x=es.combo,y=avg.sra, color=system)) +
  geom_point(size=3,position=position_dodge(width=0.3),shape=4,
             stroke=1.5) +
  xlab("Ecosystem service") +
  ylab("Mean Sequential Rank Agreement (SRA)") +
  scale_color_manual(values=c("orange","slateblue","red"),
                     labels=c("Bahia Falsa","Carpinteria",
                              "Estero de Punta Banda")) +
  labs(col="System") +
  scale_x_discrete(labels=c("Birdwatching","Carbon Storage",
                            "Water Filtration","Fishery",
                            "Waterfowl Hunting")) +
  coord_flip() +
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12))
  

