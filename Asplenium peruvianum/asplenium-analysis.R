setwd("C:/Users/Megan/Desktop/Hawaiian species/Asplenium peruvianum var. insulare")
growth<-read.csv("ggb-growth-percent.csv",header=TRUE)
library(lme4)
library(lmerTest)
library(ggplot2)

growth$Genotype<-as.factor(growth$Genotype)
plot(growth$Genotype, growth$Avg.perc.inc.)
plot(growth$Medium, growth$Avg.perc.inc.)
plot(growth$Auxin, growth$Avg.perc.inc.)
plot(growth$Cytokinin, growth$Avg.perc.inc.)
plot(growth$Cytokinin.amt, growth$Avg.perc.inc.)
hist(growth$Avg.perc.inc.)

tapply(growth$Avg.perc.inc., growth$Medium, summary)
tapply(growth$Avg.perc.inc., growth$Auxin, summary)
tapply(growth$Avg.perc.inc., growth$Cytokinin, summary)

growth$Medium<-relevel(growth$Medium,ref="MS")
growth$Cytokinin<-relevel(growth$Cytokinin,ref="none")
m1<-lmer(Avg.perc.inc.~Auxin+Cytokinin+ (1|Genotype), data=growth)
summary(m1)
tapply(growth$Avg.perc.inc., growth$Auxin, summary)
tapply(growth$Avg.perc.inc., growth$Cytokinin, summary)
plot(m1)
qqnorm(resid(m1))
qqline(resid(m1))
#use of either cytokinin is significantly associated with lower growth, auxin has no effect
plot1<-ggplot(growth, aes(x=Cytokinin, y=Avg.perc.inc.)) +
  geom_boxplot() + 
  geom_jitter(width=0.2, aes(col=Auxin)) + 
  labs(y="Percent increase growth (%)", x="Cytokinin") +
  theme_minimal()
plot1

m2<-lmer(Avg.perc.inc.~Medium+(1|Genotype), data=growth)
summary(m2)
plot(m2)
qqnorm(resid(m2))
qqline(resid(m2))
#1B is significantly associated with lower growth
#1TDZ .1N is nearly significantly associated with lower growth
growth$Medium<-factor(growth$Medium, levels=c('MS','.1N','1N','.5B','.5B .1N','1B','1B .1N','.2TDZ','.2TDZ .1N','.5TDZ','.5TDZ .1N','1TDZ','1TDZ .1N'))
plot2<-ggplot(growth, aes(x=Medium, y=Avg.perc.inc.)) +
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2)) +
  labs(y="Percent increase growth (%)", x="Medium") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
plot2

count <- read.csv("ggb-count.csv",header=TRUE)
hist(count$GGB.count)
count$Genotype<-as.factor(count$Genotype)
count$Medium<-relevel(count$Medium,ref="MS")
count$Cytokinin<-relevel(count$Cytokinin,ref="none")

tapply(count$GGB.count, count$Medium, summary)
tapply(count$GGB.count, count$Auxin, summary)
tapply(count$GGB.count, count$Cytokinin, summary)

m3<-glmer(GGB.count~Auxin+Cytokinin+(1|Genotype), poisson, data=count)
summary(m3)
#Auxin is significantly associated with increased count
#Use of BAP is significantly associated with decreased count
plot3<-ggplot(count, aes(x=Cytokinin, y=GGB.count)) +
  geom_boxplot() +
  geom_jitter(width=0.2, aes(col=Auxin)) + 
  labs(y="GGB count", x="Cytokinin") +
  theme_minimal()
plot3

plot5<-ggplot(count, aes(x=Auxin, y=GGB.count)) +
  geom_boxplot() +
  geom_jitter(width=0.2, aes(col=Cytokinin)) + 
  labs(y="GGB count", x="Auxin") +
  theme_minimal()
plot5

m4<-glmer(GGB.count~Medium+(1|Genotype), poisson, data=count)
summary(m4)
#.1N, .2TDZ, .2TDZ .1N, .5B, .5TDZ .1N, and 1TDZ .1N are significantly associated with increased count
#1B and 1B .1N are significantly associated with decreased count

count$Medium<-factor(count$Medium, levels=c('MS','.1N','1N','.5B','.5B .1N','1B','1B .1N','.2TDZ','.2TDZ .1N','.5TDZ','.5TDZ .1N','1TDZ','1TDZ .1N'))
plot4<-ggplot(count, aes(x=Medium, y=GGB.count)) +
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2)) +
  labs(y="GGB count", x="Medium") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
plot4

plot6<-ggplot(count, aes(x=Genotype, y=GGB.count)) +
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2)) +
  labs(y="GGB count", x="Genotype") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
plot6

aggregate(count[, 6], list(count$Medium), mean)

library(multcomp)
cld(m4, mcp(Medium="Tukey"))
