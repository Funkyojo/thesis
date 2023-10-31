library(epiDisplay)
library(ggplot2)
library(dplyr)

# Calculate RR corrected for size

setwd("C:/Users/funky/Dropbox/document/Homework/PublishingStackedBoundaries/20230418_E12E11")
mydata = read.csv("mydata_20231002_all3.txt", sep = "\t")
colnames(mydata) = c("EP", "Config", "Size")
mydata$EP <- as.factor(mydata$EP)
# RR for each category
for (c in 1:3){
  #1 = merge, 2 = stack, 3 = Other
  print(paste("performing for", c))
  mydata_temp = mydata
  mydata_temp$Config[mydata$Config != c] = 0
  mydata_temp$Config[mydata$Config == c] = 1
  mydata_temp$Config <- as.factor(mydata_temp$Config)
  fit0 <- glm(EP~Config,data=mydata_temp,family=binomial(link="log"))
  fit1 <- glm(EP~Size,data=mydata_temp,family=binomial(link="log"))
  fit2 <- glm(EP~Config+Size,data=mydata_temp,family=binomial(link="log"))
  print(logistic.display(fit0))
  print(logistic.display(fit1))
  print(logistic.display(fit2))
}
# Plotting adjusted RR
df = read.csv("adjusted_RR.txt", sep = '\t', header = FALSE, col.names = c("Assay", "fit", "lCI", "RR", "rCI", "p"))
df$Assay[df$Assay==1] = "Merge"
df$Assay[df$Assay==2] = "Stack"
df$Assay[df$Assay==3] = "Other"
df$Assay = factor(df$Assay,levels = c("Merge", "Stack", "Other"))
df$fit = factor(df$fit)

ggplot(subset(df, fit != 1), aes(x=Assay, y=RR, ymin = lCI, ymax = rCI, color = fit)) + 
  geom_pointrange(position = position_dodge(0.5), size=1.5) + 
  scale_y_log10(breaks = c(0.6, 1, 1.5))+
  geom_hline(yintercept=1, lty=2) + 
  #geom_text(angle=90, position=position_dodge(0.5), hjust=0.5, vjust = 1) + 
  ylab("Risk Ratio") + 
  xlab("") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 30))
ggsave("20231002_RRadjusted.eps", dpi = 300, units = "mm")

# #set other as reference
# mydata$Config[mydata$Config == 3] = 0
# mydata$Config[mydata$Config == 1] = 3
# mydata$Config[mydata$Config == 0] = 1
# mydata$EP <- as.factor(mydata$EP)
# mydata$Config <- as.factor(mydata$Config)
# 
# fit0 <- glm(EP~Config,data=mydata,family=binomial(link="log"))
# fit1 <- glm(EP~Size,data=mydata,family=binomial(link="log"))
# fit2 <- glm(EP~Config+Size,data=mydata,family=binomial(link="log"))
# fit3 <- glm(EP~Config*Size,data=mydata,family=binomial(link="log"))
# AIC(fit0,fit1, fit2, fit3)
# exp(coef(fit0)) # Config2 = Stack, config3 = merge. Displayed are RR.
# exp(coef(fit1))
# exp(coef(fit2)) # RR accounted for size
# exp(coef(fit3)) #since AIC ~= 2, interaction won't improve model
# 
# A = sum(mydata[,1] == 1 & mydata[,2] == 2)
# B = sum(mydata[,1] == 0 & mydata[,2] == 2)
# C = sum(mydata[,1] == 1 & mydata[,2] != 2)
# D = sum(mydata[,1] == 0 & mydata[,2] != 2)
# 
# RR = (A/(A+B))/(C/(C+D))
# 
# logistic.display(fit0)
# logistic.display(fit2)

# draw histogram of size distribution
ggplot(subset(mydata, Config != 0), aes(x = Size, fill = Config, color = Config)) +
  #geom_histogram(alpha = 0.3, position = "identity", aes(y = after_stat(..density..))) +
  geom_freqpoly(position = "identity", aes(y = after_stat(..density..))) +
  theme_light()
ggsave("20231002_sizeHisto.eps", dpi = 300, units = "mm")
# KS test
ks.test(mydata$Size[mydata$Config==1], mydata$Size[mydata$Config==2])

