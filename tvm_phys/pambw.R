library('ggplot2')
library('cowplot')
library('ggpubr')
tvm.pambw <- read.csv(file="thermvar_microbes_pambw.csv",header=TRUE)
#edits I made: added species column, added treatment column

#### pam ####
tvm.pambw$pam.avg.pos <- (tvm.pambw$pam_t4_1+tvm.pambw$pam_t4_2+tvm.pambw$pam_t4_3)/3
tvm.pambw$pam.avg.pre <- (tvm.pambw$pam_t3_1+tvm.pambw$pam_t3_2+tvm.pambw$pam_t3_3)/3

#tvm.pambw.fra <- subset(tvm.pambw,species=="frank")
#tvm.pambw.fav <- subset(tvm.pambw,species=="fav")

#pam.avg.pre <- summarySE(tvm.pambw.fra,measurevar="pam.avg.pre",groupvars=c("treatment"))
#pam.avg.pos.com <- tvm.pambw.fra[complete.cases(tvm.pambw.fra$pam.avg.pos),]
#pam.avg.pos <- summarySE(tvm.pambw.fra,measurevar="pam.avg.pre",groupvars=c("treatment"))

ggplot(tvm.pambw,aes(x=treatment,y=pam.avg.pre,color=treatment))+
  scale_color_manual(values=c("#B9C184","#D66982"))+
  geom_boxplot()+
  facet_grid(species~.)+
  theme_cowplot()
ggsave("pam.preheat.pdf",width=3.5)

ggplot(tvm.pambw,aes(x=treatment,y=pam.avg.pos,color=treatment))+
  scale_color_manual(values=c("#B9C184","#D66982"))+
  geom_boxplot()+
  facet_grid(species~.)+
  theme_cowplot()
ggsave("pam.postheat.pdf",width=3.5)

#ggplot(tvm.pambw.fra,aes(x=treatment,y=pam.avg.pos,color=treatment))+
#  geom_boxplot()
  

#geom_violin()+
  scale_color_manual(values=c('paleturquoise4','orchid4'))+
  geom_jitter()+
  #geom_dotplot(binaxis="y",stackdir="center")+
  geom_boxplot(alpha=0.5)+
  theme_cowplot()+
  theme(legend.position="none")+
  ylab("")+
  xlab("Treatment")+
  scale_x_discrete(labels=c("Control","Variation"))+
  ggtitle("Orbicella faveolata")+
  ylim(0.500, 0.650)

scale_color_manual(values=c('paleturquoise4','orchid4'))+
  geom_jitter()+
  geom_boxplot(alpha=0.5)+
  theme_cowplot()+
  theme(legend.position="none")+
  ylab("Photosynthetic efficiency")+
  xlab("Treatment")+
  scale_x_discrete(labels=c("Control","Variation"))+
  ggtitle("Orbicella franksi")+
  ylim(0.500,0.650)
gg.frank

gg.fav <- ggplot(d90_fav,aes(x=treatment,y=pam_avg,color=treatment))+
  #geom_violin()+
  scale_color_manual(values=c('paleturquoise4','orchid4'))+
  geom_jitter()+
  #geom_dotplot(binaxis="y",stackdir="center")+
  geom_boxplot(alpha=0.5)+
  theme_cowplot()+
  theme(legend.position="none")+
  ylab("")+
  xlab("Treatment")+
  scale_x_discrete(labels=c("Control","Variation"))+
  ggtitle("Orbicella faveolata")+
  ylim(0.500, 0.650)
gg.fav

quartz()
ggarrange(gg.frank,gg.fav,title="hello?")

shapiro.test(log(d90_fav$pam_avg))
a1 <- aov(log(pam_avg)~treatment,data=d90_fav)
summary(a1)

shapiro.test(d90_frank$pam_avg)
a1 <- aov(pam_avg~treatment,data=d90_frank)
summary(a1)

a1 <- aov(pam_avg~treatment*species,data=day90)
summary(a1)

#### bw ####
day90$bw3_avg <- (day90$bw_t3_1+day90$bw_t3_2)/2
day90$bw0_avg <- (day90$bw_t0_1+day90$bw_t0_2)/2
day90$bw_per <- ((day90$bw3_avg-day90$bw0_avg)/day90$bw0_avg)*100

d90_frank <- subset(day90,species=="frank")
d90_fav <- subset(day90,species=="fav")

ggplot(d90_fav,aes(x=treatment,y=bw_per,color=treatment))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  scale_color_manual(values=c('paleturquoise4','orchid4'),name="Treatment",labels=c("Control","Variation"))
#geom_jitter()

ggplot(d90_frank,aes(x=treatment,y=bw_per,color=treatment))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  scale_color_manual(values=c('paleturquoise4','orchid4'),name="Treatment",labels=c("Control","Variation"))
#geom_jitter()

summary(d90_fav)

shapiro.test(log(d90_fav$bw_per))
a1 <- aov(log(bw_per)~treatment,data=d90_fav)
summary(a1) #not sig

shapiro.test(log(d90_frank$bw_per))
a1 <- aov(log(bw_per)~treatment,data=d90_frank)
summary(a1) #not sig

#### sine wave ####
x <- seq(0,8*pi,length.out=100)
y <- sin(x)
plot(x,y,type="l")
df <- data.frame(x,y)

x2 <- seq(0,8*pi,length.out=100)
y2 <- (sin(x2)/4)
df2 <- data.frame(x2,y2)

plot(x,y,type="l")
plot(x2,y2,type="l")

quartz()
ggplot(data=df,aes(x=x,y=y))+
  #geom_line(color="orchid4",size=2)+
  geom_line(data=df2,aes(x=x2,y=y2),color="paleturquoise4",size=2)+
  xlim(0,12.75)+
  theme_cowplot()+
  ylim(-1,1)
  
