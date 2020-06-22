#### Ranking upcoming FA based on role with PIPM

### plot x=opipm , y=dpipm
### Position / Role


library(tidyverse)
library(extrafont)
library(ggimage)
library(ggrepel)
library(RColorBrewer)

loadfonts(device="win")
# load black theme

fa<-readxl::read_xlsx(file.choose())
stats<- readxl::read_xlsx(file.choose())


#manipulate data
free<-intersect(fa$Player,stats$PLAYER)
stats$POSITION <- factor(stats$POSITION)
stats$TEAM <- factor(stats$TEAM)
fa$`FA Type` <- factor(fa$`FA Type`)
fa$Position <- factor(fa$Position)
fa$Role <- factor(fa$Role)

statlist <- NULL
falist<-NULL
for (i in 1:length(free)) {
  statlist<-c(statlist,which(stats$PLAYER==free[i]))
  falist <- c(falist,which(fa$Player==free[i]))
} ; rm(i)
x <- fa[falist,] ; y <- stats[statlist,]


dat <- data.frame(x[,6:9], y)
dat <- dat[,c(5,7,8,9,3,6,4,1,2,10:16)]





ggplot(dat, aes(y=PIPM, x=Position))+
  geom_boxplot(fill="transparent", aes(colour=Position),
               show.legend = F, outlier.shape = 15, outlier.size = 2) + 
  scale_color_manual(values=c("lawngreen","magenta","orangered",
                              "turquoise2","yellow2","khaki"))+
  labs(title="Evaluating 2020 Free Agents: PIPM",
       subtitle="Based on different position types",
       caption = "Source: @earlybirdrights, @The_BBall_Index") +
  theme_black()


png("b-pipm.png", units="in", width=9, height=7, res=500)

dev.off()


ggplot(data=dat, aes(x=O.PIPM, y=D.PIPM, colour=Role)) +
  geom_point(size=2) +
  scale_color_manual(values=c("forestgreen","darkorchid1","firebrick1","tan1",
                               "royalblue1","goldenrod1","violetred1","seagreen1"))+
  geom_text_repel(aes(label=ifelse(O.PIPM>2,PLAYER,'')),
                  family = "Segoe UI",hjust=0.7,vjust=0) + 
  geom_text_repel(aes(label=ifelse(D.PIPM>2.5&O.PIPM>-0.5,PLAYER,'')),
                  family = "Segoe UI",hjust=0.7,vjust=0) + 
    theme_black() +
  labs(title = "Evaluating 2020 Free Agents: \nO-PIPM vs D-PIPM", x="O-PIPM", y="D-PIPM",
       subtitle = "Based on different role types",
       caption = "Source: @earlybirdrights, @The_BBall_Index") 



