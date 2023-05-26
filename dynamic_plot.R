library(readxl)
library(dplyr)
library(gapminder)
library(dplyr)
library(ggplot2)
library(gganimate)
pts_wk_new <- read_excel("E:/2023/0324asthmaregistry/asthmaapp0417/pts_wk_new.xlsx")
dy <- pts_wk_new %>% 
  mutate(mylbl = as.character(mylbl))

staticplot <- ggplot(dy, aes(rank, group = hosp,
                             fill = as.factor(hosp), color = as.factor(hosp))) +
  geom_tile(aes(y = Freq/2,
                height = Freq,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(hosp_lbl, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Freq,label = mylbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        #panel.grid.major.x = element_line( size=.1, color="grey" ),
        #panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=20, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=12, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=10, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank()
        ,
        plot.margin = margin(t =0,r=3, b=0, l=6, "cm") #第一个调上，第三个调下；
        ##第四个调右边； 第三个调整左边
        )

anim<-staticplot + 
  transition_states(week, transition_length =5, state_length = 5) +
  enter_fade() +
  exit_fade() + 
  ease_aes('quadratic-in-out')+
  labs(title = '2023年第 {closest_state} 周\n哮喘患者人数排名(TOP10)',
       subtitle  =  " ",
       caption  = "")

#animate(anim,renderer = gifski_renderer(),rewind=FALSE)

animate(anim,duration=20,end_pause = 5, renderer = gifski_renderer()) #200, fps = 20,  width = 1200, height = 1000)
anim_save("04-bar chart-Freq.gif")

