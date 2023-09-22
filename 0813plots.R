

# create dummy data
dat <- data.frame(
  name=letters[1:5],
  value=sample(seq(4,15),5),
  sd=c(1,0.2,3,2,4)
)

# rectangle
ggplot(data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.5) +
  geom_crossbar( aes(x=name, y=value, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

# line
ggplot(data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.5) +
  geom_linerange( aes(x=name, ymin=value-sd, ymax=value+sd), colour="orange", alpha=0.9, size=1.3)

# line + dot
ggplot(data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.5) +
  geom_pointrange( aes(x=name, y=value, ymin=value-sd, ymax=value+sd), colour="orange", alpha=0.9, size=1.3)

# horizontal
ggplot(data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.5) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), 
                 width=0.4, colour="orange", alpha=0.9, size=1.3) 


ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +  
  geom_bar( ) +
  scale_fill_manual(values = c("red", "green", "blue") ) +
  theme(legend.position="none")



p1_dat <- asthma0728 %>% 
  select(性别,疾病诊断) %>% 
  table() %>% data.frame() %>% 
  left_join(asthma0728 %>% select(疾病诊断) %>% table() %>% data.frame())


p1_dat <-  asthma0728 %>% 
  select(疾病诊断,性别) %>%
  group_by(疾病诊断,性别) %>% 
  summarise(count= n()) %>% 
  group_by(疾病诊断) %>% 
  mutate(percent= count/sum(count),
         count_overall = sum(count)) %>% 
  ungroup() %>% 
  mutate(percent_overall = count_overall/sum(count_overall))



p1_dat1 <-   bind_rows(p1_dat,p1_dat%>% mutate(性别 = '合计') %>% unique()) %>% 
  mutate(percent = ifelse(性别=='合计',percent_overall*100 %>% round(2),percent*100%>% round(2)),
         count = ifelse(性别=='合计',count_overall,count)) %>% unique() %>% 
  mutate(疾病诊断 = factor(疾病诊断,levels = c('典型支气管哮喘','咳嗽变异性哮喘','胸闷变异性哮喘')),
         性别 = factor(性别,levels = c('合计','男','女')),
         )
  

p1_dat1 %>% filter(性别!='合计') %>% 
  ggplot(aes(x=疾病诊断,y = percent,fill=性别))+
  geom_bar(position = "dodge",stat="identity",width = 0.5) +
  theme_ipsum() +
  xlab('')

##p2-----------
region_dat <- read_excel('data/省份列表.xlsx')
p2_dat <-  asthma0728 %>% select(省) %>% group_by(省) %>% 
  summarise(Freq = n()) %>% 
  left_join(region_dat,by='省')



ggdotchart(p2_dat, x = "省", y = "Freq",
           color = "区域",                                # Color by groups
           palette = c("#7fc97f", "#beaed4", "#fdc086",'#ffff99','#386cb0','#f0027f','#bf5b17'), # Custom color palette
           sorting = "descending",                        # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
          label = p2_dat$Freq,
            ggtheme = theme_pubr()                        # ggplot2 theme
)

p2_dat1 <- p2_dat %>% 
  group_by(区域) %>% 
  summarise(count=sum(Freq))


ggbarplot(p2_dat1, x = "区域", y = "count",
           color = "区域",                                # Color by groups
          # palette = c("#7fc97f", "#beaed4", "#fdc086",'#ffff99','#386cb0','#f0027f','#bf5b17'), # Custom color palette
           sorting = "descending",                        # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           label = p2_dat1$count,
           ggtheme = theme_pubr()                        # ggplot2 theme
)


p3_dat <- asthma0728 %>% select(疾病诊断,性别,年龄,BMI) %>% 
  mutate(年龄 = sub('岁','',年龄) %>% as.numeric())

p3 <- gghistogram(p3_dat, x = "年龄",
            add = "mean", rug = TRUE,
            color = "疾病诊断", fill = "疾病诊断",
            bins = 5,
            palette = c("#7fc97f", "#beaed4",'#fdc086'))



p3_dat %>%
  ggplot( aes(x=疾病诊断, y=年龄, fill=疾病诊断)) +
  geom_boxplot() +
 # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
 # geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
 # ggtitle("A boxplot with jitter") +
  xlab("")

library(viridis)
p3_dat %>%
  ggplot( aes(x=疾病诊断, y=年龄, fill=疾病诊断)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
 # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("Violin chart") +
  xlab("")






p31 <- ggdensity(p3_dat, x = "年龄",
          add = "mean", rug = TRUE,
          color = "性别", fill = "性别",
          palette = c("#00AFBB", "#E7B800"))



ggdensity(wdata, x = "weight",
          add = "mean", rug = TRUE,
          color = "sex", fill = "sex",
          palette = c("#00AFBB", "#E7B800"))

library(highcharter)
hchart(p2_dat1 %>% arrange(desc(count)), type = 'bar', hcaes( y = count, x = 区域),name='患者人数') %>% 
  hc_yAxis(title = list(text = "")) %>% 
  hc_xAxis( title = list(text = "")) %>% 
  hc_colors("orange") 

library(echarts4r)
library(echarts4r.maps)
p2_dat2 <- left_join(pts_address2,p2_dat,by=c('prov'='省'))

  p2_dat2 |>
  e_charts(prov1) |> # 区域
  em_map("China") |>
  e_map(Freq.y, map = "China") |>
  e_visual_map(Freq.y)
  
  p4_dat <- asthma0728 %>% select(教育程度) %>% table() %>% data.frame()


  
  spirometry_out
  
  
  
  
  
  data <- data.frame(
    month = paste0(c(1:12), "月"),
    Evaporation = sample(2:200, 12),
    Precipitation = sample(2:200, 12),
    Temperature = sample(2:25, 12, replace = TRUE)
  )
  
  data.inverse <- transform(data, Evaporation_i = -Evaporation)
  data.inverse |>
    e_charts(month) |>
    e_bar(Precipitation, stack = "group1", name = "男") |>
    e_bar(Evaporation_i, stack = "group1", name = "女") |>
    e_y_axis(show = FALSE) |>
    e_flip_coords()
  

 

asthma0728 %>% select(所属医院,唯一识别号,姓名,性别,年龄,名族,BMI,教育程度,
                      省,首次就诊时间,)
  


