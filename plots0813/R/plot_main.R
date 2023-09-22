source('R/helper.R')

asthma0728 <- read_excel('E:/2023/0324asthmaregistry/asthmaapp0417/data/截止7月28的患者数据.xlsx')

symptom_dat1 <- asthma0728 %>% select(`哮喘发病以来，有以下哪些症状`) %>% 
  mutate(咳嗽  = str_detect(`哮喘发病以来，有以下哪些症状`,'[咳嗽]'),
         喘息  = str_detect(`哮喘发病以来，有以下哪些症状`,'[喘息]'),
         `气促/呼吸不畅`  = str_detect(`哮喘发病以来，有以下哪些症状`,'[气促/呼吸不畅]'),
         胸闷  = str_detect(`哮喘发病以来，有以下哪些症状`,'[胸闷]')
         )

treatment_dat <- workdat0801 %>% select(`过去的12个月内，哮喘治疗的主要药物有哪些`)


x <- '茶碱,口服激素,SABA（短效β2受体激动剂，如沙丁胺醇）'
str_split(x,',',simplify = TRUE)

treatment_f <- function(x){
  x1 <- str_split(x, ",")

  return(x1)
}

str_split(x,',',simplify = TRUE)


treatment_out <- apply(treatment_dat,1,treatment_f) %>% do.call(rbind.data.frame, .)

treatment_summary <- sub("[\\[].*",'',treatment_out[,1]) %>% str_remove('[:：\\]]') %>% table() %>% 
  data.frame() %>% 
  


treatment_out$c..未使用任何抗哮喘药物....未使用任何抗哮喘药物....未使用任何抗哮喘药物... %>% table()


region_dat <- read_excel('data/省份列表.xlsx')

workdat0801 <- read_excel("data/workdat0801.xlsx") %>% 
  mutate(呼出气一氧化氮 = ifelse(呼出气一氧化氮=='FeNO130','130',呼出气一氧化氮),
         呼出气一氧化氮 = ifelse(呼出气一氧化氮=='<5','5',呼出气一氧化氮),
         吸烟状况 = case_when(吸烟状况=='已戒水烟' |吸烟状况=='已戒香烟' ~ '已戒烟',
                          吸烟状况=='正在吸水烟' |吸烟状况=='正在吸香烟' ~ '正在吸烟',
                          TRUE ~ 吸烟状况),
         吸香烟指数 =  as.numeric(case_when((吸烟状况!='已戒水烟' & 吸烟状况!='正在吸水烟' & 吸香烟指数 !='null') ~ 吸香烟指数,
                           TRUE ~ NA) ),
         吸烟指数 = case_when(吸香烟指数 <10 ~ '[0,10)',
                          吸香烟指数 >=10 & 吸香烟指数 <25 ~ '[10,25)',
                          吸香烟指数 >=25 & 吸香烟指数 <50 ~ '[25,50)',
                          吸香烟指数 >=50  ~ '[50,+∞)',
                          TRUE ~ NA
                          ),
         是否有季节性 = ifelse(是否有季节性=='不适用(总病程<2年)' ,NA,是否有季节性),
         春 = str_detect(季节性,'[春]'),
         夏 = str_detect(季节性,'[夏]'),
         秋 = str_detect(季节性,'[秋]'),
         冬 = str_detect(季节性,'[冬]'),
         民族 = ifelse(民族!='汉族','少数民族',民族),
         哮喘急性发作次数 = ifelse(`过去的12个月内，有多少次哮喘急性发作`=='不适用',NA,
                           `过去的12个月内，有多少次哮喘急性发作`),
         哮喘急性导致就诊 = ifelse(`过去的12个月，因为急性发作导致额外门诊/急诊就诊次数`=='不适用',NA,
                           `过去的12个月，因为急性发作导致额外门诊/急诊就诊次数`),
         哮喘急性导致系统激素使用 = ifelse(`过去的12个月，因为急性发作导致系统激素使用(包括口服与静脉滴注，每次需连续使用≥3天)`=='不适用',NA,
                         `过去的12个月，因为急性发作导致系统激素使用(包括口服与静脉滴注，每次需连续使用≥3天)`),
         
         急性发作导致住院次数 = ifelse(`过去的12个月，因为急性发作导致住院次数`=='不适用',NA,
                               `过去的12个月，因为急性发作导致住院次数`),
         
         急性发作导致入住ICU = ifelse(`过去的12个月，因为急性发作导致入住ICU/气管插管次数`=='不适用',NA,
                             `过去的12个月，因为急性发作导致入住ICU/气管插管次数`),
         
         急性发作最难缓解症状 = ifelse(`哮喘急性发作后，通常以下哪种症状最难缓解(持续时间更长)`=='不适用',NA,
                             `哮喘急性发作后，通常以下哪种症状最难缓解(持续时间更长)`),
         年龄分层 = case_when(年龄<= 24 ~ ' (0,24]',
                          年龄>24 & 年龄<=34 ~ ' (24,34]',
                          年龄>34 & 年龄<=44 ~ ' (34,44]',
                          年龄>44 & 年龄<=54 ~ ' (44,54]',
                          年龄>54 & 年龄<=64 ~ ' (54,64]',
                          年龄>64  ~ ' (64,+∞]',
                          
         )
         
         
         ) %>% 
  left_join(region_dat,by=c('省')) %>% 
  bind_cols(symptom_dat1) %>% 
  mutate(性别 = asthma0728$性别)


# source('R/helper01_t1.R')







library(ggplot2)
library(viridis)
library(hrbrthemes)


# Graph
demo_des_dat$类别 = factor(demo_des_dat$类别,
                         levels = c(" (0,24]"," (24,34]"," (34,44]"," (44,54]"," (54,64]"  , " (64,+∞]",
                                    "女" , "男",
                                    "少数民族" ,  "汉族",
                                    "小学及以下","中学","大学及以上",
                                    "东北", "华东", "华中", "华北", "华南", "西北", "西南",
                                    "不吸烟", "已戒烟", "正在吸烟","[0,10)", "[10,25)", "[25,50)", "[50,+∞)"
                         ))


demo_des_dat$变量 = factor(demo_des_dat$变量,
                         levels = c("年龄","性别","民族",
                                    "教育程度","区域","吸烟状况","吸烟指数"))





ggplot(dfp1, aes(x = value, y= percent, fill = variable), xlab="Age Group") +
  geom_bar(stat="identity", width=.5, position = "dodge") 

ggplot(data, aes(fill=condition, y=value, x=condition)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Studying 4 species..") +
  facet_wrap(~specie) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")


ggplot(demo_des_dat , 
       aes(fill=变量, y=percent, x=类别)) + 
  geom_bar(position="dodge", stat="identity",width=0.8) +
  scale_fill_viridis(discrete = T, option = "E") 


demo_des_dat1 <- demo_des_dat

region_rank_dat <- demo_des_dat1 %>%
  filter(变量=='区域') %>% 
  arrange(desc(percent)) %>% 
  mutate(类别 = as.character(类别))

region_rank <- region_rank_dat$类别 %>% dput()


demo_des_dat1$类别 = factor(demo_des_dat1$类别,
                         levels = c('门诊','住院'," (0,24]"," (24,34]"," (34,44]"," (44,54]"," (54,64]"  , " (64,+∞]",
                                    "女" , "男",
                                      "汉族","少数民族" ,
                                    "小学及以下","中学","大学及以上",
                                    region_rank,
                                    "不吸烟", "已戒烟", "正在吸烟","[0,10)", "[10,25)", "[25,50)", "[50,+∞)"
                         ))


# ggplot(demo_des_dat1 %>% 
#          mutate(tmp = 变量)
#        
#        ,aes(fill=变量, y=percent, x=类别)) + 
#   geom_bar(position  = 'dodge', stat = "identity",width =0.8 )  +
#   scale_fill_viridis(discrete = T, option = "E") +
#   coord_flip()+
#   # facet_wrap(~tmp,ncol =1,dir='v',strip.position='left')+
#   facet_grid(~类别, space="free", scales="free")
##demo--------
demo_des_dat1$变量 = factor(demo_des_dat1$变量,
                         levels = c('就诊类型',"年龄","性别","民族",
                                    "教育程度","区域","吸烟状况","吸烟指数"))




p <- ggplot(demo_des_dat1 %>% 
              mutate(tmp = 变量), 
            aes(y=percent, x=类别,fill=变量)) + 
  geom_bar(position  = 'dodge', stat = "identity",width =0.9 )+
  coord_cartesian() + 
  scale_fill_viridis(discrete = T, option = "E") 

p
p +guides(fill=guide_legend(nrow=8))+ 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
        ) +
  facet_grid(~变量, space="free", scales="free")+
  theme(panel.spacing = unit(0.3, "cm", data = NULL))+
  
  geom_text(aes(x=类别, y=percent,
                label=paste0(count," (",percent,"%)")),hjust=1,
            angle=90,color='white',
            fontface="bold",
            family="serif",
            position = position_dodge(width=1))+
  theme_bw()+
  theme(axis.text=element_text(size=15,face="bold",family = "serif"),
        axis.text.x=element_text(size=15, angle=90,hjust=0.95,vjust=0.2,
                                 face="bold",
                                 family="serif"),
        strip.text.x = element_text(size = 15)
        )+
  xlab('')+
  ylim(c(0,120))

ggsave('output/p_demo.png')


  

  

## symptom-----


symptom_des_dat %>% 
  filter(变量=='首发症状'|变量=='主要症状') %>% 
  ggplot(aes(fill=变量, y=percent, x=类别)) + 
  geom_bar(position  =  'dodge', stat = "identity",width = 0.8)  +
  scale_fill_viridis(discrete = T, option = "E") +
  xlab('')+
  ylab('')+
  
  geom_text(aes(x=类别, y=percent,
                label=percent),
            color='black',size=5,vjust=-0.5,hjust=0.5,
            fontface="bold",
            family="serif",
            position = position_dodge(width=1))+
  
  theme(axis.text=element_text(size=15,face="bold",family = "serif"),
        axis.text.x=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        axis.text.y=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        strip.text.x = element_text(size = 15,face="bold",
                                    family="serif")
  )+
  theme(legend.position = c(0.9, 0.95))+
  theme(legend.text=element_text(size=15,face="bold",family = 'serif'))+
  theme(legend.title=element_blank())+ scale_y_continuous(
    breaks = c(0,20, 40,60),
    label = c("0%", "20%", "40%","60%")
  )+
  ylab('')



ggsave('output/症状.png',dpi=300)



symptom_des_dat %>% 
  filter(类别=='TRUE') %>% arrange(desc(percent)) %>% 
  mutate(变量 = factor(变量,levels=变量)) %>% 
  ggplot(aes( y=percent, x=变量)) + 
  geom_bar(position  =  'dodge', stat = "identity",width = 0.45)  +
  scale_fill_viridis(discrete = T, option = "E") +
  xlab('')+
  theme(axis.text=element_text(size=15,face="bold",family = "serif"),
        axis.text.x=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        axis.text.y=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        strip.text.x = element_text(size = 15,face="bold",
                                    family="serif")
  )+geom_text(aes(x=变量, y=percent,
                 label=percent),
             color='black',size=5,vjust=-0.5,hjust=0.5,
             fontface="bold",
             family="serif",
             position = position_dodge(width=1))+
  
  theme(axis.text=element_text(size=15,face="bold",family = "serif"),
        axis.text.x=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        axis.text.y=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        strip.text.x = element_text(size = 15,face="bold",
                                    family="serif")
  )+ scale_y_continuous(
    breaks = c(0,20, 40,60,80),
    label = c("0%", "20%", "40%","60%","80%")
  )+
  ylab('')

ggsave('output/症状1.png',dpi=300)




##----season-----

season_des_dat %>% mutate(ypos = cumsum(percent)- 0.5*percent ) %>% 
ggplot(aes(x=变量, y=percent)) +
  geom_bar(position  =  'dodge', stat = "identity",width = 0.7)+
  geom_text(aes(x=变量, y=percent,
                 label=percent),
             color='black',size=5,vjust=-0.5,hjust=0.5,
             fontface="bold",
             family="serif",
             position = position_dodge(width=1))+
  
  theme(axis.text=element_text(size=15,face="bold",family = "serif"),
        axis.text.x=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        axis.text.y=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        strip.text.x = element_text(size = 15,face="bold",
                                    family="serif")
  )+
  xlab("")+
  ylab("")+
  scale_fill_viridis(discrete = T, option = "E") 

ggsave('output/季节性.png',dpi=300)

 # geom_bar(stat="identity", width=1, color="white") +
 # coord_polar("y", start=1) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = 变量), color = "white", size=3) +
  scale_fill_brewer(palette="Set1")


library(plotly)
plot_ly(season_des_dat, labels = ~变量, values = ~percent, 
             type = 'pie',textposition = 'inside',textinfo = 'label+percent+count') %>%
  layout(title = 'Letters',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

## lab----------

lab_vars <- c("FEV1预计值", "FEV1实测值", "FEV1预计百分比", "FVC预计值", "FVC实测值", 
              "FVC预计百分比", "FEV1/FVC预计值", "FEV1/FVC实测值", "FEV1/FVC预计百分比", 
              "MMEF预计值", "MMEF实测值", "MMEF预计百分比", "MEF75预计值", 
              "MEF75实测值", "MEF75预计百分比", "MEF25预计值", "MEF25实测值", 
              "MEF25预计百分比", "MEF25-75预计值", "MEF25-75实测值", "MEF25-75预计百分比", 
              "MEF50预计值", "MEF50实测值", "MEF50预计百分比", "PEF预计值", 
              "PEF实测值", "PEF预计百分比",  
              "嗜酸性粒细胞计数", "嗜酸性粒细胞百分比", "中性粒细胞计数", 
              "中性粒细胞百分比", "呼出气一氧化氮")

# lab_vars <- c("FEV1预计值",   "FVC预计值", 
#               "FEV1/FVC预计值")

lab_des_dat <- workdat0801 %>% select(lab_vars )


lab_des_dat[lab_vars] <- lapply(lab_des_dat, as.numeric)

lab_des_dat1 <- lab_des_dat %>% 
  filter(FEV1预计值!=max(FEV1预计值) ) %>% 
  filter(FEV1实测值!=max(FEV1实测值)) %>% 
  filter( FEV1预计百分比!=max(FEV1预计百分比) ) %>% 
  filter(FVC预计值!=max(FVC预计值)) %>%  
  filter( FVC实测值!=max(FVC实测值) ) %>% 
  filter( FVC预计百分比!=max(FVC预计百分比) ) %>% 
  filter(`FEV1/FVC预计值`!=max(`FEV1/FVC预计值`) ) %>% 
  filter(`FEV1/FVC实测值`!=max(`FEV1/FVC实测值`)) %>%  
  filter(`FEV1/FVC预计百分比`!=max(na.omit(`FEV1/FVC预计百分比`)) ) 


lab_des_dat_long <- pivot_longer(lab_des_dat1,
                                 cols=lab_vars,
                                 names_to = '变量',
                                 values_to = 'value') %>% 
  mutate(类别 = case_when(
    str_detect(变量,'预计值') ~ '预计值',
    str_detect(变量,'实测值') ~ '实测值',
    str_detect(变量,'预计百分比') ~ '预计百分比',
    TRUE ~ 'others'
    
  ),
  变量1 = str_remove_all(变量,'[预计值实测值预计百分比]'),
     ) %>% 
  
  mutate(变量1 = factor(变量1,levels=c('FEV1','FVC','FEV1/FVC')),
         类别 = factor(类别,levels=c('预计值','实测值','预计百分比')))


lab_des_dat_long %>% 
  filter(类别!='others') %>% 
  filter(变量 %in% c("FEV1预计值", "FEV1实测值", "FEV1预计百分比", "FVC预计值", 
                   "FVC实测值", "FVC预计百分比", "FEV1/FVC预计值", "FEV1/FVC实测值", 
                   "FEV1/FVC预计百分比")) %>% 
  ggplot( aes(x=value, group=变量1, fill=变量1)) +
 geom_density(adjust=1,alpha=0.8) +
 # geom_boxplot()+
  coord_cartesian() + 
  scale_fill_viridis(discrete = T, option = "E") +
  guides(fill=guide_legend(nrow=8))+ 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
       # axis.text.x=element_text(size=15, angle=90,hjust=0.95,vjust=0.2)
        ) +
 # facet_wrap(变量1~类别,scales="free" )
facet_grid2(c("变量1", "类别"), 
            #labeller = "label_both", 
            scales = "free", independent = "all",
            switch='y')+
  ylab('')+
  
  theme(axis.text=element_text(size=15,face="bold",family = "serif"),
        axis.text.x=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        axis.text.y=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        strip.text.x = element_text(size = 15,face="bold",
                                    family="serif"),
        strip.text.y = element_text(size = 15,face="bold",
                                    family="serif")
  )

ggsave('output/肺功能.png',dpi=300)


##-----ae asthma----
ae_asthma_visit_dat1 %>% 
  mutate(变量 = case_when(变量=='哮喘急性导致就诊'~ '门诊就诊次数',
                        变量=='哮喘急性导致系统激素使用'~ '激素使用次数',
                        变量=='急性发作导致住院次数'~ '住院次数'
                        )) %>% 
ggplot(aes(y=percent, x=类别,fill=变量)) + 
  geom_bar(position  = 'dodge', stat = "identity",width =0.8 )+
  coord_cartesian() + 
  scale_fill_viridis(discrete = T, option = "E") +
guides(fill=guide_legend(nrow=8))+ 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
       # axis.text.x=element_text(size=15, angle=90,hjust=0.95,vjust=0.2)
        ) +
  facet_grid(~变量, space="free", scales="free")+
  theme(panel.spacing = unit(0.5, "cm", data = NULL))+
  ylab("")+
  
  geom_text(aes(x=类别, y=percent,
                label=percent),vjust=-0.5
            ,
            color='black',size=5,
            fontface="bold",
            family="serif",
            position = position_dodge(width=1))+
 
  theme(axis.text=element_text(size=15,face="bold",family = "serif"),
        axis.text.x=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        axis.text.y=element_text(size=15, 
                                 face="bold",
                                 family="serif"),
        strip.text.x = element_text(size = 15,face="bold",
                                    family="serif")
  )

ggsave('output/急性加重情况.png',dpi=300)


first_occur_dat <- workdat0801 %>% 
  select(首次发病时间) %>% 
  mutate(首次发病年 = str_sub(首次发病时间,1,4))


first_occur_dat$首次发病年 %>% table()


###-- 患者地图-----------

region_dat <- read_excel('data/省份列表.xlsx')
p2_dat <-  asthma0728 %>% select(省) %>% group_by(省) %>% 
  summarise(Freq = n()) %>% 
  left_join(region_dat,by='省')

pts_address2 <- read_excel('data/pts_address2.xlsx')


p2_dat2 <- left_join(pts_address2,p2_dat,by=c('prov'='省'))

p2_dat2 |>
  e_charts(prov1) |> # 区域
  em_map("China") |>
  e_map(Freq.y, map = "China") |>
  e_visual_map(Freq.y) |>
  e_labels(fontSize = 12)







library(echarts4r)
library(echarts4r.maps)
p2_dat2 <- left_join(pts_address2,p2_dat,by=c('prov'='省'))

p2_dat2 |>
  e_charts(prov1) |> # 区域
  em_map("China") |>
  e_map(Freq.y, map = "China") |>
  e_visual_map(Freq.y)
  
