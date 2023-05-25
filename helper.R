library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#ff7518")
library(timevis)
library(dplyr)
library(readxl)
library(ggplot2)
library(epitools)
library(lubridate)
library(dplyr)
library(naniar)
library(DataExplorer)
library(ggplot2)
library(epitools)
library(lubridate)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(dplyr)
library(naniar)
library(DataExplorer)
library(ggplot2)
library(epitools)
library(lubridate)
library(tidyr)
library(plotly)
library(viridis)
library(reshape2)
library(tidyverse)
library(janitor)
library(showtext)
library(MetBrewer)
library(scico)
library(ggtext)
library(patchwork)
library(gghighlight)
library(readxl)
library(fmsb)
library(gganimate)
## 0. read data-----

allfiles <-  list.files(paste0(getwd(),"/data"))
pts_files <-allfiles[grepl("患者数据",list.files(paste0(getwd(),"/data")))]

pts_path <- paste0("data/","患者数据_",Sys.Date(),".xlsx")

if(file.exists(pts_path)){
  pts <- read_excel(pts_path)
}else{
  pts <- read_excel(paste0("data/",pts_files[length(pts_files)]))
  
}
 
info <- read_excel("data/信息表.xlsx")
feno <- read_excel("data/feno.xlsx") 
agreement <- read_excel("data/agreement.xlsx") %>% mutate(协议日期 = ymd(日期))
score <- read_excel("data/score.xlsx") %>% 
  select("序号",	"用户ID",	"提交答卷时间",	"所用时间",
         	"您的单位名称",	"总分",	"您的姓名",	"您的单位名称1"
)
QC <- read_excel("data/质控表.xlsx") 
map_province22 <- read_excel("data/map_province22.xlsx") 
cn_map <- download_map_data("https://code.highcharts.com/mapdata/countries/cn/custom/cn-all-sar-taiwan.js")
phaseI_top20 <- read_excel("data/phaseI_top20.xlsx") 

earliest_date <- "2023-01-18" 

pts_wk <- pts %>% 
  mutate(首诊时间 = as.Date(首诊时间),
         week = isoweek(ymd(首诊时间))
  )%>% 
  filter(首诊时间 >= as.Date(earliest_date)) %>% 
  filter(首诊时间 <= today()) %>% 
  mutate(是否提交= ifelse(病例状态=="暂存病例",0,1))

submit_status <- pts_wk %>% 
  group_by(所属医院) %>% 
  summarise(`累计病例数` = n(),
            `已提交(%)` = round(mean(是否提交)*100,2)
            )
  

latest_pts_date <- Sys.time()
enroll_tot <- pts %>% nrow()
enroll_hosp_tot <- pts$所属医院 %>% unique() %>% length()
##1.1 output1 每天入组情况 时序图---------

by_day <- pts_wk %>% 
  group_by(首诊时间) %>% 
  summarise(per_day_enrolled = n()) %>% 
  mutate(day_cum_pts = cumsum(per_day_enrolled),
         week = isoweek(首诊时间))


per_day_hosp_input <- table(pts_wk$首诊时间, pts_wk$所属医院) %>% data.frame() %>% 
  filter(Freq >0) %>% 
  select(Var1) %>% table() %>% data.frame() 

colnames(per_day_hosp_input) <- c("Var1","当日有录入医院数量")

per_day_hosp_input$Var1 <- as.Date(per_day_hosp_input$Var1)

per_day_hosp_input1 <- inner_join(by_day,per_day_hosp_input,by = c("首诊时间"="Var1"))

per_day_hosp_cum <- table(pts_wk$首诊时间,pts_wk$所属医院) %>% data.frame() %>% 
  filter(Freq >0) %>% 
  select(-Freq) %>% 
  group_by(Var2) %>% 
  mutate(number = row_number()) %>%
  filter(number ==1)


per_day_hosp_cum1 <- per_day_hosp_cum$Var1 %>% table() %>% data.frame() %>% 
  mutate(cumsum = cumsum(Freq)) 
  

colnames(per_day_hosp_cum1) <- c("首诊时间","当日新增中心数量","当日累计中心数量")

per_day_hosp_cum1$首诊时间 <- per_day_hosp_cum1$首诊时间 %>% as.Date()

predict_dat <- inner_join(per_day_hosp_input1,per_day_hosp_cum1,by =("首诊时间"))


date_dat <- data.frame(首诊时间 = seq(as.Date("2023-01-18"),today(),1))

predict_dat1 <- left_join(date_dat,predict_dat,c=("首诊时间")) %>% 
  mutate(week = isoweek(首诊时间)) %>% 
    mutate_at (c(2,3,5,6), replace_na, 0) %>% 
  mutate(当日累计中心数量 = cumsum(当日新增中心数量),
         当日有录入医院比例 = round(100*当日有录入医院数量/当日累计中心数量,2 )) %>% 
  select(首诊时间,week,当日新增中心数量,当日累计中心数量,当日有录入医院数量,当日有录入医院比例,per_day_enrolled,
          day_cum_pts)


colnames(predict_dat1) <- c("首诊时间","周","当日新增中心数量","当日累计中心数量",
                            "当日有录入医院数量","当日有录入医院比例","当日录入患者数量","当日累计患者数量")




  

rio::export(predict_dat1,file = "predict_dat1.xlsx")


  
  



  by_week <- table(pts_wk$week) %>% data.frame() %>% 
  mutate(wk_cum_pts = cumsum(Freq),
         week = as.character(Var1) %>% as.numeric())%>% 
  filter(week <= isoweek(today())) %>% 
  select(-Var1)

by_hosp <- table(pts_wk$所属医院) %>% data.frame() %>% 
  arrange(desc(Freq)) 

by_week_hosp <- table(pts_wk$所属医院,pts_wk$week) %>% data.frame() %>% 
  mutate(week_num = as.character(Var2) %>% as.numeric()) %>% 
  filter(week_num <= isoweek(today())) 
colnames(by_week_hosp) <- c("hosp","week","Freq","week_num")

pts_wk_new <- by_week_hosp %>% 
  group_by(week) %>% 
  arrange(week,desc(Freq)) %>% 
  mutate(rank = row_number()) %>%
  mutate(mylbl = Freq) %>% 
  filter(Freq>0) %>% 
  filter(rank <=10)

staticplot <- ggplot(pts_wk_new, aes(rank, group = hosp,
                                     fill = as.factor(hosp), color = as.factor(hosp))) +
  geom_tile(aes(y = Freq/2,
                height = Freq,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(hosp, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=mylbl,label = mylbl, hjust=0)) +
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
        plot.title=element_text(size=20, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=12, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=10, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(0.5,2, 0.5, 9, "cm"))


p_dynamic_plot <- staticplot + 
  transition_states(week, transition_length = 2, state_length = 0) +
  enter_fade() +
  exit_fade() + 
  ease_aes('linear')+
  labs(subtitle = '2023年第 : {closest_state} 周入组数排名(TOP10) '
       #subtitle  =  "Top 10 hospitals",
       #caption  = ""
  )

##stop----------------
print("------test1------")

# anim_save("p_dynamic_plot.gif")

colnames(by_week) <- c("wk_Freq", "wk_cum_pts", "week")

day_wk_time_series <- full_join(by_day,by_week,by=c('week'))%>% group_by(week) %>% 
  mutate(wk_cum_pts = ifelse(首诊时间 == max(首诊时间),wk_cum_pts,NA)) %>% 
  mutate(wk_Freq = ifelse(首诊时间 == max(首诊时间),wk_Freq,NA))


QC_fill <- tidyr::fill(QC, everything())

QC_n <- QC_fill %>%select(`区域经理`,`中心 (中心名称的填写请与数据录入系统保持一致）`,患者) %>% 
  unique() %>% group_by(`中心 (中心名称的填写请与数据录入系统保持一致）`) %>% 
  summarise(QCed = n()) 

##stop----------------
print("------test2------")

pts_wk_submitted <- pts_wk %>% filter(是否提交==1) 

hosp_cum <-  table(pts_wk_submitted$所属医院) %>% data.frame() %>% 
  left_join(QC_n,by=c("Var1"="中心 (中心名称的填写请与数据录入系统保持一致）")) 

QC_table <- left_join(hosp_cum,info %>% select(中心名称,区域经理),by=c("Var1"="中心名称")) %>% 
  arrange(.,desc(Freq)) %>% 
  mutate(QCed = ifelse(is.na(QCed),0,QCed),
         urgent = ifelse(Freq >=10 & QCed <10,1,0)) %>% 
  arrange(desc(urgent),QCed)


colnames(QC_table) <- c("中心名称", "已提交例数", "已质控例数", "区域经理", "urgent")


  
  QC_cat <- QC$数据问题分类 %>% na.omit() %>% table() %>% data.frame() %>% 
    mutate(QC_percent = round((100*Freq/length(na.omit(QC$数据问题分类))),2)
           )
  QC_cat1 <- QC_cat %>% 
    mutate(cat_percent = paste0(QC_cat$.,"(",QC_percent,"%)"))
  colnames(QC_cat1) <- c("cat",'Freq','Percent',"cat_percent")
  QC_cat1$cat <- factor(QC_cat1$cat)
  QC_cat1$cat_percent <- factor(QC_cat1$cat_percent)
  
  
  my_qc_cols <- c("#374E55", "#DF8F44", "#00A1D5",
               "#B24745", "#79AF97", "#6A6599", "#80796B")
  
  

  
  colnames(QC_cat1)[4] <- "name"
  colnames(QC_cat1)[2] <- "y"
  colnames(QC_cat1)[1] <- "cat"
p_QC_pie <-   highchart() %>%
    hc_chart(type = "pie") %>%
    hc_yAxis(categories = QC_cat1$cat_percent) %>%
    hc_add_series(data = QC_cat1) %>%
    hc_colors(my_qc_cols) %>%
    hc_tooltip(formatter = JS("function(){
                            return (
                             this.y + ' 人次<br/>' +
                             Highcharts.numberFormat(this.percentage, 2) + '%<br />' )
                             }")) 
 #%>%
 #   hc_title(text = "质控数据问题分类")
  
  



flex_time_series1 <- xts::xts(x = by_day$per_day_enrolled, order.by = by_day$首诊时间)

#######1. cumulative per day------
##stop----------------
print("------test3------")

# p_time_series_cumday <- highchart(type = "stock") %>%
#   hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
#   hc_add_series(day_wk_time_series$per_day_enrolled, type = "line") %>% 
#   hc_xAxis(categories = unique(day_wk_time_series$首诊时间))

p_time_series_cumday <- highchart() %>%
  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank",encoding = 'utf-8')) %>%
  hc_chart(type = 'line') %>%
  hc_series(   list(name = '每日新增入组人数', data =day_wk_time_series$per_day_enrolled, color = '#ff7518', dashStyle = 'shortDot', marker = list(symbol = 'triangle') ),
    
    list(name = '累计入组人数（天）', data =day_wk_time_series$day_cum_pts, color='#ff7518', marker = list(symbol = 'circle') )
           
             # list(name = '每周新增入组人数', data =day_wk_time_series$wk_Freq , color = 'red', marker = list(symbol = 'circle') ),
             # list(name = '累计入组人数（周）', data =day_wk_time_series$wk_cum_pts, color = 'red', dashStyle = 'shortDot', marker = list(symbol = 'triangle')  )
  ) %>%
  #hc_xAxis( categories = unique(flex_time_series$首诊时间) ) 

hc_xAxis_multiples(
  list(categories = unique(day_wk_time_series$首诊时间)),
  list(categories =day_wk_time_series$首诊时间[which(!is.na(day_wk_time_series$wk_cum_pts))])
)%>%

  hc_yAxis( title = list(text = "入组人数")
            #,
           # labels = list( format = "${value:,.0f} m")
            ) %>%
  hc_plotOptions(column = list(
    dataLabels = list(enabled = F),
    #stacking = "normal",
    enableMouseTracking = T ) 
  )%>%
  hc_tooltip(table = TRUE,
             sort = TRUE,
             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                   " {series.name}: {point.y} 人"),
             headerFormat = '<span style="font-size: 13px">Date {point.key}</span>'
  ) %>%
  hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )

### 2. cumulative week -------
##stop----------------
print("------test4------")
p_time_series_cumweek <- highchart() %>%
  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank",encoding = 'utf-8')) %>%
  hc_chart(type = 'line') %>%
  hc_series( 
  #list(name = '累计入组人数（天）', data =day_wk_time_series$day_cum_pts, color='orange', marker = list(symbol = 'circle') ),
  #            list(name = '每日新增入组人数', data =day_wk_time_series$per_day_enrolled, color = 'red', dashStyle = 'shortDot', marker = list(symbol = 'triangle') )
              list(name = '每周新增入组人数', data =day_wk_time_series$wk_Freq %>% na.omit(), color = 'red', marker = list(symbol = 'circle') ),
              list(name = '累计入组人数（周）', data =day_wk_time_series$wk_cum_pts%>% na.omit(), color = 'red', dashStyle = 'shortDot', marker = list(symbol = 'triangle')  )
  ) %>%
  #hc_xAxis( categories = unique(flex_time_series$首诊时间) ) 
  
  hc_xAxis( categories =day_wk_time_series$首诊时间[which(!is.na(day_wk_time_series$wk_cum_pts))]
  )%>%
  
  hc_yAxis( title = list(text = "入组人数")
            #,
            # labels = list( format = "${value:,.0f} m")
  ) %>%
  hc_plotOptions(column = list(
    dataLabels = list(enabled = F),
    #stacking = "normal",
    enableMouseTracking = T ) 
  )%>%
  hc_tooltip(table = TRUE,
             sort = TRUE,
             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                   " {series.name}: {point.y} 人"),
             headerFormat = '<span style="font-size: 13px">Date {point.key}</span>'
  ) %>%
  hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )


## 3. flex day ------
flex_time_series_perday <- dygraph(flex_time_series1) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dygraphs::dyRoller(rollPeriod = 1)

## 4.hosp rank all--------
##stop----------------
print("------test5------")

  
p_by_hosp <- hchart(by_hosp[c(1:40),], type = 'bar', hcaes( y = Freq, x = Var1),name='患者人数') %>% 
  hc_yAxis(title = list(text = "")) %>% 
  hc_xAxis( title = list(text = "")) %>% 
  hc_colors("orange") 
# %>% 
#   hc_tooltip(table = TRUE,
#              sort = TRUE,
#              pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
#                                    " {point.Freq} 人"),
#              headerFormat = '<span style="font-size: 13px"> {point.key}</span>'
#   )
  

# p_by_hosp <- plot_ly(by_hosp,height=800) %>% add_bars(x = ~Freq, 
#                                            y = ~forcats::fct_reorder(Var1, Freq),
#                                            hoverinfo = "x",
#                                            color = 'orange') %>%
#   
#   layout(
#     barmode = "overlay",
#     plot_bgcolor = "#F4F5F1",
#     xaxis = list(title = "入组患者数"),
#     yaxis = list(title = "")
#   ) 


##5 top 10--------
top10_hosp_tot <- table(pts_wk$所属医院) %>% data.frame() %>% 
  mutate(rank =rank(desc(Freq)) %>% floor()) %>% 
  arrange(.,rank) %>% filter(rank <=10)

top10_center <- table(pts_wk$所属医院,pts_wk$week) %>% data.frame() %>% 
  filter(Var1 %in% top10_hosp_tot$Var1) %>% 
  mutate(week_num = as.character(Var2) %>% as.numeric())
colnames(top10_center) <- c("Center","week","Freq","week_num")

top10_center <- top10_center %>% select(Center,week_num,Freq)

fstenroll <- pts_wk  %>%group_by(所属医院) %>% 
  mutate( 首例入组时间 = min(首诊时间) )%>%  
  select(所属医院,首例入组时间)%>%unique() %>% 
  mutate(duration= today()-首例入组时间)
##--------center_rank--------
##stop----------------
print("------test6------")


by_hosp_total <- table(pts_wk_submitted$所属医院) %>% data.frame() %>% 
  inner_join(fstenroll,by=c("Var1"="所属医院")) %>% 
  mutate(day_avg =Freq/as.numeric(duration),
         week_avg = Freq/ (as.numeric(duration)/7)) %>% 
  mutate(week_avg = ifelse( (as.numeric(duration) < 7 & (week_avg>Freq)),Freq,week_avg)) %>% 
  mutate(enrollment_rank = rank(desc(day_avg))) %>% 
  arrange(.,enrollment_rank) %>% 
  mutate(每周入组人数得分 = percentile(day_avg)*0.1) 


center_rank <- full_join(by_hosp_total,phaseI_top20,by=c("Var1"="一期优秀中心名称")) %>% 
  mutate(是否重点单位得分 = ifelse(is.na(score2),0,score2),
         PM打分 = NA,
         质控合格率得分=NA) %>% 
  select(-score2,-enrollment_rank,-首例入组时间)
colnames(center_rank) <- c("中心名称","已提交病例数","首例提交至今(天)","天平均入组人数","平均每周提交病例数","每周入组人数得分","是否重点单位得分","PM打分","质控合格率得分")

center_rank_final <- left_join(center_rank,info %>% select(中心名称,区域经理),by=c("中心名称")) %>% 
  mutate(每周入组人数得分 = ifelse(is.na(平均每周提交病例数),0,每周入组人数得分)) %>% 
  arrange(.,desc(是否重点单位得分),desc(每周入组人数得分)) %>% 
  select("中心名称", "已提交病例数","首例提交至今(天)","天平均入组人数","平均每周提交病例数", "每周入组人数得分", "是否重点单位得分", 
         "区域经理","PM打分", "质控合格率得分")  %>% 
  mutate(天平均入组人数 = round(天平均入组人数,2),
         平均每周提交病例数 = round(平均每周提交病例数,2)) %>% 
  mutate(总分= (ifelse(is.na(每周入组人数得分),0,每周入组人数得分)+
                 ifelse(is.na(是否重点单位得分),0,是否重点单位得分)+
                ifelse(is.na(PM打分),0,PM打分)+
                ifelse(is.na(质控合格率得分),0,质控合格率得分)
                ))

center_rank_final1 <- left_join(center_rank_final,submit_status,by=c("中心名称"="所属医院")) %>% 
  select(c("中心名称", "累计病例数","已提交病例数", "已提交(%)","首例提交至今(天)",
           #"天平均入组人数", 
           "平均每周提交病例数", "每周入组人数得分", "是否重点单位得分", "区域经理", 
           "PM打分", "质控合格率得分", "总分"))

week_mean <- ( na.omit(center_rank_final1$平均每周提交病例数) %>% mean() )%>% round(1)

##stop----------------
print("------test7------")
top10_center1 <- left_join(top10_center,fstenroll,by=c("Center"="所属医院")) 

top_order <- top10_center %>% group_by(Center)%>%
  summarise(total_n=sum(Freq)) %>%
  arrange(.,desc(total_n))

top2 <- top10_center %>% group_by(Center) %>%
  slice_max(week_num)

top_label <- top10_center %>%
  mutate(Freq = ifelse(Freq==0,NA,Freq))

print("------test70------")
font <- "Gudea"
print("------test8------")
# font_add_google(family=font, font, db_cache = TRUE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
#theme_set(theme_minimal(base_family = font, base_size = 10))
bg <- "#F4F5F1"
  txt_col <- "black"
    
  print("------test9------") 
  p_top10_time_series <-   top10_center1 %>%
    ggplot() +
    geom_hline(yintercept = 10,linetype="dashed", linewidth=.25,color='grey') +
    geom_point(data=top2,
               aes(x=week_num, y=Freq, color=Center),shape=16,size=2) +
    geom_line(aes(x=week_num, y=Freq, color=Center)) +
    gghighlight(use_direct_label = FALSE,
                unhighlighted_params = list(colour = alpha("grey85", 1)))+
    # geom_text(data=top2,
    #           aes(x=week_num, y=Freq, color=Center, label = Freq),
    #           hjust = -.5, vjust = .5, size=3, family=font, fontface="bold")+
    scale_color_met_d(name="Redon") +
    
    scale_x_continuous(breaks = seq(3,15,2),
                       labels = seq(3,15,2)
    )+
    facet_wrap(~  factor(Center,levels =top_order$Center ),nrow=3)+
    coord_cartesian(clip = "off")+
    theme(
      axis.title = element_blank(),
      axis.text = element_text(color=txt_col, size=10),
      strip.text.x = element_text(face="bold"),
      plot.title = element_markdown(hjust=.5,size=34, color=txt_col,lineheight=.8, face="bold", margin=margin(20,0,30,0)),
      plot.subtitle = element_markdown(hjust=.5,size=18, color=txt_col,lineheight = 1, margin=margin(10,0,30,0)),
      plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color=txt_col, lineheight = 1.2),
      plot.caption.position = "plot",
      plot.background = element_rect(color=bg, fill=bg),
      plot.margin = margin(10,10,10,10),
      legend.position = "none",
      legend.title = element_text(face="bold")
    )
  
  
  #ggplotly(p1,height = 600,width = 1000) %>% layout(margin = list(l=100))
##stop----------------
print("------test10------")

## output 区域经理 pm11-----
tb <- left_join(pts,info,by=c("所属医院"="中心名称"))

t10 <- table(info$区域经理)

t11 <- table(tb$区域经理)

tb1 <- tb %>% select(`所属医院`,`区域经理`) %>% unique()

t12 <-  table(tb1$区域经理)

pm <- data.frame(t10,t11, t12)[,c(1,2,6,4)] 

pm <- data.frame(t10,t11, t12)[,c(2,6,4)] %>% as.list() 

pm_a <- data.frame(t10,t11, t12)[,c(1,2,6,4)] 
pm_b <- lapply(pm, sum) %>% as.data.frame()
pm11 <- bind_rows(pm_a,pm_b) %>% data.frame() %>% 
  mutate(Var1 = as.character(Var1))
pm11[6,1] <- '总计'

colnames(pm11)   <- c("PM","负责中心数量","已启动中心数","入组人数")

pm11 <- pm11 %>% 
  mutate(`启动医院比例(%)` = round(100*`已启动中心数`/`负责中心数量`,2),
         `平均各中心入组人数` = round(`入组人数`/`已启动中心数`,2)) %>% 
  mutate(`已启动/负责中心数量`=paste0(`已启动中心数`,"/",`负责中心数量`)) %>% 
  select(PM,`已启动/负责中心数量`,`启动医院比例(%)`,`入组人数`,`平均各中心入组人数`)

## province--------
##stop----------------
print("------test11------")

tb2 <- tb %>% select(`所属医院`,`省份`) %>% unique()

tb20 <- table(info$省份) %>% data.frame()

t21 <- table(tb2$省份) %>% data.frame()

t22 <- tb$省份 %>% table()

province <- data.frame(t21,t22)[,c(1,2,4)]

province1 <- left_join(tb20,province,by=c("Var1")) 

colnames(province1) <- c("省份","中心数量","已入中心数","患者数量")

province2 <- province1 %>% 
  mutate(已入中心比例 = ifelse(is.na(round(100*`已入中心数`/`中心数量`,2)),0,
                         round(100*`已入中心数`/`中心数量`,2)),
         
         已入中心数 = ifelse(is.na(已入中心数),0,已入中心数),
         患者数量 = ifelse(is.na(患者数量),0,患者数量)) %>% 
  select(省份,中心数量,已入中心数,已入中心比例,患者数量)

pm_province <- table(info$省份,info$区域经理)

PROVINCE <- rownames(pm_province)

pm_province1 <- cbind(PROVINCE,pm_province) %>% data.frame()

province_final <- left_join(pm_province1,province2,by=c("PROVINCE"="省份")) %>% 
  arrange(., desc(已入中心比例))

province_summary <- apply(apply(province_final,2,as.numeric),2,sum) %>% t() %>% as.data.frame()

province_summary$PROVINCE <- province_summary$PROVINCE %>% as.character()

province_summary[1,1] <- '总计'


province_final1 <- rbind(province_final,province_summary)
province_final1[23,9] <-  round(100*province_final1[23,8]/province_final1[23,7],2)


##---province map

map_stats <- full_join(province_final1,map_province22,by=c("PROVINCE"="province_cn")) %>% 
  filter(PROVINCE !="总计")

#############functions----------
##stop----------------
print("------test12------")


dropdownMenuCustom <-     function (..., type = c("messages", "notifications", "tasks"), 
                                    badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence) 
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- tags$span(class = paste0("label label-", badgeStatus), 
                       numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}


customSentence <- function(numItems, type) {
  paste("Feedback & suggestions")
}


customSentence_share <- function(numItems, type) {
  paste("Love it? Share it!")
}

VB_style <- function(msg = 'Hello', style="font-size: 100%;"){
  tags$p( msg , style = style )
}

##stop----------------
print("------test13------")
## map----------

# library("rjson")
# json_file <- "https://code.highcharts.com/mapdata/countries/cn/custom/cn-all-sar-taiwan.geo.json"
# json_data <- fromJSON(paste(readLines(json_file), collapse=""))
# 
# 
# province_name <- NULL
# for( i in c(1: length(json_data$features ))){
#   
#   province_name_tmp <- json_data$features[[i]]$properties$`woe-name`
#   
#   province_name <- cbind(province_name_tmp,province_name)
# }
# 
# province_name <- province_name %>% data.frame() %>% t()%>% data.frame() 

color_n <- 5

stops1 <- data.frame(
  q = 0:color_n / color_n,
  c = c(  "#FCDACAFF", "#FFB2B2FF" ,"#E57E7EFF", "#CC5151FF" ,"#B22C2CFF" ,"#990F0FFF"),
  

  stringsAsFactors = FALSE
)

stops2 <- data.frame(
  q = 0:color_n / color_n,
  c = c("#FFCA99FF", "#FFAD66FF", "#FFAD66FF","#FF8F33FF", "#CC5500FF"  ,"#FF6E00FF"),
  
  
  stringsAsFactors = FALSE
)

stops1 <- list_parse2(stops1)

stops2 <- list_parse2(stops2)

p_map_province_ratio <- highchart() %>%
  hc_add_series_map(
    cn_map, map_stats, value = "已入中心比例", joinBy = c('woe-name','province_en'),
    name = "已入中心比例"
  )  %>% 
  hc_colorAxis(stops =stops1) %>% 
 # hc_title(text = "各省份中心启动比例") %>% 
  hc_subtitle(text = "中心启动比例") 


p_map_province_pts <- highchart() %>%
  hc_add_series_map(
    cn_map, map_stats, value = "患者数量", joinBy = c('woe-name','province_en'),
    name = "已入患者数量"
  )  %>% 
  hc_colorAxis(stops =stops2) %>% 
  #hc_title(text = "各省份入组患者数量") %>% 
  hc_subtitle(text = "入组患者总数")

##-----------last----------
print('part a')

#### follow-up1 ------------
library(lubridate)

pts_followup <- pts_wk %>% 
  mutate(复诊1超窗 = case_when(实际复诊时间 <= 预计结束复诊时间 ~"未超窗",
                        (预计结束复诊时间<Sys.Date() & is.na(实际复诊时间)) ~ "超窗",
                        TRUE ~NA)
       
                        
  ) %>% 
  select("所属医院", "姓名", "性别", "年龄",  "首诊时间", "预计复诊时间", "预计开始复诊时间", 
               "预计结束复诊时间", "实际复诊时间", "复诊1超窗","预计复诊2时间", "预计开始复诊2时间", 
               "预计结束复诊2时间", "实际复诊2时间", "当前病历", "病例状态", 
               "week", "是否提交" )%>% 
  arrange(desc(复诊1超窗))




  # filter(within_2wk == TRUE) %>% 
  # group_by(所属医院) %>% 
  # summarise(未来两周需复诊人数 = n()) %>% 
  # arrange(desc(未来两周需复诊人数))

# follow_up1_pts_day <- pts_followup %>% group_by(fu1_low) %>% 
#   mutate(需复诊人数 = n()) %>% 
#   select(fu1_low,需复诊人数) %>% unique()


### best case: 24 days;
### worse case: 54 days;
### m: 39 days;
## 
tO <-  24
tM <-  39
tP <-  54
tE <-  (tO+4*tM+tP)/6 
sigma <- (tP-tO)/6 

1/ ((tE + sigma)/(108-58))

3/((tE - sigma)/(108-58))




predict_dat1
T_opt = data.frame(首诊时间 =
                      seq(predict_dat1$首诊时间[nrow(predict_dat1)]+1,as.Date("2023-09-30"),'day'),
                      当日新增中心数量 = c(rep(2,25),1,
                                   rep(0,length(seq(predict_dat1$首诊时间[nrow(predict_dat1)]+1,as.Date("2023-09-30"),'day'))-26
                                                 
                                                 )))

predict_dat1
T_opt = data.frame(首诊时间 =
                     seq(predict_dat1$首诊时间[nrow(predict_dat1)]+1,as.Date("2023-09-30"),'day'),
                   当日新增中心数量 = c(rep(2,25),1,
                                rep(0,length(seq(predict_dat1$首诊时间[nrow(predict_dat1)]+1,as.Date("2023-09-30"),'day'))-26
                                    
                                )))







hosp_input_ratio <- 0.4
hosp_input_pts_per_day <- 1

tmp <- full_join(bind_rows(predict_dat1,T_opt),
                 follow_up1_pts_day,by=c("首诊时间"="fu1_low")) %>% 
  mutate(周 = isoweek(首诊时间),
         当日累计中心数量 = cumsum(当日新增中心数量),
         当日有录入医院数量 = ifelse(is.na(当日有录入医院数量),
                            round(当日累计中心数量*hosp_input_ratio),当日有录入医院数量),
         当日录入患者数量 = ifelse(is.na(当日录入患者数量),round(当日有录入医院数量*hosp_input_pts_per_day),当日录入患者数量),
         当日累计患者数量 = cumsum(当日录入患者数量),
         当日累计患者数量_考虑复诊 = cumsum(当日录入患者数量-ifelse(is.na(需复诊人数),0,需复诊人数))
         )
  

#######weekly updates----------

agreement <- agreement %>% mutate(协议日期 = ymd(日期))


#(feno$中心名称 %in% info$中心名称)%>% table()

info_feno <- left_join(info,feno,by=c("中心名称"="中心名称"))%>% 
  select(id,区域经理,中心名称,计划发货时间,实际发货时间,预计到货时间,是否完成装机,备注)

#########每次更新日期即可
#table(agreement$参加单位名称 %in% info$中心名称)


info_agreement <- left_join(info,agreement,by=c("中心名称"="参加单位名称"))%>% 
  select(中心名称,协议日期)


info_agreement_fstenroll <- left_join(info_agreement,fstenroll,by=c("中心名称"="所属医院") )%>% 
  mutate(首例入组时间大于协议时间 = 首例入组时间 >= 协议日期 )



score1 <- score  %>% select(您的单位名称1,总分)  
score2 <- score1 %>%
  pivot_wider(names_from = 您的单位名称1, values_from = 总分) %>%t() %>% data.frame()

score2$单位名称 <- rownames(score2)

info_agreement_fstenroll_crcscore <- left_join(info_agreement_fstenroll,score2,by=c("中心名称"="单位名称"))


weekly_updates <- left_join(info_feno, 
                        info_agreement_fstenroll_crcscore,
                        by="中心名称") %>% select(-duration)

colnames(weekly_updates) <- c("id", "区域经理", "中心名称", "计划发货时间", "实际发货时间", 
                              "预计到货时间", "是否完成装机", "备注", "协议日期", "首例入组时间", 
                              "首例入组时间大于协议时间", "CRC考核分数")
  


# a = pts$所属医院 %>% table() %>% data.frame()
# b = pts_wk_submitted$所属医院 %>% table() %>% data.frame()
# 
# a$.[ !a$. %in% b$.]


