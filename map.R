library(readxl)
library(dplyr)
library(REmap)
info <- read_excel("E:/2023/0324asthmaregistry/asthmaapp0417/data/信息表.xlsx")
pts <- read_excel("E:/2023/0324asthmaregistry/asthmaapp0417/data/患者数据_2023-06-05.xlsx")
mydata <- read_excel("E:/2023/0324asthmaregistry/asthmaapp0417/外省广附一.xlsx") %>% 
  as.data.frame()

mydata1 <- mydata %>% as.data.frame()


outside_dat <- 
  full_join(pts,info,by=c("所属医院"="中心名称")) %>% 
  mutate(本省就诊 = 省==中心所在省) %>% 
  filter(本省就诊 == FALSE) %>% 
  filter(所属医院=="广州医科大学附属第一医院") %>% 
  group_by(省) %>% 
  summarise(N = n() )



outside_dat1 <- outside_dat %>% 
  mutate(N = as.numeric(N)*100) %>% 
  as.data.frame()


origin <- rep("广州",nrow(outside_dat))

destination <- c("云南","内蒙古",
                 "吉林",
                 "四川",
                 "安徽",
                 "山东",
                 "山西",
                 "广西",
                 "新疆",
                 "江苏",
                 "江西",
                 "河南",
                 "海南",
                 "湖北",
                 "湖南",
                 "甘肃",
                 "贵州",
                 "辽宁",
                 "黑龙江"
)

line_data <- data.frame(origin,destination)

chinaIphone,mydata,by=c("V1","")

tmp <- rbind(demoC,line_data) %>% filter(origin=="广州")

remapC(chinaIphone,
       markLineData = demoC,
       markPointData = demoC[,2])

map_out1 <- remapC(,
                   maptype = "china",
                   #  title="",
                   theme = get_theme("Dark"),
                  # color=c("#CD0000","#FFEC8B"),
                     
                   
                   markLineData=line_data,   
                   
                   markLineTheme=markLineControl(
                      # color="white",
                       lineWidth=c(rep(2,10),rep(3,9)),
                       lineType="dashed"
                     ),
                   
                   markPointData=line_data[,2],
                   

                   markPointTheme=markPointControl(
                     symbolSize=13,
                     effect=T,
                     effectType="scale"
                     #,
                     #color="white"
                   )
)   


origin <- rep("广州",nrow(outside_dat))

destination <- c("昆明",
                 "乌兰浩特",
                 "长春",
                 "成都",
                 "安庆",
                 "青岛",
                 "运城",
                 "南宁",
                 "乌鲁木齐",
                 "南京",
                 "宜春",
                 "郑州",
                 "海口",
                 "武汉",
                 "长沙",
                 "兰州",
                 "贵阳",
                 "丹东",
                 "哈尔滨"
)

heatmap_dat <- read_excel("E:/2023/0324asthmaregistry/tmp.xlsx") %>% as.data.frame()

tmp <- data.frame(origin =destination,
                  destination = origin )

#full_join(chinaIphone,mydata,by=c("V1"="出发地"))

tmp1 <- rbind(demo,tmp) %>% filter(destination=="广州")
remapC(heatmap_dat,
     #  theme = get_theme("None"),
     theme = get_theme('none',backgroundColor = '#1b1b1b',
                       titleColor = "#1b1b1b",
                       pointShow = T),
        color=c("#FF8F33","#fff"),
       markLineData = tmp1,
       
       markLineTheme=markLineControl(
        # color="orange",
         symbolSize = c(0,0),
         lineWidth=2
         ,
         lineType="dashed"
       ),
       markPointData = tmp1[,2],
       
       markPointTheme=markPointControl(
         symbolSize=5,
         #symbol = 'star',
         effect=T,
         effectType="scale"
        # ,
        # color="white"
       )
       
       )

)
