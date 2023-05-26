

options(remap.ak="aIqvBLop7fLREi8Obe6ljrWimIZ3IQ0H")

library(devtools)   #工具包
devtools::install_github('lchiffon/REmap') 
library("REmap")

get_city_coord("Shanghai")    #get_city_coord 获取一个城市的经纬度
city_vec=c("北京","Shanghai","广州")
get_geo_position(city_vec)    #get_geo_position  获取一个城市向量式的经纬度

remap(demoC %>% select(destination,origin),
      title="REmap",           #标题
      subtitle ="theme:dark",  #副标题
      theme=get_theme("dark")  #主题，背景颜色
)


destination = rep("北京",10)
origin = c('上海','北京','大连','南宁','南昌',
                '拉萨','长春','包头','重庆','常州')
dat = data.frame(origin,destination)
out = remap(dat,title = "REmap实例数据",subtitle = "theme:Dark")
out

outside_dat <- local_table <- full_join(pts,info,by=c("所属医院"="中心名称")) %>% 
  mutate(本省就诊 = 省==中心所在省) %>% 
  filter(本省就诊 == FALSE) %>% 
  filter(所属医院=="广州医科大学附属第一医院")


origin = unique(outside_dat$省)
destination = rep("广州",length(origin))

get_geo_position(origin)  

loc_dat = data.frame(origin,destination)
rio::export(loc_dat,file="data/loc_dat.xlsx")

out = remap(dat,title = "REmap实例数据",subtitle = "theme:Dark")  
out = remap(dat,title = "REmap实例数据",subtitle = "theme:Dark",
            theme = get_theme("None",
                              lineColor = "orange"))
plot(out)

 tmp1 <- table(outside_dat$`省`) %>% data.frame()

  remapB(title = "Bmap迁徙图示例",
       color = "Blue",
       markLineData = tmp1,       #标记线数据
       markPointData = tmp1[,2]  #标记点数据
)

remapC(chinaIphone,
    
       title = "remapC实例地图",
       theme = get_theme('none',
                         backgroundColor = '#fff',
                         titleColor = "#1b1b1b",
                         pointShow = T),
       max = 2000)
library(readxl)
loc_dat <- read_excel("E:/2023/0324asthmaregistry/asthmaapp0417/data/loc_dat.xlsx")
out = remap(dat,#title = "REmap实例数据",subtitle = "theme:Dark",
            theme = get_theme("Dark")) %>% 
  remapC(chinaIphone)

options(remap.js.web = T)

