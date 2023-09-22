## 患者地图-------------
library(echarts4r)
library(echarts4r.maps)
map_out <- map_dat |>
  e_charts(prov1) |> # 区域
  em_map("China") |>
  e_map(Freq.y, map = "China") |>
  e_visual_map(Freq.y,color = c('#08519c','#3182bd','#6baed6','#eff3ff')) |>
  e_color('blue') |>
  e_labels(fontSize = 12, fontWeight='bold')


htmlwidgets::saveWidget(widget = map_out, file = "map_out.html")

# I had to set this for webshot2 to work on Ubuntu 20.04 LTS
# Sys.setenv(CHROMOTE_CHROME = '/snap/bin/chromium')
#  use 'Sys.which("chromium")' to get your path.

#webshot('map_out.html', file = 'map_out.png')

by_region_summary <- by_region_dat %>% select(省,Freq) %>% 
  mutate(`占比(%)`= round(Freq/nrow(asthmadat)*100,2) )%>% 
  rlang::set_names('省份','患者例数(N)',"比例(%)") %>% 
  arrange(desc(`患者例数(N)`))

rio::export(by_region_summary, file = "output/by_region_summary.xlsx")
## treatment summary----

treatment_summary <- sub("[\\[].*",'',treatment_out[,1]) %>% str_remove('[:：\\]]') %>% table() %>% 
  data.frame() %>% arrange(desc(Freq)) %>% 
  mutate(`占比(%)`= round(Freq/nrow(asthmadat)*100,2) )%>% 
  rlang::set_names('药物名称','患者例数(N)',"比例(%)")

rio::export(treatment_summary, file = "output/treatment_summary.xlsx")


##comorbidity----
comorbidity_summary <- sub("[\\[].*",'',comorbidity_out[,1]) %>% str_remove('[:：\\]]') %>% table() %>% 
  data.frame() %>% arrange(desc(Freq))%>% 
  mutate(`占比(%)`= round(Freq/nrow(asthmadat)*100,2) ) %>% 
  rlang::set_names('合并症名称','患者例数(N)',"比例(%)")

rio::export(comorbidity_summary, file = "output/comorbidity_summary.xlsx")


##tableone----------

factor_vars <- c("就诊类型","性别","年龄分层","民族",  "教育程度", "区域",  
                 "吸烟状况",  
                 "吸烟指数",  "香烟烟雾（二手烟）", "屋尘螨/粉尘螨", "污染空气（患者自评居住地空气质量差）", 
                 "宠物饲养", "牲畜饲养", "发霉物质（墙壁等）", "煤炭", "生物燃料烟雾",
                 "首发症状", "主要症状", 
                 "鼻部相关症状", "咽喉部相关症状", "反流相关症状", "是否有季节性", 
                 "春","夏","秋","冬","咳嗽", "喘息", "气促/呼吸不畅", 
                 "胸闷","儿童时期(≤14岁)是否诊断哮喘","鼻窦炎", "鼻息肉", 
                 "高血压", "骨质疏松症", "冠心病", "过敏性鼻炎", "过敏性结膜炎", 
                 "甲状腺相关疾病", "慢性阻塞性肺疾病", "其他", "湿疹", "睡眠呼吸暂停综合征", 
                 "糖尿病", "特应性皮炎", "胃食管反流病", "无其他合并症", "心、脑血管性疾病", 
                 "荨麻疹", "支气管扩张",'哮喘急性发作次数',"哮喘急性导致就诊3","哮喘急性导致系统激素使用3",
                 "急性发作导致住院次数3","急性发作导致入住ICU","急性发作最难缓解症状",
                 "未使用任何抗哮喘药物", "吸入激素/长效β受体激动剂（ICS/LABA）", 
                 "苏黄、中药/中成药（汤剂/复方制剂等）", "白三烯受体拮抗剂(如孟鲁斯特)", 
                 "茶碱", "SABA（短效β2受体激动剂，如沙丁胺醇）", "吸入激素（ICS）", 
                 "口服激素", "吸入激素/长效β受体激动剂/长效毒碱受体持抗剂(ICS/LABA/LAMA)", 
                 "吸入激素/长效B受体激动剂/长效抗胆碱药(ICS/LABA/LAMA)", "生物制剂",
                 "急性呼吸道感染(感冒、支气管炎等)", "天气变化", "无明显诱因", 
                 "空气污染(空气质量不好)", "尘螨(屋尘螨、粉尘螨)", "蒿草、花粉、柳絮", 
                 "宠物毛发及皮屑", "停用治疗药物", "虾蟹等食物","疾病诊断","严重程度评估")


num_vars <- c("年龄", "BMI",   "病程.月.", 
              "FEV1预计值", "FEV1实测值", "FEV1预计百分比", "FVC预计值", "FVC实测值", 
              "FVC预计百分比", "FEV1/FVC预计值", "FEV1/FVC实测值", "FEV1/FVC预计百分比", 
              "MMEF预计值", "MMEF实测值", "MMEF预计百分比", "MEF75预计值", 
              "MEF75实测值", "MEF75预计百分比", "MEF25预计值", "MEF25实测值", 
              "MEF25预计百分比", "MEF25-75预计值", "MEF25-75实测值", "MEF25-75预计百分比", 
              "MEF50预计值", "MEF50实测值", "MEF50预计百分比", "PEF预计值", 
              "PEF实测值", "PEF预计百分比", "咳嗽VAS评分", 
              "嗜酸性粒细胞计数", "嗜酸性粒细胞百分比", "中性粒细胞计数", 
              "中性粒细胞百分比" ,
               "呼出气一氧化氮"
              )


varstoNum <- num_vars
workdat1[varstoNum] <- lapply(workdat1[varstoNum], as.numeric)

varstoFct <- factor_vars
workdat1[varstoFct] <- lapply(workdat1[varstoFct], as.factor)


vars <- c("就诊类型","年龄","年龄分层","性别", "BMI","教育程度","区域", "民族",
          "病程.月.", "吸烟状况",  "吸烟指数",
          "香烟烟雾（二手烟）", "屋尘螨/粉尘螨", "污染空气（患者自评居住地空气质量差）", 
          "宠物饲养", "牲畜饲养", "发霉物质（墙壁等）", "煤炭", 
          "生物燃料烟雾",
          "首发症状", "主要症状","咳嗽", "喘息", "气促/呼吸不畅", 
          "胸闷","咳嗽VAS评分", 
          "鼻部相关症状", "咽喉部相关症状", "反流相关症状",
          "儿童时期(≤14岁)是否诊断哮喘","鼻窦炎", "鼻息肉", 
          "高血压", "骨质疏松症", "冠心病", "过敏性鼻炎", "过敏性结膜炎", 
          "甲状腺相关疾病", "慢性阻塞性肺疾病", "其他", "湿疹", "睡眠呼吸暂停综合征", 
          "糖尿病", "特应性皮炎", "胃食管反流病", "无其他合并症", "心、脑血管性疾病", 
          "荨麻疹", "支气管扩张",
          '哮喘急性发作次数',"哮喘急性导致就诊3","哮喘急性导致系统激素使用3",
          "急性发作导致住院次数3","急性发作导致入住ICU","急性发作最难缓解症状", "是否有季节性", 
          "春","夏","秋","冬", 
          #"FEV1预计值", 
          "FEV1实测值", "FEV1预计百分比", 
          #"FVC预计值", 
          "FVC实测值", 
          "FVC预计百分比", 
          # "FEV1/FVC预计值",
          "FEV1/FVC实测值", "FEV1/FVC预计百分比", 
         "MMEF预计值", "MMEF实测值", "MMEF预计百分比", "MEF75预计值",
         "MEF75实测值", "MEF75预计百分比", "MEF25预计值", "MEF25实测值",
         "MEF25预计百分比", "MEF25-75预计值", "MEF25-75实测值", "MEF25-75预计百分比",
         "MEF50预计值", "MEF50实测值", "MEF50预计百分比", "PEF预计值",
         "PEF实测值", "PEF预计百分比",
          "嗜酸性粒细胞计数", "嗜酸性粒细胞百分比", "中性粒细胞计数", 
          "中性粒细胞百分比", "呼出气一氧化氮",
         "未使用任何抗哮喘药物", "吸入激素/长效β受体激动剂（ICS/LABA）", 
         "苏黄、中药/中成药（汤剂/复方制剂等）", "白三烯受体拮抗剂(如孟鲁斯特)", 
         "茶碱", "SABA（短效β2受体激动剂，如沙丁胺醇）", "吸入激素（ICS）", 
         "口服激素", "吸入激素/长效β受体激动剂/长效毒碱受体持抗剂(ICS/LABA/LAMA)", 
         "吸入激素/长效B受体激动剂/长效抗胆碱药(ICS/LABA/LAMA)", "生物制剂",
         "急性呼吸道感染(感冒、支气管炎等)", "天气变化", "无明显诱因", 
         "空气污染(空气质量不好)", "尘螨(屋尘螨、粉尘螨)", "蒿草、花粉、柳絮", 
         "宠物毛发及皮屑", "停用治疗药物", "虾蟹等食物","疾病诊断","严重程度评估"
        
         )




library(tableone)

tmp <- CreateTableOne(vars = vars,factorVars=factor_vars, 
                             data = workdat1)


tmp1 <- print(tmp, nonnormal = num_vars, 
                        quote = FALSE, noSpaces = TRUE, 
                        printToggle = FALSE,showAllLevels = TRUE)

tmp1

#rio::export(tmp1, file = "output/tmp1.xlsx",rowNames=TRUE)


library(DescTools)

num_vars1 <- c("年龄", "BMI",   "病程.月.", 
                      # "FEV1预计值", 
                      "FEV1实测值", "FEV1预计百分比", 
                      # "FVC预计值", 
                      "FVC实测值", 
                      "FVC预计百分比", 
                      # "FEV1/FVC预计值", 
                      "FEV1/FVC实测值", "FEV1/FVC预计百分比", 
                      # "MMEF预计值", "MMEF实测值", "MMEF预计百分比", "MEF75预计值", 
                      "MEF75实测值", "MEF75预计百分比", "MEF25预计值", "MEF25实测值", 
                      # "MEF25预计百分比", "MEF25-75预计值", "MEF25-75实测值", "MEF25-75预计百分比", 
                      # "MEF50预计值", "MEF50实测值", "MEF50预计百分比", "PEF预计值", 
                      #"PEF实测值", "PEF预计百分比", 
                      "咳嗽VAS评分", 
                      "嗜酸性粒细胞计数", "嗜酸性粒细胞百分比", "中性粒细胞计数", 
                      "中性粒细胞百分比" ,
                       "呼出气一氧化氮"
)

rio::export(workdat1,'output/workdat1.xlsx')








