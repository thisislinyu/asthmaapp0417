

factor_vars <- c("年龄分层","民族",  "教育程度", "区域",  
                 "吸烟状况",  
                 "吸烟指数",  "首发症状", "主要症状", 
                 "鼻部相关症状", "咽喉部相关症状", "反流相关症状", "是否有季节性", 
                 "春","夏","秋","冬","咳嗽", "喘息", "气促/呼吸不畅", 
                 "胸闷",'哮喘急性发作次数',"哮喘急性导致就诊","哮喘急性导致系统激素使用",
                 "急性发作导致住院次数","急性发作导致入住ICU","急性发作最难缓解症状")


num_vars <- c("年龄", "BMI",   "病程.月.", 
              "FEV1预计值", "FEV1实测值", "FEV1预计百分比", "FVC预计值", "FVC实测值", 
              "FVC预计百分比", "FEV1/FVC预计值", "FEV1/FVC实测值", "FEV1/FVC预计百分比", 
              "MMEF预计值", "MMEF实测值", "MMEF预计百分比", "MEF75预计值", 
              "MEF75实测值", "MEF75预计百分比", "MEF25预计值", "MEF25实测值", 
              "MEF25预计百分比", "MEF25-75预计值", "MEF25-75实测值", "MEF25-75预计百分比", 
              "MEF50预计值", "MEF50实测值", "MEF50预计百分比", "PEF预计值", 
              "PEF实测值", "PEF预计百分比", "咳嗽VAS评分", 
              "嗜酸性粒细胞计数", "嗜酸性粒细胞百分比", "中性粒细胞计数", 
              "中性粒细胞百分比", "呼出气一氧化氮")


varstoNum <- c("BMI","咳嗽VAS评分","嗜酸性粒细胞计数",
               '嗜酸性粒细胞百分比','中性粒细胞计数','中性粒细胞百分比','呼出气一氧化氮')
workdat0801[varstoNum] <- lapply(workdat0801[varstoNum], as.numeric)

varstoFct <- factor_vars
workdat0801[varstoFct] <- lapply(workdat0801[varstoFct], as.factor)


vars <- c("年龄","年龄分层", "BMI","教育程度","区域", "民族","病程.月.", "吸烟状况",  "吸烟指数", "首发症状", "主要症状","咳嗽", "喘息", "气促/呼吸不畅", 
          "胸闷","咳嗽VAS评分", 
          "鼻部相关症状", "咽喉部相关症状", "反流相关症状", "是否有季节性", 
          '哮喘急性发作次数',"哮喘急性导致就诊","哮喘急性导致系统激素使用",
          "急性发作导致住院次数","急性发作导致入住ICU","急性发作最难缓解症状",
          "春","夏","秋","冬", "FEV1预计值", "FEV1实测值", "FEV1预计百分比", "FVC预计值", "FVC实测值", 
          "FVC预计百分比", "FEV1/FVC预计值", "FEV1/FVC实测值", "FEV1/FVC预计百分比", 
          "MMEF预计值", "MMEF实测值", "MMEF预计百分比", "MEF75预计值", 
          "MEF75实测值", "MEF75预计百分比", "MEF25预计值", "MEF25实测值", 
          "MEF25预计百分比", "MEF25-75预计值", "MEF25-75实测值", "MEF25-75预计百分比", 
          "MEF50预计值", "MEF50实测值", "MEF50预计百分比", "PEF预计值", 
          "PEF实测值", "PEF预计百分比",  
          "嗜酸性粒细胞计数", "嗜酸性粒细胞百分比", "中性粒细胞计数", 
          "中性粒细胞百分比", "呼出气一氧化氮")

library(tableone)

t1_overall <- CreateTableOne(vars = vars,factorVars=factor_vars, 
                             data = workdat0801)


t1_overall_all <- print(t1_overall, nonnormal = num_vars, smd = TRUE,
                        quote = FALSE, noSpaces = TRUE, 
                        printToggle = FALSE)

rio::export(t1_overall_all, file = "output/t1_overall_all.xlsx",rowNames=TRUE)


###mean------



t1_overall_all_mean <- print(t1_overall,  smd = TRUE,
                        quote = FALSE, noSpaces = TRUE, 
                        printToggle = FALSE)

rio::export(t1_overall_all_mean, file = "output/t1_overall_all_mean.xlsx",rowNames=TRUE)

t1_overall_2nd <- print(t1_overall, nonnormal = num_vars, smd = TRUE,
                        quote = FALSE, noSpaces = TRUE, 
                        printToggle = FALSE,
                        showAllLevels = TRUE)

rio::export(t1_overall_2nd, file = "output/t1_overall_2nd.xlsx",rowNames=TRUE)

####-------按照疾病诊断分层

t1_diagnosis <- CreateTableOne(vars = vars,factorVars=factor_vars, 
                               data = workdat0801,strata='疾病诊断')


t1_diagnosis_all <- print(t1_diagnosis, nonnormal = num_vars, smd = TRUE,
                          quote = FALSE, noSpaces = TRUE, 
                          printToggle = FALSE)

rio::export(t1_diagnosis_all, file = "output/t1_diagnosis_all.xlsx",rowNames=TRUE)

t1_diagnosis_2nd <- print(t1_diagnosis, nonnormal = num_vars, smd = TRUE,
                          quote = FALSE, noSpaces = TRUE, 
                          printToggle = FALSE,
                          showAllLevels = TRUE)

rio::export(t1_diagnosis_2nd, file = "output/t1_diagnosis_2nd.xlsx",rowNames=TRUE)

## 严重程度评估----
t1_severity <- CreateTableOne(vars = vars,factorVars=factor_vars, 
                              data = workdat0801,strata='严重程度评估')


t1_severity_all <- print(t1_severity, nonnormal = num_vars, smd = TRUE,
                         quote = FALSE, noSpaces = TRUE, 
                         printToggle = FALSE)

rio::export(t1_severity_all, file = "output/t1_severity_all.xlsx",rowNames=TRUE)

t1_severity_2nd <- print(t1_severity, nonnormal = num_vars, smd = TRUE,
                         quote = FALSE, noSpaces = TRUE, 
                         printToggle = FALSE,
                         showAllLevels = TRUE)

rio::export(t1_severity_2nd, file = "output/t1_severity_2nd.xlsx",rowNames=TRUE)


# # showAllLevels = TRUE
# hosp2_t1 <- hosp2  %>%  t1_f(
#   vars = vars,
#   num_vars = num_vars,
#   factor_vars= factor_vars,
#   showlevel_p=T,
#   strata=strata) %>% DT::datatable(extensions = 'Buttons',
#                                    options = list(dom = 'Blfrtip',
#                                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                                                   lengthMenu = list(c(10,25,50,-1),
#                                                                     c(10,25,50,"All"))))
