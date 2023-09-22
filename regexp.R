

spirometry_f <- function(x){
  x1 <-  str_split(x, ",", simplify = TRUE)
  x2 <- str_replace_all(x1,"[-FEV(:)FVC(FEV/FVC(MMEF(MEF(MEF(MEF(PEF(L%;]",'') 
  
  x3 <- sub("1预计值|实测值|预计百分比|预计值|75预计值|25预计值|2575预计值|50预计值|%", '',
            x2 %>% as.character()) 
  
  

  
  return(x3)
}

a <- 'FEV1(预计值:2.98L,实测值2.58L,预计百分比86.58%),FVC(预计值:3.43L,实测值2.91L,预计百分比84.84%),FEV1/FVC(预计值:84.02L,实测值88.7L,预计百分比105.57%),MMEF(预计值:3.96L,实测值3.05L,预计百分比77.02%),MEF75(预计值:L,实测值L,预计百分比%),MEF25(预计值:2.08L,实测值1.34L,预计百分比64.42%),MEF25-75(预计值:L,实测值L,预计百分比%),MEF50(预计值:4.36L,实测值3.97L,预计百分比91.06%),PEF(预计值:6.8L,实测值6.3L,预计百分比92.65%);'

b <- 'FEV1(预计值:3.81L,实测值3.31L,预计百分比86.88%),FVC(预计值:4.54L,实测值4.34L,预计百分比95.59%),FEV1/FVC(预计值:83.92L,实测值76.4L,预计百分比91.04%),MMEF(预计值:L,实测值L,预计百分比%),MEF75(预计值:L,实测值L,预计百分比%),MEF25(预计值:L,实测值L,预计百分比%),MEF25-75(预计值:L,实测值L,预计百分比%),MEF50(预计值:L,实测值L,预计百分比%),PEF(预计值:L,实测值L,预计百分比%);'


spirometry_out <- apply(spirometry_dat,1,spirometry_f) %>%
  data.frame() %>% t() %>% data.frame() %>%
  set_names('FEV1预计值'	,'FEV1实测值'	,'FEV1预计百分比',
            'FVC预计值'	,'FVC实测值'	,'FVC预计百分比',
            'FEV1/FVC预计值'	,'FEV1/FVC实测值'	,'FEV1/FVC预计百分比',
            'MMEF预计值'	,'MMEF实测值'	,'MMEF预计百分比',
            'MEF75预计值'	,'MEF75实测值'	,'MEF75预计百分比',
            'MEF25预计值'	,'MEF25实测值'	,'MEF25预计百分比',
            'MEF25-75预计值'	,'MEF25-75实测值'	,'MEF25-75预计百分比',
            'MEF50预计值'	,'MEF50实测值'	,'MEF50预计百分比',
            'PEF预计值'	,'PEF实测值'	,'PEF预计百分比'
  ) %>% data.frame()


rio::export(spirometry_out,file='data/spirometry_out.xlsx')


library(stringr)
library(dplyr)

library(readxl)

asthma0728 <- read_excel('data/截止7月28的患者数据.xlsx')

smoking_dat <- asthma0728 %>% select(您目前的吸烟状况属于以下哪种)

spirometry_dat <-  asthma0728 %>% select(肺通气功能)

# 
duration_dat 

cod_dat <- asthma0728 %>% select(哮喘首次发病的时间)

x <- '2023-01[总病程：0月]'

cod_f <- function(x){
  x1 <- str_split(x, "\\[", simplify = TRUE) 
  if(length(x1)==2){
    x2 <-  sub(".*[:：]",'',x1[[2]]) %>% 
      sub('\\]','',.) %>% 
      sub('月','',.) 
    
    
  }else{x2 <- ""}
  
  x3 <- c(x1[[1]],x2) 
  
  return(x3)
}


cod_out <- apply(cod_dat,1,cod_f) %>% data.frame() %>% t() %>% data.frame() %>%
  mutate(X2 = as.numeric(X2)) %>% 
  set_names('首次发病时间'	,'病程(月)'	) %>% data.frame() 
  

x = '有[春,秋]'
season_f <- function(x){
  x1 <- str_split(x, "\\[", simplify = TRUE) 
  if(length(x1)==2){
    x2 <-  sub(".*[:：]",'',x1[[2]]) %>% 
      sub('\\]','',.) 
    
    
  }else{x2 <- ""}
  
  x3 <- c(x1[[1]],x2) 
  
  return(x3)
}


season_out <- apply(season_dat,1,season_f) %>% data.frame() %>% t() %>% data.frame() %>%
  set_names('是否有季节性'	,'季节性'	) %>% data.frame()

prescription_dat <- asthma0728 %>% select(
  `过去的12个月内，哮喘治疗的主要药物有哪些`,	
  `过去的12个月内，使用吸入激素（ICS）【包括单纯ICS或ICS与长效β2受体激动剂的联合制剂（LCS/LABA）或ICS与LABA与M受体拮抗剂（LAMA）】的总体情况`,
  `过去的12个月内，使用ICS或ICS/LABA或ICS/LAMA/LABA情况`,
  `过去的12个月内，是否使用长效抗胆碱药物(如噻托溴铵、格隆溴铵、乌美溴铵)，含联合制剂`,	
  `过去的12个月内，使用SABA【短效β2受体激动剂，如沙丁胺醇】的总体情况`,
  `过去的12个月内，使用过哪种生物制剂`,`过去12个月内，哮喘总体的控制情况（基于患者主观评价）`
  
)


demo_dat <- asthma0728 %>% select(年龄,名族,BMI,教育程度,省,`儿童时期(≤14岁)是否诊断哮喘`) %>% 
  mutate(年龄 = sub('岁','',年龄) %>% as.numeric(),
         BMI = as.numeric(BMI))

RBC_dat <- asthma0728 %>% select(血常规检查)





rbc_f <- function(x){
  x1 <- str_split(x, "\\%",simplify = TRUE) %>% sub(".*[:：]",'',.) %>% 
    str_split(',') %>% unlist() %>% sub("[(].*",'',.)
  return(x1)
}


RBC_out <- apply(RBC_dat,1,rbc_f) %>% data.frame() %>% t() %>% data.frame() %>%
  set_names('嗜酸性粒细胞计数'	,'嗜酸性粒细胞百分比','中性粒细胞计数','中性粒细胞百分比','var5') %>% data.frame() %>% 
  select(-var5)

NO_dat <- asthma0728 %>% select(`呼出气一氧化氮(必填。本次访视检查结果，或允许时间窗为本次访视时间±1周)`)

NO_f <- function(x){
  x1 <- str_split(x, ";",simplify = TRUE) %>% sub(".*[:：]",'',.) %>% 
    sub('ppb','',.)
  return(x1)
}


NO_out <- apply(NO_dat,1,NO_f) %>% data.frame() %>% t() %>% data.frame() %>%
  set_names('呼出气一氧化氮'	,'呼出气一氧化氮仪器') %>% data.frame() 


rio::export(cod_out,'data/cod_out.xlsx')

tmp <- bind_cols(cod_out,spirometry_out,smoking_out,cod_dat,spirometry_dat,smoking_dat)

season_dat <- asthma0728 %>% select(`哮喘症状(喘息、气促/呼吸不畅、胸闷、咳嗽)是否有季节性`)

symptom_dat <- asthma0728 %>% select(`哮喘发病初期，首发的症状`,
                                    # `哮喘发病以来，有以下哪些症状`,
                                     `哮喘发病以来，主要的症状为`,
                                    `哮喘发病以来，咳嗽VAS评分得分`,
                                    `过去的12个月内，是否有鼻部相关症状(鼻塞 、鼻痒、流涕、打喷嚏等)`,
                                    `过去的12个月内，是否有咽喉部相关症状(咽痒、咽干、异物感、咽痛、声音嘶哑等)`,
                                    `过去的12个月内，是否有反流相关症状(烧心、反酸、嗳气等)`,
                                    
                                    
                                    ) %>% 
  mutate(`哮喘发病初期，首发的症状` = ifelse(`哮喘发病初期，首发的症状`=='不确定',NA,`哮喘发病初期，首发的症状`),
         `哮喘发病以来，主要的症状为` = ifelse(`哮喘发病以来，主要的症状为`=='不确定',NA,`哮喘发病以来，主要的症状为`),
         `哮喘发病以来，咳嗽VAS评分得分` = ifelse(`哮喘发病以来，咳嗽VAS评分得分`=='-',NA,`哮喘发病以来，咳嗽VAS评分得分`)) %>% 
  set_names('首发症状','主要症状','咳嗽VAS评分','鼻部相关症状','咽喉部相关症状','反流相关症状')



ae_dat <- asthma0728 %>% select(`过去的12个月内，有多少次哮喘急性发作`,
                                `过去的12个月，因为急性发作导致额外门诊/急诊就诊次数`,
                                `过去的12个月，因为急性发作导致系统激素使用(包括口服与静脉滴注，每次需连续使用≥3天)`,
                                `过去的12个月，因为急性发作导致住院次数`,
                                `过去的12个月，因为急性发作导致入住ICU/气管插管次数`,
                                `哮喘急性发作后，通常以下哪种症状最难缓解(持续时间更长)`
                                )



control_dat <- asthma0728 %>% select(`在过去4周内，在工作、学习或家中,有多少时候哮喘妨碍您进行日常活动`,
                                     `在过去4周内，您有多少次呼吸困难`,
                                     `在过去4周内，因为哮喘（喘息、咳嗽呼吸困难、胸闷或疼痛），您有多少次在夜间醒来或早上比平时早醒`,
                                     `在过去4周内，您有多少次使用急救药物治疗(如沙丁胺醇)`,
                                     `您如何评价过去4周内您的哮喘控制情况`
)

diagnosis_dat <- asthma0728 %>% select(疾病诊断,严重程度评估)


spirometry_out <- apply(spirometry_out,1,as.numeric) %>% data.frame() 

colnames(spirometry_out) <- 
  c('FEV1预计值'	,'FEV1实测值'	,'FEV1预计百分比',
            'FVC预计值'	,'FVC实测值'	,'FVC预计百分比',
            'FEV1/FVC预计值'	,'FEV1/FVC实测值'	,'FEV1/FVC预计百分比',
            'MMEF预计值'	,'MMEF实测值'	,'MMEF预计百分比',
            'MEF75预计值'	,'MEF75实测值'	,'MEF75预计百分比',
            'MEF25预计值'	,'MEF25实测值'	,'MEF25预计百分比',
            'MEF25-75预计值'	,'MEF25-75实测值'	,'MEF25-75预计百分比',
            'MEF50预计值'	,'MEF50实测值'	,'MEF50预计百分比',
            'PEF预计值'	,'PEF实测值'	,'PEF预计百分比')
   
workdat0801 <- cbind(demo_dat,cod_out,spirometry_out,smoking_out,symptom_dat,season_out,RBC_out,
                     NO_out,ae_dat,control_dat,prescription_dat,diagnosis_dat)


rio::export(workdat0801,'data/workdat0801.xlsx')

  
smoking_f <- function(x){
  x1 <- str_split(x, "\\[", simplify = TRUE) %>% 
    str_split(',')
  if(length(x1)==2){
    x2 <-  sub(".*[:：]",'',x1[[2]]) %>% 
      sub('\\]','',.) %>% 
      sub('包/年|包/天','',.) 
      
    
  }else{x2 <- ""}
  
  
  x3 <- c(x1[[1]],x2) 
 
 return(x3)
  
}



library(tidyverse)

smoking_out <- apply(smoking_dat,1,smoking_f) %>% do.call(rbind.data.frame, .) %>% 
  set_names("吸烟状况",'吸香烟数量','吸香烟年数','吸香烟指数','戒烟年','戒烟月')

rio::export(smoking_out,file='data/smoking_out.xlsx')


tmp <- '已戒香烟[曾今吸香烟数量：0.5包/年,吸香烟年数：13.5年,吸香烟指数：6.75包/年,戒香烟时长(戒香烟多久了)：2年,0月]'





asthma0731 <- cbind(spirometry_out,smoking_dat,asthma0728)


rio::export(asthma0731,file='data/asthma0731.xlsx')

asthma0728$疾病诊断 %>% table()


  
 

