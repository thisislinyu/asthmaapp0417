### ---demo------------
demo_des_dat <- workdat0801 %>% 
  filter(!is.na(性别)) %>% 
  group_by(性别) %>% 
  summarise(count= n()) %>% 
  mutate(percent= round(100*count/sum(count),2),
         变量 = '性别',
         类别 = 性别) %>% 
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(年龄分层)) %>% 
      group_by(年龄分层) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '年龄',
             类别 = 年龄分层) 
  ) %>% 
  
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(教育程度)) %>% 
      group_by(教育程度) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '教育程度',
             类别 = 教育程度) 
    
  )%>% 
  
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(区域)) %>% 
      group_by(区域) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '区域',
             类别 = 区域) 
    
  )%>% 
  
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(民族)) %>% 
      group_by(民族) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '民族',
             类别 = 民族) 
    
  ) %>% 
  
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(吸烟状况)) %>% 
      group_by(吸烟状况) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '吸烟状况',
             类别 = 吸烟状况) 
    
  ) %>% 
  
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(吸烟指数)) %>% 
      group_by(吸烟指数) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '吸烟指数',
             类别 = 吸烟指数) 
    
  ) %>% 
  bind_rows(
    asthma0728 %>% 
      select(门诊号,住院号) %>% 
      mutate(门诊号 = ifelse(门诊号=='-',NA,门诊号),
             住院号 = ifelse(住院号=='-',NA,住院号),
             就诊类型 = ifelse(!is.na(门诊号),'门诊','住院')
      ) %>% 
      filter(!is.na(就诊类型)) %>% 
      group_by(就诊类型) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '就诊类型',
             类别 = 就诊类型) 
      
  ) %>% 
  
  select(变量,类别,percent,count)
###------symptom-----------------------

symptom_des_dat <- workdat0801 %>% 
  filter(!is.na(首发症状)) %>% 
  group_by(首发症状) %>% 
  summarise(count= n()) %>% 
  mutate(percent= round(100*count/sum(count),2),
         变量 = '首发症状',
         类别 = 首发症状) %>% 
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(主要症状)) %>% 
    group_by(主要症状) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '主要症状',
             类别 = 主要症状)
  ) %>% 
  
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(咳嗽)) %>% 
    group_by(咳嗽) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '咳嗽',
             类别 = 咳嗽 %>% as.character())
    
  ) %>% 
  
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(喘息)) %>% 
    group_by(喘息) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '喘息',
             类别 = 喘息 %>% as.character())
    
  )%>% 
  
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(`气促/呼吸不畅`)) %>% 
    group_by(`气促/呼吸不畅`) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '气促/呼吸不畅',
             类别 = `气促/呼吸不畅` %>% as.character())
    
  ) %>% 
  
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(胸闷)) %>% 
    group_by(胸闷) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '胸闷',
             类别 = 胸闷 %>% as.character()) 
    
  )  %>% 
  
  select(变量,类别,percent,count)



##-----season-----


season_des_dat <- workdat0801 %>% 
  filter(!is.na(春)) %>% 
  group_by(春) %>% 
  summarise(count= n()) %>% 
  mutate(percent= round(100*count/sum(count),2),
         变量 = '春',
         类别 = 春%>% as.character()) %>% 
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(夏)) %>% 
      group_by(夏) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '夏',
             类别 = 夏%>% as.character())
  ) %>% 
  
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(秋)) %>% 
      group_by(秋) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '秋',
             类别 = 秋 %>% as.character())
    
  ) %>% 
  
  bind_rows(
    workdat0801 %>% 
      filter(!is.na(冬)) %>% 
      group_by(冬) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '冬',
             类别 = 冬 %>% as.character())
    
  )%>% 
  
  select(变量,类别,percent,count) %>% 
  filter(类别=='TRUE')

##ae_asthma-------

ae_asthma_visit_dat <- workdat0801 %>% 
  select(哮喘急性导致就诊,哮喘急性导致系统激素使用,急性发作导致住院次数,) %>% 
  mutate(哮喘急性导致就诊1 = str_extract(哮喘急性导致就诊,'[0-9无]+'),
         哮喘急性导致就诊2 = ifelse(哮喘急性导致就诊1=='无',NA,哮喘急性导致就诊1) %>% as.numeric(),
         哮喘急性导致就诊3 = ifelse(哮喘急性导致就诊2>=3,'>=3次',paste0(哮喘急性导致就诊2,'次')),
         
         哮喘急性导致系统激素使用1 = str_extract(哮喘急性导致系统激素使用,'[0-9无]+'),
         哮喘急性导致系统激素使用2 = ifelse(哮喘急性导致系统激素使用1=='无',NA,哮喘急性导致系统激素使用1) %>% as.numeric(),
         哮喘急性导致系统激素使用3 = ifelse(哮喘急性导致系统激素使用2>=3,'>=3次',paste0(哮喘急性导致系统激素使用2,'次')),
         
         急性发作导致住院次数1 = str_extract(急性发作导致住院次数,'[0-9无]+'),
         急性发作导致住院次数2 = ifelse(急性发作导致住院次数1=='无',NA,急性发作导致住院次数1) %>% as.numeric(),
         急性发作导致住院次数3 = ifelse(急性发作导致住院次数2>=3,'>=3次',paste0(急性发作导致住院次数2,'次')),

         )


ae_asthma_visit_dat1 <- ae_asthma_visit_dat %>% 
  
  filter(!is.na(哮喘急性导致就诊3)) %>% 
  group_by(哮喘急性导致就诊3) %>% 
  summarise(count= n()) %>% 
  mutate(percent= round(100*count/sum(count),2),
         变量 = '哮喘急性导致就诊',
         类别 = 哮喘急性导致就诊3 %>% as.character()) %>% 
  
  bind_rows(
    ae_asthma_visit_dat %>% 
      filter(!is.na(哮喘急性导致系统激素使用3)) %>% 
      group_by(哮喘急性导致系统激素使用3) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '哮喘急性导致系统激素使用',
             类别 = 哮喘急性导致系统激素使用3 %>% as.character())
    
  )%>% 
  bind_rows(
    ae_asthma_visit_dat %>% 
      filter(!is.na(急性发作导致住院次数3)) %>% 
      group_by(急性发作导致住院次数3) %>% 
      summarise(count= n()) %>% 
      mutate(percent= round(100*count/sum(count),2),
             变量 = '急性发作导致住院次数',
             类别 = 急性发作导致住院次数3 %>% as.character())
    
  ) %>% 

  select(变量,类别,percent,count) %>% 
  mutate(类别 = factor(类别,levels=c('1次','2次','>=3次')),
         变量 = factor(变量,levels= c('哮喘急性导致就诊',
                                  '哮喘急性导致系统激素使用',
                                  '急性发作导致住院次数')))



###-------


pts_address <- data.frame(table(pts$省) ) 

colnames(pts_address) <- c("prov","Freq")

pts_address1 <-  full_join(pts_address ,map_province22,by=c("prov"= "province_cn"))

