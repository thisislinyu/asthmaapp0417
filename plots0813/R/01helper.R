#### --lib--------
library(stringr)
library(dplyr)
library(readxl)




#-- 正则----

# 肺功能

spirometry_f <- function(x) {
  x1 <- str_split(x, ",", simplify = TRUE)
  x2 <- str_replace_all(x1, "[-FEV(:)FVC(FEV/FVC(MMEF(MEF(MEF(MEF(PEF(L%;]", "")

  x3 <- sub(
    "1预计值|实测值|预计百分比|预计值|75预计值|25预计值|2575预计值|50预计值|%", "",
    x2 %>% as.character()
  )

  return(x3)
}

# a <- "FEV1(预计值:2.98L,实测值2.58L,预计百分比86.58%),FVC(预计值:3.43L,实测值2.91L,预计百分比84.84%),FEV1/FVC(预计值:84.02L,实测值88.7L,预计百分比105.57%),MMEF(预计值:3.96L,实测值3.05L,预计百分比77.02%),MEF75(预计值:L,实测值L,预计百分比%),MEF25(预计值:2.08L,实测值1.34L,预计百分比64.42%),MEF25-75(预计值:L,实测值L,预计百分比%),MEF50(预计值:4.36L,实测值3.97L,预计百分比91.06%),PEF(预计值:6.8L,实测值6.3L,预计百分比92.65%);"
#
# b <- "FEV1(预计值:3.81L,实测值3.31L,预计百分比86.88%),FVC(预计值:4.54L,实测值4.34L,预计百分比95.59%),FEV1/FVC(预计值:83.92L,实测值76.4L,预计百分比91.04%),MMEF(预计值:L,实测值L,预计百分比%),MEF75(预计值:L,实测值L,预计百分比%),MEF25(预计值:L,实测值L,预计百分比%),MEF25-75(预计值:L,实测值L,预计百分比%),MEF50(预计值:L,实测值L,预计百分比%),PEF(预计值:L,实测值L,预计百分比%);"
#


# 病程
cod_f <- function(x) {
  x1 <- str_split(x, "\\[", simplify = TRUE)
  if (length(x1) == 2) {
    x2 <- sub(".*[:：]", "", x1[[2]]) %>%
      sub("\\]", "", .) %>%
      sub("月", "", .)
  } else {
    x2 <- ""
  }

  x3 <- c(x1[[1]], x2)

  return(x3)
}


# a <- '2023-01[总病程：0月]'


# 季节性
season_f <- function(x) {
  x1 <- str_split(x, "\\[", simplify = TRUE)
  if (length(x1) == 2) {
    x2 <- sub(".*[:：]", "", x1[[2]]) %>%
      sub("\\]", "", .)
  } else {
    x2 <- ""
  }

  x3 <- c(x1[[1]], x2)

  return(x3)
}

# 嗜酸性粒细胞计数'	,'嗜酸性粒细胞百分比','中性粒细胞计数','中性粒细胞百分比'

rbc_f <- function(x) {
  x1 <- str_split(x, "\\%", simplify = TRUE) %>%
    sub(".*[:：]", "", .) %>%
    str_split(",") %>%
    unlist() %>%
    sub("[(].*", "", .)
  return(x1)
}


### 一氧化氮

NO_f <- function(x) {
  x1 <- str_split(x, ";", simplify = TRUE) %>%
    sub(".*[:：]", "", .) %>%
    sub("ppb", "", .)
  return(x1)
}

# 吸烟
smoking_f <- function(x) {
  x1 <- str_split(x, "\\[", simplify = TRUE) %>%
    str_split(",")
  if (length(x1) == 2) {
    x2 <- sub(".*[:：]", "", x1[[2]]) %>%
      sub("\\]", "", .) %>%
      sub("包/年|包/天", "", .)
  } else {
    x2 <- ""
  }


  x3 <- c(x1[[1]], x2)

  return(x3)
}


# treatment 治疗方案

treatment_f <- function(x) {
  x1 <- str_split(x, ",")

  return(x1)
}

# a <- '茶碱,口服激素,SABA（短效β2受体激动剂，如沙丁胺醇）'

comorbidity_f <- function(x) {
  x1 <- str_split(x, ",")
  
  return(x1)
}

