####
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



## 0. load data-----------
source("helper.R",encoding = 'utf-8')
pts <- read_excel("data/pts.xlsx") 
info <- read_excel("data/信息表.xlsx")
feno <- read_excel("data/feno.xlsx") 
agreement <- read_excel("data/agreement.xlsx") %>% mutate(协议日期 = ymd(日期))
score <- read_excel("data/score.xlsx") 
QC <- read_excel("data/质控表.xlsx") 


## 1. header -------------------------------
header <- 
  dashboardHeader( title = HTML("“全新呼吸”中国哮喘患者登记库"), 
                   disable = FALSE, 
                   titleWidth  = 520
                   # 
                   #,
                   # dropdownMenuCustom( type = 'message',
                   #                     customSentence = customSentence,
                   #                     messageItem(
                   #                       from = "yul@np-data.com",#'Feedback and suggestions',
                   #                       message =  "",#paste0("TR_SharedMailbox@mbie.govt.nz" ),
                   #                       icon = icon("envelope"),
                   #                       href = "mailto:yul@np-data.com"
                   #                     ),
                   #                     icon = icon('comment')
                   # # 
                   # ),
                   # dropdownMenuCustom( type = 'message',
                   #                     customSentence = customSentence_share,
                   #                     icon = icon("share-alt"),
                   #                     messageItem(
                   #                       from = 'Twitter',
                   #                       message = "",
                   #                       icon = icon("twitter"),
                   #                       href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&text=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'Facebook',
                   #                       message = "",
                   #                       icon = icon("facebook"),
                   #                       href = "https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'Google+',
                   #                       message = "",
                   #                       icon = icon("google-plus"),
                   #                       href = "https://plus.google.com/share?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'Sina Weibo',
                   #                       message = "",
                   #                       icon = icon("weibo"),
                   #                       href = "http://service.weibo.com/share/share.php?url=http://example.com&appkey=&title=New%20Zealand%20Trade%20Intelligence%20Dashboard%20http%3A%2F%2Ftradeintelligence.mbie.govt.nz&pic=&ralateUid=&language=zh_cn"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'Pinterest',
                   #                       message = "",
                   #                       icon = icon("pinterest-p"),
                   #                       href = "http://pinterest.com/pin/create/button/?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&media=&description=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'LinkedIn',
                   #                       message = "",
                   #                       icon = icon("linkedin"),
                   #                       href = "http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&title=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                   #                     ),
                   #                     messageItem(
                   #                       from = 'Tumblr',
                   #                       message = "",
                   #                       icon = icon("tumblr"),
                   #                       href = "http://www.tumblr.com/share?v=3&u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&t=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                   #                     )
                   # )
                   
  )

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='https://www.np-data.com/',
                                            tags$img(src='main_page_logo_np1.png'),
                                            target = '_blank',height='67',width='228.6', align = 'left') #,





## 2. sidebar--------------

sidebar <- 
  dashboardSidebar(
    width = 230,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      
      menuItem("进度概览", tabName = 'tab1', icon = icon('dashboard'),
               badgeLabel = latest_pts_date, badgeColor = "green" ),
      
      menuItem("中心入组情况", tabName = 'tab2', icon = icon('dashboard'),
               badgeLabel = '', badgeColor = "green" ),
      menuItem("中心综合排名情况", tabName = 'tab3', icon = icon('dashboard'),
               badgeLabel = '', badgeColor = "green" ),
      menuItem("信息表更新", tabName = 'tab4', icon = icon('dashboard'),
               badgeLabel = '', badgeColor = "green" ),
      menuItem("超窗统计", tabName = 'tab5', icon = icon('dashboard'),
               badgeLabel = '', badgeColor = "green" ),
      menuItem("PM质控表", tabName = 'tab6', icon = icon('dashboard'),
               badgeLabel = '', badgeColor = "green" )
     ,
     menuItem("tmp", tabName = 'tab7', icon = icon('dashboard'),
              badgeLabel = '', badgeColor = "green" )
    )
  )

## 3. body --------------------------------
body <- dashboardBody(
  
  ## 3.0. CSS styles in header ----------------------------
  tags$head(
    
    tags$script("document.title = 'Asthma Registry Dashboard'"),
    
    ### Styles 
    tags$style(HTML(".small-box {height: 65px}")),
    tags$style(HTML(".fa { font-size: 35px; }")),
    tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
    tags$style(HTML(".fa-dashboard { font-size: 20px; }")),
    tags$style(HTML(".fa-globe { font-size: 20px; }")),
    tags$style(HTML(".fa-barcode { font-size: 20px; }")),
    tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
    tags$style(HTML(".fa-wrench { font-size: 15px; }")),
    tags$style(HTML(".fa-refresh { font-size: 15px; }")),
    tags$style(HTML(".fa-search { font-size: 15px; }")),
    tags$style(HTML(".fa-comment { font-size: 20px; }")),
    tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
    tags$style(HTML(".fa-envelope { font-size: 20px; }")),
    tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
    tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
    tags$style(HTML(".fa-bell { font-size: 17px; }")),
    tags$style(HTML(".fa-check { font-size: 14px; }")),
    tags$style(HTML(".fa-times { font-size: 14px; }")),
    
    #tags$style(HTML(".fa-twitter { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-facebook { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-google-plus { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-pinterest-p { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-linkedin { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-tumblr { font-size: 10px; color:red;}")),
    
    ## modify the dashboard's skin color
    tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #ff7518;
                       }

                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #ff7518;
                       }

                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #ff7518;
                       }

                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #ff7518;
                                 }
                       ')
    ),
    
    ## modify icon size in the sub side bar menu
    tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }

                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }

                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      '
    )) ,
    
    tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
    
    ## to not show error message in shiny
    tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
    tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
    
    ## heand dropdown menu size
    #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
  ),
  
  tabItems(
    #####tab1------
    tabItem( tabName = 'tab1',
      
      ## 0. overall 
     # h1(paste0("入组概况(截止至 ", latest_pts_date,")")) ,
      fluidRow(
        valueBoxOutput("enroll_hosp_tot_box"),
        valueBoxOutput("enroll_tot_box") %>% withSpinner(type=4),
        valueBoxOutput("week_mean")
        
        # valueBoxOutput("BlTotBox")
      ),
            
      ## 1.1 progress ----------------------------------------
      # h2(paste0("入组时序图")),
      fluidRow( column( width = 6,h4("每日入组人数时序图", align = 'center'), highchartOutput("time_series_perday")),
                column( width = 6,h4("每周入组人数时序图", align = 'center'), highchartOutput("time_series_week") )
      ),
     
     fluidRow( 
       column( width = 6,h4("哮喘患者居住地分布图", align = 'center'), highchartOutput("map_province_pts_address")),
               column( width = 6,h4("患者入组数TOP10中心入组情况(周)", align = 'center'), plotlyOutput("top10_time_series") )
     )
      
      # fluidRow( 
      #   
      #   column( width = 6,h4("各省份启动情况", align = 'center'), 
      #                   fluidRow(
      #                     column(width=6,highchartOutput("map_province_ratio")),
      #                     column(width=6,highchartOutput("map_province_pts"))
      #                     
      #                   )
      #                   ),
      #           column( width = 6,h4("患者入组数TOP10中心入组情况", align = 'center'), plotlyOutput("top10_time_series") )
      #          
      #           )
      
      
      
    ),
    
    #####tab2---------
    tabItem( tabName = 'tab2',
             
             ####################some output here-----------
              fluidRow( column( width = 6,h4("各中心患者入组数排名", align = 'center'), highchartOutput("by_hosp_tot")),
             #,
             column( width = 6,h4("各区域经理负责中心启动情况", align = 'center'),DT::dataTableOutput("pm_out")  )
             ),
             
             fluidRow( 
               #column( width = 6,h4("周入组人数排名", align = 'center'), renderUI("dy_rank")),
                       
                       column( width = 6,h4("质控数据问题分类", align = 'center'), highchartOutput("QC_pie")),
                       column( width = 6,h4(paste0("质控进度情况(已质控",QC_center_nums ,"家中心,",QC_nums,"例患者,合格率",QC_pass_p,"%)"), align = 'center'),DT::dataTableOutput("QC_table")  )
                      
             )
             
    ),
    ### tab3---------
    tabItem( tabName = 'tab3',
             
             ####################some output here-----------
             # fluidRow( column( width = 6,h4("各中心患者入组数排名", align = 'center'), highchartOutput("by_hosp_tot")),
             #           #,
             #           column( width = 6,h4("患者入组数TOP10中心入组情况", align = 'center'), plotlyOutput("top10_time_series") )
             # ),
             
             fluidRow( column( width = 12,h4("中心综合排名情况(仅已提交病例)", align = 'center'), DT::dataTableOutput("centerrank_out"))
                       
             )
             
    ),
    
    tabItem( tabName = 'tab4',
             
             ####################some output here-----------
             # fluidRow( column( width = 6,h4("各中心患者入组数排名", align = 'center'), highchartOutput("by_hosp_tot")),
             #           #,
             #           column( width = 6,h4("患者入组数TOP10中心入组情况", align = 'center'), plotlyOutput("top10_time_series") )
             # ),
             
             fluidRow( column( width = 12,h4("信息表更新", align = 'center'), DT::dataTableOutput("my_weekly_updates"))
                       
             )
             
    ) ,
    
    tabItem( tabName = 'tab5',
             
             ####################some output here-----------
             # fluidRow( column( width = 6,h4("各中心患者入组数排名", align = 'center'), highchartOutput("by_hosp_tot")),
             #           #,
             #           column( width = 6,h4("患者入组数TOP10中心入组情况", align = 'center'), plotlyOutput("top10_time_series") )
             # ),
             
             fluidRow( column( width = 12,h4("超窗患者列表", align = 'center'), DT::dataTableOutput("pts_followup_aft_window_t")),
                       column( width = 12,h4("随访1情况", align = 'center'), DT::DTOutput("fu1_notice_t")),
                       column( width = 12,h4("随访情况列表", align = 'center'), DT::dataTableOutput("pts_firstFL"))
                       
                       
             )
             
    ),
    tabItem( tabName = 'tab6',
             
             ####################some output here-----------
             # fluidRow( column( width = 6,h4("各中心患者入组数排名", align = 'center'), highchartOutput("by_hosp_tot")),
             #           #,
             #           column( width = 6,h4("患者入组数TOP10中心入组情况", align = 'center'), plotlyOutput("top10_time_series") )
             # ),
             
             fluidRow( column( width = 12,h4("项目经理质控表", align = 'center'), DT::dataTableOutput("QC_fill_t"))
                       
             )
             
    )
    ,
    tabItem( tabName = 'tab7',

             ####################some output here-----------
             # fluidRow( column( width = 6,h4("各中心患者入组数排名", align = 'center'), highchartOutput("by_hosp_tot")),
             #           #,
             #           column( width = 6,h4("患者入组数TOP10中心入组情况", align = 'center'), plotlyOutput("top10_time_series") )
             # ),

             fluidRow(

             #  column( width = 12,h4("trend", align = 'center'), plotlyOutput("hosp_time_trend_plot")  )


                          column(width=6,
                                 box(style='width:330px;overflow-x: scroll;height:500px;overflow-y: scroll;',
                                     plotOutput("hosp_time_trend_plot1",width="300px",height="2500px")
                                 ))
                          #,
                         # column(width=6,highchartOutput("map_province_pts_address"))

             )

    )

    
    
    
    
  )
  
  
)

####ui all in ont-------
ui <- 
  dashboardPage(header, sidebar, body )

#shiny::runApp(host = getOption("shiny.host", "0.0.0.0"),port = 5257)

