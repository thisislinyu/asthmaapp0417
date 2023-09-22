source('helper.R',encoding = 'utf-8')

server <- 
  function(input, output, session) {
    
    ### 1. overall-----------
    output$enroll_tot_box <- renderValueBox({
      valueBox(
        VB_style(enroll_tot , "font-size: 60%;"  ),
        "入组总人数", 
        icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
        color = "yellow"
          # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, 
          # olive, lime, orange, fuchsia, purple, maroon, black.
      )
    })
    output$enroll_hosp_tot_box <- renderValueBox({
      valueBox(
        VB_style(enroll_hosp_tot , "font-size: 60%;"  ),
        "已启动中心数量", 
        icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
        color = "yellow"
          # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, 
        # olive, lime, orange, fuchsia, purple, maroon, black.
      )
    })
    output$week_mean <- renderValueBox({
      valueBox(
        VB_style(week_mean, "font-size: 60%;"  ),
        "周平均入组人数", 
        icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
        color = "yellow"
          # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, 
        # olive, lime, orange, fuchsia, purple, maroon, black.
      )
    })
    ### 2output time_series_perday ----
    output$time_series_perday <- renderHighchart({p_time_series_cumday})
    ### 3 output cumulative by week------
    output$time_series_week <- renderHighchart({p_time_series_cumweek})
    
    
    
    ### 4. output pm----
    output$pm_out <- DT::renderDataTable({

      DT::datatable(pm12, filter='top', editable = 'cell',extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(nrow(pm11),25,50,100,-1),
                                                     c(nrow(pm11),25,50,100,"All")),
                                   columnDefs = list(list(className = 'dt-center', targets = c(1,2))),
                                   columnDefs = list(list(className = 'dt-left', targets = c(3:5)))
                                   
                                   )) %>% 
      formatStyle(columns = 'PM', target = 'row', 
                  fontWeight = styleEqual(c('总计'), c('bold')),
                  backgroundColor = styleEqual(c('总计'), c('lightgrey'))) %>% 
      formatStyle(
        c('启动医院比例(%)'),
        background = styleColorBar( c(0,max(pm11$`启动医院比例(%)`)*2) , '#ff7518'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>% 
        formatStyle(
          c( '入组人数'),
          background = styleColorBar( c(0,max(pm11$入组人数)*2) , '#ff7518'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )%>% 
        formatStyle(
          c( '平均各中心入组人数'),
          background = styleColorBar( c(0,max(pm11$平均各中心入组人数)*2) , '#ff7518'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      # %>%
      #formatStyle(
      #   c( '前十例质控完成数/录入超过十例中心数量'),
      #   background = styleColorBar( 
      #     c(0,max( map(pm11$`前十例质控完成数/录入超过十例中心数量`,~eval(parse(text = .x)))%>%unlist())*200) , '#ff7518'),
      #   backgroundSize = '100% 90%',
      #   backgroundRepeat = 'no-repeat',
      #   backgroundPosition = 'center'
      # )
                                                     
    })
    
    ### 5. map province ratio--------
    
    output$map_province_ratio <- renderHighchart({
      
      hc_size(p_map_province_ratio,1000,1000)
      
      })
    
    output$map_province_pts <- renderHighchart({
      
      hc_size(p_map_province_pts,600,600)
      
    })
    
   # output$map_province_pts_address <- renderEcharts4r({
      
  #    p_map_province_pts_address
     # hc_size(p_map_province_pts_address,600,600)
      
   # })
    
    
    
    ### 6.output province-----------
    output$province_out <- DT::renderDataTable({
 
      DT::datatable(province_final1, filter='top', editable = 'cell',extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(nrow(province_final1),25,50,100,-1),
                                                     c(nrow(province_final1),25,50,100,"All"))))
    })
    
    
    
    ## tab2----------
    ## 2.1 by hosp total------
    output$address_all_p1 <- renderHighchart({ 
      
      hc_size(address_all_p,800,800)
      # %>% layout(height = 600, width = 1000)
    })
    
    ### 2.2 top 10 ----------
    
    output$top10_time_series <- renderPlotly({ 
      ggplotly(p_top10_time_series,height = 400,width = 800) 
      })
    
    output$all_time_series <- renderPlotly({ 
      ggplotly(hosp_time_trend_plot1,height = 400,width = 2000) 
    })
    ### 2.2 0606 trend all-----
    # output$hosp_time_trend_plot <- renderPlotly({ 
    #   ggplotly(hosp_time_trend_plot,height = 3000,width = 2000) 
    # })
    
    # output$hosp_time_trend_plot <- renderPlotly({ 
    #   ggplotly(hosp_time_trend_plot,height = 3000,width = 2000) 
    # })
    
    output$hosp_time_trend_plot1 <- renderPlot({ 
      hosp_time_trend_plot1
    })

  
    
    ### 2.3 QC---------
    output$QC_pie <- renderHighchart({
      
      hc_size(p_QC_pie,550,550)
      
    })
    
    output$QC_table <- DT::renderDataTable({
      
      DT::datatable(QC_table, filter='top', editable = 'cell',extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,25,50,100,-1),
                                                     c(10,25,50,100,"All")),
                                 columnDefs = list(list(targets = 5, visible = FALSE))
                                   )
                    ) %>% formatStyle(
                      '已质控例数', 'urgent',
                      backgroundColor = styleEqual(c(1), c('orange'))
                    )
    })
    
    output$QC3_table <- DT::renderDataTable({
      
      DT::datatable(QC3_table, filter='top', editable = 'cell',extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,25,50,100,-1),
                                                     c(10,25,50,100,"All"))
                                   #,
                                   #columnDefs = list(list(targets = 5, visible = FALSE))
                    )
      )
      #%>% formatStyle(
      #  '已质控例数', 'urgent',
      #  backgroundColor = styleEqual(c(1), c('orange'))
     # )
    })
    
    
    
    ## --dynamic plot--------
    
    output$dy_rank <- renderUI({
      
      p_dynamic_plot
    })
    
    # tab3 --------
    ####### center rank-------------
    output$centerrank_out <- DT::renderDataTable({
      
      DT::datatable(center_rank_final1, filter='top', editable = 'cell',extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(nrow(center_rank_final1),25,50,100,-1),
                                                     c(nrow(center_rank_final1),25,50,100,"All")),
                                   columnDefs = list(list(className = 'dt-center', targets = 2:12))
                                   ))
    })
    
    
    output$my_weekly_updates <- DT::renderDataTable({
      
      DT::datatable(weekly_updates, filter='top', editable = 'cell',extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(nrow(weekly_updates),30,50,100,-1),
                                                     c(nrow(weekly_updates),30,50,100,"All"))))
    })
    
    
    output$pts_firstFL <- DT::renderDataTable({
      
      DT::datatable(pts_followup, filter='top', editable = 'cell',extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(5,25,50,100,-1),
                                                     c(5,25,50,100,"All")))
    })
    
    output$pts_followup_aft_window_t <- DT::renderDataTable({
      
      DT::datatable(pts_followup_aft_window, filter='top', editable = 'cell',extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,25,50,100,-1),
                                                     c(10,25,50,100,"All"))))
    })
    
    # output$fu1_notice_t <- DT::renderDataTable({
    #   
    #   DT::datatable(fu1_notice, filter='top', editable = 'cell',extensions = 'Buttons',
    #                 options = list(dom = 'Blfrtip',
    #                                scrollX = TRUE,
    #                                scrollY = TRUE,
    #                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    #                                lengthMenu = list(c(nrow(fu1_notice),25,50,100,-1),
    #                                                  c(nrow(fu1_notice),25,50,100,"All"))))
    # })
    
    output$QC_fill_t <- DT::renderDataTable({
      
      DT::datatable(QC_fill, filter='top', editable = 'cell',extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(nrow(QC_fill),25,50,100,-1),
                                                     c(nrow(QC_fill),25,50,100,"All"))))
    })
    
    mysketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, '区域经理'),
          th(rowspan = 2,'所属医院'),
          th(colspan = 2, '已完成复诊1'),
          th(colspan = 2, '待复诊1'),
          th(rowspan = 2, '未到复诊1开始时间')
        ),
        tr(
          lapply(c( "按期完成复诊1","超窗完成复诊1",
                    '7天内正常待复诊|所有正常待复诊1', "超窗待复诊1"), th)
        )
      )
    ))
    
    
    
    headjs <- "function(thead) {
   $(thead).closest('thead').find('th').eq(2).css('background-color','FFCA99FF');
  $(thead).closest('thead').find('th').eq(4).css('background-color', 'FFCA99FF');
  $(thead).closest('thead').find('th').eq(5).css('background-color', 'FFCA99FF');

    $(thead).closest('thead').find('th').eq(3).css('background-color', 'FF8F33FF');
  $(thead).closest('thead').find('th').eq(6).css('background-color', 'FF8F33FF');
  $(thead).closest('thead').find('th').eq(7).css('background-color', 'FF8F33FF');

 }"
   
    
    output$fu1_notice_t <- DT::renderDT({
      DT::datatable(fu1_notice3, filter='top', editable = 'cell',extensions = 'Buttons',
                                      container = mysketch, 
                                     rownames = FALSE,
                                     options = list(dom = 'Blfrtip',
                                                      headerCallback = JS(headjs),
                                                    scrollX = TRUE,
                                                    scrollY = TRUE,
                                                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                    lengthMenu = list(c(nrow(fu1_notice3),25,50,100,-1),
                                                                      c(nrow(fu1_notice3),25,50,100,"All")),
                                                    columnDefs = list(list(className = 'dt-center', targets =c(2,3,4,5) ))
                                     ))

    })
  
   
  }

##3.weekly update--------

## 4. follow up-------




