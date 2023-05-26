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

      DT::datatable(pm11, filter='top', editable = 'cell',extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(nrow(pm11),25,50,100,-1),
                                                     c(nrow(pm11),25,50,100,"All")),
                                   columnDefs = list(list(className = 'dt-center', targets = c(1,2,6))),
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
        )%>%
      formatStyle(
        c( '前十例质控完成数/录入超过十例中心数量'),
        background = styleColorBar( 
          c(0,max( map(pm11$`前十例质控完成数/录入超过十例中心数量`,~eval(parse(text = .x)))%>%unlist())*200) , '#ff7518'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
                                                     
    })
    
    ### 5. map province ratio--------
    
    output$map_province_ratio <- renderHighchart({
      
      hc_size(p_map_province_ratio,1000,1000)
      
      })
    
    output$map_province_pts <- renderHighchart({
      
      hc_size(p_map_province_pts,600,600)
      
    })
    
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
    output$by_hosp_tot <- renderHighchart({ 
      
      hc_size(p_by_hosp,400,800)
      # %>% layout(height = 600, width = 1000)
    })
    
    ### 2.2 top 10 ----------
    
    output$top10_time_series <- renderPlotly({ 
      ggplotly(p_top10_time_series,height = 400,width = 800) 
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
                                   lengthMenu = list(c(nrow(weekly_updates),25,50,100,-1),
                                                     c(nrow(weekly_updates),25,50,100,"All"))))
    })
    
    
    output$pts_firstFL <- DT::renderDataTable({
      
      DT::datatable(pts_followup, filter='top', editable = 'cell',extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   scrollY = TRUE,
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(nrow(pts_followup),25,50,100,-1),
                                                     c(nrow(pts_followup),25,50,100,"All"))))
    })
    
  
   
  }

##3.weekly update--------



