
server <- shinyServer(
  function(input, output, session) {
    zone.rg <- readOGR(dsn="faf4_zone2.shp",layer="faf4_zone2", encoding = "UTF-8")
    
    od_mode_vol <- read.csv(file = 'od_mode_vol_45.csv')
    
    centroid <- read.csv(file = 'centroid.csv')
    
    selected_zone <- reactive({
      p <- input$Zone_shape_click
      subset(centroid, id==p$id )
    })
    
    click_count <- 0
    type <- 0
    origin <- ""
    dest <- ""
    origin_id <- 0
    dest_id <- 0
    
    selected_od <- reactive({
      p <- input$Zone_shape_click
      
      selected <- subset(centroid, id==p$id )
      od_pair <- data.frame()
      if (type ==0 ){
        origin <<- selected$name
        dest <<- ""
        origin_id <<- selected$id
        dest_id <<- 0
      }
      
      if (type == 1){
        dest_id <<- selected$id
        dest <<- selected$name
        od_pair <- data.frame(origin, origin_id, dest, dest_id)
        colnames(od_pair)<- c("origin", "origin_id", "dest", "dest_id")
      }
      od_pair
      
    })
    
    output$Zone <- renderLeaflet({
      zone_labels <- sprintf(
        "<strong>%s</strong><br/>",
        paste(zone.rg$id, "--", zone.rg$name, sep='')
      ) %>% lapply(htmltools::HTML)
      
      m<-leaflet() %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Default Maptile",
                         options = providerTileOptions(noWrap = TRUE))%>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Maptile") %>%
        setView(lng = -95.0491410487803, lat = 38.8977674296551, zoom = 4)%>%
        addLayersControl(
          baseGroups = c("Default Maptile", "Satellite Maptile"),
          options = layersControlOptions(collapsed = TRUE)
        )%>%
        addPolygons(data=zone.rg, col="black", weight = 1, layerId = ~id, label = zone_labels,
                    highlight = highlightOptions(color = "blue",weight = 2, bringToFront = F, opacity = 0.7))
    })
    
    observeEvent(input$Zone_shape_click, {
      p <- input$Zone_shape_click
      if(is.null(p))
        return()
      proxy <- leafletProxy("Zone", session)
    })
    
    observe({
      p <- input$Zone_shape_click
      if (is.null(p))
        return()
                            
      m2<-leafletProxy("Zone", session = session)
      
      zone_labels <- sprintf(
        "<strong>%s</strong><br/>",
        paste(centroid$id, "--", centroid$name, sep='')
      ) %>% lapply(htmltools::HTML)
      
      selected <- selected_zone()
      selected_zone_labels <- sprintf(
        "<strong>%s</strong><br/>",
        paste(selected$id, "--", selected$name, sep='')
      ) %>% lapply(htmltools::HTML)
      
      type <<- click_count%%2
      if (type ==0 ){
        m2 %>% clearMarkers()%>%
        addCircleMarkers(data=selected, radius=6, color="green", lng =~x, lat =~y, stroke=FALSE, label = selected_zone_labels,
                         fillOpacity=1, layerId = ~id) 
      }
      
      if (type == 1){
        m2 %>% 
        addCircleMarkers(data=selected, radius=6, color="red", lng =~x, lat =~y, stroke=FALSE, label = selected_zone_labels,
                         fillOpacity=1, layerId = ~id) 
      }
      click_count <<- click_count+1
    })
    
    output$od_info <- renderText({ 
      p <- input$Zone_shape_click
      
      selected <- subset(centroid, id==p$id )
      
      if (type ==0 ){
        origin <<- selected$name
        dest <<- ""
        origin_id <<- selected$id
        dest_id <<- 0
      }
      
      if (type == 1){
        dest_id <<- selected$id
        dest <<- selected$name
      }
        
      paste(
        "<strong> <span style = \'font-weight: 700;\'> Origin:            </span> </strong> 
        <strong> <span style = \'font-weight: 500;\'> ",origin, "</span> </strong>
          <br>",
        "<strong> <span style = \'font-weight: 700;\'> Destination:       </span> </strong> 
        <strong> <span style = \'font-weight: 500;\'> ",dest, "</span> </strong> 
          <br>"
        ,sep = '')
    })
    
    output$od_vol <- DT::renderDataTable(server = FALSE,{
      vol<-data.frame()
      selected <- selected_od()
      
      if (length(selected)){
        vol <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id)%>%
          select(mode, tons, tons5, value, value5)
        
        vol$tons <- format(round(vol$tons, 2), big.mark=",")
        vol$tons5 <- format(round(vol$tons5, 2), big.mark=",")
        vol$value <- format(round(vol$value, 2), big.mark=",")
        vol$value5 <- format(round(vol$value5, 2), big.mark=",")
        colnames(vol) <- c('Mode', 'Tons FAF4 (000s)', 'Tons FAF5 (000s)', 'Value FAF4 (Millions)', 'Value FAF5 (Millions)' )
      }
      vol
    }, 
    rownames = FALSE,  class="compact", width="80%", 
    options = list(paging = FALSE, searching = FALSE, ordering=F, dom='t',columnDefs = list(list(className = 'dt-left', targets = 0:4)))
    )
    
    output$od_total <- renderText({ 
      total_tons <- 0
      total_value <- 0
      
      selected <- selected_od()
      
      if (length(selected)){
        df <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id)%>%
          select(tons, value) %>%
          summarize(tons=sum(tons, na.rm = TRUE), value = sum(value, na.rm = TRUE))
        total_tons <- format(round(df$tons, 2), big.mark=",")
        total_value <- format(round(df$value, 2), big.mark=",")
      }
      
      paste(
        "<strong> <span style = \'font-weight: 700;\'> FAF4: By all modes for the selected OD </span> </strong> 
          <br><br>",
        "<strong> <span style = \'font-weight: 500;\'> Total Tons (000s):            ",total_tons, "</span> </strong> 
          <br>",
        "<strong> <span style = \'font-weight: 500;\'> Total Value (Millions):       ",total_value, "</span> </strong> 
          <br>"
        ,sep = '')
    })
    
    output$od_total_5 <- renderText({ 
      total_tons <- 0
      total_value <- 0
      
      selected <- selected_od()
      
      if (length(selected)){
        df <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id)%>%
          select(tons5, value5)%>%
          summarize(tons=sum(tons5, na.rm = TRUE), value = sum(value5, na.rm = TRUE))
        total_tons <- format(round(df$tons, 2), big.mark=",")
        total_value <- format(round(df$value, 2), big.mark=",")
      }
      
      paste(
        "<strong> <span style = \'font-weight: 700;\'> FAF5: By all modes for the selected OD </span> </strong> 
          <br><br>",
        "<strong> <span style = \'font-weight: 500;\'> Total Tons (000s):            ",total_tons, "</span> </strong> 
          <br>",
        "<strong> <span style = \'font-weight: 500;\'> Total Value (Millions):       ",total_value, "</span> </strong> 
          <br>"
        ,sep = '')
      
    })
    
    output$od_ton_chart = renderPlotly({
      
      m <- list(l = 3, r = 10, b = 30, t = 80, pad = 4)
      selected <- selected_od()
      
      if (length(selected)){
        df_sub <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id) %>%
          select(mode, tons, tons5)
        na.omit(df_sub)
      if(nrow(df_sub)){
         plot_ly(df_sub, x = ~mode, y = ~tons, type = 'bar', name = 'FAF4 Tons')%>% 
            add_trace(y = ~tons5, name = 'FAF5 Tons')%>% 
            layout(yaxis = list(title = 'tons (000)'), barmode = 'group')
      }
      }
    })
    
    output$od_value_chart = renderPlotly({
      
      m <- list(l = 3, r = 10, b = 30, t = 80, pad = 4)
      selected <- selected_od()
      
      if (length(selected)){
        df_sub_value <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id) %>%
          select(mode, value, value5)
        na.omit(df_sub_value)
        
        if(nrow(df_sub_value)){
          plot_ly(df_sub_value, x = ~mode, y = ~value, type = 'bar', name = 'FAF4 Value')%>% 
            add_trace(y = ~value5, name = 'FAF5 Value')%>% 
            layout(yaxis = list(title = '$ (millions)'), barmode = 'group')
        }
      }
    })
    
    output$od_ton_pie = renderPlotly({
      
      m <- list(l = 3, r = 10, b = 30, t = 80, pad = 4)
      selected <- selected_od()
      
      if (length(selected)){
        df_sub <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id) %>%
          select(mode, tons)
        if(nrow(df_sub)){
          colnames(df_sub) <- c("Mode", "Tons") 
          p <- df_sub%>% 
            plot_ly(labels = ~Mode, values = ~round(Tons, 2),
                    width = 350, height = 300) %>%
            add_pie(hole = 0.4)%>%
            layout(title = "FAF4 Weight by Mode", 
                   font = list(family='Arial', size = 11), margin = m,
                   showlegend = T, autosize = F, 
                   legend = list(orientation = 'h', x=0, font = list( family = 'Arial', size = 10)),
                   paper_bgcolor='transparent',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        }
      }
    })
    
    output$od_ton_pie_5 = renderPlotly({
      
      m <- list(l = 3, r = 10, b = 30, t = 80, pad = 4)
      selected <- selected_od()
      
      if (length(selected)){
        df_sub <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id) %>%
          select(mode, tons5)
        if(nrow(df_sub)){
          colnames(df_sub) <- c("Mode", "Tons") 
          p <- df_sub%>% 
            plot_ly(labels = ~Mode, values = ~round(Tons, 2),
                    width = 350, height = 300) %>%
            add_pie(hole = 0.4)%>%
            layout(title = "FAF5 Weight by Mode", 
                   font = list(family='Arial', size = 11), margin = m,
                   showlegend = T, autosize = F, 
                   legend = list(orientation = 'h', x=0, font = list( family = 'Arial', size = 10)),
                   paper_bgcolor='transparent',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        }
      }
    })
    
    output$od_value_pie = renderPlotly({
      
      m <- list(l = 3, r = 10, b = 30, t = 80, pad = 4)
      selected <- selected_od()
      
      if (length(selected)){
        df_sub <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id) %>%
          select(mode, value)
        if(nrow(df_sub)){
          colnames(df_sub) <- c("Mode", "Value") 
          p <- df_sub%>% 
            plot_ly(labels = ~Mode, values = ~round(Value, 2),
                    width = 350, height = 300) %>%
            add_pie(hole = 0.4)%>%
            layout(title = "FAF4 Value by Mode", 
                   font = list(family='Arial', size = 11), margin = m,
                   showlegend = T, autosize = F, 
                   legend = list(orientation = 'h', x=0, font = list( family = 'Arial', size = 10)),
                   paper_bgcolor='transparent',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        }
      }
    })
    
    output$od_value_pie_5 = renderPlotly({
      
      m <- list(l = 3, r = 10, b = 30, t = 80, pad = 4)
      selected <- selected_od()
      
      if (length(selected)){
        df_sub <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id) %>%
          select(mode, value5)
        if(nrow(df_sub)){
          colnames(df_sub) <- c("Mode", "Value") 
          p <- df_sub%>% 
            plot_ly(labels = ~Mode, values = ~round(Value, 2),
                    width = 350, height = 300) %>%
            add_pie(hole = 0.4)%>%
            layout(title = "FAF5 Value by Mode", 
                   font = list(family='Arial', size = 11), margin = m,
                   showlegend = T, autosize = F, 
                   legend = list(orientation = 'h', x=0, font = list( family = 'Arial', size = 10)),
                   paper_bgcolor='transparent',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        }
      }
    })

})
