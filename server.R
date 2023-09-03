# server.R
server <- function(input, output) {
  
  startTime <- as.numeric(Sys.time())
  
  token <- anonymous_login(project_api = "AIzaSyDt2yl4_YFhPmaLnlowccxGJKARPfMhFjE")
  #purl = "https://esp32-firebase-demo-b9d6b-default-rtdb.firebaseio.com/TestEC3/fakeData/"
  purl = "https://esp32-firebase-demo-b9d6b-default-rtdb.firebaseio.com/"
  #fname = "test3"
  
if(FALSE){
  output$distPlot <- renderPlot({
    dist <- rnorm(input$obs)
    hist(dist,
         col="purple",
         xlab="Random values")
  })
  output$distPlot2 <- renderPlot({
    dist2 <- rnorm(input$obs2)
    hist(dist2,
         col="green",
         xlab="Random values 2")
  })
}
  
  

if(FALSE) {
  dataInput <- reactive({
    fname = input$firebase_node
    urlPath = paste0(purl,fname,".json")
    data = httr::GET(url = urlPath)
    xx = jsonlite::fromJSON(httr::content(data,"text"))
    return(xx)
  })
}
  
  
  if(FALSE) {  
  sensorInput <- reactive({
    fname = input$firebase_test
    nodename = input$node
    # fname = 'allDataSensor01'
    #urlPath = paste0(purl,"/",fname,".json")
    # urlPath = paste0(purl, nodename, "/", "fakeData", fname)
    urlPath = paste0(purl, nodename, "/", "fakeData/")
    x.df = download(projectURL = urlPath, fileName = fname)
    # x.df2 = as.data.frame(x.df) %>% drop_na() %>%
    #   rename_all(list( ~gsub("fakeData.fakeSensor03", "sensor_03", .) )) %>%
    #   mutate(
    #     datetime = as_datetime(sensor_03.time$ts/1000),   
    #     date = as.Date(datetime),
    #     time = time(datetime),
    #     hour = hour(datetime),
    #     minute = minute(datetime),
    #     second = second(datetime)
    #   )
    # x.df = download(projectURL = purl, fileName = "allDataSensor01")
    x.df2 = x.df %>%
      mutate(
        ID = as.integer(rownames(.))-1,
        datetime = as.integer(rownames(.)),
        #datetime = Sys.Date() + as.integer(rownames(.)),
        # datetime = as_datetime(time$ts/1000),  
        # date = as.Date(datetime),
        # time1 = time(datetime),
        # hour = hour(datetime),
        # minute = minute(datetime),
        # second = second(datetime),
        obs = 1
      ) %>% 
      select(
        ID,
        datetime,
        everything(),
        -time
      )
    
    return(x.df2)
  })
  }
  
  if(FALSE){
      output$airtemp <- renderValueBox({
        db = sensorInput()
        x = db %>%filter(ID == max(ID))
        fb.value = x$temperature$value[1]
        valueBox(
          value = formatC(fb.value, digits = 2, format = "f"),
          subtitle = "Air Temp (F)",
          icon = icon("temperature-half"),
          color = "yellow"
          # width = 22
          #color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
        )
      })   
    
    output$humidity <- renderValueBox({
      db = sensorInput()
      x = db %>%filter(ID == max(ID))
      fb.value = x$humidity$value[1]
      valueBox(
        value = formatC(fb.value, digits = 1, format = "f"),
        subtitle = "Humidity (%)",
        icon = icon("percent"),
        color = "yellow"
      )
    })  
    
    output$pressure <- renderValueBox({
      db = sensorInput()
      x = db %>%filter(ID == max(ID))
      fb.value = x$pressure$value[1]
      valueBox(
        value = formatC(fb.value, digits = 1, format = "f"),
        subtitle = "Pressure (bar)",
        icon = icon("temperature-half"),
        color = "yellow"
      )
    }) 
  
    output$gas <- renderValueBox({
      db = sensorInput()
      x = db %>%filter(ID == max(ID))
      fb.value = x$gas$value[1]
      valueBox(
        value = formatC(fb.value, digits = 1, format = "f"),
        subtitle = "Gas (%)",
        icon = icon("fire-flame-simple"),
        color = "yellow"
      )
    }) 
  }

  if(FALSE){
  output$watertemp <- renderValueBox({
    db = sensorInput()
    x = db %>%filter(ID == max(ID))
    fb.value = 99.9
    valueBox(
      value = formatC(fb.value, digits = 1, format = "f"),
      subtitle = "H20 Temp (F)",
      icon = icon("fire-flame-simple"),
      color = "yellow"
    )
  }) 
  
  output$waterph <- renderValueBox({
    db = sensorInput()
    x = db %>%filter(ID == max(ID))
    fb.value = 7.4
    valueBox(
      value = formatC(fb.value, digits = 1, format = "f"),
      subtitle = "pH",
      icon = icon("chart-simple"),
      color = "yellow"
    )
  }) 
  
  output$turbidity <- renderValueBox({
    db = sensorInput()
    x = db %>%filter(ID == max(ID))
    fb.value = 99.9
    valueBox(
      value = formatC(fb.value, digits = 1, format = "f"),
      subtitle = "Turbidity",
      icon = icon("vial"),
      color = "yellow"
    )
  }) 
  
  output$voltage <- renderValueBox({
    db = sensorInput()
    x = db %>%filter(ID == max(ID))
    fb.value = x$voltage$value[1]
    # fb.value = 99.9
    valueBox(
      value = formatC(fb.value, digits = 3, format = "f"),
      subtitle = "Voltage (V)",
      icon = icon("bolt"),
      color = "yellow"
    )
  }) 
  }
  
  
  if(FALSE){
    
    output$tempPlot<-renderPlotly({
      df = sensorInput()
        plot_ly(
          #x = ~sensor_02.ID$value,
          x = ~df$datetime,
          y = ~df$temperature$value,
          line = list(shape = "spline", color = 'orange'),
          marker = list(
            color = "orange",
            size = 0.9
            )
        ) |> 
          layout(
            title = "Temperature",
            xaxis = list(rangemode = "normal",
                         title = 'Datetime',
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff'
            ),
            yaxis = list(rangemode = "normal",
                         title = 'Temperature [F]',
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff'
            ),
            plot_bgcolor='#e5ecf6', width = 900
          )
  
    })
    
    
    output$humidityPlot<-renderPlotly({
      df = sensorInput()
      plot_ly(
        x = ~df$datetime,
        y = ~df$humidity$value,
        line = list(shape = "spline", color = 'orange'),
        marker = list(
          color = "orange",
          size = 0.9
        )
      ) |> 
        layout(
          title = "Humidity",
          xaxis = list(rangemode = "normal",
                       title = 'Datetime',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          yaxis = list(rangemode = "normal",
                       title = 'Humidity [%]',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          plot_bgcolor='#e5ecf6', width = 900
        )
      
    })
    
    
  
    
    output$pressurePlot<-renderPlotly({
      df = sensorInput()
      plot_ly(
        x = ~df$datetime,
        y = ~df$pressure$value,
        line = list(shape = "spline", color = 'orange'),
        marker = list(
          color = "orange",
          size = 0.9
        )
      ) |> 
        layout(
          title = list(text = "Pressure"),
          xaxis = list(rangemode = "normal",
                       title = 'Datetime',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          yaxis = list(rangemode = "normal",
                       title = 'Pressure [bar]',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          plot_bgcolor='#e5ecf6', width = 900
        )
      
    })
  
    
    output$gasPlot<-renderPlotly({
      df = sensorInput()
      plot_ly(
        x = ~df$datetime,
        y = ~df$gas$value,
        line = list(shape = "spline", color = 'orange'),
        marker = list(
          color = "orange",
          size = 0.9
        )
      ) |> 
        layout(
          title = list(text = "Gas"),
          xaxis = list(rangemode = "normal",
                       title = 'Datetime',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          yaxis = list(rangemode = "normal",
                       title = 'Gas [bad]',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          plot_bgcolor='#e5ecf6', width = 900
        )
      
    })
    
    
    output$watertempPlot<-renderPlotly({
      df = sensorInput()
      plot_ly(
        x = ~df$datetime,
        y = ~df$temperature$value,
        line = list(shape = "spline", color = 'blue'),
        marker = list(
          color = "blue",
          size = 0.9
        )
      ) |> 
        layout(
          title = list(text = "H20 Temperature"),
          xaxis = list(rangemode = "normal",
                       title = 'Datetime',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          yaxis = list(rangemode = "normal",
                       title = 'H20 Temp [F]',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          plot_bgcolor='#e5ecf6', width = 900
        )
      
    })  
    
  
    
    output$waterphPlot<-renderPlotly({
      df = sensorInput()
      plot_ly(
        x = ~df$datetime,
        y = ~df$temperature$value,
        line = list(shape = "spline", color = 'blue'),
        marker = list(
          color = "blue",
          size = 0.9
        )
      ) |> 
        layout(
          title = list(text = "H20 pH"),
          xaxis = list(rangemode = "normal",
                       title = 'Datetime',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          yaxis = list(rangemode = "normal",
                       title = 'H20 pH',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          plot_bgcolor='#e5ecf6', width = 900
        )
      
    })  
    
    output$turbidityPlot<-renderPlotly({
      df = sensorInput()
      plot_ly(
        x = ~df$datetime,
        y = ~df$temperature$value,
        line = list(shape = "spline", color = 'blue'),
        marker = list(
          color = "blue",
          size = 0.9
        )
      ) |> 
        layout(
          title = list(text = "TURBIDITY"),
          xaxis = list(rangemode = "normal",
                       title = 'Datetime',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          yaxis = list(rangemode = "normal",
                       title = 'Turbidity',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          plot_bgcolor='#e5ecf6', width = 900
        )
      
    })  
    
    
    output$voltagePlot<-renderPlotly({
      df = sensorInput()
      plot_ly(
        x = ~df$datetime,
        y = ~df$voltage$value,
        line = list(shape = "spline", color = 'blue'),
        marker = list(
          color = "blue",
          size = 0.9
        )
      ) |> 
        layout(
          title = list(text = "Voltage"),
          xaxis = list(rangemode = "normal",
                       title = 'Datetime',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          yaxis = list(rangemode = "normal",
                       title = 'Voltage [V]',
                       zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'
          ),
          plot_bgcolor='#e5ecf6', width = 900
        )
      
    })    
  
    
  }
  

  
}