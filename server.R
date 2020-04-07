
server <- function(input, output, session) {
  
  # code to collapse navbar after selecting a menu item
  observeEvent(
    eventExpr = input$navBar, 
    {
      runjs(
        'var elem = document.getElementsByClassName("navbar-collapse")[0]
       elem.setAttribute("aria-expanded", "false");
       elem.setAttribute("class", "navbar-collapse collapse");'
      )
    }
  )
  
#####################################
# proj - airbnb
####################################
  #####################################
  # the properties map
  #####################################
  output$nycMap <- renderLeaflet(
    {
      # map properties from a sample of 25
      mapData <- nyc %>% 
        filter(boro == input$boro) %>% 
        sample_n(25)
      # change icon color based on room type
      aIcon <- makeIcon(
        iconUrl = ifelse(str_sub(mapData$room_type, 1, 1) == 'E', 'www/airbnb.png',
                  ifelse(str_sub(mapData$room_type, 1, 1) == 'P', 'www/airbnb_blue.png',
                                                                  'www/airbnb_orange.png')),
        iconWidth = 30,
        iconHeight = 28,
        iconAnchorX = 15,
        iconAnchorY = 0
      )
      # the map
      leaflet(
        data = mapData,
        options = leafletOptions(zoomControl = TRUE)
      ) %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        # addTiles()
        # addProviderTiles(providers$Stamen.Toner)
        # addProviderTiles(providers$Esri.NatGeoWorldMap)
        # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
        addMarkers(
          lng = ~lng,
          lat = ~lat,
          label = ~name,
          icon = aIcon
        )
    }
  )
  
  #####################################
  # reactive variable to hold values for selected airbnb property
  #####################################
  aLoc <- reactiveValues(
    nm = NULL, nh = NULL, # nm = name,     nh = neighborhood
    lt = NULL, lg = NULL, # lt = latitude, lg = longitude
    pr = NULL, rt = NULL  # pr = price,    rt = room type
  )
  
  #####################################
  # what happens when marker clicked
  #####################################
  observeEvent(
    eventExpr = input$nycMap_marker_click,
    {
      aa <- input$nycMap_marker_click
      # isolate the row in nyc data containing the selected property
      a <- nyc %>% 
        filter(lat == aa$lat & lng == aa$lng)
      # populate aLoc with the various attributes from the selected row
      aLoc$nm <- a$name
      aLoc$nh <- a$neighborhood
      aLoc$lt <- a$lat
      aLoc$lg <- a$lng
      aLoc$pr <- a$price
      aLoc$rt <- a$room_type
    }
  )
  
  #####################################
  # some text to display after property selected
  #####################################
  output$propSel <- renderText(
    {
      if (!is.null(aLoc$nm)) {
        paste0(
          '<h3>', aLoc$nm, '</h3>',
          '<table style = "width:100%;">',
          '<tr><td colspan = "2"><h4>- ', aLoc$nh, ' -</h4></td></tr>',
          '<tr><td><h4>', aLoc$rt, '</h4></td>',
          '<td style = "text-align:right"><h2>$', aLoc$pr, '</h2></td></tr>',
          '</table>'
        )
      }
    }
  )
  
  #####################################
  # density plot of selected property price relative to other properties
  #####################################
  output$propPlot <- renderPlot(
    {
      if (!is.null(aLoc$nm)) {
        z <- nyc %>% 
          filter(boro == input$boro) %>% 
          filter(neighborhood == aLoc$nh) %>% 
          sample_n(100)
        ggplot(z, aes(x = price, ..scaled.. , color = room_type, fill = room_type)) +
          geom_density(alpha = 0.5) +
          geom_vline(xintercept = aLoc$pr) +
          xlim(0, 300) +
          labs(x= 'Price', y='Relative Prob') +
          theme_minimal(base_size = 12) +
          theme(legend.position = 'bottom')
      }
    }
  )
  #####################################
  # proj - a/b testing
  #####################################
  #####################################
  # the plot
  #####################################
  output$plotAB <- renderPlotly(
    {
      plot_ly(
        data = ab %>% 
          filter(
            s == 0 * (as.numeric(input$a3) + as.numeric(input$b3) +
                        as.numeric(input$a2) + as.numeric(input$b2)) + 
              ifelse(input$a1, 0, 0) + ifelse(input$b1, 0, 0) +
              sample(1:5, 1)
          ) %>% 
          accumulate_by(~t),
        x = ~t, 
        y = ~y,
        frame = ~frame, 
        type = 'scatter',
        mode = 'lines', 
        line = list(simplyfy = FALSE),
        fill = 'tozeroy', 
        fillcolor='rgba(127, 229, 240, 0.5)'
      ) %>% 
        layout(
          xaxis = list(title = 'Time', zeroline = TRUE),
          yaxis = list(title = 'Market Share', zeroline = TRUE, range = c(0, 1))
        ) %>% 
        animation_opts(frame = 100, transition = 0, redraw = FALSE) %>%
        animation_slider(hide = TRUE) %>% 
        animation_button(visible = TRUE, label = 'RUN') 
    }
  )
  
  #####################################
  # proj - baseball
  #####################################
  #####################################
  # reactive variable to hold stadium info
  #####################################
  baseLoc <- reactiveValues(
    tm = NULL, st = NULL, lt = NULL, lg = NULL #<--team, stadium, latitude, longitude
  )
  #####################################
  # the stadium map
  #####################################
  output$baseMap <- renderLeaflet(
    {
      bIcon <- makeIcon(
        iconUrl = paste0(
          'www/markers/mlb/marker_', base$team_id, '.png'
        ),
        iconWidth = 100,
        iconHeight = 100,
        iconAnchorX = 50,
        iconAnchorY = 100
      )
      leaflet(
        data = base,
        options = leafletOptions(zoomControl = TRUE)
      ) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>% 
        addMarkers(
          lng = ~lng,
          lat = ~lat,
          label = ~site,
          icon = bIcon
        ) %>% 
        setView(lat = 36.778084, lng = -96.7433089, zoom = 5)
    }
  )
  
  #####################################
  # proj - sampling
  #####################################
  #####################################
  # the initial graph
  #####################################
  output$grphSQLa <- renderVisNetwork(
    {
      nodz <- tibble(id = 'x', group = 'A', image = 'cu_light.png') #<--the center node
      edgz <- NULL
      visNetwork(
        nodes = nodz,
        edges = edgz
      ) %>% 
        visNodes(
          shape = 'image',
          physics = TRUE
        ) %>% 
        visGroups(
          groupname = 'A', size = 60, font = list(color = '#000000', size = 1)
        ) %>%
        visGroups(
          groupname = 'Q', size = 40, font = list(color = '#065535', size = 18)
        ) %>% 
        visPhysics(
          stabilization = TRUE,
          solver = 'forceAtlas2Based',
          minVelocity = 0.5
        ) %>% 
        visOptions(
          highlightNearest = list(
            enabled = TRUE,
            hover = TRUE,
            degree = 1
          )
        )
    }
  )

  #####################################
  # action when button clicked
  #####################################
  observeEvent(
    eventExpr = input$sample, #<--id of actnbutton
    {
      visNetworkProxy(shinyId = 'grphSQLa') %>% #<--modify the existing network
        visRemoveNodes(id = c(paste0('q_', 1:50))) %>% 
        visRemoveEdges(id = paste0('e_', 1:50)) 
      m <- input$nSel #<--value selected from dropdown list
      n <- sample(1:50, m) #<--randomly sample m values
      for (i in n) {
        nm <- sql_r %>% filter(id == i) %>% select(id, last, first, image)
        myName <- paste(nm$last, nm$first, sep = ', ')
        myId <- nm$id
        visNetworkProxy(shinyId = 'grphSQLa') %>% #<--modify the existing network
          visUpdateNodes( #<--create new nodes
            nodes = tibble(
              id = paste0('q_', nm$id), 
              group = 'Q', 
              label = paste(nm$first, nm$last, sep = '\n'),
              image = paste('sql', nm$image, sep = '/')
            )
          ) %>% 
          visUpdateEdges( #<--create new edges
            edges = tibble(
              from = 'x',
              to = paste0('q_', nm$id),
              id = paste0('e_', nm$id)
            )
          )
      }
      visNetworkProxy(shinyId = 'grphSQLa') %>% #<--center the graph on center node
        visFocus(
          id = 'x',
          scale = 1.0,
          animation = list(
            duration = 500,
            easingFunction = 'easeInOutQuad'
          )
        )
    }
  )
  
##########################################
# experience
##########################################
  
  output$timx <- renderText(
    # experience 1
    if (input$expBlk == 7) {
      paste0(
        '<h3><table style = "width:100%;">',
        '<tr><td style = "text-align:left;">Larsen & Toubro &mdash; Mumbai, India</td>',
        '<td style = "text-align:right; color: #428bca;">5/2014 &mdash; 8/2014</td></tr>',
        '<tr><td style = "font-style: italic; padding:10px;">Engineering Intern',
        '</td><tr></table></h3><hr>',
        '<ul><h3><li style = "padding:10px">Minimizing welding defects due to crack & slag inclusion by observing the most common defects and analyzing the causes and ways of reducing the defects</li>',
        '<li style = "padding:10px">Reduced defects by 5% in boilers used in Oil and Gas Industry</li>',
        '<li style = "padding:10px">Reduce inefficiencies in the workflow process to enable lower turnaround time</li>',
        '</ul></h3>'
      )
    } else if (input$expBlk == 8) {
      paste0(
        '<h3><table style = "width:100%;">',
        '<tr><td style = "text-align:left;">General Electric &mdash; Pune, India</td>',
        '<td style = "text-align:right; color: #428bca;">4/2015 &mdash; 9/2015</td></tr>',
        '<tr><td style = "font-style: italic; padding:10px;">Management Intern',
        '</td><tr></table></h3><hr>',
        '<ul><h3><li style = "padding:10px">Develop capability for CS/SS and Alloy flow cell fabrication at GE</li>',
        '<li style = "padding:10px">Optimized process for Assembly, Testing and shipment of assembled flow cells to local and global customers</li>',
        '<li style = "padding:10px">Achievements: Reduced cycle time by 20% & reduced inventory of sub parts substantially using Six Sigma Methodology tools.
• Skills gained: Oracle R12 and six sigma implementations.</li>',
        '</ul></h3>'
      )
    # experience 2
    } else if (input$expBlk == 9) {
      paste0(
        '<h3><table style = "width:100%;">',
        '<tr><td style = "text-align:left;">Rhenus Logistics &mdash; Mumbai, India</td>',
        '<td style = "text-align:right; color: #428bca;">6/2016 &mdash; 09/2018</td></tr>',
        '<tr><td style = "font-style: italic; padding:10px;">Assistant Manager - Integrated Logistics Solutions</td><tr></table></h3><hr>',
        '<ul><h3><li style = "padding:10px">Reduced Costing and Improving Profit - Studying, analysing and then providing solutions to various RFQ’s for warehousing and transportation for the customers to give them a consolidated and cost effective solution</li>',
        '<li style = "padding:10px">Designing system - Visiting the clients on a daily basis to identify any loopholes in the current system and to rectify them using data available on transport management system (TMS)</li>',
        '<li style = "padding:10px">Effective Reporting and SOP - Prepare various monthly operational and financial reports for internal & external stakeholders of the company and develop SOP’s for the customers according to their requirements</li>',
        '<li style = "padding:10px"> Liaison with Management & Clients for better & smooth functionality - Working and managing multiple customers at the same time across India including operations whose monthly billing averaged to INR 1.5 cr and simultaneously managing a team of 8-10 people for smooth functioning of the warehouse</li>',
        '<li style = "padding:10px">CRM: Used SugarCRM system to implement sales-forces automation and use its Relationship Analytics solution to maximize revenues by influencing pipelines & renewals and build robust customer relationships</li>',
        '<li style = "padding:10px">Achievements: Cut costs for customer by 6% while maintaining same profitability for company by analysing their most busy lane using excel tools and consolidating them to gain economies of scale
        • Brought down incident rate (leakages) from 16% to 5% by using alternative packaging materials keeping in line with the customer requirements</li>',
        '</ul></h3>'
      )
    # experience 3
    } else if (input$expBlk == 10) {
      paste0(
        '<h3><table style = "width:100%;">',
        '<tr><td style = "text-align:left;">Analytics Consulting Club, &mdash; New York, NY</td>',
        '<td style = "text-align:right; color: #428bca;">9/2019 &mdash; Present</td></tr>',
        '<tr><td style = "font-style: italic; padding:10px;">Secretary</td><tr></table></h3><hr>',
        '<h3><ul><li style = "padding:10px">Setup meetings and enable interaction among members</li>',
        '<li style = "padding:10px">Host events to enhance student skills and provide a platform to educate students about the consulting industry</li>',
        '<li style = "padding:10px">Hold panel sessions from the industry experts</li>',
        '</ul></h3>'
      )
    }  
  )
  
  timB <- reactiveValues(a = NULL, b = NULL)
  
  # action when time block selected
  observeEvent(
    eventExpr = input$expBlk,
    {
      if      (input$expBlk == 7) {timB$a = 2012; timB$b = 2014} 
      else if (input$expBlk == 8) {timB$a = 2014; timB$b = 2015} 
      else if (input$expBlk == 9) {timB$a = 2015; timB$b = 2017} 
      else                        {timB$a = 2017; timB$b = 2019}
      updateSliderInput(
        session = session, 
        inputId = 'expTim', 
        value = c(timB$a, timB$b)
      )
    }
  )
  
  # action when slider changed; prevent anything from happening
  observeEvent(
    eventExpr = input$expTim,
    {
      if      (input$expBlk == 7) {timB$a = 2012; timB$b = 2014}
      else if (input$expBlk == 8) {timB$a = 2014; timB$b = 2015} 
      else if (input$expBlk == 9) {timB$a = 2015; timB$b = 2017} 
      else                        {timB$a = 2017; timB$b = 2019}
      updateSliderInput(
        session = session, 
        inputId = 'expTim', 
        value = c(timB$a, timB$b)
      )
    }
  )
  
##########################################
# education
##########################################
    
    output$eduX <- renderText(
      paste0('<h2><a href = "', ed$u, '" target = "_blank">', 
             ed$n, '</a></h2>',
             '<h4>', ed$c, '</h4><hr>',
             '<h3>', ed$d, '</h3>')
    )
    
    ed <- reactiveValues(
      n = NULL, c = NULL, d = NULL, #<--name, city, degree
      lt = NULL, lg = NULL, u = NULL) #<--lat, lng, url
    
    observeEvent(
      eventExpr = input$eduOpt,
      {
        # undergraduate
        if (input$eduOpt == 1) {
          ed$n <- 'Narsee Monjee Institute of Management Studies' #<--univ name
          ed$c <- 'Mumbai, India' #<--city, state
          ed$d <- 'B.Tech Mechanical Engineering + MBA Technology Management' #<--degree and major
          ed$lt <- 19.10306 #<--latitude of campus
          ed$lg <- 72.83652 #<--longitude of campus
          ed$u <- 'https://engineering.nmims.edu/mechanical-engineering-department/' #<--website of department
        # graduate
        } else if (input$eduOpt == 2) {
          ed$n <- 'Columbia University'
          ed$c <- 'New York, NY'
          ed$d <- 'MS in Applied Analytics'
          ed$lt <- 40.808242
          ed$lg <- -73.9618887
          ed$u <- 'https://sps.columbia.edu/academics/masters/applied-analytics'
        } 
        # modify map of campus
        leafletProxy('eduMap') %>%
          removeMarker('a') %>%
          addAwesomeMarkers(
            layerId = 'a',
            lng = ed$lg,
            lat = ed$lt,
            label = ed$n
            # icon = awesomeIcons(icon = 'education',
            #                     lib = 'glyphicon')
          ) %>% 
          setView(lat = ed$lt, lng = ed$lg, zoom = 17)
      }
      
    )
    
    #map of campus
    output$eduMap <- renderLeaflet(
      {
        leaflet() %>% 
          addProviderTiles(providers$Esri.WorldImagery) %>% 
          addAwesomeMarkers(
            layerId = 'a',
            lng = 40.808242, #<--initial location
            lat = -73.9618887, #<--initial location
            label = 'Columbia University'
          ) %>%
          setView(lat = 40.808242, lng = -73.9618887, zoom = 17)
      }
    )
    
    logoNm <- reactiveValues(n = NULL)
    
    #logo of school in www folder (edu1.png, edu2.png, etc)
    observeEvent(input$eduOpt,
                 {logoNm$n <- paste0('www/edu', input$eduOpt, '.png')}
    )
    
    #render school logo
    output$eduLogo <- renderImage(
      list(src = logoNm$n, width = '100%'),
      deleteFile = FALSE
    )
    

}
    