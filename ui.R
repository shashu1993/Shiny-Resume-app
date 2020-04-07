
ui <- tagList(
  
  useShinyjs(),
  
  setBackgroundImage(
    src = 'mumbai.jpg' #<--background image for entire app / keep in www folder
  ),
  
  ###########################
  # page layout
  ###########################
  navbarPage(
    id = 'navBar',
    title = 'Shashank', #<--your name here
    windowTitle = 'Life in NY', #<--what the tab on browser will state
    position = 'fixed-top', #<--fixed-bottom if you want menu bar on bottom
    collapsible = TRUE, #<--responsive to smaller screens
    inverse = FALSE, #<--invert the colors
    theme = shinytheme('lumen'), #<--select shinytheme https://rstudio.github.io/shinythemes/
      # cerulean
      # cosmo
      # cyborg
      # darkly
      # flatly
      # journal
      # lumen
      # paper
      # readable
      # sandstone
      # simplex
      # slate
      # spacelab 
      # superhero
      # united
      # yeti
    ###########################
    # home page
    ###########################
    tabPanel(
      title = div(
        img(src = 'SHASH.jpg', height = 30) #<--some image/icon to represent you
      ),
      tags$head(
        tags$style(
          type = 'text/css', 
          'body {padding-top: 70px;}' #<--padding for the menu bar; padding-bottom if position = fixed-bottom
        )
      )
    ),
    ###########################
    # projects menu - display all your projects here
    ###########################
    navbarMenu(
      title = 'Projects',
      ###########################
      # airbnb
      ###########################
      tabPanel(
        title = 'Airbnb mapping', #<--menu title for project
        div(
          class = 'right', #<--defined by the styles.css file
            #choose 'left' to see map on 2/3 of left side
            #choose 'right' to see map on 2/3 of right side
          tags$head(includeCSS('styles.CSS')),
          # the map to be defined in the server
          leafletOutput(
            outputId = 'nycMap',
            width = '100%',
            height = '100%'
          )
        ),
        # the control panel on the left or right side depending on
        # the div class above and the settings below
        absolutePanel(
          id = 'controls', class = 'panel panel-default', 
          fixed = TRUE, draggable = FALSE, 
          top = 60, left = 0, right = 'auto', bottom = 0,
          width = '33%', height = '100%',
          pickerInput(
            inputId = 'boro',
            label = 'NYC Borough:',
            choices = c(
              'Manhattan', 
              'Brooklyn', 
              'Queens', 
              'Bronx', 
              'Staten Island'
            ),
            selected = 'Manhattan',
            width = '100%'
          ),
          hr(),
          htmlOutput(
            outputId = 'propSel'
          ),
          plotOutput(
            outputId = 'propPlot'
          )
        )
      ),
      ###########################
      # a/b testing
      ###########################
      tabPanel(
        title = 'A/B Testing', #<--menu title for project
        wellPanel(h3('A/B Testing')),
        wellPanel(
          fluidRow(
            ###########################
            # the controls
            ###########################
            column(width = 5, align = 'center',
              wellPanel(
                style = 'background-color:#b4d2c2;',
                fluidRow(
                  column(width = 2, h4('Feature')),
                  column(width = 5, h4('Product A')),
                  column(width = 5, h4('Product B'))
                ),
                hr(),
                fluidRow(
                  column(width = 2, h4('1')),
                  column(width = 5, switchInput(inputId = 'a1', onLabel = 'Yes', offLabel = 'No', value = FALSE, onStatus = 'danger')),
                  column(width = 5, switchInput(inputId = 'b1', onLabel = 'Yes', offLabel = 'No', value = TRUE))
                ),
                hr(),
                fluidRow(
                  column(width = 2, h4('2')),
                  column(width = 5,
                    radioGroupButtons(
                      inputId = 'a2',
                      label = NULL,
                      choices = c('L' = 1, 'M' = 2, 'H' = 3),
                      selected = 1,
                      justified = TRUE,
                      status = 'danger',
                      checkIcon = list(
                        yes = icon('ok', lib = 'glyphicon')
                      )
                    )
                  ),
                  column(width = 5,
                    radioGroupButtons(
                      inputId = 'b2',
                      label = NULL,
                      choices = c('L' = 1, 'M' = 2, 'H' = 3),
                      selected = 3,
                      justified = TRUE,
                      status = 'primary',
                      checkIcon = list(
                        yes = icon('ok', lib = 'glyphicon')
                      )
                    )
                  )
                ),
                hr(),
                fluidRow(
                  column(width = 2, h4('3')),
                  column(width = 5,
                    sliderInput(
                      inputId = 'a3',
                      label = NULL,
                      min = 0,
                      max = 100,
                      value = 25,
                      ticks = FALSE
                    )
                  ),
                  column(width = 5,
                    sliderInput(
                      inputId = 'b3',
                      label = NULL,
                      min = 0,
                      max = 100,
                      value = 75,
                      ticks = FALSE
                    )
                  )
                )
              )
            ),
            ###########################
            # the plot
            ###########################
            column(width = 7,
              wellPanel(style = 'background-color: #b4d2c2;',
                plotlyOutput(outputId = 'plotAB')
              )
            )
          )
        )
      ),
      ###########################
      # baseball
      ###########################
      tabPanel(
        title = 'Baseball', #<--menu title for project
        div(
          class = 'outer', #<--defined by the styles.css file
          tags$head(includeCSS('styles.CSS')),
          # the map
          leafletOutput(
            outputId = 'baseMap',
            width = '100%',
            height = '100%'
          )
        )
      ),
      ###########################
      # sampling
      ###########################
      tabPanel(
        title = 'Sampling', #<--menu title for project
        div(
          class = 'outer', #<--defined by the styles.css file
          tags$head(includeCSS('styles.CSS')),
          # the graph network
          visNetworkOutput(
            outputId = 'grphSQLa', 
            width = '100%', 
            height = '100%'
          )
        ),
        # the drop down list
        pickerInput(
          inputId = 'nSel',
          label = NULL,
          choices = c(1:50),
          selected = 10,
          multiple = FALSE,
          width = '80px'
        ),
        # the refresh button
        actionBttn(
          inputId = 'sample',
          label = NULL,
          style = 'simple',
          color = 'royal',
          size = 'lg',
          icon = icon(
            name = 'refresh',
            lib = 'glyphicon'
          )
        )
      )
    ),
    
    ###########################
    # about me menu
    ###########################
    navbarMenu(
      title = 'About Me', 
      ###########################
      # education
      ###########################
      tabPanel(
        title = 'Education', #<--menu title for about me
        icon = icon('graduation-cap'), #<--icon next to menu item
        wellPanel(h3('Education')),
        hr(), #<--horizontal line
        fluidRow(
          column(
            width = 5,
            wellPanel(
              align = 'center',
              radioGroupButtons(
                inputId = 'eduOpt',
                label = 'Select an education category to see its details',
                choices = c(
                  'Undergraduate' = 1,
                  'Graduate' = 2
                ),
                selected = 2,
                direction = 'horizontal',
                justified = TRUE,
                status = 'primary',
                size = 'lg',
                checkIcon = list(
                  yes = tags$i(
                    class = 'fa fa-check', 
                    style = 'color: cyan'
                  )
                )
              ),
              conditionalPanel(
                condition = 'input.eduOpt > 0',
                fluidRow(
                  column(
                    width = 9,
                    htmlOutput('eduX')
                  ),
                  column(
                    width = 3,
                    imageOutput(
                      outputId = 'eduLogo', 
                      height = '100%'
                    )
                  )
                )
              )
            )
          ),
          column(
            width = 7,
            wellPanel(
              leafletOutput(
                outputId = 'eduMap',
                height = '600px'
              )
            )
          )
        )
      ),
      ###########################
      # experience
      ###########################
      tabPanel(
        title = 'Experience',
        icon = icon('briefcase'),
        wellPanel(h3('Experience')),
        hr(),
        ###########################
        # Timeblock Viewer
        ###########################
        wellPanel(
          align = 'center',
          radioGroupButtons(
            inputId = 'expBlk',
            label = 'Select a time block to see details from that time period',
            choices = c(
              '2014' = 7,
              '2015' = 8,
              '2016-2018' = 9,
              '2019 - Present' = 10
            ),
            selected = 7,
            justified = FALSE,
            status = 'primary',
            individual = TRUE,
            checkIcon = list(
              yes = tags$i(
                class = 'fa fa-check', 
                style = 'color: cyan'
              )
            )
          ),
          sliderInput(
            inputId = 'expTim',
            label = NULL,
            min =  2012,
            max = 2019,
            value = c(2017, 2019),
            sep = '',
            ticks = FALSE,
            width = '75%'
          )
        ),
        wellPanel(
          htmlOutput('timx')
        )
      ),
      ###########################
      # links
      ###########################
      tabPanel(
        title = 'Links',
        icon = icon('link'),
        h3('Links'),
        hr(),
        fluidRow(
          column(
            width = 8,
            wellPanel(
              fluidRow(
                column(
                  width = 4,
                  align = 'right',
                  h3('Email:')
                ),
                column(
                  width = 8,
                  HTML(
                    paste0(
                      '<h3><a href="mailto:ssc2204columbia.edu">',
                      'ssc2204@columbia.edu</a></h3>'
                    )
                  )
                )
              )
            ),
            wellPanel(
              fluidRow(
                column(
                  width = 4,
                  align = 'right',
                  h3('Resume:')
                ),
                column(
                  width = 8,
                  HTML(
                    paste0(
                      '<a href = "https://www.dropbox.com/s/wihwvxfdsbui8ck/Shashank%20Resume.pdf?dl=0" ',
                      'download = "https://www.dropbox.com/s/wihwvxfdsbui8ck/Shashank%20Resume.pdf?dl=0">',
                      '<img src = "pdficon.png"></img></a>'
                    )
                  )
                )
              )
            ),
            wellPanel(
              fluidRow(
                column(
                  width = 4,
                  align = 'right',
                  h3('LinkedIn:')
                ),
                column(
                  width = 8,
                  HTML(
                    paste0(
                      '<a href = "https://www.linkedin.com/in/shash1993/" ',
                      'target = "_blank">',
                      '<img src = "linkedin.png"></img></a>'
                    )
                  )
                )
              )
            ),
            wellPanel(
              fluidRow(
                column(
                  width = 4,
                  align = 'right',
                  h3('Wechat:')
                ),
                column(
                  width = 8,
                  HTML(
                    paste0(
                      '<a href = "https://www.dropbox.com/s/x7ra50rvth2azsv/IMG_2567.JPG?dl=0',
                      'raw=1" target = "_blank">',
                      '<img src = "wechat.png"></img></a>'
                    )
                  )
                )
              )
            ),
            wellPanel(
              fluidRow(
                column(
                  width = 4,
                  align = 'right',
                  h3('Twitter:')
                ),
                column(
                  width = 8,
                  HTML(
                    paste0(
                      '<a href = "https://twitter.com/shashank93c" ',
                      'target = "_blank">',
                      '<img src = "twitter.png"></img></a>'
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  
)

