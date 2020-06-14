# seems like we need to have library calls so that shinyapps.io can detect what pkgs to
# install
library(plotly)
library(leaflet)
library(DT)
library(dplyr)
library(fatalencounters)
library(usmap)
library(shiny)
library(forcats)

DF <- state_total_calculate() %>%
  filter(YEAR < 2020 & !is.na(YEAR))

DF <- DF %>%
    group_by(YEAR) %>%
    summarize(
        deaths = mean(deaths),
        death_rate = mean(death_rate)) %>%
    mutate(State = "National Average") %>%
    filter(YEAR < 2020) %>%
  bind_rows(DF)

#-------------------------------------------------------UI-----------------------------------------------------------------#

ui <- navbarPage(title = "FFSG", id = "navbar",

  #About Page
  tabPanel(title = "About", value = "tab1",
           fluidPage(fluidRow(
             column(10,
                    h1("Fatal Force Study Group")),
             column(2,
                    icon('question-circle', class='fa-2x helper-btn'),
                    tags$div(class="helper-box", style="display:none",
                             p('Use the tabs across the top navigation bar to access the data 
                               or information.  Each tab has additional help buttons with context 
                               specific info.')),
                    actionLink('abtleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), 
                               label=NULL),
                    actionLink('abtright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), 
                               label=NULL)

             )
             )),
           sidebarLayout(
             sidebarPanel(

               h3("Data Resources"),
               uiOutput("felink"),
               #h6("This is a crowd-sourced data set that was started in 2012, and has back-filled data to 2000.  It is the only actively maintained dataset with this range of coverage. This dataset is open-source and can be downloaded.  Please consider donating"),
               #h3("Additional Information")

             ),
             mainPanel(
               h2("About"),
               p("The UW Fatal Force Study Group (FFSG) was originally established at the University of 
               Washington by Prof Martina Morris (Statistics and Sociology), with assistance 
               from Prof Ben Marwick (Anthropology, e-Science) and a group of undergraduate
               students. The purpose of this project 
               is to provide direct public access to the only national dataset available that 
               tracks the number of people killed by police use of deadly force back to 2000."),
               p("The codebase was initially developed by undergraduate research students at UW as 
                  part of an independent research project.  The group expanded to include 
                  undergraduates from Western Washington University, and graduate students from UW."),
               h3("Our goals"),
               p("FFSG's mission is to help bring justice and peace to communities most impacted 
                  by police brutality."),
               strong("First:"),
               ("We hope that by providing simple access to these data, 
                  community members are empowered to use the information in their fight to 
                  change the policies that govern the use of deadly force by law enforcement, 
                  and the accountbility of officers who abuse this power."),
               p(),
               strong("Second:"),
                ("We have made this an
                  open-source project that follows the principles of reproducible research,
                  relies on free software (R, GitHub, shinyapps.io), and encourages collaboration.
                  We welcome others to contribute and improve both the
                  dataset and the app for exploring it"),
               p(),
               strong("Finally"), 
               ("this is our first draft of the data exploration app.  We are releasing it
                  so that we can get comments and suggestions from people like you.  Please
                  stay tuned as it evolves."),
               h3("A note on the data:"),
               ("While the Fatal Encounters dataset is the most complete record
                  we have of deaths caused by law enforcement in the US, it is neither complete,
                  nor error free.  Missing data occurs at many levels:  cases may be missing entirely,
                  variables of interest are not included (for example, the names of the officers), and
                  even when a variable is included it may still be missing for a substantial
                  number of cases.  For this 
                  reason it is important to interpret the values, trends and comparisons you observe
                  here with care.  One thing you can be certain of is that the numbers of cases 
                  recorded here are underestimates, we just don't know by how much."),
             )
           )
  ),

  #Tab for Tables and Plots
  navbarMenu(
    "Tables and Plots",

    #Page for counts and per capita values of Fatal Encounters
    tabPanel(title = "State Trends", value = "tab2",

             fluidPage(
               fluidRow(
                 column(10,
                        h1("Trends: 2000-2019")),
                 column(2,
                          icon('question-circle', class='fa-2x helper-btn'),
                          tags$div(class="helper-box", style="display:none",
                                 p("View trends by state over the years
                                    2000 to 2019. The 'Plot' and 'Table' tabs
                                    allow you to switch between viewing
                                    the data graphically or tabulated by 
                                    demographic attributes.")),
                        actionLink('cntleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                        actionLink('cntright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)

                 )
               )
             ),

             sidebarLayout(
               sidebarPanel(
                 selectInput("state", "State", c(sort(c(state.name, "District of Columbia")), "National Average"), selected = "Washington"),
                 checkboxInput("all", "Display with other states", FALSE),
                 icon('question-circle', class='fa-2x helper-btn-small'),
                 tags$div(class="helper-box-small", style="display:none",
                                   p("Selected state is highlighted,
                                    with the other states and the US average displayed
                                     in background. (US average is only
                                     shown for per capita values)")),

                 checkboxInput("per capita", "Calculate per 100K", TRUE),
                 icon('question-circle', class='fa-2x helper-btn-small'),
                 tags$div(class="helper-box-small", style="display:none",
                                   p("When selected values are
                                     calculated as a number of
                                     fatalities per 100K
                                     persons in the population.
                                     Otherwise displays total
                                     number of fatalities."))
               ),
               #Creates plot and table tabs so user can view data in either form
               mainPanel(tabsetPanel(
                 type = "tabs",
                 tabPanel("plot", plotOutput("perCapitaPlot")),
                 tabPanel("table", dataTableOutput("perCapitaDT"))
               ))
             )
    ),

    #Page for stats based on demographics: Race, Gender, Age
    tabPanel(title= "Demographic Breakdowns", value="tab3",

             fluidPage(
               fluidRow(
                 column(10,
                        h1("Breakdowns")),
                 column(2,
                        icon('question-circle', class='fa-2x helper-btn'),
                        tags$div(class="helper-box", style="display:none",
                            p("Displays total number of fatal
                              encounters in the US by demographic
                              (race, age, or gender).")),
                        actionLink('dsleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                        actionLink('dsright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)

                 )
               )
             ),

             sidebarLayout(
               sidebarPanel(
                 selectInput("dem", "Demographic Attribute", c("Race", "Gender", "Age")),
                 h6("Disclaimer: Please take note that the data on demographic attributes can be missing, especially for race."),
                 icon('question-circle', class='fa-2x helper-btn-small'),
                 tags$div(class="helper-box-small", style="display:none",
                          p("Trends will differ based on
                            if the data is missing at
                            random (meaning that each group
                            is just as likely to have been
                            marked as unspecified) or not.
                            If not the trends will change."))
               ),
               mainPanel(dataTableOutput("dstbl"), plotOutput("dsplt"))
             )
    )
  ),

  #Tab for Maps
  navbarMenu("Maps",

             #Choropleth Page
             tabPanel(title = "Choropleth", value = "tab4",

                      fluidPage(
                        fluidRow(
                          column(10,
                                 h1("Choropleth Map of Deaths per Capita")),
                          column(2,
                                 icon('question-circle', class='fa-2x helper-btn'),
                                 tags$div(class="helper-box", style="display:none",
                                          p("Map displays distribution of fatal
                                            events by state. States that are
                                            darker have more deaths per capita.
                                            Hovering over a state displays
                                            the state's name and number of
                                            fatal events per capita.")),
                                 actionLink('cpleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                 actionLink('cpright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)

                          )
                        )
                      ),

                      sidebarLayout(
                        sidebarPanel(
                          #displays mean and when selected shows values by year
                          h5("Map is shown for mean values, to select a year choose Select Year"),
                          checkboxInput("yearselect", "Select Year", FALSE),
                          conditionalPanel("input.yearselect",
                                           sliderInput("year", "Year", 2000, max(DF$YEAR, na.rm = T),
                                                       value = 2000,
                                                       animate = animationOptions(1500, TRUE),
                                                       sep = "") # BM: make the numbers look like years
                          )
                        ),
                        mainPanel(plotOutput("choropleth"))
                      )
             ),

             #Interactive Map Page
             tabPanel(title = "Interactive", value = "tab5",
                      fluidPage(
                        fluidRow(
                          column(10,
                                 h1("Interactive Map")),
                          column(2,
                                 icon('question-circle', class='fa-2x helper-btn'),
                                 tags$div(class="helper-box", style="display:none",
                                          p("Map displays counts based on region
                                            clicking on bubbles or zooming in
                                            breaks bubbles into smaller areas.
                                            At lowest level individual cases
                                            are showed and can be clicked on to
                                            display more info in a pop-up.")),
                                 actionLink('intleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                 actionLink('intright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)

                          )
                        ),
                        leafletOutput("intmap")
                      )
             )
  ),

  #THIS IS TAB PANEL PURGATORY
  #tabPanel("Data Analytics"),
  #
  #tabPanel("Data Compiling"),
  inverse = TRUE,
  tags$head(
    tags$script(type="text/javascript", src="alert.js"),
    tags$link(rel="stylesheet", type="text/css",href="style.css")
  )


)

#-------------------------------------------------------Server----------------------------------------------------------#

server <- function(input, output, session) {

  #update active tab in navbar when arrows are clicked
  leftarrowclicks <- reactive({
    input$abtleft+input$cntleft+input$dsleft+input$cpleft+input$intleft
  })
  rightarrowclicks <- reactive({
    input$abtright+input$cntright+input$dsright+input$cpright+input$intright
  })
  observe({
    if(leftarrowclicks() == 0) {return()}
    tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5')
    current <- isolate(which(input$navbar==tabOptions))
    updateTabsetPanel(session, 'navbar', selected=tabOptions[current-1])
  })
  observe({
    if(rightarrowclicks() == 0) {return()}
    tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5')
    current <- isolate(which(input$navbar==tabOptions))
    updateTabsetPanel(session, 'navbar', selected=tabOptions[current+1])
  })

  #Outputs for tables, plots, and maps
  #Plot for fatal encounter total or capita values by state
  output$perCapitaPlot <-
    renderPlot({
        if(input$all | input$state == "National Average"){
          DF %>%
              filter(YEAR < 2020) %>%
              mutate(out = ifelse(rep(input$`per capita`, nrow(.)), death_rate, deaths)) %>%
              mutate(col_ = case_when(
                  State == input$state ~ input$state,
                  State == "National Average" ~ "National Average",
                  TRUE ~ "Other States"
              )) %>%
              mutate(alpha_ = ifelse(
                  State == input$state | State == "National Average", .9, .4)) %>%
              mutate(col_ = factor(
                col_, unique(c("National Average", "Other States", input$state)))) %>%
              filter(State == input$state | rep(input$all, nrow(.))) %>%
              ggplot(aes(x = YEAR, y = out, color = col_, alpha = alpha_, group = State)) +
              geom_line() +
              theme_classic() +
              guides(alpha=FALSE) +
              labs(x="Year", y=ifelse(input$`per capita`, "Rate per 100k", "Count"),
                   color="") +
              theme(
                  legend.text = element_text(size=13),
                  legend.title = element_text(size=15),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=17),
                  title =  element_text(size=20))
        }
        else{
          DF %>%
            filter(YEAR < 2020) %>%
            mutate(out = ifelse(rep(input$`per capita`, nrow(.)), death_rate, deaths)) %>%
            mutate(col_ = case_when(
              State == input$state ~ input$state,
              State == "National Average" ~ "National Average",
              TRUE ~ "Other State"
            )) %>%
            mutate(alpha_ = ifelse(
              State == input$state | State == "National Average", .9, .4)) %>%
            mutate(col_ = factor(
              col_, unique(c("National Average", "Other State", input$state)))) %>%
            filter(State == input$state | rep(input$all, nrow(.))) %>%
            ggplot(aes(x = YEAR, y = out, alpha = alpha_, group = State)) +
            geom_line(color="blue") +
            theme_classic() +
            guides(alpha=FALSE) +
            labs(x="Year",
                 y=ifelse(input$`per capita`, "Rate per 100k", "Count"),
                 color="") +
            theme(
              legend.text = element_text(size=13),
              legend.title = element_text(size=15),
              axis.text = element_text(size=13),
              axis.title = element_text(size=17),
              title =  element_text(size=20))
          }
    })
  #Data table for fatal encounter total or capita values by state
  output$perCapitaDT <-
    renderDataTable({
      DF %>%
        filter(YEAR != 2100) %>%
        filter(State == input$state | rep(input$all, nrow(.))) %>%
        mutate(death_rate = round(death_rate, 2)) %>%
        mutate(deaths = round(deaths, 2))
    })
  #Choropleth Map
  output$choropleth <-
    renderPlot({
      if(input$yearselect){
        DF %>%
          filter(State != "National Average") %>%
          filter(YEAR == input$year) %>%
          select(death_rate, fips = GEOID) %>%
          {plot_usmap(data=., values = "death_rate")} +
          scale_fill_distiller(
            limits = c(0, max(DF$death_rate, na.rm = TRUE)),
            name = "Deaths per\n100k",
            palette = "Spectral") +
          theme(legend.position = "right")
      }else{
        DF %>%
          filter(State != "National Average") %>%
          group_by(GEOID) %>%
          summarize(death_rate = mean(death_rate, na.rm = TRUE)) %>%
          rename(fips = GEOID) %>%
          {plot_usmap(data=., values = "death_rate")} +
          scale_fill_distiller(
            name = "Deaths per\n100k",
            palette = "Spectral") +
          theme(legend.position = "right")
      }
    })
  #Interactive Leaflet Map
  output$intmap <-
    renderLeaflet({
        leaflet(data = fe_df, width = "100%") %>% addTiles() %>%
            addMarkers( ~ as.double(fe_df$Longitude),
                        ~ as.double(fe_df$Latitude),
                        popup = ~ paste0(
                            "Name: ", fe_df$`Subject's name`,
                            "<br><br>City: ", fe_df$`Location of death (city)`,
                            "<br><br>Description ",fe_df$`Date&Description`),
                        label = ~ fe_df$`Subject's name`,
                        clusterOptions = markerClusterOptions())
    })
  #Data Table for demographics (Race, Gender, Age)
  output$dstbl <- renderDataTable({
      if (input$dem == "Age") {
          df <- fe_df %>%
              mutate(Age = as.numeric(`Subject's age`)) %>%
              mutate(Age = cut(
                  Age, c(0, 15, 35, 65, Inf),
                  c("0-14", "15-34", "35-64", "65+"))) %>%
              mutate(Age = fct_explicit_na(Age, "Missing")) %>%
              group_by(Age) %>%
              summarize(N=n())
      }
      else if(input$dem == "Gender") {
          df <- fe_df %>%
              mutate(`Subject's gender` = case_when(
                  is.na(`Subject's gender`) ~ "Missing",
                  `Subject's gender` == "White" ~ "Missing",
                  `Subject's gender` == "Transexual" ~ "Transgender",
                  TRUE ~ `Subject's gender`
              )) %>%
              mutate(`Subject's gender` = fct_relevel(
                  `Subject's gender`, "Missing", after = Inf
              )) %>%
              group_by(`Subject's gender`) %>%
              summarize(N=n())
      }
      else {
          df <- fe_df %>%
            mutate(`Subject's race with imputations` = case_when(
                  `Subject's race with imputations` == "NA" ~ "Missing",
                  is.na(`Subject's race with imputations`) ~ "Missing",
                  `Subject's race with imputations` == "HIspanic/Latino" ~ "Hispanic/Latino",
                  `Subject's race with imputations` == "Middle Eastern" ~ "Other Race",
                  `Subject's race with imputations` == "European American/White" ~ "European-American/White",
                  `Subject's race with imputations` == "Race unspecified" ~ "Missing",
                  TRUE ~ `Subject's race with imputations`
              )) %>%
              mutate(`Subject's race with imputations` = fct_relevel(
                  `Subject's race with imputations`, "Missing", after = Inf
              )) %>%
              group_by(`Subject's race with imputations`) %>%
              summarize(N=n())
      }
      df
  })
  #Plot for demographics (Race, Gender, Age)
  output$dsplt <- renderPlot({
      if (input$dem == "Age") {
          df <- fe_df %>%
              mutate(`Subject's age` = as.numeric(`Subject's age`)) %>%
              ggplot(aes(x=`Subject's age`)) +
              geom_bar() +
              theme_classic() +
              theme(
                  legend.text = element_text(size=13),
                  legend.title = element_text(size=15),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=17),
                  title = element_text(size=20))
      }
      else if(input$dem == "Gender") {
          df <- fe_df %>%
              mutate(`Subject's gender` = case_when(
                  is.na(`Subject's gender`) ~ "Missing",
                  `Subject's gender` == "White" ~ "Missing",
                  `Subject's gender` == "Transexual" ~ "Transgender",
                  TRUE ~ `Subject's gender`
              )) %>%
              mutate(`Subject's gender` = fct_relevel(
                  `Subject's gender`, "Missing", after = Inf
              )) %>%
              ggplot(aes(x=`Subject's gender`)) +
              geom_bar() +
              theme_classic() +
              theme(
                  legend.text = element_text(size=13),
                  legend.title = element_text(size=15),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=17),
                  title = element_text(size=20))
      }
      else {
          df <- fe_df %>%
              mutate(`Subject's race with imputations` = case_when(
                  `Subject's race with imputations` == "NA" ~ "Missing",
                  is.na(`Subject's race with imputations`) ~ "Missing",
                  `Subject's race with imputations` == "HIspanic/Latino" ~ "Hispanic/Latino",
                  `Subject's race with imputations` == "Middle Eastern" ~ "Other Race",
                  `Subject's race with imputations` == "European American/White" ~ "European-American/White",
                  `Subject's race with imputations` == "Race unspecified" ~ "Missing",
                  TRUE ~ `Subject's race with imputations`
              )) %>%
              mutate(`Subject's race with imputations` = fct_relevel(
                  `Subject's race with imputations`, "Missing", after = Inf
              )) %>%
              ggplot(aes(x=`Subject's race with imputations`)) +
              geom_bar() +
              theme_classic() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              theme(
                  legend.text = element_text(size=13),
                  legend.title = element_text(size=15),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=17),
                  title =  element_text(size=20))
      }
      df
  })

  output$felink <- renderUI({
    tagList(a("Fatal Encounters", href="http://www.fatalencounters.org"), 
            "- This is a crowd-sourced dataset that was started in 2012, 
            and has back-filled data to 2000.  The people behind this effort
            are continuing to add information fields
            and cases, making this the only actively maintained dataset 
            with this range of coverage. The dataset is open-source, can be downloaded, and
            the maintainers encourage corrections and submissions from the public.",
            p(),
            p("There is no official government data collected on these fatalities,
            so the Fatal Encounters dataset is a critical public resource.
            Please consider donating to the organization that produces it."))  
    })
}
# deleted KBP link

shinyApp(ui, server)
