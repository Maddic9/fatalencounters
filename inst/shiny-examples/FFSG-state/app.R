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
library(stringr)

clean_plotly_leg <- function(.plotly_x) {
    for(i in 1: length(.plotly_x)){
    if ("legendgroup" %in% names(.plotly_x[[i]])) {
        .plotly_x[[i]]$legendgroup <- str_sub(.plotly_x[[i]]$legendgroup, 4, -2)
        .plotly_x[[i]]$name <- str_sub(.plotly_x[[i]]$name, 4, -2)
        } 
    }
  .plotly_x
}

DF <- state_total_calculate() %>%
    group_by(YEAR) %>%
    mutate(`Rank(Rate)` = 51 - rank(death_rate, ties.method = "first")) %>%
    mutate(`Rank(Count)` = 51 - rank(deaths, ties.method = "first")) %>%
    ungroup()

DF <- DF %>%
    group_by(YEAR) %>%
    summarize(
        death_rate = sum(deaths)/ sum(Population) * 100000,
        deaths = sum(deaths),
        Population = sum(Population)) %>%
    mutate(State = "United States") %>%
  bind_rows(DF) %>%
  select(-state_abb)

raceDF <- state_race_calculate() %>%
    group_by(Race) %>%
    summarize(deaths = sum(deaths), Population = sum(Population)) %>%
    mutate(Race = factor(Race)) %>%
    mutate(Race = fct_relevel(Race, "Missing", after = Inf)) %>%
    mutate(death_rate = deaths/Population*100000) %>%
    mutate(Proportion = deaths/sum(deaths)) %>%
    select(-Population)

ageDF <- state_age_calculate() %>%
    group_by(Age) %>%
    summarize(deaths = sum(deaths), Population = sum(Population)) %>%
    mutate(Age = c(
        rep("0-14", 3), rep("15-44", 5), rep("45+", 5), "Missing")) %>%
    group_by(Age) %>%
    summarize(deaths = sum(deaths), Population = sum(Population)) %>%
    mutate(death_rate = deaths/Population*100000) %>%
    mutate(Proportion = deaths/sum(deaths)) %>%
    select(-Population)

sexDF <- state_sex_calculate() %>%
    group_by(Sex) %>%
    summarize(deaths = sum(deaths), Population = sum(Population)) %>%
    mutate(Sex = factor(Sex)) %>%
    mutate(Sex = fct_relevel(Sex, "Missing", after = Inf)) %>%
    mutate(death_rate = deaths/Population*100000) %>%
    mutate(Proportion = deaths/sum(deaths)) %>%
    select(-Population)

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
               
               h3("Contact Us:"),
               tagList(
                 "If you have suggestions for how to improve this app or see",
                 " any errors with the data feel free to contact us by ",
                 " sending an email to ",
                 a("nmmarquez@protonmail.com", 
                   href="mailto:nmmarquez@protonmail.com"), 
                 " with the subject Fatal Encounters App."),
               
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
                  so that we can get comments and suggestions from people like you (use the
                  'Contact Us'
                  link in sidebar at left).  Please stay tuned as it evolves."),
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
                 selectInput("state", "State", sort(c(state.name, "District of Columbia")), selected = "Washington"),

                 checkboxInput("per capita", "Calculate per 100K", TRUE),
                 icon('question-circle', class='fa-2x helper-btn-small'),
                 tags$div(class="helper-box-small", style="display:none",
                                   p("When selected values are
                                     calculated as a number of
                                     fatalities per 100K
                                     persons in the population.
                                     Otherwise displays total
                                     number of fatalities.")),
                 
                 checkboxInput("national", "Include National Values", FALSE),
                 icon('question-circle', class='fa-2x helper-btn-small'),
                 tags$div(class="helper-box-small", style="display:none",
                          p("When selected national values are shown in the line graph."))
               ),
               #Creates plot and table tabs so user can view data in either form
               mainPanel(tabsetPanel(
                 type = "tabs",
                 tabPanel("plot", plotlyOutput("perCapitaPlot")),
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
                 h6("Disclaimer: Note that the data on demographic attributes 
                    can be missing in the news reports from which some of these data are
                    drawn.  This mainly affects the race variable: about 25% of cases
                    are missing race in the original reports.  The Fatal Encounters
                    dataset provides imputed values for these missing data, and we use
                    those imputed values here."),
                 icon('question-circle', class='fa-2x helper-btn-small'),
                 tags$div(class="helper-box-small", style="display:none",
                          p("Counts/Rates by race include imputed values."))
               ),
               mainPanel(dataTableOutput("dstbl"), plotOutput("dsplt"))
             )
    ),
    
    tabPanel(title= "Incident Description", value="tab4",
             
             fluidPage(
               fluidRow(
                 column(10,
                        h1("Descriptors")),
                 column(2,
                        icon('question-circle', class='fa-2x helper-btn'),
                        tags$div(class="helper-box", style="display:none",
                                 p("Displays aspects of the incident.")),
                        actionLink('cirleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                        actionLink('cirright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                        
                 )
               )
             ),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "cir", "Topic", 
                   c("Case Disposition", "Crisis/Alcohol/Drugs", "Cause of Death")),
                 h6("Characteristics of an incident."),
                 icon('question-circle', class='fa-2x helper-btn-small'),
                 tags$div(class="helper-box-small", style="display:none",
                          p("Data exclude incidents where the cause of death 
                            mentioned suicide."))
               ),
               mainPanel(dataTableOutput("cirtbl"), plotOutput("cirplt"))
             )
    )
  ),

  #Tab for Maps
  navbarMenu("Maps",

             #Choropleth Page
             tabPanel(title = "Choropleth", value = "tab5",

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
                          h5("Initial map shows the mean state rates across all years; 
                             clicking the `Select Year` box allows you to pick a specific 
                             year, or to animate the changes over time."),
                          checkboxInput("yearselect", "Select Year", FALSE),
                          conditionalPanel("input.yearselect",
                                           sliderInput("year", "Year", 2000, max(DF$YEAR, na.rm = T),
                                                       value = 2000,
                                                       animate = animationOptions(1500, TRUE),
                                                       sep = "") # BM: make the numbers look like years
                          )
                        ),
                        mainPanel(plotlyOutput("choropleth"))
                      )
             ),

             #Interactive Map Page
             tabPanel(title = "Interactive", value = "tab6",
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
    input$abtleft+input$cntleft+input$dsleft+input$cirleft+input$cpleft+input$intleft
  })
  rightarrowclicks <- reactive({
    input$abtright+input$cntright+input$dsright+input$cirright+input$cpright+input$intright
  })
  observe({
    if(leftarrowclicks() == 0) {return()}
    tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5', 'tab6')
    current <- isolate(which(input$navbar==tabOptions))
    updateTabsetPanel(session, 'navbar', selected=tabOptions[current-1])
  })
  observe({
    if(rightarrowclicks() == 0) {return()}
    tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5', 'tab6')
    current <- isolate(which(input$navbar==tabOptions))
    updateTabsetPanel(session, 'navbar', selected=tabOptions[current+1])
  })

  #Outputs for tables, plots, and maps
  #Plot for fatal encounter total or per capita values by state
  output$perCapitaPlot <-
    renderPlotly({
          out <- DF %>%
              mutate(out = ifelse(rep(input$`per capita`, nrow(.)), death_rate, deaths)) %>%
              mutate(col_ = case_when(
                  State == input$state ~ input$state,
                  State == "United States" ~ "United States",
                  TRUE ~ "Other States"
              )) %>%
              mutate(alpha_ = ifelse(
                  State == input$state | State == "United States", .9, .6)) %>%
              mutate(lw_ = ifelse(State == input$state , "2", "1")) %>%
              mutate(col_ = factor(
                col_, unique(c("Other States", input$state, "United States")))) %>%
              filter(State != "United States" | rep(input$national, nrow(.))) %>% 
              ggplot(aes(
                x = YEAR, y = out, color = col_, alpha = alpha_, group = State,
                text = paste0(
                    "State: ", State, "\n",
                    "Year: ", YEAR, "\n",
                    ifelse(input$`per capita`, "Rate per 100k", "Count"), ": ",
                    round(out, 2))
                    )) +
              geom_line(aes(size=lw_), alpha=0.5) +
              theme_classic() +
              guides(alpha=FALSE, size=FALSE) +
              labs(x="Year", y=ifelse(input$`per capita`, "Rate per 100k", "Count"),
                   color="") +
              scale_size_manual( values = c(`2` = 1, `1` = .5)) +
              scale_colour_manual(values = c("gray85", "steelblue", "orangered4")) +
              theme(
                  legend.text = element_text(size=13),
                  legend.title = element_text(size=15),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=17),
                  title =  element_text(size=20))
      
      ggpout <- ggplotly(out, tooltip = "text")
      ggpout$x$data <- clean_plotly_leg(ggpout$x$data)
      ggpout
    })
  #Data table for fatal encounter total, per capita values and ranks by state
  output$perCapitaDT <-
    renderDataTable({
        DF %>%
            filter(State == input$state) %>% 
            mutate(`Death Rate` = round(death_rate, 2)) %>%
            mutate(`Death Count` = round(deaths, 2)) %>%
            select(
              Year=YEAR, State, `Death Count`, `Death Rate`,
              `Rank(Count)`, `Rank(Rate)`) %>%
        left_join(
            DF %>%
                filter(State == "United States") %>%
                mutate(`US Rate` = round(death_rate, 2)) %>%
                select(Year=YEAR, `US Rate`),
            by = "Year"
        )
    }, caption = "Ranks:  1=highest, 51=lowest (with DC)")
  #Choropleth Map
  output$choropleth <-
    renderPlotly({
      if(input$yearselect){
        out <- DF %>%
          filter(State != "United States") %>%
          filter(YEAR == input$year) %>%
          select(death_rate, fips = GEOID, State) %>%
          mutate(t_ = paste0(
            "State: ", State, "\n", "Rate: ", round(death_rate, 2))) %>%
          {plot_usmap(data=., values = "death_rate")} +
          scale_fill_distiller(
            limits = c(0, max(DF$death_rate, na.rm = TRUE)),
            name = "Deaths per\n100k",
            palette = "YlOrRd", direction = 1) +
          aes(text=t_) +
          theme(legend.position = "right")
      }else{
        out <- DF %>%
          filter(State != "United States") %>%
          group_by(GEOID, State) %>%
          summarize(death_rate = mean(death_rate, na.rm = TRUE)) %>%
          mutate(t_ = paste0(
              "State: ", State, "\n", "Rate: ", round(death_rate, 2))) %>%
          rename(fips = GEOID) %>%
          {plot_usmap(
              data=., values = "death_rate")} +
          aes(text=t_) +
          scale_fill_distiller(
            name = "Deaths per\n100k",
            palette = "YlOrRd", direction = 1) +
          theme(legend.position = "right")
      }
      ggplotly(out, tooltip = "text")
    })
  #Interactive Leaflet Map
  output$intmap <-
    renderLeaflet({
        fe_df %>%
            # error in lat long here
            filter(`Subject's name` != "James Edward Blackmon") %>%
            leaflet(width = "100%") %>% 
            addTiles() %>%
            addMarkers( ~ as.double(Longitude),
                        ~ as.double(Latitude),
                        popup = ~ paste0(
                            "Name: ", `Subject's name`,
                            "<br><br>City: ", `Location of death (city)`,
                            "<br><br>Description ",`Date&Description`),
                        label = ~ `Subject's name`,
                        clusterOptions = markerClusterOptions())
    })
  #Data Table for demographics (Race, Gender, Age)
  output$dstbl <- renderDataTable({
      if (input$dem == "Age") {
          df <- ageDF %>%
              mutate(Age = as.character(Age)) %>%
              mutate(death_rate = round(death_rate, 2)) %>%
              mutate(deaths = round(deaths, 2)) %>%
              rename(`Death Count` = deaths, `Death Rate` = death_rate) %>%
              mutate(Proportion = round(Proportion, 2))
      }
      else if(input$dem == "Gender") {
          df <- sexDF %>%
              rename(Gender = Sex) %>%
              mutate(Gender = as.character(Gender)) %>%
              mutate(death_rate = round(death_rate, 2)) %>%
              mutate(deaths = round(deaths, 2)) %>%
              rename(`Death Count` = deaths, `Death Rate` = death_rate) %>%
              mutate(Proportion = round(Proportion, 2))
      }
      else {
          df <- raceDF %>%
              mutate(Race = as.character(Race)) %>%
              mutate(death_rate = round(death_rate, 2)) %>%
              mutate(deaths = round(deaths, 2)) %>%
              rename(`Death Count` = deaths, `Death Rate` = death_rate) %>%
              mutate(Proportion = round(Proportion, 2))
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
              labs(x="Age", y="Deaths") +
              theme(
                  legend.text = element_text(size=13),
                  legend.title = element_text(size=15),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=17),
                  title = element_text(size=20))
      }
      else if(input$dem == "Gender") {
          df <- sexDF %>%
              ggplot(aes(x=Sex, y=deaths)) +
              geom_col() +
              theme_classic() +
              labs(x="Gender", y="Deaths") +
              theme(
                  legend.text = element_text(size=13),
                  legend.title = element_text(size=15),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=17),
                  title = element_text(size=20))
      }
      else {
          df <- raceDF %>%
              ggplot(aes(x=Race, y=deaths)) +
              geom_col() +
              theme_classic() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              labs(x="Race/Ethnicity", y="Deaths") +
              theme(
                  legend.text = element_text(size=13),
                  legend.title = element_text(size=15),
                  axis.text = element_text(size=13),
                  axis.title = element_text(size=17),
                  title =  element_text(size=20))
      }
      df
  })
  
  #Data Table for incident attributes (Disposition, MI/Alch/DU, COD)
  output$cirtbl <- renderDataTable({
    if (input$cir == "Case Disposition") {
      df <- fe_df_clean %>%
        group_by(disposition) %>%
        summarize(Events = n()) %>%
        rename(`Disposition` = disposition) %>%
        mutate(Proportion = round(Events / sum(Events), 2))
    }
    else if(input$cir == "Crisis/Alcohol/Drugs") {
      df <- fe_df_clean %>%
        group_by(mental_illness) %>%
        summarize(Events = n()) %>%
        rename(`Mental Illness` = mental_illness) %>%
        mutate(Proportion = round(Events / sum(Events), 2))
    }
    else {
      df <- fe_df_clean %>%
        group_by(cod) %>%
        summarize(Events = n()) %>%
        rename(`Cause of Death` = cod) %>%
        mutate(Proportion = round(Events / sum(Events), 2))
    }
    df
  })
  #Plot for incident attributes (Disposition, MI/Alch/DU, COD)
  output$cirplt <- renderPlot({
    if (input$cir == "Case Disposition") {
      df <- fe_df_clean %>%
        ggplot(aes(x=disposition)) +
        geom_bar() +
        theme_classic() +
        labs(x="Disposition", y="Events") +
        theme(
          legend.text = element_text(size=13),
          legend.title = element_text(size=15),
          axis.text = element_text(size=13),
          axis.title = element_text(size=17),
          title = element_text(size=20))
    }
    else if(input$cir == "Crisis/Alcohol/Drugs") {
      df <- fe_df_clean %>%
        ggplot(aes(x=mental_illness)) +
        geom_bar() +
        theme_classic() +
        labs(x="Victim Assesment", y="Events") +
        theme(
          legend.text = element_text(size=13),
          legend.title = element_text(size=15),
          axis.text = element_text(size=13),
          axis.title = element_text(size=17),
          title = element_text(size=20))
    }
    else {
      df <- fe_df_clean %>%
        ggplot(aes(x=cod)) +
        geom_bar() +
        theme_classic() +
        labs(x="Cause of Death", y="Events") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(
          legend.text = element_text(size=13),
          legend.title = element_text(size=15),
          axis.text = element_text(size=13),
          axis.title = element_text(size=17),
          title = element_text(size=20))
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
