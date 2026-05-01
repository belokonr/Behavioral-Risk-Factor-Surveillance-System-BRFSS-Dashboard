# Final Draft

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
source("initialize.r")


# ----------------------------------------------------------
# REGION DEFINITIONS
# ----------------------------------------------------------
regions <- list(
  "All States" = NULL,
  "By Region" = c("ME","NH","VT","MA","RI","CT","NY","NJ","PA","OH","MI","IN","IL","WI","MN","IA","MO","ND","SD","NE","KS",
                  "DE","MD","DC","VA","WV","NC","SC","GA","FL",
                  "KY","TN","AL","MS","AR","LA","OK","TX", "MT","ID","WY","CO","NM","AZ","UT","NV",
                  "WA","OR","CA","AK","HI", "PR", "VI", "GU"),
  "Northeast" = c("ME","NH","VT","MA","RI","CT","NY","NJ","PA"),
  "Midwest"   = c("OH","MI","IN","IL","WI","MN","IA","MO","ND","SD","NE","KS"),
  "South"     = c("DE","MD","DC","VA","WV","NC","SC","GA","FL",
                  "KY","TN","AL","MS","AR","LA","OK","TX"),
  "West"      = c("MT","ID","WY","CO","NM","AZ","UT","NV",
                  "WA","OR","CA","AK","HI"),
  "Territories" = c("PR", "VI", "GU")
)

# ----------------------------------------------------------
# AGE GROUP DEFINITIONS
# ----------------------------------------------------------
age_groups <- list(
  "All Ages" = NULL,
  "18 - 24" = c("18-24"),
  "25 - 34" = c("25-34"),
  "35 - 44" = c("35-44"),
  "45 - 54" = c("45-54"),
  "55 - 64" = c("55-64"),
  "65+"     = c("65+")
)


# ----------------------------------------------------------
# SEX GROUP DEFINITIONS
# ----------------------------------------------------------
sex_groups <- list(
  "All Sexes" = NULL,
  "Male"      = c("Male"),
  "Female"      = c("Female")
)



# ----------------------------------------------------------
# EDUCATION GROUP DEFINITIONS
# ----------------------------------------------------------
edu_groups <- list(
  "All Education" = NULL,
  "Less than H.S."      = c("Less than H.S."),
  "H.S. or G.E.D."      = c("H.S. or G.E.D."),
  "Some post-H.S."      = c("Some post-H.S."),
  "College graduate"  = c("College graduate")
)


# ----------------------------------------------------------
# INCOME GROUP DEFINITIONS
# ----------------------------------------------------------
income_groups <- list(
  "All Incomes" = NULL,
  "Less than $15,000"      = "Less than $15,000",
  "15–24,999"  = "$15,000 - $24,999",
  "25–34,999"  = "$25,000 - $34,999",
  "35–44,999"  = "$35,000 - $49,999",
  "$50,000+" = "$50,000+"
)


# ----------------------------------------------------------
# RACE GROUP DEFINITIONS
# ----------------------------------------------------------
race_groups <- list(
  "All Races" = NULL,
  "White"  = c("White"),
  "Black"   = c("Black" ),  
  "A/A Native, Asian,Other" = c("A/A Native, Asian,Other"),
  "Multiracial" = c("Multiracial"),                           
  "Hispanic"  = c("Hispanic")
)



# ADD BELOW
# ----------------------------------------------------------
# TEMPORAL GROUP DEFINITIONS
# ----------------------------------------------------------
year_groups <- list(
  "All Years" = NULL,
  "By Decade" = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"),
  "2010's" = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
  "2020's" = c("2020", "2021", "2022", "2023"),
  "2011"="2011",
  "2012"="2012",
  "2013"="2013",
  "2014"="2014",
  "2015"="2015",
  "2016"="2016",
  "2017"="2017",
  "2018"="2018",
  "2019"="2019",
  "2020"="2020",
  "2021"="2021",
  "2022"="2022",
  "2023"="2023"
)

# ----------------------------------------------------------
# UI
# ----------------------------------------------------------
ui <- fluidPage(
  
  theme = bs_theme(version = 5),
  
  # ---------------- CLASS + QUESTION + OVERVIEW PANELS ----------------
  fluidRow(
    column(3,
           card(
             selectInput("class_choice", "Class Choice",
                         choices = unique(layerQ$Class)),
             
             selectInput("topic_choice", "Topic Choice", 
                         choices = NULL),
             
             selectInput("q_choice", "Question Choice", choices = NULL),
             
             textOutput("my_q")
           )
    ),
    
    column(7,
           card(
             card_header("Overall"),
             plotOutput("overallPlot", height="250px", click="overall_click"),
             tableOutput("overall_click_info")
           )
    )
  ),
  
  # ============================================================
  # ONE FULL-WIDTH ROW PER CATEGORY
  # ============================================================
  

  # YEAR
  fluidRow(
    column(10,
           selectInput("year_group", "Select Year", choices = names(year_groups)),
           card(
             card_header("Temporal"),
             plotOutput("temporalPlot", height="250px", click=clickOpts(id="temporal_click")),
             tableOutput("temporal_click_info")
           )
    )
  ),
  
  # SEX
  fluidRow(
    column(10,
           selectInput("sex_group", "Select Sex", choices = names(sex_groups)),
                       card(
                         card_header("Sex"),
                         plotOutput("sexPlot", height="250px", click=clickOpts(id="sex_click")),
                         tableOutput("sex_click_info")
                       )
           )
    
  ),
  
  
  # AGE
  fluidRow(
    column(10,
           selectInput("age_group", "Select Age Group", choices = names(age_groups)),
           card(
             card_header("Age"),
             plotOutput("agePlot", height="250px", click=clickOpts(id="age_click")),
             tableOutput("age_click_info")
           )
    )
  ),
  
  
  # EDUCATION (FIXED)
  fluidRow(
    column(10,
           selectInput("edu_group", "Select Education Level",
                       choices = names(edu_groups)),
           card(
             card_header("Education"),
             plotOutput("eduPlot", height="250px", click=clickOpts(id="edu_click")),
             tableOutput("edu_click_info")
           )
    )
  ),
  
  # RACE
  fluidRow(
    column(10,
           selectInput("race_group", "Select Race",
                       choices = names(race_groups)),
           card(
             card_header("Race"),
             plotOutput("racePlot", height="250px", click=clickOpts(id="race_click")),
             tableOutput("race_click_info")
           )
    )
  ),
  
  # INCOME
  fluidRow(
    column(10,
           selectInput("income_group", "Select Income Level",
                       choices = names(income_groups)),
           card(
             card_header("Income"),
             plotOutput("incomePlot", height="250px", click=clickOpts(id="income_click")),
             tableOutput("income_click_info")
           )
    )
  ),
  
  # REGION
  fluidRow(
    column(10,
           selectInput("region", "Select Region",
                       choices = names(regions)),
           card(
             card_header("States"),
             plotOutput("statePlot", height="350px", click=clickOpts(id="region_click")),
             tableOutput("region_click_info")
           )
    )
  ),
  
  # fixed legend panel 
  absolutePanel(
    id = "fixedPanel",
    right = 0, top = 0, width = 160, height = "auto",
    fixed = TRUE,
    draggable = TRUE,
    
    card(
      card_header("Legend"),
      uiOutput("legend_ui")
    )
  )
  
)

# ----------------------------------------------------------
# SERVER
# ----------------------------------------------------------
server <- function(input, output, session) {
  
  # ------------------------------
  # QUESTION TEXT
  # ------------------------------
  
  # When class_choice changes, update q_choice
  observeEvent(input$class_choice, {
    
    filtered_topics <- layerQ$Topic[layerQ$Class == input$class_choice]
    
    updateSelectInput(session,
                     inputId = "topic_choice",
                     choices = filtered_topics 
    )
  })
  
  observeEvent(input$topic_choice, {
    
    
    # Filter questions based on selected class
    filtered_questions <- layerQ$Question[layerQ$Topic == input$topic_choice]
    
    updateSelectInput(session,
                      inputId = "q_choice",
                      choices = filtered_questions
    )
  })
  
  # Show selected question
  output$my_q <- renderText({
    my_q <- input$q_choice
    
    qDf <- df |>
      filter(Question==my_q) |>
      filter(!(Locationabbr %in% c("US","UW")))
    
    # merges
    qDf$ResponseID  <- merge_ResponseID(qDf$ResponseID)
    qDf$Response    <- merge_Response(unlist(qDf$ResponseID), unlist(qDf$Response))
    qDf$BreakoutID  <- merge_BreakoutID(qDf$BreakoutID)
    qDf$Break_Out   <- merge_Break_Out(unlist(qDf$BreakoutID), unlist(qDf$Break_Out))
    
    paste("You selected:", my_q)
  })
  
  
  # ==========================================================
  # SHARED FUNCTION (OPTIONAL)
  # Applies filters + merges for repeated code
  # ==========================================================
  get_qdf <- function(input) {
    my_q <- input$q_choice
    
    qDf <- df |>
      filter(Question == my_q) |>
      filter(!(Locationabbr %in% c("US","UW")))
    
    # merges
    qDf$ResponseID  <- merge_ResponseID(qDf$ResponseID)
    qDf$Response    <- merge_Response(unlist(qDf$ResponseID), unlist(qDf$Response))
    qDf$BreakoutID  <- merge_BreakoutID(qDf$BreakoutID)
    qDf$Break_Out   <- merge_Break_Out(unlist(qDf$BreakoutID), unlist(qDf$Break_Out))
    
    return(qDf)
  }
  
  
  
  # LEGEND (TESTING)
  output$legend_ui <- renderUI({
    
    qDf <- get_qdf(input)
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT1") |>
      select(Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Response) |>
      reframe(agg_persons = sum(persons))
    
    
    # Get the unique responses
    legend_items <- unique(plotDf$Response)
    
    # Use ggplot2's default palette (or your own)
    palette <- scales::hue_pal()(length(legend_items))
    
    tagList(
      h5("Legend"),
      lapply(seq_along(legend_items), function(i) {
        div(
          style = "display:flex; align-items:center; margin-bottom:4px;",
          div(style = paste0("width:15px; height:15px; margin-right:6px; background-color:",
                             palette[i], ";")),
          span(legend_items[i])
        )
      })
    )
  })
  
  
  
  
  # ------------------------------
  # OVERALL PLOT
  # ------------------------------
  output$overallPlot <- renderPlot({
    qDf <- get_qdf(input)
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT1") |>
      select(Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Response) |>
      reframe(agg_persons = sum(persons))
    
    plotDf <- plotDf |>
      mutate(agg_ss = sum(agg_persons)) |>
      mutate(agg_percent = agg_persons * 100 / agg_ss) |>
      mutate(agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss)) |>
      mutate(Lower_CI  = agg_percent - 2*agg_percent_sdev) |>
      mutate(Upper_CI = agg_percent + 2*agg_percent_sdev) |>
      select(-c(agg_persons, agg_ss, agg_percent_sdev))
    
    ggplot(plotDf, aes(x="overall", y=agg_percent, fill=Response, color=Response)) +
      geom_col() +
      theme(axis.text.x = element_text(size = 20), 
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.position="none") +
      xlab(NULL)
  })
  
  
  
  output$overall_click_info <- renderTable({
    qDf <- get_qdf(input)
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT1") |>
      select(Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Response) |>
      reframe(agg_persons = sum(persons))
    
    plotDf <- plotDf |>
      mutate(agg_ss = sum(agg_persons)) |>
      mutate(agg_percent = agg_persons * 100 / agg_ss) |>
      mutate(agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss)) |>
      mutate(Lower_CI  = agg_percent - 2*agg_percent_sdev) |>
      mutate(Upper_CI = agg_percent + 2*agg_percent_sdev) |>
      select(-c(agg_persons, agg_ss, agg_percent_sdev))
    
    click <- input$overall_click
    if (is.null(click)) return("Click on graph to see Confidence Interval details")
    

    plotDf
    
  })
  
  # 
  # # ------------------------------
  # # TEMPORAL PLOT
  # # ------------------------------
  output$temporalPlot <- renderPlot({
    qDf <- get_qdf(input)
    
    # YEAR FILTER APPLIED HERE
    year_choice <- year_groups[[ input$year_group ]]
    
    if (!is.null(year_choice)) {
      qDf <- qDf |> filter(Year %in% year_choice)
    }
    
    
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT1") |>
      select(Year, Response, Sample_Size) |>
      rename(persons = Sample_Size) |>
      group_by(Year, Response) |>
      summarize(persons = sum(persons)) |>
      group_by(Year) |>
      mutate(
        total = sum(persons),
        percent = persons * 100 / total
      )
    
    
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID == "CAT1") |>
      select(Year, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size)
    
    if (input$year_group=="By Decade") {
      plotDf <- plotDf |>
        mutate(
          Year= case_when(
            Year %in% c("2011","2012","2013","2014","2015","2016","2017","2018","2019") ~ "2010's",
            Year %in% c("2020","2021","2022","2023") ~ "2020's"
          )
        )   # replace actual years with group label
    }
    
    # Aggregate
    
    plotDf <- plotDf |>
      group_by(Year) |>
      reframe(Response = Response, persons = persons, agg_ss = sum(persons))
    
    
    # 
    # plotDf <- plotDf |>
    #   group_by(Year, Response) |>
    #   summarise(
    #     agg_persons = sum(persons),
    #     agg_ss = sum(persons),   # total sample size across group
    #     .groups = "drop"
    #   ) |>
    #   mutate(
    #     agg_percent = agg_persons * 100 / agg_ss,
    #     agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
    #     Lower_CI  = agg_percent - 2 * agg_percent_sdev,
    #     Upper_CI  = agg_percent + 2 * agg_percent_sdev
    #   ) |>
    #   select(-c(agg_percent_sdev, agg_ss))
    # 
    
 
    plotDf <- plotDf |>
      group_by(Year, Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        agg_low_ci_limit  = agg_percent - 2*agg_percent_sdev,
        agg_high_ci_limit = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
   
    
    
    ggplot(plotDf, aes(x = Year, y = agg_percent, fill = Response, color = Response)) +
      geom_col() +
      theme(
        axis.text.x  = element_text(angle = 10, hjust = 1, size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = "none"
      )
  })
  
  
  
  
  output$temporal_click_info <- renderTable({
    qDf <- get_qdf(input)
    
    # YEAR FILTER APPLIED HERE
    year_choice <- year_groups[[ input$year_group ]]
    
    if (!is.null(year_choice)) {
      qDf <- qDf |> filter(Year %in% year_choice)
    }
    
    
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT1") |>
      select(Year, Response, Sample_Size) |>
      rename(persons = Sample_Size) |>
      group_by(Year, Response) |>
      summarize(persons = sum(persons)) |>
      group_by(Year) |>
      mutate(
        total = sum(persons),
        percent = persons * 100 / total
      )
    
    
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID == "CAT1") |>
      select(Year, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size)
    
    if (input$year_group=="By Decade") {
      plotDf <- plotDf |>
        mutate(
          Year= case_when(
            Year %in% c("2011","2012","2013","2014","2015","2016","2017","2018","2019") ~ "2010's",
            Year %in% c("2020","2021","2022","2023") ~ "2020's"
          )
        )   # replace actual years with group label
    }
    
    # Aggregate
    
    plotDf <- plotDf |>
      group_by(Year) |>
      reframe(Response = Response, persons = persons, agg_ss = sum(persons))
    
    
    # 
    # plotDf <- plotDf |>
    #   group_by(Year, Response) |>
    #   summarise(
    #     agg_persons = sum(persons),
    #     agg_ss = sum(persons),   # total sample size across group
    #     .groups = "drop"
    #   ) |>
    #   mutate(
    #     agg_percent = agg_persons * 100 / agg_ss,
    #     agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
    #     Lower_CI  = agg_percent - 2 * agg_percent_sdev,
    #     Upper_CI  = agg_percent + 2 * agg_percent_sdev
    #   ) |>
    #   select(-c(agg_percent_sdev, agg_ss))
    # 
    
    
    plotDf <- plotDf |>
      group_by(Year, Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        agg_low_ci_limit  = agg_percent - 2*agg_percent_sdev,
        agg_high_ci_limit = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    click <- input$temporal_click
    if (is.null(click)) return("Click on graph to see Confidence Interval details")

    
    plotDf
    
  })
  
  
  # ------------------------------
  # STATE PLOT (WITH REGION FILTER)
  # ------------------------------
  output$statePlot <- renderPlot({
      qDf <- get_qdf(input)
      
      # REGION FILTER APPLIED HERE
      region_states <- regions[[ input$region ]]
      if (!is.null(region_states)) {
        qDf <- qDf |> filter(Locationabbr %in% region_states)
      }
      
      plotDf <- qDf |>
        filter(BreakOutCategoryID == "CAT1") |>
        select(Locationabbr, Response, Sample_Size) |>
        na.omit() |>
        rename(persons = Sample_Size)
      
      # Special case: group states into regions
      if (input$region == "By Region") {
        plotDf <- plotDf |>
          mutate(
            Locationabbr = case_when(
              Locationabbr %in% c("ME","NH","VT","MA","RI","CT","NY","NJ","PA") ~ "Northeast",
              Locationabbr %in% c("DE","MD","DC","VA","WV","NC","SC","GA","FL","KY","TN","AL","MS","AR","LA","OK","TX") ~ "South",
              Locationabbr %in% c("OH","MI","IN","IL","WI","MN","IA","MO","ND","SD","NE","KS") ~ "Midwest",
              Locationabbr %in% c("MT","ID","WY","CO","NM","AZ","UT","NV","CA","OR","WA","AK","HI") ~ "West",
              Locationabbr %in% c("GU", "PR", "VI") ~ "Territories",
              TRUE ~ Locationabbr   # fallback if not matched
            )
          )
      }
      
      # Aggregate
      
        plotDf <- plotDf |>
          group_by(Locationabbr) |>
          reframe(Response = Response, persons = persons, agg_ss = sum(persons))
        
        plotDf <- plotDf |>
          group_by(Locationabbr, Response) |>
          reframe(
            agg_ss = agg_ss,
            agg_persons = sum(persons),
            agg_percent = agg_persons * 100 / agg_ss,
            agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
            agg_low_ci_limit = agg_percent - 2*agg_percent_sdev,
            agg_high_ci_limit = agg_percent + 2*agg_percent_sdev
          ) |>
          distinct() |>
          select(-c(agg_persons, agg_percent_sdev, agg_ss))
        
        
        # 
        # group_by(Locationabbr, Response) |>
        # summarise(
        #   agg_persons = sum(persons),
        #   agg_ss = sum(persons),
        #   .groups = "drop"
        # ) |>
        # mutate(
        #   agg_percent = agg_persons * 100 / agg_ss,
        #   agg_percent_sdev = sqrt(agg_percent * (100 - agg_percent) / agg_ss),
        #   Lower_CI  = agg_percent - 2 * agg_percent_sdev,
        #   Upper_CI  = agg_percent + 2 * agg_percent_sdev
        # ) |>
        # select(-c(agg_percent_sdev, agg_ss))
      
      ggplot(plotDf, aes(x = Locationabbr, y = agg_percent, fill = Response, color = Response)) +
        geom_col() +
        theme(
          axis.text.x  = element_text(angle = 90, hjust = 1, size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.position = "none"
        )
    })
  
  output$region_click_info <- renderTable({
    qDf <- get_qdf(input)
    
    # REGION FILTER APPLIED HERE
    region_states <- regions[[ input$region ]]
    if (!is.null(region_states)) {
      qDf <- qDf |> filter(Locationabbr %in% region_states)
    }
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID == "CAT1") |>
      select(Locationabbr, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size)
    
    # Special case: group states into regions
    if (input$region == "By Region") {
      plotDf <- plotDf |>
        mutate(
          Locationabbr = case_when(
            Locationabbr %in% c("ME","NH","VT","MA","RI","CT","NY","NJ","PA") ~ "Northeast",
            Locationabbr %in% c("DE","MD","DC","VA","WV","NC","SC","GA","FL","KY","TN","AL","MS","AR","LA","OK","TX") ~ "South",
            Locationabbr %in% c("OH","MI","IN","IL","WI","MN","IA","MO","ND","SD","NE","KS") ~ "Midwest",
            Locationabbr %in% c("MT","ID","WY","CO","NM","AZ","UT","NV","CA","OR","WA","AK","HI") ~ "West",
            Locationabbr %in% c("GU", "PR", "VI") ~ "Territories",
            TRUE ~ Locationabbr   # fallback if not matched
          )
        )
    }
    
    # Aggregate
    
    plotDf <- plotDf |>
      group_by(Locationabbr) |>
      reframe(Response = Response, persons = persons, agg_ss = sum(persons))
    
    plotDf <- plotDf |>
      group_by(Locationabbr, Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons * 100 / agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        agg_low_ci_limit = agg_percent - 2*agg_percent_sdev,
        agg_high_ci_limit = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    click <- input$region_click
    if (is.null(click)) return("Click on graph to see Confidence Interval details")
    

    
    plotDf
    
  })
  
  
  # ------------------------------
  # AGE PLOT
  # ------------------------------
  output$agePlot <- renderPlot({
    qDf <- get_qdf(input)
    
    # AGE GROUP FILTER
    selected_age <- age_groups[[ input$age_group ]]
    if (!is.null(selected_age)) {
      qDf <- qDf |> filter(Break_Out %in% selected_age)
    }
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT3") |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response=Response, persons=persons, agg_ss=sum(persons))
    
    plotDf <- plotDf |>
      group_by(Break_Out,Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        Lower_CI  = agg_percent - 2*agg_percent_sdev,
        Upper_CI = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    ggplot(plotDf, aes(x=Break_Out, y=agg_percent, fill=Response, color=Response)) +
      geom_col() +
      theme(axis.text.x = element_text(size=20),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.position="none")
  })
  
  output$age_click_info <- renderTable({
    qDf <- get_qdf(input)
    
    # AGE GROUP FILTER
    selected_age <- age_groups[[ input$age_group ]]
    if (!is.null(selected_age)) {
      qDf <- qDf |> filter(Break_Out %in% selected_age)
    }
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT3") |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response=Response, persons=persons, agg_ss=sum(persons))
    
    plotDf <- plotDf |>
      group_by(Break_Out,Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        Lower_CI  = agg_percent - 2*agg_percent_sdev,
        Upper_CI = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    click <- input$age_click
    if (is.null(click)) return("Click on graph to see Confidence Interval details")
    

    
    plotDf
    
  })
  
  
  # ------------------------------
  # RACE PLOT
  # ------------------------------
  output$racePlot <- renderPlot({
    qDf <- get_qdf(input)
    
    # RACE FILTER APPLIED HERE
    race_choice <- race_groups[[ input$race_group ]]
    if (!is.null(race_choice)) {
      qDf <- qDf |> filter(Break_Out %in% race_choice)
    }
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT4") |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response=Response, persons=persons, agg_ss=sum(persons))
    
    plotDf <- plotDf |>
      group_by(Break_Out,Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        Lower_CI  = agg_percent - 2*agg_percent_sdev,
        Upper_CI = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    ggplot(plotDf, aes(x=Break_Out, y=agg_percent, fill=Response, color=Response)) +
      geom_col() +
      theme(axis.text.x = element_text(size=20),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.position="none")
  })
  
  output$race_click_info <- renderTable({
    qDf <- get_qdf(input)
    
    # RACE FILTER APPLIED HERE
    race_choice <- race_groups[[ input$race_group ]]
    if (!is.null(race_choice)) {
      qDf <- qDf |> filter(Break_Out %in% race_choice)
    }
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT4") |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response=Response, persons=persons, agg_ss=sum(persons))
    
    plotDf <- plotDf |>
      group_by(Break_Out,Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        Lower_CI  = agg_percent - 2*agg_percent_sdev,
        Upper_CI = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    click <- input$race_click
    if (is.null(click)) return("Click on graph to see Confidence Interval details")
    

    
    plotDf
    
  })
  
  
  # ------------------------------
  # SEX PLOT
  # ------------------------------
  output$sexPlot <- renderPlot({
    qDf <- get_qdf(input)
    
    # SEX FILTER APPLIED HERE
    sex_choice <- sex_groups[[ input$sex_group ]]
    if (!is.null(sex_choice)) {
      qDf <- qDf |> filter(Break_Out %in% sex_choice)
    }
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT2") |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response=Response, persons=persons, agg_ss=sum(persons))
    
    plotDf <- plotDf |>
      group_by(Break_Out,Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        Lower_CI  = agg_percent - 2*agg_percent_sdev,
        Upper_CI = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    ggplot(plotDf, aes(x=Break_Out, y=agg_percent, fill=Response, color=Response)) +
      geom_col() +
      theme(axis.text.x = element_text(size=20),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.position="none")
  })
  
  output$sex_click_info <- renderTable({
    qDf <- get_qdf(input)
    
    # SEX FILTER APPLIED HERE
    sex_choice <- sex_groups[[ input$sex_group ]]
    if (!is.null(sex_choice)) {
      qDf <- qDf |> filter(Break_Out %in% sex_choice)
    }
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT2") |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response = Response, persons = persons, agg_ss = sum(persons))
    
    plotDf <- plotDf |>
      group_by(Break_Out, Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        Lower_CI  = agg_percent - 2*agg_percent_sdev,
        Upper_CI = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    click <- input$sex_click
    if (is.null(click)) return("Click on graph to see Confidence Interval details")
    

    plotDf
    
  })
  
  
  # ------------------------------
  # EDUCATION PLOT
  # ------------------------------
  output$eduPlot <- renderPlot({
    qDf <- get_qdf(input)
    
    
    
    # edu GROUP FILTER
    selected_edu <- edu_groups[[ input$edu_group ]]
    if (!is.null(selected_edu)) {
      qDf <- qDf |> filter(Break_Out %in% selected_edu)
    }
    
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT5") |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response=Response, persons=persons, agg_ss=sum(persons))
    
    plotDf <- plotDf |>
      group_by(Break_Out,Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        Lower_CI  = agg_percent - 2*agg_percent_sdev,
        Upper_CI = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    
    ggplot(plotDf, aes(x=Break_Out, y=agg_percent, fill=Response, color=Response)) +
      geom_col() +
      theme(axis.text.x = element_text(size=20),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.position="none")
  })
  
  output$edu_click_info <- renderTable({
    qDf <- get_qdf(input)
    
    
    # edu GROUP FILTER
    selected_edu <- edu_groups[[ input$edu_group ]]
    if (!is.null(selected_edu)) {
      qDf <- qDf |> filter(Break_Out %in% selected_edu)
    }
    
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT5") |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response = Response, persons = persons, agg_ss = sum(persons))
    
    plotDf <- plotDf |>
      group_by(Break_Out, Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        Lower_CI  = agg_percent - 2*agg_percent_sdev,
        Upper_CI = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    
    click <- input$edu_click
    if (is.null(click)) return("Click on graph to see Confidence Interval details")
    

    plotDf
    
  })
  
  
  # ------------------------------
  # INCOME PLOT
  # ------------------------------
  output$incomePlot <- renderPlot({
    qDf <- get_qdf(input)
    
    # INCOME FILTER APPLIED HERE
    income_choice <- income_groups[[ input$income_group ]]
    if (!is.null(income_choice)) {
      qDf <- qDf |> filter(Break_Out %in% income_choice)
    }
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT6") |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response=Response, persons=persons, agg_ss=sum(persons))
    
    plotDf <- plotDf |>
      group_by(Break_Out,Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        Lower_CI  = agg_percent - 2*agg_percent_sdev,
        Upper_CI = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    ggplot(plotDf, aes(x=Break_Out, y=agg_percent, fill=Response, color=Response)) +
      geom_col() +
      theme(axis.text.x = element_text(size=20),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            legend.position="none")
  })
  
  output$income_click_info <- renderTable({
    qDf <- get_qdf(input)
    
    # INCOME FILTER APPLIED HERE
    income_choice <- income_groups[[ input$income_group ]]
    if (!is.null(income_choice)) {
      qDf <- qDf |> filter(Break_Out %in% income_choice)
    }
    
    plotDf <- qDf |>
      filter(BreakOutCategoryID=="CAT6") |>
      select(Break_Out, Response, Sample_Size) |>
      na.omit() |>
      rename(persons = Sample_Size) |>
      group_by(Break_Out) |>
      reframe(Response = Response, persons = persons, agg_ss = sum(persons))
    
    plotDf <- plotDf |>
      group_by(Break_Out, Response) |>
      reframe(
        agg_ss = agg_ss,
        agg_persons = sum(persons),
        agg_percent = agg_persons*100/agg_ss,
        agg_percent_sdev = sqrt(agg_percent*(100-agg_percent)/agg_ss),
        Lower_CI  = agg_percent - 2*agg_percent_sdev,
        Upper_CI = agg_percent + 2*agg_percent_sdev
      ) |>
      distinct() |>
      select(-c(agg_persons, agg_percent_sdev, agg_ss))
    
    click <- input$income_click
    if (is.null(click)) return("Click on graph to see Confidence Interval details")
    
 
    
    plotDf
    
  })
  
  
}

# ----------------------------------------------------------
# RUN APP
# ----------------------------------------------------------
shinyApp(ui = ui, server = server)
