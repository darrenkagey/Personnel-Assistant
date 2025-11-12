#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

### load packages
library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(rsconnect)
library(bslib)
library(shinythemes)
library(gt)
library(gtExtras)

### load data
load("OPcounts_22.24.RData")
load("OFcounts_22.24.RData")
load("DPcounts_22.24.RData")
load("BoxCounts_22.24.RData")
load("PRCounts_22.24.RData")
load("CovCounts_22.24.RData")
load("OPPlaycalls_22.24.RData")
load("OFPlaycalls_22.24.RData")
load("OPbyDown_22.24.RData")
load("OFbyDown_22.24.RData")
load("DPbyDown_22.24.RData")
load("BoxbyDown_22.24.RData")
load("PRbyDown_22.24.RData")
load("CovbyDown_22.24.RData")
load("DPvsOP_22.24.RData")
load("BoxvsOP_22.24.RData")
load("PRvsOP_22.24.RData")
load("CovvsOP_22.24.RData")
load("OP_player_count_22.24.RData")
load("DP_player_count_22.24.RData")

### Building User Interface
ui<-fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel(p(
    h3("The Personnel Asisstant", align = "left"))),
  mainPanel(
    navbarPage("Darren Kagey",
               navset_card_pill(
                 nav_panel("Offense",
                            fluidRow(
                              column(4, align= "center",
                                     # Offensive Team Selection Input
                                     selectInput(inputId = "Off_Team",
                                                 label = "Offense",
                                                 choices = sort(unique(OP_counts_bind$posteam)),
                                                 selected = "ARI")),
                              column(4, align = "center",
                                     # Season Type Selection Input
                                     selectInput(inputId = "Off_Season_Type",
                                                 label = "Season Type",
                                                 choices = sort(unique(OP_counts_bind$season_type)),
                                                 selected = "REG+POST")),
                              column(4, align = "center",
                                     # Season Selection Input
                                     selectInput(inputId = "Off_Season",
                                                 label = "Season",
                                                 choices = sort(unique(OP_counts_bind$season)),
                                                 selected = 2024))),
                            br(),
                            column(12, align = 'center',
                                   uiOutput("offenseheading")),
                            column(12, align = "center",
                                  tags$h3("Offensive Personnel Usage")),
                            tableOutput("OPCounts"),
                            column(12, align="center",
                                   tags$h3("Offensive Personnel Run/Pass Splits")),
                            tableOutput("OPPlaycalls"),
                            column(12, align="center",
                                   tags$h3("Offensive Personnel by Down & Distance")),
                            tableOutput("OPbyDown"),
                            column(12, align="center",
                                   tags$h3("Player Snaps by Offensive Personnel")),
                            tableOutput("OPPlayers"),
                            column(12, align="center",
                                   tags$h3("Offensive Formation Usage")),
                            tableOutput("OFCounts"),
                            column(12, align="center",
                                   tags$h3("Offensive Formation Run/Pass Splits")),
                            tableOutput("OFPlaycalls"),
                            column(12, align="center",
                                   tags$h3("Offensive Formation by Down & Distance")),
                            tableOutput("OFbyDown"),
                            br(),
                            br(),
                            column(12, align = "left")),
                 nav_panel("Defense",
                           fluidRow(
                             column(4, align= "center",
                                    # Defensive Team Selection Input
                                    selectInput(inputId = "Def_Team",
                                                label = "Defense",
                                                choices = sort(unique(DP_counts_bind$defteam)),
                                                selected = "ARI")),
                             column(4, align = "center",
                                    # Season Type Selection Input
                                    selectInput(inputId = "Def_Season_Type",
                                                label = "Season Type",
                                                choices = sort(unique(DP_counts_bind$season_type)),
                                                selected = "REG+POST")),
                             column(4, align = "center",
                                    # Season Selection Input
                                    selectInput(inputId = "Def_Season",
                                                label = "Season",
                                                choices = sort(unique(DP_counts_bind$season)),
                                                selected = 2023))),
                           br(),
                           column(12, align = 'center',
                                  uiOutput("defenseheading"),
                                  tags$h3("Defensive Personnel Usage")),
                           tableOutput("DPCounts"),
                           column(12, align="center",
                                  tags$h3("Defensive Personnel Usage vs Off Personnel")),
                           tableOutput("DPvsOP"),
                           column(12, align="center",
                                  tags$h3("Defensive Personnel by Down & Distance")),
                           tableOutput("DPbyDown"),
                           column(12, align="center",
                                  tags$h3("Player Snaps by Defensive Personnel")),
                           tableOutput("DPPlayers"),
                           column(12, align="center",
                                  tags$h3("Usage of Box Defenders")),
                           tableOutput("BoxCounts"),
                           column(12, align ="center",
                                  tags$h3("Usage of Box Defenders vs Off Personnel")),
                           tableOutput("BoxvsOP"),
                           column(12, align="center",
                                  tags$h3("Box Defenders By Down & Distance")),
                           tableOutput("BoxbyDown"),
                           column(12, align="center",
                                  tags$h3("Usage of Pass Rushers")),
                           tableOutput("PRCounts"),
                           column(12, align="center",
                                  tags$h3("Pass Rusher Count vs Off Personnel")),
                           tableOutput("PRvsOP"),
                           column(12, align="center",
                                  tags$h3("Pass Rushers by Down & Distance")),
                           tableOutput("PRbyDown"),
                           column(12, align="center",
                                  tags$h3("Coverage Scheme Usage")),
                           tableOutput("CovCounts"),
                           column(12, align="center",
                                  tags$h3("Coverage Schemes vs Off Personnel")),
                           tableOutput("CovvsOP"),
                           column(12, align="center",
                                  tags$h3("Coverage Scheme by Down & Distance")),
                           tableOutput("CovbyDown")),
                 nav_panel("Glossary",
                   fluidRow(
                     column(12, align = "left",
                            tags$h4("Terms"),
                            tags$h5("Usage % (Play Count): Percentage of plays that a Personnel/Formation 
                                              is used on, with the number of plays
                                              it is used in parenthesis"),
                            br(),
                            tags$h5("EPA: Expected Points; Expected Points Added measures 
                                       the offensive outcome of a play, compared to the expected outcome, 
                                       given context such as down, distance, field position, etc."),
                            br(),
                            tags$h5("EPA/Play: Average EPA gained/lost per play"),
                            br(),
                            tags$h5("Success %: Percentage of plays where the EPA is above 0"),
                            br(),
                            tags$h5("EPA/Run: Average EPA gained/lost per design run (excluding scrambles)"),
                            br(),
                            tags$h5("EPA/Pass: Average EPA gained/lost per dropback (including scrambles)"),
                            br(),
                            tags$h5("Motion %: Percentage of plays where an offensive player is in motion before or at the snap"),
                            br(),
                            tags$h5("Play Action %: Percentage of plays with a play action fake"),
                            br(),
                            br(),
                            tags$h4("Rankings Explained"),
                            tags$h5("League Rankings: Where the listed value ranks across all 32 NFL teams,
                                              colored in blue"),
                            br(),
                            tags$h5("Team Rank: Where the listed value ranks within the selected team 
                                              at the Down & Distance, colored in Gray"),
                            br(),
                            br(),
                            tags$h4("What is Highlighted?"),
                            tags$h5("Within the 'Run/Pass Splits' tables in the Offense tab, any Personnel/Formation used 15+ 
                                              times that has a run or pass percentage >= 70% is highlighted"),
                            br(),
                            tags$h5("Within the 'vs Offensive Personnel' tables in the Defense tab, any 
                                              Personnel/Defender Count/Coverage used 15+ times that 
                                              has a usage percentage >= 50% is highlighted"),
                            br(),
                            tags$h5("Within the 'by Down & Distance' tables in the Offense and Defense tabs, 
                                    any Personnel/Formation/Defender Count/Coverage used at a
                                    down & distance 5+ times with a usage percentage >= 50% is highlighted"),
                            br(),
                            tags$h5("The reason for only highlighting Personnels/Formations/Defender Counts/Coverages that are used 
                                             a certain number of times is to prevent from highlighting uninsightful 
                                             tendencies. Personnels/Formations/Defender Counts/Coverages with low play counts are more likely 
                                             to have have extreme tendencies due to a small sample size. However, 
                                             knowing extreme tendencies across super small play samples is much 
                                             less useful to coaches than knowing extreme tendencies across 
                                             relatively large samples."),
                            br(),
                            br(),
                            tags$h4("Data Notes"),
                            tags$h5("Based on my experience working with coaches in football, I've found 
                                              that they typically prefer for garbage time situations to be filtered 
                                              out when calculating personnel usage and tendencies. 
                                              This minimizes potential skewing of team tendencies that may 
                                              occur in garbage time when more backups are playing and playcalling 
                                              is either ultra conservative or aggressive. Therefore, all the data 
                                              used to calculate the tendencies in this app comes from plays in games where the win 
                                              probabilities is between 5% and 95%")))),
                 nav_panel("Note",
                           fluidRow(
                             column(12, align = "left",
                                    tags$h5("This app has been built with the intention of providing analytical 
                                            insight into how NFL teams utilize personnels on both sides of the ball,
                                            in a way that anyone intersted in football can digest. I hope this app 
                                            can be a tool for fans and coaches alike to better understand personnel 
                                            strategies used around the NFL."),
                                    br(),
                                    tags$h5("To make this app, I pulled data exclusively from the nflreadr package in nflverse,
                                            so all data used here is publicly available to anyone, with nothing 
                                            behind a paywall. Further, the tables that are in this app were made primarily 
                                            with the gt package in R. I recommend anyone interested in sport 
                                            analytics to become familiar with both nflverse and gt, as they are 
                                            each incredibly useful tools that provide free advanced data and many 
                                            ways to present data, respectively."),
                                    br(),
                                    tags$h5("I want to say thank you to Ben Baldwin, Sebastian Carl, Lee Sharpe,
                                            Tan Ho, Justin Edwards, and everyone who has worked on the nflverse package, 
                                            as well as the many authors of the gt package, including Richard Iannone, Joe Cheng, 
                                            Alexandra Lauer, Ellis Hughes, and many others. Without their amazing 
                                            work I would not be able to complete projects like 
                                            this one and have the opportunities that I do because of it."),
                                    br(),
                                    tags$h5("I also would like to give a huge thank you to Arjun Menon, who's shiny 
                                            app 'The Scout' was a major inspiration in the building and design of this 
                                            app. Not only that, but because Arjun was generous enough to make the code for
                                            'The Scout' open-source to help others like myself, I was able to learn how to
                                            build apps like this one. So, once again, thank you Arjun. For anyone 
                                            interested in his work, I've linked his 'The Scout' app at the bottom 
                                            of this page."),
                                    br(),
                                    tags$h5("If anyone has any questions or feedback, or would just like to connect,
                                            I'm on X @KageyDarren, or you can email me at darren.kagey@gmail.com"),
                                    br(),
                                    tags$h5("Arjun Menon's 'The Scout': https://arjunmenon.shinyapps.io/TheScout/")))))
    )
  )
)
server <- function(input, output, session) {
  output$offenseheading <- renderUI(tags$h1(input$Off_Team, " Offensive Scouting Report"))
  output$defenseheading <- renderUI(tags$h1(input$Def_Team, " Defensive Scouting Report"))
  
  output$OPCounts <- render_gt({
    
    
    OPCounts_filtered <- OP_counts_bind %>%  filter(posteam == input$Off_Team & season_type == input$Off_Season_Type & season == input$Off_Season) %>% 
      ungroup() %>% 
      mutate(total_plays = sum(play_count)) %>% 
      group_by(offense_personnel) %>% 
      mutate(usage_perc = round((play_count/total_plays)*100,2),
             usage_count = paste0(usage_perc, "% (", play_count, ")")) %>%
      ungroup() %>% 
      slice_max(play_count, n=5) %>% 
      filter(play_count >= 15) %>% 
      select(offense_personnel, usage_count, epa_per_play, epa_play_rank, epa_per_run, epa_run_rank, 
             epa_per_pass,epa_pass_rank, grouped_off_success_rate, suc_rate_rank, motion_perc, motion_rank, pa_perc, pa_rank)
    
    OPCounts_Table <- OPCounts_filtered %>% 
      gt() %>% 
      fmt_percent(c(grouped_off_success_rate,motion_perc, pa_perc), decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_play, col2 = epa_play_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= epa_per_run, col2 = epa_run_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate, col2 = suc_rate_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= motion_perc, col2 = motion_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= pa_perc, col2 = pa_rank, palette = c ("black", "navyblue")) %>% 
      cols_align("center") %>% 
      cols_label(
        offense_personnel = "Off Personnel",
        usage_count = "Usage % (Play Count)",
        epa_per_play = "EPA/Play",
        grouped_off_success_rate = "SUccess %",
        epa_per_run = "EPA/Run",
        epa_per_pass = "EPA/Pass",
        motion_perc = "Motion %",
        pa_perc = "Play Action %"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("League Rank in Blue | Table: @KageyDarren") %>% 
      data_color(
        columns = epa_play_rank,
        target_columns = epa_per_play,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = suc_rate_rank,
        target_columns = grouped_off_success_rate,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = epa_run_rank,
        target_columns = epa_per_run,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = epa_pass_rank,
        target_columns = epa_per_pass,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = motion_rank,
        target_columns = motion_perc,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = pa_rank,
        target_columns = pa_perc,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        ))
  }, width = 1100)
  
  output$OPPlaycalls <- render_gt({
    
    
    OPPlaycalls_filtered <- OP_playcalls_bind %>%  filter(posteam == input$Off_Team & season_type == input$Off_Season_Type & season == input$Off_Season) %>% 
      ungroup() %>% 
      slice_max(play_count, n=5) %>% 
      group_by(offense_personnel) %>% 
      mutate(run_perc_count = paste0(run_perc*100, "% (", grouped_run_count, ")"),
             pass_perc_count = paste0(pass_perc*100, "% (", grouped_pass_count, ")")) %>% 
      select(offense_personnel, run_perc_count, epa_per_run, pass_perc_count, epa_per_pass, motion_perc, pa_perc, pass_perc, run_perc, 
             play_count) %>% ungroup()
    
    
    
    OPPlaycalls_table <- OPPlaycalls_filtered %>% 
      gt() %>% 
      fmt_percent(c(motion_perc, pa_perc), decimals = 2) %>% 
      gt_highlight_rows(columns = c(run_perc_count, pass_perc_count),
                        rows = (run_perc >= .7 & play_count >= 15) | (pass_perc >= .7 & play_count >= 15),
                        fill = "#def440") %>% 
      cols_align("center") %>% 
      cols_label(
        offense_personnel = "Off Personnel",
        run_perc_count = "Run % (Play Count)",
        epa_per_run = "EPA/Run",
        pass_perc_count = "Pass % (Play Count)",
        epa_per_pass = "EPA/Pass",
        motion_perc = "Motion %",
        pa_perc = "Play Action %"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("Table: @KageyDarren") %>% 
      cols_hide(columns = c(run_perc, pass_perc, play_count))
    
  }, width = 1100)
  
  output$OPbyDown <-  render_gt({
    
    
    OPbyDown_filtered <- OP_by_down_bind %>%  filter(posteam == input$Off_Team & season_type == input$Off_Season_Type & season == input$Off_Season) %>% 
      group_by(down) %>% 
      mutate(down_plays = sum(plays_by_down)) %>% 
      mutate(epa_play_rank = rank(-epa_per_play, ties.method = "min"),
             epa_pass_rank = rank(-epa_per_pass, ties.method = "min"),
             epa_run_rank = rank(-epa_per_run, ties.method = "min"),
             suc_rate_rank = rank(-grouped_off_success_rate, ties.method = "min"),
             motion_rank = rank(-motion_perc, ties.method = "min"),
             pa_rank = rank(-pa_perc, ties.method = "min")) %>% 
      slice_max(plays_by_down, n=3) %>% 
      ungroup() %>% 
      mutate(down_usage_perc = round((plays_by_down/down_plays)*100,2),
             down_usage_count = paste0(down_usage_perc,  "% (", plays_by_down, ")")) %>% 
      select(down, offense_personnel, down_usage_count, epa_per_play, epa_play_rank, grouped_off_success_rate, 
             suc_rate_rank, epa_per_run, epa_run_rank, epa_per_pass, epa_pass_rank, motion_perc, motion_rank, pa_perc, pa_rank, down_usage_perc, plays_by_down)
    
    
    
    OPbyDown_table <- OPbyDown_filtered %>% 
      gt(groupname_col = "down") %>% 
      gt_highlight_rows(columns = down_usage_count,
                        rows = (down_usage_perc >= 50 & plays_by_down >= 5),
                        fill = "#def440") %>% 
      cols_hide(down_usage_perc) %>% 
      fmt_percent(c(grouped_off_success_rate, motion_perc, pa_perc), decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_play, col2 = epa_play_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= epa_per_run, col2 = epa_run_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate, col2 = suc_rate_rank, palette = c ("black", "gray40")) %>%
      gt_merge_stack(col1= motion_perc, col2 = motion_rank, palette = c ("black", "gray40")) %>%
      gt_merge_stack(col1= pa_perc, col2 = pa_rank, palette = c ("black", "gray40")) %>%
      cols_align("center") %>% 
      cols_label(
        offense_personnel = "Off Personnel",
        down = "Down & Distance",
        down_usage_count = "Down Usage % (Play Count)",
        epa_per_play = "EPA/Play",
        grouped_off_success_rate = "SUccess %",
        epa_per_run = "EPA/Run",
        epa_per_pass = "EPA/Pass",
        motion_perc = "Motion %",
        pa_perc = "Play Action %"
      ) %>% 
      tab_source_note("Team Rank in Gray under value | Table: @KageyDarren") %>% 
      gt_theme_espn() %>% 
      cols_hide(plays_by_down)
  }, width = 1100) 
  
  output$OPPlayers <- render_gt({
    
    OPPlayers_filtered <-  OP_player_play_count_bind %>% filter(team == input$Off_Team & season_type == input$Off_Season_Type& season == input$Off_Season) %>% 
      pivot_longer(`10`:`total`,
                   names_to = "offense_personnel",
                   values_to = "personnel_snaps") %>% 
      filter(offense_personnel != "total") %>% 
      group_by(display_name) %>% 
      mutate(total_snaps = sum(personnel_snaps)) %>% 
      ungroup() %>% 
      right_join((OP_counts_bind %>% filter(posteam == input$Off_Team & season_type == input$Off_Season_Type & season == input$Off_Season) %>% 
                    ungroup() %>% 
                    slice_max(play_count, n=5)), 
                 by=c("team"="posteam", "season_type", "season", "offense_personnel")) %>% 
      select(`team`:`play_count`) %>% 
      group_by(display_name) %>% 
      arrange(-play_count, .by_group = TRUE) %>% 
      ungroup() %>% 
      select(-play_count) %>% 
      pivot_wider(names_from = offense_personnel,
                  values_from = personnel_snaps) %>% 
      arrange(-total_snaps) %>% 
      arrange(position) %>% 
      relocate(total_snaps, .after = last_col()) %>% 
      group_by(position) %>% 
      slice_max(total_snaps, n=6) %>% 
      filter(total_snaps >= 10)
    
    
    OPPlayers_table <- OPPlayers_filtered %>% 
      gt(groupname_col = "position") %>% 
      cols_align("center") %>% 
      cols_label(
        display_name = "Name",
        position = "Position",
        season_type = "Season Type",
        total_snaps = "Total Snaps"
      ) %>% 
      sub_missing(columns = everything(),
                  rows = everything(),
                  missing_text = "-") %>% 
      gt_theme_espn() %>% 
      tab_source_note("Table: @KageyDarren, inspired by @arjunmenon100") 
  }, width = 1100)
  
  output$OFCounts <- render_gt({
    
    OFCounts_filtered <- OF_counts_bind %>%  filter(posteam == input$Off_Team & season_type == input$Off_Season_Type & season == input$Off_Season) %>% 
      ungroup() %>% 
      mutate(total_plays = sum(play_count)) %>% 
      group_by(offense_formation) %>% 
      mutate(usage_perc = round((play_count/total_plays)*100,2),
             usage_count = paste0(usage_perc, "% (", play_count, ")")) %>%
      ungroup() %>% 
      slice_max(play_count, n=5) %>%        
      filter(play_count >= 15) %>% 
      select(offense_formation, usage_count, epa_per_play, epa_play_rank, epa_per_run, epa_run_rank, 
             epa_per_pass,epa_pass_rank, grouped_off_success_rate, suc_rate_rank, motion_perc, motion_rank, pa_perc, pa_rank)
    
    OFCount_Table <- OFCounts_filtered %>% 
      gt() %>% 
      fmt_percent(c(grouped_off_success_rate, motion_perc, pa_perc), decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_play, col2 = epa_play_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= epa_per_run, col2 = epa_run_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate, col2 = suc_rate_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= motion_perc, col2 = motion_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= pa_perc, col2 = pa_rank, palette = c ("black", "navyblue")) %>% 
      cols_align("center") %>% 
      cols_label(
        offense_formation = "Off Formation",
        usage_count = "Usage % (Play Count)",
        epa_per_play = "EPA/Play",
        grouped_off_success_rate = "SUccess %",
        epa_per_run = "EPA/Run",
        epa_per_pass = "EPA/Pass",
        motion_perc = "Motion %",
        pa_perc = "Play Action %"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("League Rank in Blue | Table: @KageyDarren") %>% 
      data_color(
        columns = epa_play_rank,
        target_columns = epa_per_play,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = suc_rate_rank,
        target_columns = grouped_off_success_rate,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = epa_run_rank,
        target_columns = epa_per_run,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = epa_pass_rank,
        target_columns = epa_per_pass,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = motion_rank,
        target_columns = motion_perc,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = pa_rank,
        target_columns = pa_perc,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        ))
  }, width = 1100)
  
  output$OFPlaycalls <- render_gt({
    
    OFPlaycalls_filtered <- OF_playcalls_bind %>%  filter(posteam == input$Off_Team & season_type == input$Off_Season_Type & season == input$Off_Season) %>% 
      ungroup() %>% 
      slice_max(play_count, n=5) %>% 
      group_by(offense_formation) %>% 
      mutate(run_perc_count = paste0(run_perc*100, "% (", grouped_run_count, ")"),
             pass_perc_count = paste0(pass_perc*100, "% (", grouped_pass_count, ")")) %>% 
      select(offense_formation, play_count, run_perc_count, epa_per_run, pass_perc_count, epa_per_pass, motion_perc, pa_perc, run_perc, 
             pass_perc, play_count) %>% ungroup()
    
    OFPlaycalls_table <- OFPlaycalls_filtered %>% 
      gt() %>% 
      fmt_percent(c(motion_perc, pa_perc), decimals = 2)
      gt_highlight_rows(columns = c(run_perc_count, pass_perc_count),
                        rows = (run_perc >= .7 & play_count >= 15) | (pass_perc >= .7 & play_count >= 15),
                        fill = "#def440") %>% 
      cols_align("center") %>% 
      cols_label(
        offense_formation = "Off Formation",
        run_perc_count = "Run % (Play Count)",
        epa_per_run = "EPA/Run",
        pass_perc_count = "Pass % (Play Count)",
        epa_per_pass = "EPA/Pass",
        motion_perc = "Motion %",
        pa_perc = "Play Action %"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("Table: @KageyDarren") %>% 
      cols_hide(columns = c(run_perc, pass_perc, play_count))
  }, width = 1100)
  
  output$OFbyDown <- render_gt({
    
    OFbyDown_filtered <- OF_by_down_bind %>%  filter(posteam == input$Off_Team & season_type == input$Off_Season_Type & season == input$Off_Season) %>% 
      group_by(down) %>% 
      mutate(down_plays = sum(plays_by_down)) %>% 
      mutate(epa_play_rank = rank(-epa_per_play, ties.method = "min"),
             epa_pass_rank = rank(-epa_per_pass, ties.method = "min"),
             epa_run_rank = rank(-epa_per_run, ties.method = "min"),
             suc_rate_rank = rank(-grouped_off_success_rate, ties.method = "min"),
             motion_rank = rank(-motion_perc, ties.method = "min"),
             pa_rank = rank(-pa_perc, ties.method = "min")) %>% 
      slice_max(plays_by_down, n=3) %>% 
      ungroup() %>% 
      mutate(down_usage_perc = round((plays_by_down/down_plays)*100,2),
             down_usage_count = paste0(down_usage_perc,  "% (", plays_by_down, ")")) %>% 
      select(down, offense_formation, down_usage_count, epa_per_play, epa_play_rank, grouped_off_success_rate, 
             suc_rate_rank, epa_per_run, epa_run_rank, epa_per_pass, epa_pass_rank, motion_perc, motion_rank, pa_perc, pa_rank, down_usage_perc, plays_by_down)
    
    OFbyDown_table <- OFbyDown_filtered %>% 
      gt(groupname_col = "down") %>% 
      gt_highlight_rows(columns = down_usage_count,
                        rows = (down_usage_perc >= 50 & plays_by_down >= 5),
                        fill = "#def440") %>% 
      cols_hide(down_usage_perc) %>% 
      fmt_percent(grouped_off_success_rate, decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_play, col2 = epa_play_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= epa_per_run, col2 = epa_run_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate, col2 = suc_rate_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= motion_perc, col2 = motion_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= pa_perc, col2 = pa_rank, palette = c ("black", "gray40")) %>% 
      cols_align("center") %>% 
      cols_label(
        offense_formation = "Off Formation",
        down = "Down & Distance",
        down_usage_count = "Down Usage % (Play Count)",
        epa_per_play = "EPA/Play",
        grouped_off_success_rate = "SUccess %",
        epa_per_run = "EPA/Run",
        epa_per_pass = "EPA/Pass",
        motion_perc = "Motion %",
        pa_perc = "Play Action %"
      ) %>% 
      tab_source_note("Team Rank in Gray under value | Table: @KageyDarren") %>% 
      gt_theme_espn() %>% 
      cols_hide(plays_by_down)
  }, width = 1100)
  
  output$DPCounts <- render_gt({
    
    DPCounts_filtered <- DP_counts_bind %>%  filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      ungroup() %>% 
      mutate(total_plays = sum(play_count)) %>% 
      group_by(defense_personnel) %>% 
      mutate(usage_perc = round((play_count/total_plays)*100,2),
             usage_count = paste0(usage_perc, "% (", play_count, ")")) %>%
      ungroup() %>% 
      slice_max(play_count, n=5) %>%        
      filter(play_count >= 15) %>% 
      select(defense_personnel, usage_count, epa_per_play, epa_play_rank, epa_per_run, epa_run_rank, 
             epa_per_pass,epa_pass_rank, grouped_off_success_rate, suc_rate_rank)
    
    DPCounts_Table <- DPCounts_filtered %>% 
      gt() %>% 
      fmt_percent(grouped_off_success_rate, decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_play, col2 = epa_play_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= epa_per_run, col2 = epa_run_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate, col2 = suc_rate_rank, palette = c ("black", "navyblue")) %>% 
      cols_align("center") %>% 
      cols_label(
        defense_personnel = "Def Personnel",
        usage_count = "Usage % (Play Count)",
        epa_per_play = "EPA/Play Allowed",
        grouped_off_success_rate = "SUccess % Allowed",
        epa_per_run = "EPA/Run Allowed",
        epa_per_pass = "EPA/Pass Allowed"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("League Rank in Blue | Table: @KageyDarren") %>% 
      data_color(
        columns = epa_play_rank,
        target_columns = epa_per_play,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = suc_rate_rank,
        target_columns = grouped_off_success_rate,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = epa_run_rank,
        target_columns = epa_per_run,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = epa_pass_rank,
        target_columns = epa_per_pass,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        ))
  }, width = 1100)
  
  output$DPvsOP <- render_gt({
    
    DPvsOP_filtered <- DP_vs_OP_bind %>% filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      group_by(offense_personnel) %>% 
      mutate(total_plays_vs_OP = sum(plays_vs_OP)) %>% 
      slice_max(plays_vs_OP, n=5) %>% 
      mutate(usage_perc = round((plays_vs_OP/total_plays_vs_OP)*100,2),
             usage_count = paste0(usage_perc, "% (", plays_vs_OP, ")")) %>% 
      select(defense_personnel, offense_personnel, usage_count, epa_per_play, grouped_off_success_rate, 
             epa_per_run, epa_per_pass, usage_perc, plays_vs_OP) %>% ungroup()
    
    DPvsOP_table <- DPvsOP_filtered %>% 
      gt() %>% 
      gt_highlight_rows(columns = usage_count,
                        rows = (usage_perc >= 50 & plays_vs_OP >= 15),
                        fill = "#def440") %>% 
      fmt_percent(grouped_off_success_rate, decimals = 2) %>% 
      cols_align("center") %>% 
      cols_label(
        defense_personnel = "Def Personnel",
        offense_personnel = "Off Personnel",
        usage_count = "Usage % vs Off Pers (Play Count)",
        epa_per_play = "EPA/Play Allowed",
        grouped_off_success_rate = "Success % Allowed",
        epa_per_run = "EPA/Run Allowed",
        epa_per_pass = "EPA/Pass Allowed"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("Table: @KageyDarren") %>% 
      cols_hide(columns = c(usage_perc, plays_vs_OP))
  }, width = 1100)
  
  output$DPbyDown <- render_gt({
    
    DPbyDown_filtered <- DP_by_down_bind %>%  filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      group_by(down) %>% 
      mutate(down_plays = sum(plays_by_down)) %>% 
      mutate(epa_play_rank = rank(epa_per_play, ties.method = "min"),
             epa_pass_rank = rank(epa_per_pass, ties.method = "min"),
             epa_run_rank = rank(epa_per_run, ties.method = "min"),
             suc_rate_rank = rank(grouped_off_success_rate, ties.method = "min")) %>% 
      slice_max(plays_by_down, n=3) %>% 
      ungroup() %>% 
      mutate(down_usage_perc = round((plays_by_down/down_plays)*100,2),
             down_usage_count = paste0(down_usage_perc,  "% (", plays_by_down, ")")) %>% 
      select(down, defense_personnel, down_usage_count, epa_per_play, epa_play_rank, 
             grouped_off_success_rate, suc_rate_rank, epa_per_run, epa_run_rank, epa_per_pass, epa_pass_rank,
             down_usage_perc, plays_by_down)
    
    DPbyDown_table <- DPbyDown_filtered %>% 
      gt(groupname_col = "down") %>% 
      gt_highlight_rows(columns = down_usage_count,
                        rows = (down_usage_perc >= 50 & plays_by_down >= 5),
                        fill = "#def440") %>% 
      cols_hide(down_usage_perc) %>% 
      fmt_percent(grouped_off_success_rate, decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_play, col2 = epa_play_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= epa_per_run, col2 = epa_run_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate, col2 = suc_rate_rank, palette = c ("black", "gray40")) %>% 
      cols_align("center") %>% 
      cols_label(
        defense_personnel = "Def Personnel",
        down = "Down & Distance",
        down_usage_count = "Down Usage % (Play Count)",
        epa_per_play = "EPA/Play Allowed",
        grouped_off_success_rate = "SUccess % Allowed",
        epa_per_run = "EPA/Run Allowed",
        epa_per_pass = "EPA/Pass Allowed"
      ) %>% 
      tab_source_note("Team Rank in Gray under value | Table: @KageyDarren") %>% 
      gt_theme_espn() %>% 
      cols_hide(plays_by_down)
  }, width = 1100)
  
  output$DPPlayers <- render_gt({
    
    DPPlayers_filtered <- DP_player_play_count_bind %>% filter(team == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      filter(!position %in% c("QB", "RB", "WR", "TE", "G", "C", "T", "OL")) %>% 
      pivot_longer(`7+ DBs`:`total`,
                   names_to = "defense_personnel",
                   values_to = "personnel_snaps") %>% 
      filter(defense_personnel != "total") %>% 
      group_by(display_name) %>% 
      mutate(total_snaps = sum(personnel_snaps)) %>% 
      ungroup() %>% 
      right_join((DP_counts_bind %>% filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
                    ungroup() %>% 
                    slice_max(play_count, n=5)), 
                 by=c("team"="defteam", "season_type", "season", "defense_personnel")) %>% 
      select(`team`:`play_count`) %>% 
      group_by(display_name) %>% 
      arrange(-play_count, .by_group = TRUE) %>% 
      ungroup() %>% 
      select(-play_count) %>% 
      pivot_wider(names_from = defense_personnel,
                  values_from = personnel_snaps) %>% 
      arrange(-total_snaps) %>% 
      mutate(position = factor(position, levels =c("iDL", "DE", "LB", "CB", "Saf", "DB"))) %>% 
      arrange(position) %>% 
      relocate(total_snaps, .after = last_col()) %>% 
      group_by(position) %>% 
      slice_max(total_snaps, n=6) %>% 
      filter(total_snaps >= 10)
    
    DPPlayers_table <- DPPlayers_filtered %>% 
      gt(groupname_col = "position") %>% 
      cols_align("center") %>% 
      cols_label(
        display_name = "Name",
        position = "Position",
        season_type = "Season Type",
        total_snaps = "Total Snaps"
      ) %>% 
      sub_missing(columns = everything(),
                  rows = everything(),
                  missing_text = "-") %>% 
    gt_theme_espn() %>% 
      tab_source_note("Table: @KageyDarren, inspired by @arjunmenon100")
  }, width = 1100)
  
  output$BoxCounts <- render_gt({
    
    BoxCounts_filtered <- Box_counts_bind %>%  filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      ungroup() %>% 
      mutate(total_plays = sum(play_count)) %>% 
      group_by(defenders_in_box) %>% 
      mutate(usage_perc = round((play_count/total_plays)*100,2),
             usage_count = paste0(usage_perc, "% (", play_count, ")")) %>%
      ungroup() %>% 
      slice_max(play_count, n=5) %>%       
      filter(play_count >= 15) %>% 
      select(defenders_in_box, usage_count, epa_per_play, epa_play_rank, epa_per_run, epa_run_rank, 
             epa_per_pass,epa_pass_rank, grouped_off_success_rate, suc_rate_rank)
    
    BoxCounts_Table <- BoxCounts_filtered %>% 
      gt() %>% 
      fmt_percent(grouped_off_success_rate, decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_play, col2 = epa_play_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= epa_per_run, col2 = epa_run_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate, col2 = suc_rate_rank, palette = c ("black", "navyblue")) %>% 
      cols_align("center") %>% 
      cols_label(
        defenders_in_box = "Box Defenders Count",
        usage_count = "Usage % (Play Count)",
        epa_per_play = "EPA/Play Allowed",
        grouped_off_success_rate = "SUccess % Allowed",
        epa_per_run = "EPA/Run Allowed",
        epa_per_pass = "EPA/Pass Allowed"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("League Rank in Blue | Table: @KageyDarren") %>% 
      data_color(
        columns = epa_play_rank,
        target_columns = epa_per_play,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = suc_rate_rank,
        target_columns = grouped_off_success_rate,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = epa_run_rank,
        target_columns = epa_per_run,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = epa_pass_rank,
        target_columns = epa_per_pass,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        ))
  }, width = 1100)
  
  output$BoxvsOP <- render_gt({
    
    BoxvsOP_filtered <- Box_vs_OP_bind %>% filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      group_by(offense_personnel) %>% 
      mutate(total_plays_vs_OP = sum(plays_vs_OP)) %>% 
      slice_max(plays_vs_OP, n=5) %>% 
      mutate(usage_perc = round((plays_vs_OP/total_plays_vs_OP)*100,2),
             usage_count = paste0(usage_perc, "% (", plays_vs_OP, ")")) %>% 
      select(defenders_in_box, offense_personnel, usage_count, epa_per_play, grouped_off_success_rate, 
             epa_per_run, epa_per_pass, usage_perc, plays_vs_OP) %>%  ungroup()
    
    BoxvsOP_table <- BoxvsOP_filtered %>% 
      gt() %>% 
      gt_highlight_rows(columns = usage_count,
                        rows = (usage_perc >= 50 & plays_vs_OP >= 15),
                        fill = "#def440") %>% 
      fmt_percent(grouped_off_success_rate, decimals = 2) %>% 
      cols_align("center") %>% 
      cols_label(
        defenders_in_box = "Box Defenders Count",
        offense_personnel = "Off Personnel",
        usage_count = "Usage % vs Off Pers (Play Count)",
        epa_per_play = "EPA/Play Allowed",
        grouped_off_success_rate = "SUccess % Allowed",
        epa_per_run = "EPA/Run Allowed",
        epa_per_pass = "EPA/Pass Allowed"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("Table: @KageyDarren") %>% 
      cols_hide(columns = c(usage_perc,plays_vs_OP))
  }, width = 1100)
  
  output$BoxbyDown <- render_gt({
    
    BoxbyDown_filtered <- Box_by_down_bind %>%  filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      group_by(down) %>% 
      mutate(down_plays = sum(plays_by_down)) %>% 
      mutate(epa_play_rank = rank(epa_per_play, ties.method = "min"),
             epa_pass_rank = rank(epa_per_pass, ties.method = "min"),
             epa_run_rank = rank(epa_per_run, ties.method = "min"),
             suc_rate_rank = rank(grouped_off_success_rate, ties.method = "min")) %>% 
      slice_max(plays_by_down, n=3) %>% 
      ungroup() %>% 
      mutate(down_usage_perc = round((plays_by_down/down_plays)*100,2),
             down_usage_count = paste0(down_usage_perc,  "% (", plays_by_down, ")")) %>% 
      select(down, defenders_in_box, down_usage_count, epa_per_play, epa_play_rank, grouped_off_success_rate, 
             suc_rate_rank, epa_per_run, epa_run_rank, epa_per_pass, epa_pass_rank, down_usage_perc, plays_by_down)
    
    BoxbyDown_table <- BoxbyDown_filtered %>% 
      gt(groupname_col = "down") %>% 
      gt_highlight_rows(columns = down_usage_count,
                        rows = (down_usage_perc >= 50 & plays_by_down >= 5),
                        fill = "#def440") %>% 
      cols_hide(down_usage_perc) %>% 
      fmt_percent(grouped_off_success_rate, decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_play, col2 = epa_play_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= epa_per_run, col2 = epa_run_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate, col2 = suc_rate_rank, palette = c ("black", "gray40")) %>% 
      cols_align("center") %>% 
      cols_label(
        defenders_in_box = "Box Defenders Count",
        down = "Down & Distance",
        down_usage_count = "Down Usage % (Play Count)",
        epa_per_play = "EPA/Play Allowed",
        grouped_off_success_rate = "SUccess % Allowed",
        epa_per_run = "EPA/Run Allowed",
        epa_per_pass = "EPA/Pass Allowed"
      ) %>% 
      tab_source_note("Team Rank in Gray under value | Table: @KageyDarren") %>% 
      gt_theme_espn() %>% 
      cols_hide(plays_by_down)
  }, width = 1100)
  
  output$PRCounts <- render_gt({
    
    PRCounts_filtered <- PR_counts_bind %>%  filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      ungroup() %>% 
      mutate(total_plays = sum(total_pass_count_PO)) %>% 
      group_by(number_of_pass_rushers) %>% 
      mutate(usage_perc = round((total_pass_count_PO/total_plays)*100,2),
             usage_count = paste0(usage_perc, "% (", total_pass_count_PO, ")")) %>%
      ungroup() %>% 
      slice_max(total_pass_count_PO, n=5) %>% 
      select(number_of_pass_rushers, usage_count, epa_per_pass, epa_pass_rank, grouped_off_success_rate_PO, 
             suc_rate_rank)
    
    PRCounts_Table <- PRCounts_filtered %>% 
      gt() %>% 
      fmt_percent(grouped_off_success_rate_PO, decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate_PO, col2 = suc_rate_rank, palette = c ("black", "navyblue")) %>% 
      cols_align("center") %>% 
      cols_label(
        number_of_pass_rushers = "Pass Rusher Count",
        usage_count = "Usage % (Play Count)",
        epa_per_pass = "EPA/Pass Allowed",
        grouped_off_success_rate_PO = "SUccess % Allowed"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("League Rank in Blue | Table: @KageyDarren") %>% 
      data_color(
        columns = epa_pass_rank,
        target_columns = epa_per_pass,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = suc_rate_rank,
        target_columns = grouped_off_success_rate_PO,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) 
    
  }, width = 1100)
  
  output$PRvsOP <- render_gt({
    
    PRvsOP_filtered <- PR_vs_OP_bind %>% filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      group_by(offense_personnel) %>% 
      mutate(total_pass_vs_OP_PO = sum(pass_vs_OP_PO)) %>% 
      slice_max(pass_vs_OP_PO, n=5) %>% 
      mutate(usage_perc = round((pass_vs_OP_PO/total_pass_vs_OP_PO)*100,2),
             usage_count = paste0(usage_perc, "% (", pass_vs_OP_PO, ")")) %>% 
      select(number_of_pass_rushers, offense_personnel, usage_count, epa_per_pass, grouped_off_success_rate_PO,
             usage_perc, pass_vs_OP_PO) %>% ungroup()
    
    PRvsOP_table <- PRvsOP_filtered %>% 
      gt() %>% 
      gt_highlight_rows(columns = usage_count,
                        rows = (usage_perc >= 50 & pass_vs_OP_PO >= 5),
                        fill = "#def440") %>% 
      fmt_percent(grouped_off_success_rate_PO, decimals = 2) %>% 
      cols_align("center") %>% 
      cols_label(
        number_of_pass_rushers = "Pass Rusher Count",
        offense_personnel = "Off Personnel",
        usage_count = "Usage % vs Off Pers (Play Count)",
        epa_per_pass = "EPA/Pass Allowed",
        grouped_off_success_rate_PO = "SUccess % Allowed"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("Table: @KageyDarren") %>% 
      cols_hide(columns = c(usage_perc, pass_vs_OP_PO))
  }, width = 1100)
  
  output$PRbyDown <- render_gt({
    
    PRbyDown_filtered <- PR_by_down_bind %>%  filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      group_by(down) %>% 
      mutate(down_plays = sum(down_pass_count)) %>% 
      mutate(epa_pass_rank = rank(epa_per_pass, ties.method = "min"),
             suc_rate_rank = rank(grouped_off_success_rate_PO, ties.method = "min")) %>% 
      slice_max(down_pass_count, n=3) %>% 
      ungroup() %>% 
      mutate(down_usage_perc = round((down_pass_count/down_plays)*100,2),
             down_usage_count = paste0(down_usage_perc,  "% (", down_pass_count, ")")) %>% 
      select(down, number_of_pass_rushers, down_usage_count, epa_per_pass, epa_pass_rank, 
             grouped_off_success_rate_PO, suc_rate_rank, down_usage_perc, down_pass_count)
    
    PRbyDown_table <- PRbyDown_filtered %>% 
      gt(groupname_col = "down") %>% 
      gt_highlight_rows(columns = down_usage_count,
                        rows = (down_usage_perc >= 50 & down_pass_count >= 5),
                        fill = "#def440") %>% 
      cols_hide(down_usage_perc) %>% 
      fmt_percent(grouped_off_success_rate_PO, decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate_PO, col2 = suc_rate_rank, palette = c ("black", "gray40")) %>% 
      cols_align("center") %>% 
      cols_label(
        number_of_pass_rushers = "Pass Rusher Count",
        down = "Down & Distance",
        down_usage_count = "Down Usage % (Play Count)",
        epa_per_pass = "EPA/Pass Allowed",
        grouped_off_success_rate_PO = "SUccess % Allowed"
      ) %>% 
      tab_source_note("Team Rank in Gray under value | Table: @KageyDarren") %>% 
      gt_theme_espn() %>% 
      cols_hide(down_pass_count)
  }, width = 1100)
  
  output$CovCounts <- render_gt({
    
    CovCounts_filtered <- Cov_counts_bind %>%  filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      ungroup() %>% 
      mutate(total_plays = sum(total_pass_count_PO)) %>% 
      group_by(defense_coverage_type) %>% 
      mutate(usage_perc = round((total_pass_count_PO/total_plays)*100,2),
             usage_count = paste0(usage_perc, "% (", total_pass_count_PO, ")")) %>%
      ungroup() %>% 
      slice_max(total_pass_count_PO, n=5) %>% 
      select(defense_coverage_type, usage_count, epa_per_pass, epa_pass_rank, grouped_off_success_rate_PO, 
             suc_rate_rank)
    
    CovCounts_Table <- CovCounts_filtered %>% 
      gt() %>% 
      fmt_percent(grouped_off_success_rate_PO, decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "navyblue")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate_PO, col2 = suc_rate_rank, palette = c ("black", "navyblue")) %>% 
      cols_align("center") %>% 
      cols_label(
        defense_coverage_type = "Coverage Scheme",
        usage_count = "Usage % (Play Count)",
        epa_per_pass = "EPA/Pass Allowed",
        grouped_off_success_rate_PO = "SUccess % Allowed"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("League Rank in Blue | Table: @KageyDarren") %>% 
      data_color(
        columns = epa_pass_rank,
        target_columns = epa_per_pass,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) %>% 
      data_color(
        columns = suc_rate_rank,
        target_columns = grouped_off_success_rate_PO,
        colors = scales::col_numeric(
          palette = c("#5fa861", "#a6cea7","#dbebdc","#ffffff","#fbd9da","#f6a1a2","#ee565c"),
          domain = c(1:32)
        )) 
  }, width = 1100)
  
  output$CovvsOP <- render_gt({
    
    CovvsOP_filtered <- Cov_vs_OP_bind %>% filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      group_by(offense_personnel) %>% 
      mutate(total_pass_vs_OP_PO = sum(pass_vs_OP_PO)) %>% 
      slice_max(pass_vs_OP_PO, n=5) %>% 
      mutate(usage_perc = round((pass_vs_OP_PO/total_pass_vs_OP_PO)*100,2),
             usage_count = paste0(usage_perc, "% (", pass_vs_OP_PO, ")")) %>% 
      select(defense_coverage_type, offense_personnel, usage_count, epa_per_pass, grouped_off_success_rate_PO,
             usage_perc, pass_vs_OP_PO) %>%  ungroup()
    
    CovvsOP_table <- CovvsOP_filtered %>% 
      gt() %>% 
      gt_highlight_rows(columns = usage_count,
                        rows = (usage_perc >= 50 & pass_vs_OP_PO >= 5),
                        fill = "#def440") %>% 
      fmt_percent(grouped_off_success_rate_PO, decimals = 2) %>% 
      cols_align("center") %>% 
      cols_label(
        defense_coverage_type = "Coverage Scheme",
        offense_personnel = "Off Personnel",
        usage_count = "Usage % vs Off Pers (Play Count)",
        epa_per_pass = "EPA/Pass Allowed",
        grouped_off_success_rate_PO = "SUccess % Allowed"
      ) %>% 
      gt_theme_espn() %>% 
      tab_source_note("Table: @KageyDarren") %>% 
      cols_hide(columns = c(usage_perc, pass_vs_OP_PO))
  }, width = 1100)
  
  output$CovbyDown <- render_gt({
    
    CovbyDown_filtered <- Cov_by_down_bind %>%  filter(defteam == input$Def_Team & season_type == input$Def_Season_Type & season == input$Def_Season) %>% 
      group_by(down) %>% 
      mutate(down_plays = sum(down_pass_count)) %>% 
      mutate(epa_pass_rank = rank(epa_per_pass, ties.method = "min"),
             suc_rate_rank = rank(grouped_off_success_rate_PO, ties.method = "min")) %>% 
      slice_max(down_pass_count, n=3) %>% 
      ungroup() %>% 
      mutate(down_usage_perc = round((down_pass_count/down_plays)*100,2),
             down_usage_count = paste0(down_usage_perc,  "% (", down_pass_count, ")")) %>% 
      select(down, defense_coverage_type, down_usage_count, epa_per_pass, epa_pass_rank, 
             grouped_off_success_rate_PO, suc_rate_rank, down_usage_perc, down_pass_count)
    
    CovbyDown_table <- CovbyDown_filtered %>% 
      gt(groupname_col = "down") %>% 
      gt_highlight_rows(columns = down_usage_count,
                        rows = (down_usage_perc >= 50 & down_pass_count >= 5),
                        fill = "#def440") %>% 
      cols_hide(down_usage_perc) %>% 
      fmt_percent(grouped_off_success_rate_PO, decimals = 2) %>% 
      gt_merge_stack(col1= epa_per_pass, col2 = epa_pass_rank, palette = c ("black", "gray40")) %>% 
      gt_merge_stack(col1= grouped_off_success_rate_PO, col2 = suc_rate_rank, palette = c ("black", "gray40")) %>% 
      cols_align("center") %>% 
      cols_label(
        defense_coverage_type = "Coverage Scheme",
        down = "Down & Distance",
        down_usage_count = "Down Usage % (Play Count)",
        epa_per_pass = "EPA/Pass Allowed",
        grouped_off_success_rate_PO = "SUccess % Allowed"
      ) %>% 
      tab_source_note("Team Rank in Gray under value | Table: @KageyDarren") %>% 
      gt_theme_espn() %>% 
      cols_hide(down_pass_count)
  }, width = 1100)
}



# Run the app
shinyApp(ui=ui, server=server)