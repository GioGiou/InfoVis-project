library(tidyverse)
library(stringr)
library(lubridate)
library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(htmlwidgets)
library(shinythemes)
library(bslib)
library(thematic)

# Data prep
data<-read.csv("animes.csv")

data %>% separate_wider_delim(aired, delim = " to ", names = c("start", "end"),
                              too_few = "align_start") %>%
  mutate(start=mdy(start), end = mdy(end)) %>% mutate(end = coalesce(end,start)) %>%
  dplyr::filter(start<=Sys.Date()) ->data #aired

data %>% separate_wider_delim(premiered, delim = " ", names = c("season", "year"),
                              too_few = "align_start")-> data #season

n = max(str_count(data$genres, ", ") + 1)
data %>% separate_wider_delim(genres, delim = ", ", names = paste0("genre",seq_len(n)),
                              too_few = "align_start") %>%
  pivot_longer(contains("genre"), names_to = NULL, values_to = "genre")->dataGanreSep


#shiny



css <-list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: auto; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: auto;    /* Firefox */ 
                                   column-count: auto; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 "
                                    )) 
))

ui <- fluidPage(theme = bs_theme(bootswatch = "sketchy"),
  useShinyjs(),
  css,
  # App title ----
  titlePanel("AniViz"),
  mainPanel(
    tabsetPanel(
      tabPanel("Score",
        fluidRow(
          column(9,plotlyOutput("GrafS")),
          column(3,selectInput("XosScore","X:",
                               choices = c("year", "season", "genres","studios"),
                               selected = "year"),
                 selectInput("YosScore","Y:",
                             choices = c("Min-Max", "Average"),
                             selected = "Min-Max"),
                 tags$div(align = 'left',
                          style='overflow-x: scroll;overflow-y: scroll;',
                          class = 'multicol',checkboxGroupInput("CatScore", "Genres:", 
                                                                choices = unique(dataGanreSep$genre), selected = unique(dataGanreSep$genre),
                                                                inline   = FALSE)),
                 tags$div(align = 'left',
                          style='overflow-x: scroll;overflow-y: scroll;',
                          class = 'multicol',checkboxGroupInput("StudioScore", "Studios:", 
                                                                choices = unique(data$studios), selected = "A-1 Pictures",
                                                                inline   = FALSE)),
                 actionButton("allStudioS","Select All Studios"),
                 actionButton("allGenreS","Select All Genres"))#selectInput()
        ),
        fluidRow(
          column(9,sliderInput("yearS","Year", min = year(min(data$start)),
                                max = year(max(data$start)), value = c(2010,2022),
                                width="100%")
                 )
        ),
        fluidRow(
          column(9, DT::dataTableOutput("tabS")),
          column(3, textOutput("nameS"),
                 uiOutput("imgS"))
        )
      ),
      tabPanel("Genre",fluidRow(
          column(9,plotlyOutput("GrafG",width = "100%")),
          column(3, selectInput("XosG","X:",
                                choices = c("year", "season"),
                                selected = "year"),
                 selectInput("YosG","Y:",
                             choices = c("Min-Max", "Number"),
                             selected = "Min-Max"),
                 tags$div(align = 'left',
                          style='overflow-x: scroll;overflow-y: scroll;',
                          class = 'multicol',checkboxGroupInput("Cat", "Genres:", 
                                      choices = unique(dataGanreSep$genre), selected = "Isekai",
                                      inline   = FALSE)))#selectInput()
        ),
        fluidRow(
          column(9,sliderInput("yearG","Year", min = year(min(data$start)),
                                max = year(max(data$start)), value = c(2010,2022),
                                width="100%")
          ),
          column(3)
        ),
        fluidRow(
          column(9, DT::dataTableOutput("tabG")),
          column(3, textOutput("nameG"),
                 uiOutput("imgG"))
        )
      ),
      tabPanel("Studio",fluidRow(
        column(9,plotlyOutput("GrafSt",width = "100%")),
        column(3, selectInput("XosS","X:",
                              choices = c("year", "season", "genres"),
                              selected = "year"),
               tags$div(align = 'left',
                        style='overflow-x: scroll;overflow-y: scroll;',
                        class = 'multicol',checkboxGroupInput("CatS", "Genres:", 
                                                              choices = unique(dataGanreSep$genre), selected = unique(dataGanreSep$genre),
                                                              inline   = FALSE)),
               tags$div(align = 'left',
                 style='overflow-x: scroll;overflow-y: scroll;',
                 class = 'multicol',checkboxGroupInput("StudioS", "Studios:", 
                                                       choices = unique(data$studios), selected = "A-1 Pictures",
                                                       inline   = FALSE)),
        actionButton("allStudio","Select All Studios"),
        actionButton("allGenre","Select All Genres"))
        #selectInput()
      ),
      fluidRow(
        column(9,sliderInput("yearSt","Year", min = year(min(data$start)),
                             max = year(max(data$start)), value = c(2010,2022),
                             width="100%")
        ),
        column(3)
      ),
      fluidRow(DT::dataTableOutput("tabSt"))
      ),
      tabPanel("Popularity/Rankig",fluidRow(
        plotlyOutput("GraphPR",width = "100%")),
        fluidRow(
          column(9, DT::dataTableOutput("tabPR")),
          column(3, textOutput("namePR"),
                 uiOutput("imgPR"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  dataS <- reactive({
    data %>% dplyr::filter(start>=ymd(input$yearS[1],truncated = 2L),end<ymd(input$yearS[2]+1,truncated = 2L))})
  dataStY <- reactive({
    dataGanreSep %>% dplyr::filter(start>=ymd(input$yearSt[1],truncated = 2L),end<ymd(input$yearSt[2]+1,truncated = 2L)) %>%
      dplyr::filter(genre %in% input$CatS) %>% dplyr::filter(studios %in% input$StudioS) %>% pull(anime_id)-> b
    data %>% dplyr::filter(anime_id %in% b)
    })
  dataSt <- reactive({
    dataGanreSep %>% dplyr::filter(genre %in% input$CatS) %>%
      dplyr::filter(studios %in% input$StudioS) %>% pull(anime_id)-> b
    data %>% dplyr::filter(anime_id %in% b)
  })
  dataStG <- reactive({
    dataGanreSep %>% dplyr::filter(genre %in% input$CatS) %>%
      dplyr::filter(studios %in% input$StudioS)
  })
  dataGY <- reactive({
    dataGanreSep %>% dplyr::filter(start>=ymd(input$yearG[1],truncated = 2L),end<ymd(input$yearG[2]+1,truncated = 2L)) %>%
      dplyr::filter(genre %in% input$Cat)})
  dataG <- reactive({
    dataGanreSep %>%
      dplyr::filter(genre %in% input$Cat)})
  dataGS <- reactive({
    dataGanreSep %>%
      dplyr::filter(genre %in% input$CatScore)})
  
  #Score
  observe({
    toggle(id = "CatScore", condition = input$XosScore == "genres")
    toggle(id = "allGenreS", condition = input$XosScore == "genres")
  })
  observe({
    toggle(id = "StudioScore", condition = input$XosScore == "studios")
    toggle(id = "allStudioS", condition = input$XosScore == "genres")
  })
  observe({
    toggle(id = "yearS", condition = input$XosScore == "year")
  })
  observe({
    if(input$allStudio== 1){
      updateCheckboxGroupInput(session,"StudioS", "Studios:", 
                               choices = unique(data$studios),
                               selected =unique(data$studios))
      
    }
  })
  observeEvent(input$allGenreS,{
    a <- unique(dataGanreSep$genre)
    updateCheckboxGroupInput(session,"CatStudios", "Genres:", 
                             choices = a,
                             selected = a)
  })
  observe({
    if(input$allStudioS== 1){
      updateCheckboxGroupInput(session,"StudioScore", "Studios:", 
                               choices = unique(data$studios),
                               selected =unique(data$studios))
      
    }
  })
  
  
  
  output$GrafS<-renderPlotly({
    tmp <-data
    if(input$XosScore == "year"){
      if(input$YosScore  == "Min-Max"){
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp <-dataS()
        tmp %>% dplyr::group_by(year) %>% dplyr::summarise(min=min(score,na.rm=T),max= max(score,na.rm=T),IDmin = anime_id[which.min(score)],IDmax = anime_id[which.max(score)]) %>%
          plot_ly(x= ~year) %>% 
          add_markers(y= ~min,color= I("red")) %>% 
          add_markers(y= ~max,color= I("green")) %>% 
          layout(showlegend = FALSE) %>% 
          config(displaylogo = FALSE) %>% 
          layout(dragmode = "select") %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
          event_register("plotly_selected")})
      }
      else{
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp <-dataS()
        tmp %>% dplyr::group_by(year) %>% dplyr::summarise(mean=mean(score,na.rm=T)) %>%
          plot_ly(x= ~year) %>% 
          add_markers(y= ~mean) %>% 
          layout(showlegend = FALSE) %>% 
          config(displaylogo = FALSE) %>% 
          layout(dragmode = "select") %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
          event_register("plotly_selected")})
      }
      
    }
    else if(input$XosScore == "season"){
      if(input$YosScore  == "Min-Max"){
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp %>% dplyr::group_by(season) %>% dplyr::summarise(min=min(score,na.rm=T),max= max(score,na.rm=T),IDmin = anime_id[which.min(score)],IDmax = anime_id[which.max(score)]) %>%
          plot_ly(x= ~season) %>% 
          add_markers(y= ~min,color= I("red")) %>% 
          add_markers(y= ~max,color= I("green")) %>% 
          layout(showlegend = FALSE) %>% 
          config(displaylogo = FALSE) %>% 
          layout(dragmode = "select") %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
          event_register("plotly_selected")})
      }
      else{
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp %>% dplyr::group_by(season) %>% dplyr::summarise(mean = mean(score,na.rm=T)) %>%
          plot_ly(x= ~season) %>% 
          add_markers(y= ~mean) %>% 
          layout(showlegend = FALSE) %>% 
          config(displaylogo = FALSE) %>% 
          layout(dragmode = "select") %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
          event_register("plotly_selected")})
      }
      
    }
    else if(input$XosScore == "genres"){
      if(input$YosScore  == "Min-Max"){
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp <-dataGS()
        tmp %>% dplyr::group_by(genre) %>% dplyr::summarise(min=min(score,na.rm=T),max= max(score,na.rm=T),IDmin = anime_id[which.min(score)],IDmax = anime_id[which.max(score)]) %>%
          plot_ly(x= ~genre) %>% 
          add_markers(y= ~min,color= I("red")) %>% 
          add_markers(y= ~max,color= I("green")) %>% 
          layout(showlegend = FALSE) %>% 
          config(displaylogo = FALSE) %>% 
          layout(dragmode = "select") %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
          event_register("plotly_selected")})
      }
      else{
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp <-dataGS()
        tmp %>% dplyr::group_by(genre) %>% dplyr::summarise(mean = mean(score,na.rm=T)) %>%
          plot_ly(x= ~genre) %>% 
          add_markers(y= ~mean) %>% 
          layout(showlegend = FALSE) %>% 
          config(displaylogo = FALSE) %>% 
          layout(dragmode = "select") %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
          event_register("plotly_selected")})
      }
      
    }
    else{
      if(input$YosScore  == "Min-Max"){
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp %>% dplyr::group_by(studios) %>% dplyr::summarise(min=min(score,na.rm=T),max= max(score,na.rm=T),IDmin = anime_id[which.min(score)],IDmax = anime_id[which.max(score)]) %>%
          plot_ly(x= ~studios) %>% 
          add_markers(y= ~min,color= I("red")) %>% 
          add_markers(y= ~max,color= I("green")) %>% 
          layout(showlegend = FALSE) %>% 
          config(displaylogo = FALSE) %>% 
          layout(dragmode = "select") %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
          event_register("plotly_selected")})
      }
      else{
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp %>% dplyr::group_by(studios) %>% dplyr::summarise(mean = mean(score,na.rm=T)) %>%
          plot_ly(x= ~studios) %>% 
          add_markers(y= ~mean) %>% 
          layout(showlegend = FALSE) %>% 
          config(displaylogo = FALSE) %>% 
          layout(dragmode = "select") %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
          event_register("plotly_selected")})
      }
    }
  })
  
  output$tabS <-  DT::renderDataTable({
    tmp<-data
    point <- event_data(event = "plotly_selected", priority = "event")
    if(input$XosScore == "year"){
      if(input$YosScore  == "Min-Max"){
        tmp <-dataS()
        if(length(point) == 0){tmp %>% select(title, score, start, end)}
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(year == point$x[i] & score == point$y[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          tmp %>% select(title, score, start, end)
        }
      }
      else{
        tmp <-dataS()
        if(length(point) == 0){
          a<- tmp %>% group_by(year) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "year") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd) %>%  select(title, score, start, end)
          }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(year == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(year) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "year") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd) %>%  select(title, score, start, end)
        }
      }
      
    }
    else if(input$XosScore == "season"){
      if(input$YosScore  == "Min-Max"){
        if(length(point) == 0){tmp %>% select(title, score, season)}
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(season == point$x[i] & score == point$y[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          tmp %>% select(title, score, season)
        }
      }
      else{
        if(length(point) == 0){
          a<- tmp %>% group_by(season) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "season") %>% 
            dplyr::filter(score<=mean+sd & score>=mean-sd) %>% select(title, score, season)
          }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(season == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(season) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "season") %>% 
            dplyr::filter(score<=mean+sd & score>=mean-sd) %>% select(title, score, season)
        }
      }
    }
    else if(input$XosScore == "genres"){
      if(input$YosScore  == "Min-Max"){
        tmp <-dataGS()
        if(length(point) == 0){tmp %>% select(title, score, genre)}
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(genre == point$x[i] & score == point$y[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          tmp %>% select(title, score, genre)
        }
      }
      else{
        tmp <-dataGS()
        if(length(point) == 0){
          a<- tmp %>% group_by(genre) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "genre") %>% 
            dplyr::filter(score<=mean+sd & score>=mean-sd) %>%  select(title, score, genre)
          }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(genre == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(genre) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "genre") %>% 
            dplyr::filter(score<=mean+sd & score>=mean-sd) %>%  select(title, score, genre)
        }
      }
      
    }
    else{
      if(input$YosScore  == "Min-Max"){
        if(length(point) == 0){tmp %>% select(title, score, studios)}
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(studios == point$x[i] & score == point$y[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          tmp %>% select(title, score, studios)
        }
      }
      else{
        if(length(point) == 0){
          a<- tmp %>% group_by(studios) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "studios") %>% 
            dplyr::filter(score<=mean+sd & score>=mean-sd) %>% select(title, score, studios)
          }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(studios == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(studios) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "studios") %>% 
            dplyr::filter(score<=mean+sd & score>=mean-sd) %>% select(title, score, studios)
        }
      }
      
    }

    
    
    
  },server = T, selection = list(mode = "single",
                               target = "row"))
  output$imgS <- renderText({
    row = input$tabS_rows_selected
    tmp<-data
    point <- event_data(event = "plotly_selected", priority = "event")
    if(input$XosScore == "year"){
      tmp<-dataS()
      if(input$YosScore  == "Min-Max"){
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(year == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
      }
      else{
        if(length(point) == 0){
          a<- tmp %>% group_by(year) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "year") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd)->tmp
        }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(year == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(year) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "year") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd) -> tmp
          
        }
      }
      a <- ""
      if(length(row) == 1){
        src = tmp$poster[row]
        alt = tmp$title[row]
       ref = paste0("https://myanimelist.net/anime/",tmp$anime_id[row],"/")
       a<-paste0('<a href="',ref,'" target="_blank" rel="noopener noreferrer"> <img src="',src,'" alt ="',alt,'", style = "position:relative;width: 300px;  object-fit: contain;"></a>')}
      a
    }
    else if(input$XosScore == "season"){
      if(input$YosScore  == "Min-Max"){
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(season == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
      }
      else{
        if(length(point) == 0){
          a<- tmp %>% group_by(season) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "season") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd)->tmp
        }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(season == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(season) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "season") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd) -> tmp
          
        }
      }
      
      a <- ""
      if(length(row) == 1){
        src = tmp$poster[row]
        alt = tmp$title[row]
        ref = paste0("https://myanimelist.net/anime/",tmp$anime_id[row],"/")
        a<-paste0('<a href="',ref,'" target="_blank" rel="noopener noreferrer"> <img src="',src,'" alt ="',alt,'", style = "position:relative;width: 300px;  object-fit: contain;"></a>')}
      a
    }
    else if(input$XosScore == "genres"){
      tmp <-dataGS()
      if(input$YosScore  == "Min-Max"){
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(genre == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
      }
      else{
        if(length(point) == 0){
          a<- tmp %>% group_by(genre) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "genre") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd)->tmp
        }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(genre == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(genre) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "genre") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd) -> tmp
          
        }
      }
      
      a <- ""
      if(length(row) == 1){
        src = tmp$poster[row]
        alt = tmp$title[row]
       ref = paste0("https://myanimelist.net/anime/",tmp$anime_id[row],"/")
       a<-paste0('<a href="',ref,'" target="_blank" rel="noopener noreferrer"> <img src="',src,'" alt ="',alt,'", style = "position:relative;width: 300px;  object-fit: contain;"></a>')}
      a
    }
    else{
      if(input$YosScore  == "Min-Max"){
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(studios == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
      }
      else{
        if(length(point) == 0){
          a<- tmp %>% group_by(studios) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "studios") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd)->tmp
        }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(studios == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(studios) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "studios") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd) -> tmp
          
        }
      }
      a <- ""
      if(length(row) == 1){
        src = tmp$poster[row]
        alt = tmp$title[row]
       ref = paste0("https://myanimelist.net/anime/",tmp$anime_id[row],"/")
       a<-paste0('<a href="',ref,'" target="_blank" rel="noopener noreferrer"> <img src="',src,'" alt ="',alt,'", style = "position:relative;width: 300px;  object-fit: contain;"></a>')}
      a
    }

    
  })
  
  output$nameS<-renderText({
    row = input$tabS_rows_selected
    tmp<-data
    point <- event_data(event = "plotly_selected", priority = "event")
    
    if(input$XosScore == "year"){
      tmp<-dataS()
      if(input$YosScore  == "Min-Max"){
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(year == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
      }
      else{
        if(length(point) == 0){
          a<- tmp %>% group_by(year) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "year") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd)->tmp
        }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(year == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(year) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "year") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd) -> tmp
          
        }
      }
      a <- ""
      if(length(row) == 1){
        src = tmp$poster[row]
        alt = tmp$title[row]
        a<-alt}
      a
    }
    else if(input$XosScore == "season"){
      if(input$YosScore  == "Min-Max"){
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(season == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
      }
      else{
        if(length(point) == 0){
          a<- tmp %>% group_by(season) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "season") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd)->tmp
        }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(season == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(season) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "season") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd) -> tmp
          
        }
      }
      a <- ""
      if(length(row) == 1){
        src = tmp$poster[row]
        alt = tmp$title[row]
        a<-alt}
      a
    }
    else if(input$XosScore == "genres"){
      tmp <-dataGS()
      if(input$YosScore  == "Min-Max"){
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(genre == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
      }
      else{
        if(length(point) == 0){
          a<- tmp %>% group_by(genre) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "genre") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd)->tmp
        }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(genre == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(genre) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "genre") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd) -> tmp
          
        }
      }
      a <- ""
      if(length(row) == 1){
        src = tmp$poster[row]
        alt = tmp$title[row]
        a<-alt}
      a
    }
    else{
      if(input$YosScore  == "Min-Max"){
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(studios == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
      }
      else{
        if(length(point) == 0){
          a<- tmp %>% group_by(studios) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "studios") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd)->tmp
        }
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(studios == point$x[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
          a<- tmp %>% group_by(studios) %>%dplyr::summarise(mean=mean(score, na.rm = T), sd = sd(score, na.rm = T))
          tmp %>% left_join(a, by = "studios") %>%  dplyr::filter(score<=mean+sd & score>=mean-sd) -> tmp
          
        }
      }
      a <- ""
      if(length(row) == 1){
        src = tmp$poster[row]
        alt = tmp$title[row]
        a<-alt}
      a
      
    }
    
    
  }  )
  
  
  #Genre
  
  observe({
    toggle(id = "yearG", condition = input$XosG == "year")
  })
  
  output$GrafG<-renderPlotly({
    tmp <-dataG()
    if(input$XosG =="year"){
      tmp <-dataGY()
      if(input$YosG == "Min-Max"){
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp %>% drop_na(genre) %>%  dplyr::group_by(year,genre) %>% dplyr::summarise(min=min(score,na.rm=T),max= max(score,na.rm=T),IDmin = anime_id[which.min(score)],IDmax = anime_id[which.max(score)]) %>%
          plot_ly(x= ~year,color = ~genre) %>% 
          add_markers(y= ~min, marker = list(
            line = list(color = 'rgba(152, 0, 0, .8)',
                        width = 2)
          )
          ) %>% 
          add_markers(y= ~max, marker = list(
            line = list(color = 'rgba(0, 152, 0, .8)',
                        width = 2)
          )
          ) %>% 
          config(displaylogo = FALSE) %>% 
          layout(dragmode = "select")%>% 
          layout(showlegend = FALSE) %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
          event_register("plotly_selected")})
      }
      else{
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp %>% drop_na(genre) %>%  dplyr::group_by(year,genre) %>% dplyr::summarise(n=n()) %>%
          plot_ly(x= ~year,color = ~genre) %>% 
          add_markers(y= ~n) %>% 
          config(displaylogo = FALSE) %>% 
          layout(dragmode = "select")%>% 
          layout(showlegend = FALSE) %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
          event_register("plotly_selected")})
      }
    }
    else{
      if(input$YosG == "Min-Max"){
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp %>% drop_na(genre) %>%  dplyr::group_by(season,genre) %>% dplyr::summarise(min=min(score,na.rm=T),max= max(score,na.rm=T),IDmin = anime_id[which.min(score)],IDmax = anime_id[which.max(score)]) %>%
        plot_ly(x= ~season,color = ~genre) %>% 
        add_markers(y= ~min, marker = list(
          line = list(color = 'rgba(152, 0, 0, .8)',
                      width = 2)
        )
        ) %>% 
        add_markers(y= ~max, marker = list(
          line = list(color = 'rgba(0, 152, 0, .8)',
                      width = 2)
        )
        ) %>% 
        config(displaylogo = FALSE) %>% 
        layout(dragmode = "select")%>% 
        layout(showlegend = FALSE) %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
        event_register("plotly_selected")})
      }
      else{
        thematic_with_theme(thematic_theme(accent = "#007bff"),{
        tmp %>% drop_na(genre) %>%  dplyr::group_by(season,genre) %>% dplyr::summarise(n=n()) %>%
          plot_ly(x= ~season,color = ~genre) %>% 
          add_markers(y= ~n
          ) %>% 
          config(displaylogo = FALSE) %>% 
          layout(dragmode = "select")%>% 
          layout(showlegend = FALSE) %>% 
          layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
          event_register("plotly_selected")})
      }
      
    }
  })
  
  output$tabG <-  DT::renderDataTable({
    tmp<-dataG()
    point <- event_data(event = "plotly_selected", priority = "event")
    if(input$XosG =="year"){
      if(input$YosG == "Min-Max"){
        if(length(point) == 0){
          tmp %>% pull(anime_id)-> b
          tmp = data %>% dplyr::filter(anime_id %in% b)
          tmp  %>%  select(title, score, genres, start, end)}
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(year == point$x[i] & score == point$y[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>% dplyr::distinct()
          tmp %>% pull(anime_id)-> b
          tmp = data %>% dplyr::filter(anime_id %in% b)
          tmp  %>%  select(title, score, genres, start, end)
        }
      }
      else{
        if(length(point) == 0){
          tmp  %>%  dplyr::group_by(year,genre) %>% dplyr::summarise(number = n())}
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>%  dplyr::group_by(year,genre) %>% dplyr::summarise(number = n()) %>% 
              dplyr::filter(year == point$x[i] & number == point$y[i])->b
            a<-rbind(a,b)
          }
          a
        }
      }
      
    }
    else{
      if(input$YosG == "Min-Max"){
        if(length(point) == 0){
          tmp %>% pull(anime_id)-> b
          tmp = data %>% dplyr::filter(anime_id %in% b)
          tmp  %>%  select(title, score, genres, season)}
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(season == point$x[i] & score == point$y[i])->b
            a<-rbind(a,b)
          }
          tmp= a %>% dplyr::distinct()
          tmp %>% pull(anime_id)-> b
          tmp = data %>% dplyr::filter(anime_id %in% b)
          tmp  %>%  select(title, score, genres, season)
        }
      
      }
      else{
        if(length(point) == 0){
          tmp  %>%  dplyr::group_by(season,genre) %>% dplyr::summarise(number = n())}
        else{
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>%  dplyr::group_by(season,genre) %>% dplyr::summarise(number = n()) %>% 
              dplyr::filter(season == point$x[i] & number == point$y[i])->b
            a<-rbind(a,b)
          }
          a
        }
      }
    }
   
    
  },server = T, selection = list(mode = "single",target = "row"))
  
  output$imgG <- renderText({
    row = input$tabG_rows_selected
    tmp<-dataG()
    point <- event_data(event = "plotly_selected", priority = "event")
    cat<-input$traces
    if(input$YosG== "Min-Max"){
      if(input$XosG =="year"){
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(year == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
        tmp %>% pull(anime_id)-> b
        tmp = data %>% dplyr::filter(anime_id %in% b)
        a <- ""
        if(length(row) == 1){
          src = tmp$poster[row]
          alt = tmp$title[row]
         ref = paste0("https://myanimelist.net/anime/",tmp$anime_id[row],"/")
         a<-paste0('<a href="',ref,'" target="_blank" rel="noopener noreferrer"> <img src="',src,'" alt ="',alt,'", style = "position:relative;width: 300px;  object-fit: contain;"></a>')}
        a
      }
      else{
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(season == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
        tmp %>% pull(anime_id)-> b
        tmp = data %>% dplyr::filter(anime_id %in% b)
        a <- ""
        if(length(row) == 1){
          src = tmp$poster[row]
          alt = tmp$title[row]
         ref = paste0("https://myanimelist.net/anime/",tmp$anime_id[row],"/")
         a<-paste0('<a href="',ref,'" target="_blank" rel="noopener noreferrer"> <img src="',src,'" alt ="',alt,'", style = "position:relative;width: 300px;  object-fit: contain;"></a>')}
        a
      }
    }
    else{
      a<-""
      a
    }
    
  })
  
  
  output$nameG<-renderText({
    row = input$tabG_rows_selected
    tmp<-dataG()
    point <- event_data(event = "plotly_selected", priority = "event")
    if(input$YosG == "Min-Max"){
      if(input$XosG =="year"){
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(year == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
        tmp %>% pull(anime_id)-> b
        tmp = data %>% dplyr::filter(anime_id %in% b)
        a <- ""
        if(length(row) == 1){
          src = tmp$poster[row]
          alt = tmp$title[row]
          a<-alt}
        a
      }
      else{
        if(length(point) != 0){
          a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
          names(a)<-names(tmp)
          for (i in 1:nrow(point)) {
            tmp %>% dplyr::filter(season == point$x[i], score == point$y[i])->b
            a <-rbind(a,b)
          }
          tmp= a %>%dplyr::distinct()
        }
        tmp %>% pull(anime_id)-> b
        tmp = data %>% dplyr::filter(anime_id %in% b)
        a <- ""
        if(length(row) == 1){
          src = tmp$poster[row]
          alt = tmp$title[row]
          a<-alt}
        a
      }
    }
    else{
      a <- ""
      a
    }
    
    
  }  )
  
  # Studio
  output$GrafSt<-renderPlotly({
    tmp <-dataSt()
    if(input$XosS == "year"){
      tmp <-dataStY()
      thematic_with_theme(thematic_theme(accent = "#007bff"),{
      p<-tmp %>%  dplyr::group_by(studios,year) %>% dplyr::summarise(number = n()) %>% 
        ggplot()+
        aes(x=year, y=studios, fill = number)+
        geom_tile()+
        theme_minimal()
      
      ggplotly(p) %>% 
        config(displaylogo = FALSE) %>% 
        layout(dragmode = "zoom")%>% 
        layout(showlegend = FALSE) %>% 
        layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor   = "rgba(0, 0, 0, 0)")})
    }
    else if(input$XosS == "season"){
      thematic_with_theme(thematic_theme(accent = "#007bff"),{
        p<-tmp %>%  dplyr::group_by(season,studios) %>% dplyr::summarise(number = n()) %>% 
        ggplot()+
        aes(x=season, y=studios, fill = number)+
        geom_tile()+
        theme_minimal()
      
      ggplotly(p) %>% 
        config(displaylogo = FALSE) %>% 
        layout(dragmode = "zoom")%>% 
        layout(showlegend = FALSE) %>% 
        layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor   = "rgba(0, 0, 0, 0)")})
    }
    else{
      tmp <-dataStG()
      thematic_with_theme(thematic_theme(accent = "#007bff"),{
        p<-tmp %>%  dplyr::group_by(genre,studios) %>% dplyr::summarise(number = n()) %>% 
        ggplot()+
        aes(x=genre, y=studios, fill = number)+
        geom_tile()+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
      ggplotly(p) %>% 
        config(displaylogo = FALSE) %>% 
        layout(dragmode = "zoom")%>% 
        layout(showlegend = FALSE) %>% 
        layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor   = "rgba(0, 0, 0, 0)")})
    }
    
  })
  observe({
    toggle(id = "CatS", condition = input$XosS == "genres")
    toggle(id = "allGenre", condition = input$XosS == "genres")
  })
  observe({
    toggle(id = "yearSt", condition = input$XosS == "year")
  })
  observe({
    if(input$allStudio== 1){
      updateCheckboxGroupInput(session,"StudioS", "Studios:", 
                               choices = unique(data$studios),
                               selected =unique(data$studios))

    }
  })
  observeEvent(input$allGenre,{
    a <- unique(dataGanreSep$genre)
      updateCheckboxGroupInput(session,"CatS", "Genres:", 
                               choices = a,
                               selected = a)
  })
  output$tabSt <-  DT::renderDataTable({
    tmp <-dataSt()
    if(input$XosS == "year"){
      tmp <-dataStY()
      
      tmp %>%  dplyr::group_by(studios,year) %>% dplyr::summarise(number = n()) 
      
    }
    else if(input$XosS == "season"){
      tmp %>%  dplyr::group_by(season,studios) %>% dplyr::summarise(number = n()) 
     
    }
    else{
      tmp <-dataStG()
      tmp %>%  dplyr::group_by(genre,studios) %>% dplyr::summarise(number = n()) 
      
    }
    },server = T, selection = list(mode = "single",target = "row"))
  
  # Rankig/popularity
  
  output$GraphPR<-renderPlotly({
    thematic_with_theme(thematic_theme(accent = "#007bff"),{
    tmp <-data
    tmp %>% ggplot()+
      aes(x = popularity, y = ranked, color =ranked-popularity )+
      geom_point(alpha=0.2)+
      theme_minimal()+
      theme(legend.position = "none")->p
      
      ggplotly(p) %>% 
      config(displaylogo = FALSE) %>% 
      layout(dragmode = "select")%>% 
      layout(showlegend = FALSE) %>% 
      layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
             paper_bgcolor = "rgba(0, 0, 0, 0)",
             fig_bgcolor   = "rgba(0, 0, 0, 0)") %>% 
      event_register("plotly_selected")})
  })
  
  output$tabPR <-  DT::renderDataTable({
    tmp<-data
    
    point <- event_data(event = "plotly_selected", priority = "event")
    if(length(point) == 0){tmp %>% select(title, popularity, ranked, score, members)}
    else{
      a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
      names(a)<-names(tmp)
      for (i in 1:nrow(point)) {
        tmp %>% dplyr::filter(popularity == point$x[i] & ranked == point$y[i])->b
        a<-rbind(a,b)
      }
      tmp= a %>%dplyr::distinct()
      tmp %>% select(title, popularity, ranked, score, members)
    }
    
  },server = T, selection = list(mode = "single",
                                 target = "row"))
  output$imgPR <- renderText({
    row = input$tabPR_rows_selected
    tmp<-data
    point <- event_data(event = "plotly_selected", priority = "event")
    if(length(point) != 0){
      a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
      names(a)<-names(tmp)
      for (i in 1:nrow(point)) {
        tmp %>% dplyr::filter(popularity == point$x[i] & ranked == point$y[i])->b
        a<-rbind(a,b)
      }
      tmp= a %>%dplyr::distinct()
    }
    a <- ""
    if(length(row) == 1){
      src = tmp$poster[row]
      alt = tmp$title[row]
     ref = paste0("https://myanimelist.net/anime/",tmp$anime_id[row],"/")
     a<-paste0('<a href="',ref,'" target="_blank" rel="noopener noreferrer"> <img src="',src,'" alt ="',alt,'", style = "position:relative;width: 300px;  object-fit: contain;"></a>')}
    a
  })
  output$namePR<-renderText({
    row = input$tabPR_rows_selected
    tmp<-data
    point <- event_data(event = "plotly_selected", priority = "event")
    if(length(point) != 0){
      a = data.frame(matrix(ncol = ncol(tmp),nrow = 0))
      names(a)<-names(tmp)
      for (i in 1:nrow(point)) {
        tmp %>% dplyr::filter(popularity == point$x[i] & ranked == point$y[i])->b
        a<-rbind(a,b)
      }
      tmp= a %>%dplyr::distinct()
    }
    a <- ""
    if(length(row) == 1){
      src = tmp$poster[row]
      alt = tmp$title[row]
      a<-alt}
    a
  }  )
  
}

thematic_shiny()

shinyApp(ui = ui, server = server)