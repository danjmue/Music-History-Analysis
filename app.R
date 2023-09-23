library(tidyverse)
library(shiny)
library(shinythemes)
library(bslib)
library(thematic)
library(ragg)
library(jpeg)
library(spotifyr)
library(data.table)
library(httr)
library(showtext)
library(wordcloud2)

source('Music.R')
source('charts.R')

ui <- navbarPage(
  # Creating a custom theme for the Shiny App
  theme = bs_theme(
    bg = "#121212", fg = "white", primary = "#FA8072", secondary = '#8072FA',
    base_font = font_google("Questrial")
  ),
  "Music",
  
  # Home panel
  tabPanel('Home',
           tabsetPanel(
             displayHome('Aalaynah'),
             displayHome('Aditya'),
             displayHome('Daniel'),
           )
           ),
  navbarMenu('Totals',
    tabPanel('Top Songs', column(12, selectInput('totalsTopSongsPersonSelect', 'Person', personChoices, selected = 'DM')),
             displayTopSongPlaylists(),
             fluidRow(column(12, dataTableOutput('totalsTopSongs')))
             ),
    tabPanel('Top Artists', column(12, selectInput('totalsTopArtistsPersonSelect', 'Person', personChoices, selected = 'DM')),
             fluidRow(column(12, dataTableOutput('totalsTopArtists'))),
             fluidRow(column(12, dataTableOutput('totalsTopArtistRates')))
             ),
    tabPanel('Top Albums', column(12, selectInput('totalsTopAlbumsPersonSelect', 'Person', personChoices, selected = 'DM')),
             fluidRow(column(12, dataTableOutput('totalsTopAlbums'))),
             fluidRow(column(12, dataTableOutput('totalsTopAlbumRates')))
             ),
    tabPanel('Top Days', column(12, selectInput('totalsTopDaysPersonSelect', 'Person', personChoices, selected = 'DM')),
             fluidRow(column(12, dataTableOutput('totalsTopDays')))
             )
  ),
  
  # Splits panel
  navbarMenu('Splits',
    tabPanel('By Year',
             tabsetPanel(id = 'splitsYear',
               selectInput('splitsYearSelect', 'Year', yearList),
               displaySplit('year', 'Aalaynah'),
               displaySplit('year', 'Aditya'),
               displaySplit('year', 'Daniel')
             )
             ),
    tabPanel('By Season',
             tabsetPanel(id = 'splitsSeason',
               selectInput('splitsSeasonSelect', 'Season', seasonList),
               displaySplit('season', 'Aalaynah'),
               displaySplit('season', 'Aditya'),
               displaySplit('season', 'Daniel')
             )
             ),
    tabPanel('By Month',
             tabsetPanel(id = 'splitsMonth',
               selectInput('splitsMonthSelect', 'Month', monthChoices),
               displaySplit('month', 'Aalaynah'),
               displaySplit('month', 'Aditya'),
               displaySplit('month', 'Daniel')
             )
             ),
    tabPanel('By Month & Year',
             tabsetPanel(id = 'splitsMonthyear',
               selectInput('splitsMonthyearmSelect', 'Month', monthChoices),
               selectInput('splitsMonthyearySelect', 'Year', yearList),
               displaySplit('monthyear', 'Aalaynah'),
               displaySplit('monthyear', 'Aditya'),
               displaySplit('monthyear', 'Daniel')
             )
             ),
    tabPanel('By School Year',
             tabsetPanel(id = 'splitsSchoolperiod',
               selectInput('splitsSchoolperiodSelect', 'School Year', schoolperiodList),
               displaySplit('schoolperiod', 'Aalaynah'),
               displaySplit('schoolperiod', 'Aditya'),
               displaySplit('schoolperiod', 'Daniel')
             )
             ),
    tabPanel('By Hour',
             tabsetPanel(id = 'splitsHour',
               selectInput('splitsHourSelect', 'Hour', hourChoices),
               displaySplit('hour', 'Aalaynah'),
               displaySplit('hour', 'Aditya'),
               displaySplit('hour', 'Daniel')
             )
             )
  ),
  
  # Song Breakdown panel
  tabPanel('Song Breakdown',
           fluidRow(column(4, selectInput('songBreakdownPersonSelect', 'Person', personChoices, selected = 'DM')),
                    column(4, selectInput('songBreakdownArtistSelect', 'Artist', choices = NULL)),
                    column(4, selectInput('songBreakdownSongSelect', 'Song', choices = NULL)),
           ),
           htmlOutput('songFirstListenText'),
           fluidRow(column(4, imageOutput('songTimeBarGraph')),
                    column(4, htmlOutput('songSpotifyLink')),
                    column(4, imageOutput('songAudioFeatSpiderChart'))
           ),
           htmlOutput('songMonthyearText'),
           fluidRow(column(6, imageOutput('songYearlyChart')),
                    column(6, imageOutput('songCumChart'))
           )
  ),
  
  # Artist Breakdown panel
  tabPanel('Artist Breakdown',
           fluidRow(column(6, selectInput('artistBreakdownPersonSelect', 'Person', personChoices, selected = 'DM')),
                    column(6, selectInput('artistBreakdownArtistSelect', 'Artist', choices = NULL))
           ),
           htmlOutput('artistFirstListenText'),
           fluidRow(column(4, imageOutput('artistTimeBarGraph')),
                    column(4, htmlOutput('artistSpotifyLink')),
                    column(4, imageOutput('artistCumChart'))
           ),
           htmlOutput('artistMonthyearText'),
           fluidRow(column(12, imageOutput('artistTopSongsChart'))),
           fluidRow(column(12, dataTableOutput('artistTopSongsTable'))),
           fluidRow(column(12, imageOutput('artistStackChart'))),
           fluidRow(column(12, dataTableOutput('artistTopAlbumsTable')))
  ),
  
  # Album Breakdown panel
  tabPanel('Album Breakdown',
           fluidRow(column(4, selectInput('albumBreakdownPersonSelect', 'Person', personChoices, selected = 'DM')),
                    column(4, selectInput('albumBreakdownArtistSelect', 'Artist', choices = NULL)),
                    column(4, selectInput('albumBreakdownAlbumSelect', 'Album', choices = NULL))),
           htmlOutput('albumFirstListenText'),
           fluidRow(column(4, imageOutput('albumTimeBarGraph')),
                    column(4, htmlOutput('albumSpotifyLink')),
                    column(4, imageOutput('albumCumChart'))
           ),
           htmlOutput('albumMonthyearText'),
           fluidRow(column(12, imageOutput('albumTopSongsChart'))),
           fluidRow(column(12, dataTableOutput('albumTopSongsTable')))
  ),
  
  # Custom split panel
  tabPanel('Custom Splits',
           fluidRow(column(4, selectInput('customSplitsPersonSelect', 'Person', personChoices, selected = 'DM')),
                    column(4, dateRangeInput('customSplitsDateSelect', 'Date Range', 
                                             min(dataDM$Date), max(dataDM$Date), min(dataDM$Date), max(dataDM$Date))),
                    column(4, sliderInput('customSplitsHourSelect', 'Hours (Military Time)', 
                                          as.numeric(hourChoices[1]), as.numeric(hourChoices[24]), 
                                          c(as.numeric(hourChoices[1]), as.numeric(hourChoices[24]))))
           ),
           fluidRow(column(4, sliderInput('customSplitsDanceabilityFilter', 'Danceability', 0, 1, c(0, 1))),
                    column(4, sliderInput('customSplitsEnergyFilter', 'Energy', 0, 1, c(0, 1))),
                    column(4, sliderInput('customSplitsLoudnessFilter', 'Loudness', -50, 2, c(-50, 2)))
           ),
           fluidRow(column(4, sliderInput('customSplitsSpeechinessFilter', 'Speechiness', 0, 1, c(0, 1))),
                    column(4, sliderInput('customSplitsAcousticnessFilter', 'Acousticness', 0, 1, c(0, 1))),
                    column(4, sliderInput('customSplitsInstrumentalnessFilter', 'Instrumentalness', 0, 1, c(0, 1)))
           ),
           fluidRow(column(4, sliderInput('customSplitsLivenessFilter', 'Liveness', 0, 1, c(0, 1))),
                    column(4, sliderInput('customSplitsValenceFilter', 'Valence', 0, 1, c(0, 1))),
                    column(4, sliderInput('customSplitsTempoFilter', 'Tempo', 0, 225, c(0, 225)))
           ),
           fluidRow(column(12, dataTableOutput('customSplitsTable')))
           
  ),
  
  # Comparisons panel
  tabPanel('Comparisons',
           column(6, selectInput('comparisonsPersonOneSelect', 'Person One', personChoices, selected = 'DM')),
           column(6, selectInput('comparisonsPersonTwoSelect', 'Person Two', choices = NULL)),
           column(12, htmlOutput('comparisonsPlaylist')),
           dataTableOutput('comparisonsSongs'),
           dataTableOutput('comparisonsArtists'),
           dataTableOutput('comparisonsAlbums')
  ),
  
  # Recommendations panel
  tabPanel('Recommendations',
           fluidRow(column(12, selectInput('recommendationsPersonSelect', 'Person', personChoices, selected = 'DM'))),
           fluidRow(htmlOutput('recommendationsBasicPlaylist')),
           fluidRow(htmlOutput('recommendationsUnpopularPlaylist')),
           fluidRow(htmlOutput('recommendationsRotationPlaylist')),
           fluidRow(htmlOutput('recommendationsLongPlaylist')),
           fluidRow(htmlOutput('recommendationsEDMPlaylist')),
           fluidRow(htmlOutput('recommendationsIndiePlaylist')),
           fluidRow(htmlOutput('recommendationsRapPlaylist')),
           fluidRow(htmlOutput('recommendationsAngryPlaylist')),
           fluidRow(htmlOutput('recommendationsHappyPlaylist')),
           fluidRow(htmlOutput('recommendationsPeacefulPlaylist')),
           fluidRow(htmlOutput('recommendationsDepressingPlaylist'))
  )
    
)

server <- function(input, output, session) {
  
  # Generating tables for Home panel
  output$totalsTopSongs <- renderDataTable({
    eval(parse(text = paste0('topSongs', input$totalsTopSongsPersonSelect))) -> data
    data %>%
      select(-ID) -> data
  })
  output$totalsTopArtists <- renderDataTable({
    eval(parse(text = paste0('topArtists', input$totalsTopArtistsPersonSelect))) -> data
    data %>%
      select(-c(`Plays Per Song`, Songs)) -> data
  })
  output$totalsTopArtistRates <- renderDataTable({
    eval(parse(text = paste0('topArtistRates', input$totalsTopArtistsPersonSelect))) -> data
  })
  output$totalsTopAlbums <- renderDataTable({
    eval(parse(text = paste0('topAlbums', input$totalsTopAlbumsPersonSelect))) -> data
    data %>%
      select(-c(`Plays Per Song`, Songs)) -> data
  })
  output$totalsTopAlbumRates <- renderDataTable({
    eval(parse(text = paste0('topAlbumRates', input$totalsTopAlbumsPersonSelect))) -> data
  })
  output$totalsTopDays <- renderDataTable({
    eval(parse(text = paste0('topDays', input$totalsTopDaysPersonSelect))) -> data
  })
  
  # Split Playlists
  lapply(personChoices, function(i) {
    lapply(c('Year', 'Season', 'Month', 'Monthyear', 'Schoolperiod', 'Hour'), function(j) {
      output[[paste0('splits', j, 'Playlist', i)]] <- renderUI({
        val <- ''
        if (j == 'Monthyear') {
          val <- paste0(input[['splitsMonthyearySelect']], '-', input[['splitsMonthyearmSelect']])
        } else {
          val <- input[[paste0('splits', j, 'Select')]]
        }
        tags$iframe(
          style="border-radius:12px",
          src=paste0("https://open.spotify.com/embed/playlist/", 
                     playlists[playlists$Person == i & 
                                 playlists$Split == str_to_lower(j) &
                                 playlists$Val == val,]$ID[1], 
                     "?utm_source=generator"),
          width="100%",
          height="352",
          frameBorder="0",
          allowfullscreen="",
          allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
          loading="lazy"
        ) %>%
          return()
      })
    })
  })
  # Split Artists
  lapply(personChoices, function(i) {
    lapply(c('Year', 'Season', 'Month', 'Monthyear', 'Schoolperiod', 'Hour'), function(j) {
      lapply(1:3, function(k) {
        output[[paste0('splits', j, 'Artist', i, k)]] <- renderUI({
          val <- ''
          if (j == 'Monthyear') {
            val <- paste0(input[['splitsMonthyearySelect']], '-', input[['splitsMonthyearmSelect']])
          } else {
            val <- input[[paste0('splits', j, 'Select')]]
          } 
          tags$iframe(
            src=paste0("https://open.spotify.com/embed/artist/", 
                       art[art$From == paste0(str_to_lower(j), 's') & art$Type == 'artist' & 
                             art$Val == val & art$Person == i & art$Ind == k,'Id'], 
                       "?utm_source=generator"),
            width="100%",
            height="352",
            frameBorder="0",
            allowfullscreen="",
            allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
            loading="lazy") %>%
            return()
        })
      })
    })
  })
  
  # Generating tables by year
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('years', i))) -> data
    types <- c('Song', 'Artist', 'Album')
    lapply(1:3, function(j) {
      output[[paste0('splitsYear', types[j], i)]] <- renderDataTable({
        data[[j]][[as.numeric(input$splitsYearSelect) - 2015]] %>%
          data.frame() %>%
          select(types[j], Plays) %>%
          return()
      })
    })
  })
  
  # Generating tables by season
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('seasons', i))) -> data
    types <- c('Song', 'Artist', 'Album')
    lapply(1:3, function(j) {
      output[[paste0('splitsSeason', types[j], i)]] <- renderDataTable({
        data[[j]][[which(seasonList == input$splitsSeasonSelect)[1]]] %>%
          data.frame() %>%
          select(types[j], Plays) %>%
          return()
      })
    })
  })
  
  # Generating tables by month
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('months', i))) -> data
    types <- c('Song', 'Artist', 'Album')
    lapply(1:3, function(j) {
      output[[paste0('splitsMonth', types[j], i)]] <- renderDataTable({
        data[[j]][[as.numeric(input$splitsMonthSelect)]] %>%
          data.frame() %>%
          select(types[j], Plays) %>%
          return()
      })
    })
  })
  
  # Updating month/year options based on limitations in each user's data
  observe({
    
    if (input$splitsMonthyearySelect == 2016) {
      updateSelectInput(session, 'splitsMonthyearmSelect',
                        choices = switch(input$splitsMonthyear,
                          'Aalaynah' = monthChoices[8:12],
                          'Aditya' = monthChoices[4:5],
                          'Daniel' = monthChoices
                        ))
    } else if (input$splitsMonthyearySelect == 2017) {
      updateSelectInput(session, 'splitsMonthyearmSelect',
                        choices = switch(input$splitsMonthyear,
                                         'Aalaynah' = monthChoices,
                                         'Aditya' = monthChoices[3:12],
                                         'Daniel' = monthChoices
                        ))
    } else {
      updateSelectInput(session, 'splitsMonthyearmSelect',
                        choices = monthChoices)
    }
    
  })
  # Generating tables by month & year
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('monthyears', i))) -> data
    types <- c('Song', 'Artist', 'Album')
    lapply(1:3, function(j) {
      output[[paste0('splitsMonthyear', types[j], i)]] <- renderDataTable({
        data[[j]][[which(monthyearList == paste0(input$splitsMonthyearySelect, '-', input$splitsMonthyearmSelect))[1]]] %>%
          data.frame() %>%
          select(types[j], Plays) %>%
          return()
      })
    })
  })
  
  # Updating school period options based on limitations in each user's data
  observe({
    updateSelectInput(session, 'splitsSchoolperiodSelect',
                      choices = switch(input$splitsSchoolperiod,
                                       'Aalaynah' = schoolperiodList[2:14],
                                       'Aditya' = schoolperiodList[3:14],
                                       'Daniel' = schoolperiodList
                      ))
  })
  # Generating tables by school period
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('schoolperiods', i))) -> data
    types <- c('Song', 'Artist', 'Album')
    lapply(1:3, function(j) {
      output[[paste0('splitsSchoolperiod', types[j], i)]] <- renderDataTable({
        data[[j]][[which(schoolperiodList == input$splitsSchoolperiodSelect)[1]]] %>%
          data.frame() %>%
          select(types[j], Plays) %>%
          return()
      })
    })
  })
  
  # Generating tables by hour
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('hours', i))) -> data
    types <- c('Song', 'Artist', 'Album')
    lapply(1:3, function(j) {
      output[[paste0('splitsHour', types[j], i)]] <- renderDataTable({
        data[[j]][[as.numeric(input$splitsHourSelect) + 1]] %>%
          data.frame() %>%
          select(types[j], Plays) %>%
          return()
      })
    })
  })
  
  # Updating Song Breakdown's artist selection options based on selected user
  observeEvent(input$songBreakdownPersonSelect, {
    if (input$songBreakdownPersonSelect == 'AN') {
      topSongsAN %>%
        filter(Plays > 25) -> df
      updateSelectInput(session, 'songBreakdownArtistSelect', choices = sort(unique(df$Artist)))
    } else if (input$songBreakdownPersonSelect == 'AK') {
      topSongsAK %>%
        filter(Plays > 25) -> df
      updateSelectInput(session, 'songBreakdownArtistSelect', choices = sort(unique(df$Artist)))
    } else if (input$songBreakdownPersonSelect == 'DM') {
      topSongsDM %>%
        filter(Plays > 25) -> df
      updateSelectInput(session, 'songBreakdownArtistSelect', choices = sort(unique(df$Artist)))
    }
  })
  # Updating Song Breakdown's song selection options based on selected artist
  observeEvent(input$songBreakdownArtistSelect, {
    if (input$songBreakdownPersonSelect == 'AN') {
      topSongsAN %>%
        filter(Plays > 25 & Artist == input$songBreakdownArtistSelect) -> df
      updateSelectInput(session, 'songBreakdownSongSelect', choices = sort(unique(df$Song)))
    } else if (input$songBreakdownPersonSelect == 'AK') {
      topSongsAK %>%
        filter(Plays > 25 & Artist == input$songBreakdownArtistSelect) -> df
      updateSelectInput(session, 'songBreakdownSongSelect', choices = sort(unique(df$Song)))
    } else if (input$songBreakdownPersonSelect == 'DM') {
      topSongsDM %>%
        filter(Plays > 25 & Artist == input$songBreakdownArtistSelect) -> df
      updateSelectInput(session, 'songBreakdownSongSelect', choices = sort(unique(df$Song)))
    }
  })
  # Generating html and plots for song breakdown panel
  output$songFirstListenText <- renderUI({
    
    df <- switch(input$songBreakdownPersonSelect,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    df %>%
      filter(Artist == input$songBreakdownArtistSelect & Song == input$songBreakdownSongSelect) %>%
      arrange(Date) %>%
      head(1) -> df
    
    p(paste0('You listened to ', input$songBreakdownSongSelect, ' for the first time on ', 
             monthList[monthList$num == df$Month[1],]$name[1], ' ', as.numeric(str_sub(df$Day, 4, 5)), ', ', str_sub(df$MonthYear[1], 1, 4)), 
      align = 'center', style = "font-size: 24pt;") %>%
      return()
    
  })
  output$songTimeBarGraph <- renderImage({
    
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, songTimeBarGraph(input$songBreakdownArtistSelect, 
                                             input$songBreakdownSongSelect, 
                                             input$songBreakdownPersonSelect),
           width = 1300, height = 1200, units = 'px')
    list(src = outfile,
         height = '100%',
         style="display: block; margin-left: auto; margin-right: auto;") %>%
      return()
    
  }, deleteFile = T)
  output$songSpotifyLink <- renderUI({
    
    switch (input$songBreakdownPersonSelect,
      'DM' = topSongsDM,
      'AN' = topSongsAN,
      'AK' = topSongsAK
    ) -> df
    
    tags$iframe(
      src=paste0("https://open.spotify.com/embed/track/", df[df$Artist == input$songBreakdownArtistSelect &
                                                                df$Song == input$songBreakdownSongSelect, ]$ID[1], "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy",
      style="display: block; margin-left: auto; margin-right: auto;"
    ) %>%
      return()
    
  }) 
  output$songAudioFeatSpiderChart <- renderImage({
    
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, songAudioFeatSpiderChart(input$songBreakdownArtistSelect, 
                                             input$songBreakdownSongSelect, 
                                             input$songBreakdownPersonSelect),
           width = 1300, height = 1200, units = 'px')
    list(src = outfile,
         height = '100%',
         style="display: block; margin-left: auto; margin-right: auto;") %>%
      return()
    
  }, deleteFile = T)
  output$songMonthyearText <- renderUI({
    
    df <- switch(input$songBreakdownPersonSelect,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    df %>%
      filter(Artist == input$songBreakdownArtistSelect & Song == input$songBreakdownSongSelect) %>%
      group_by(MonthYear) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      data.frame() %>%
      head(1) -> df
    
    p(paste('You listened to', input$songBreakdownSongSelect, 'the most in', 
            monthList[monthList$num == str_sub(df$MonthYear[1], 6, 7),]$name[1], str_sub(df$MonthYear[1], 1, 4)), 
      align = 'center', style = "font-size: 24pt;") %>%
      return()
    
  })
  output$songYearlyChart <- renderImage({
    
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, songYearlyChart(input$songBreakdownArtistSelect, 
                                             input$songBreakdownSongSelect, 
                                             input$songBreakdownPersonSelect),
           width = 2000, height = 1200, units = 'px')
    list(src = outfile,
         height = '100%',
         style="display: block; margin-left: auto; margin-right: auto;") %>%
      return()
    
  }, deleteFile = T)
  output$songCumChart <- renderImage({
    
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, songCumChart(input$songBreakdownArtistSelect, 
                                    input$songBreakdownSongSelect, 
                                    input$songBreakdownPersonSelect),
           width = 2000, height = 1200, units = 'px')
    list(src = outfile,
         height = '100%',
         style="display: block; margin-left: auto; margin-right: auto;") %>%
      return()
    
  }, deleteFile = T)
  
  # Updating Artist Breakdown's artist selection options based on selected user
  observeEvent(input$artistBreakdownPersonSelect, {
    if (input$artistBreakdownPersonSelect == 'AN') {
      topArtistsAN %>%
        filter(Plays >= 200) -> df
      updateSelectInput(session, 'artistBreakdownArtistSelect', choices = sort(unique(df$Artist)))
    } else if (input$artistBreakdownPersonSelect == 'AK') {
      topArtistsAK %>%
        filter(Plays >= 200) -> df
      updateSelectInput(session, 'artistBreakdownArtistSelect', choices = sort(unique(df$Artist)))
    } else if (input$artistBreakdownPersonSelect == 'DM') {
      topArtistsDM %>%
        filter(Plays >= 200) -> df
      updateSelectInput(session, 'artistBreakdownArtistSelect', choices = sort(unique(df$Artist)))
    }
  })
  # Generating html and plots for artist breakdown panel
  output$artistFirstListenText <- renderUI({
    
    df <- switch(input$artistBreakdownPersonSelect,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    df %>%
      filter(Artist == input$artistBreakdownArtistSelect) %>%
      arrange(Date) %>%
      head(1) -> df
    
    p(paste0('You listened to ', input$artistBreakdownArtistSelect, ' for the first time on ', 
             monthList[monthList$num == df$Month[1],]$name[1], ' ', as.numeric(str_sub(df$Day, 4, 5)), ', ', str_sub(df$MonthYear[1], 1, 4)), 
      align = 'center', style = "font-size: 24pt;") %>%
      return()
    
  })
  output$artistTimeBarGraph <- renderImage({
    
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, artistTimeBarGraph(input$artistBreakdownArtistSelect, 
                                     input$artistBreakdownPersonSelect),
           width = 1300, height = 1200, units = 'px')
    list(src = outfile,
         height = '100%',
         style="display: block; margin-left: auto; margin-right: auto;") %>%
      return()
    
  }, deleteFile = T)
  output$artistSpotifyLink <- renderUI({
    
    tags$iframe(
      src=paste0("https://open.spotify.com/embed/artist/", getArtistID(input$artistBreakdownArtistSelect), "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy",
      style="display: block; margin-left: auto; margin-right: auto;"
    ) %>%
      return()
    
  })
  output$artistCumChart <- renderImage({
    
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, artistCumChart(input$artistBreakdownArtistSelect, 
                                   input$artistBreakdownPersonSelect),
           width = 2000, height = 1200, units = 'px')
    list(src = outfile,
         height = '100%',
         style="display: block; margin-left: auto; margin-right: auto;") %>%
      return()
    
  }, deleteFile = T)
  output$artistMonthyearText <- renderUI({
    
    df <- switch(input$artistBreakdownPersonSelect,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    df %>%
      filter(Artist == input$artistBreakdownArtistSelect) %>%
      group_by(MonthYear) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      data.frame() %>%
      head(1) -> df
    
    p(paste('You listened to', input$artistBreakdownArtistSelect, 'the most in', 
            monthList[monthList$num == str_sub(df$MonthYear[1], 6, 7),]$name[1], str_sub(df$MonthYear[1], 1, 4)), 
      align = 'center', style = "font-size: 24pt;") %>%
      return()
    
  })
  output$artistTopSongsChart <- renderImage({
    
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, artistTopSongsChart(input$artistBreakdownArtistSelect, 
                                        input$artistBreakdownPersonSelect),
           width = 3500, height = 1000, units = 'px')
    list(src = outfile,
         height = '100%',
         style="display: block; margin-left: auto; margin-right: auto;") %>%
      return()
    
  }, deleteFile = T)
  output$artistTopSongsTable <- renderDataTable({
    
    df <- switch(input$artistBreakdownPersonSelect,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    df %>%
      filter(Artist == input$artistBreakdownArtistSelect) %>%
      group_by(Song) %>%
      summarise(
        Plays = n(),
        `First Listen` = min(Date)
      ) %>%
      arrange(desc(Plays)) %>%
      return()
    
  })
  output$artistStackChart <- renderImage({
    
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, artistStackChart(input$artistBreakdownArtistSelect, 
                                      input$artistBreakdownPersonSelect),
           width = 3500, height = 1000, units = 'px')
    list(src = outfile,
         height = '100%',
         style="display: block; margin-left: auto; margin-right: auto;") %>%
      return()
    
  }, deleteFile = T)
  output$artistTopAlbumsTable <- renderDataTable({
    
    df <- switch(input$artistBreakdownPersonSelect,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    df %>%
      filter(Artist == input$artistBreakdownArtistSelect) %>%
      group_by(Album) %>%
      summarise(
        Plays = n(),
        `First Listen` = min(Date)
      ) %>%
      filter(Plays > 25) %>%
      arrange(desc(Plays)) %>%
      return()
    
  })
  
  # Updating Album Breakdown's artist selection options based on selected user
  observeEvent(input$albumBreakdownPersonSelect, {
    if (input$albumBreakdownPersonSelect == 'AN') {
      topAlbumsAN %>%
        filter(Plays > 100) -> df
      updateSelectInput(session, 'albumBreakdownArtistSelect', choices = sort(unique(df$Artist)))
    } else if (input$albumBreakdownPersonSelect == 'AK') {
      topAlbumsAK %>%
        filter(Plays > 100) -> df
      updateSelectInput(session, 'albumBreakdownArtistSelect', choices = sort(unique(df$Artist)))
    } else if (input$albumBreakdownPersonSelect == 'DM') {
      topAlbumsDM %>%
        filter(Plays > 100) -> df
      updateSelectInput(session, 'albumBreakdownArtistSelect', choices = sort(unique(df$Artist)))
    }
  })
  # Updating Album Breakdown's album selection options based on selected artist
  observeEvent(input$albumBreakdownArtistSelect, {
    if (input$albumBreakdownPersonSelect == 'AN') {
      topAlbumsAN %>%
        filter(Plays > 100 & Artist == input$albumBreakdownArtistSelect) -> df
      updateSelectInput(session, 'albumBreakdownAlbumSelect', choices = sort(unique(df$Album)))
    } else if (input$albumBreakdownPersonSelect == 'AK') {
      topAlbumsAK %>%
        filter(Plays > 100 & Artist == input$albumBreakdownArtistSelect) -> df
      updateSelectInput(session, 'albumBreakdownAlbumSelect', choices = sort(unique(df$Album)))
    } else if (input$albumBreakdownPersonSelect == 'DM') {
      topAlbumsDM %>%
        filter(Plays > 100 & Artist == input$albumBreakdownArtistSelect) -> df
      updateSelectInput(session, 'albumBreakdownAlbumSelect', choices = sort(unique(df$Album)))
    }
  })
  # Generating html and plots for album breakdown panel
  output$albumFirstListenText <- renderUI({
    
    df <- switch(input$albumBreakdownPersonSelect,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    df %>%
      filter(Artist == input$albumBreakdownArtistSelect & Album == input$albumBreakdownAlbumSelect) %>%
      arrange(Date) %>%
      head(1) -> df
    
    p(paste0('You listened to ', input$albumBreakdownAlbumSelect, ' for the first time on ', 
             monthList[monthList$num == df$Month[1],]$name[1], ' ', as.numeric(str_sub(df$Day, 4, 5)), ', ', str_sub(df$MonthYear[1], 1, 4)), 
      align = 'center', style = "font-size: 24pt;") %>%
      return()
    
  })
  output$albumTimeBarGraph <- renderImage({
    
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, albumTimeBarGraph(input$albumBreakdownArtistSelect,
                                      input$albumBreakdownAlbumSelect,
                                       input$albumBreakdownPersonSelect),
           width = 1300, height = 1200, units = 'px')
    list(src = outfile,
         height = '100%',
         style="display: block; margin-left: auto; margin-right: auto;") %>%
      return()
    
  }, deleteFile = T)
  output$albumSpotifyLink <- renderUI({
    
    tags$iframe(
      src=paste0("https://open.spotify.com/embed/album/", getAlbumID(paste(
        input$albumBreakdownArtistSelect, '~', input$albumBreakdownAlbumSelect
      )), "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy",
      style="display: block; margin-left: auto; margin-right: auto;"
    ) %>%
      return()
    
  })
  output$albumCumChart <- renderImage({
    
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, albumCumChart(input$albumBreakdownArtistSelect, 
                                 input$albumBreakdownAlbumSelect, 
                                 input$albumBreakdownPersonSelect),
           width = 2000, height = 1200, units = 'px')
    list(src = outfile,
         height = '100%',
         style="display: block; margin-left: auto; margin-right: auto;") %>%
      return()
    
  }, deleteFile = T)
  output$albumMonthyearText <- renderUI({
    
    df <- switch(input$albumBreakdownPersonSelect,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    df %>%
      filter(Artist == input$albumBreakdownArtistSelect & Album == input$albumBreakdownAlbumSelect) %>%
      group_by(MonthYear) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      data.frame() %>%
      head(1) -> df
    
    p(paste('You listened to', input$albumBreakdownAlbumSelect, 'the most in', 
            monthList[monthList$num == str_sub(df$MonthYear[1], 6, 7),]$name[1], str_sub(df$MonthYear[1], 1, 4)), 
      align = 'center', style = "font-size: 24pt;") %>%
      return()
    
  })
  output$albumTopSongsChart <- renderImage({
    
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, albumTopSongsChart(input$albumBreakdownArtistSelect,
                                       input$albumBreakdownAlbumSelect,
                                        input$albumBreakdownPersonSelect),
           width = 3500, height = 1000, units = 'px')
    list(src = outfile,
         height = '100%',
         style="display: block; margin-left: auto; margin-right: auto;") %>%
      return()
    
  }, deleteFile = T)
  output$albumTopSongsTable <- renderDataTable({
    
    df <- switch(input$albumBreakdownPersonSelect,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    df %>%
      filter(Artist == input$albumBreakdownArtistSelect & Album == input$albumBreakdownAlbumSelect) %>%
      group_by(Song) %>%
      summarise(
        Plays = n(),
        `First Listen` = min(Date)
      ) %>%
      arrange(desc(Plays)) %>%
      return()
    
  })
  
  # Generating comparisons graphics and tables based on selected users
  observeEvent(input$comparisonsPersonOneSelect, {
    updateSelectInput(session, 'comparisonsPersonTwoSelect',
                      choices = personChoices[-c(which(personChoices == input$comparisonsPersonOneSelect)[1])])
  })
  output$comparisonsPlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == paste0(input$comparisonsPersonOneSelect, input$comparisonsPersonTwoSelect),]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  output$comparisonsSongs <- renderDataTable({
    getSharedSongs(
      eval(parse(text = paste0('topSongs', input$comparisonsPersonOneSelect))),
      eval(parse(text = paste0('topSongs', input$comparisonsPersonTwoSelect)))
    ) -> data
    names(data)[3] <- paste(names(personChoices)[which(personChoices == input$comparisonsPersonOneSelect)[1]], 'Plays')
    names(data)[4] <- paste(names(personChoices)[which(personChoices == input$comparisonsPersonTwoSelect)[1]], 'Plays')
    return(data)
  })
  output$comparisonsArtists <- renderDataTable({
    getSharedArtists(
      eval(parse(text = paste0('topArtists', input$comparisonsPersonOneSelect))),
      eval(parse(text = paste0('topArtists', input$comparisonsPersonTwoSelect)))
    ) -> data
    names(data)[2] <- paste(names(personChoices)[which(personChoices == input$comparisonsPersonOneSelect)[1]], 'Plays')
    names(data)[3] <- paste(names(personChoices)[which(personChoices == input$comparisonsPersonTwoSelect)[1]], 'Plays')
    return(data)
  })
  output$comparisonsAlbums <- renderDataTable({
    getSharedAlbums(
      eval(parse(text = paste0('topAlbums', input$comparisonsPersonOneSelect))),
      eval(parse(text = paste0('topAlbums', input$comparisonsPersonTwoSelect)))
    ) -> data
    names(data)[2] <- paste(names(personChoices)[which(personChoices == input$comparisonsPersonOneSelect)[1]], 'Plays')
    names(data)[3] <- paste(names(personChoices)[which(personChoices == input$comparisonsPersonTwoSelect)[1]], 'Plays')
    return(data)
  })
  
  # Generating custom splits table
  output$customSplitsTable <- renderDataTable({
    
    df <- switch (input$customSplitsPersonSelect,
      'DM' = dataDM,
      'AN' = dataAN,
      'AK' = dataAK
    )
    
    af <- switch (input$customSplitsPersonSelect,
                  'DM' = audioFeatsDM,
                  'AN' = audioFeatsAN,
                  'AK' = audioFeatsAK
    )
    
    df %>%
      filter(Date >= input$customSplitsDateSelect[1] & Date <= input$customSplitsDateSelect[2]) %>%
      filter(as.numeric(Hour) >= input$customSplitsHourSelect[1] & as.numeric(Hour) <= input$customSplitsHourSelect[2]) -> df
    
    df %>%
      group_by(Artist, Song) %>%
      summarise(Plays = n(), ID = getmode(ID)) %>%
      as.data.frame() %>%
      arrange(desc(Plays)) -> df
    
    if (input$customSplitsPersonSelect == 'DM') {
      df %>%
        mutate(Name = paste(Artist, '~', Song)) %>%
        left_join(select(idList, Name, id), by = 'Name', multiple = 'first') %>%
        mutate(id = as.character(id), ID = ifelse(is.na(id), ID, id)) %>%
        select(Song, Artist, Plays, ID) -> df
    }
    
    df %>%
      inner_join(af, by = c('ID' = 'id')) %>%
      filter(danceability >= input$customSplitsDanceabilityFilter[1] & danceability <= input$customSplitsDanceabilityFilter[2]) %>%
      filter(energy >= input$customSplitsEnergyFilter[1] & energy <= input$customSplitsEnergyFilter[2]) %>%
      filter(loudness >= input$customSplitsLoudnessFilter[1] & loudness <= input$customSplitsLoudnessFilter[2]) %>%
      filter(speechiness >= input$customSplitsSpeechinessFilter[1] & speechiness <= input$customSplitsSpeechinessFilter[2]) %>%
      filter(acousticness >= input$customSplitsAcousticnessFilter[1] & acousticness <= input$customSplitsAcousticnessFilter[2]) %>%
      filter(instrumentalness >= input$customSplitsInstrumentalnessFilter[1] & instrumentalness <= input$customSplitsInstrumentalnessFilter[2]) %>%
      filter(liveness >= input$customSplitsLivenessFilter[1] & liveness <= input$customSplitsLivenessFilter[2]) %>%
      filter(valence >= input$customSplitsValenceFilter[1] & valence <= input$customSplitsValenceFilter[2]) %>%
      filter(tempo >= input$customSplitsTempoFilter[1] & tempo <= input$customSplitsTempoFilter[2]) %>%
      select(Song, Artist, Plays) %>%
      arrange(desc(Plays)) %>%
      return()
    
  })
  
  # Loading playlist iframes based on selected user
  output$recommendationsBasicPlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == input$recommendationsPersonSelect & Type == 'Basic Recommendation',]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  output$recommendationsUnpopularPlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == input$recommendationsPersonSelect & Type == 'Unpopular Recommendation',]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  output$recommendationsRotationPlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == input$recommendationsPersonSelect & Type == 'Recent Rotation Recommendation',]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  output$recommendationsLongPlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == input$recommendationsPersonSelect & Type == 'Long Recommendation',]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  output$recommendationsEDMPlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == input$recommendationsPersonSelect & Type == 'EDM Recommendation',]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  output$recommendationsIndiePlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == input$recommendationsPersonSelect & Type == 'Indie Recommendation',]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  output$recommendationsRapPlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == input$recommendationsPersonSelect & Type == 'Rap Recommendation',]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  output$recommendationsAngryPlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == input$recommendationsPersonSelect & Type == 'Angry Recommendation',]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  output$recommendationsHappyPlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == input$recommendationsPersonSelect & Type == 'Happy Recommendation',]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  output$recommendationsPeacefulPlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == input$recommendationsPersonSelect & Type == 'Peaceful Recommendation',]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  output$recommendationsDepressingPlaylist <- renderUI({
    tags$iframe(
      style="border-radius:12px",
      src=paste0("https://open.spotify.com/embed/playlist/", 
                 playlists[playlists$Person == input$recommendationsPersonSelect & Type == 'Depressing Recommendation',]$ID[1], 
                 "?utm_source=generator"),
      width="100%",
      height="352",
      frameBorder="0",
      allowfullscreen="",
      allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
      loading="lazy"
    ) %>%
      return()
  })
  
}

shinyApp(ui = ui, server = server)