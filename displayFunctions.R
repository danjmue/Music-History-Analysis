# Displays home page
displayHome <- function(name) {
  tabPanel(name,
           column(12, img(src = paste0('charts/homeSongsPerDayChart', personChoices[name], '.png'), 
                          style = "display: block; margin-left: auto; margin-right: auto; max-width: 100%; width: 100%; height: auto")),
           column(12, img(src = paste0('charts/homeWeeklySongChart', personChoices[name], '.png'), 
                          style = "display: block; margin-left: auto; margin-right: auto; max-width: 100%; width: 100%; height: auto")),
           column(12, img(src = paste0('charts/homeSongsEmotionalChart', personChoices[name], '.png'), 
                          style = "display: block; margin-left: auto; margin-right: auto; max-width: 100%; width: 100%; height: auto")),
           column(12, img(src = paste0('charts/homeCumTopSongsChart', personChoices[name], '.png'), 
                          style = "display: block; margin-left: auto; margin-right: auto; max-width: 100%; width: 100%; height: auto")),
           column(12, img(src = paste0('charts/homeCumTopArtistsChart', personChoices[name], '.png'), 
                          style = "display: block; margin-left: auto; margin-right: auto; max-width: 100%; width: 100%; height: auto")),
           column(12, img(src = paste0('charts/homeCumTopAlbumsChart', personChoices[name], '.png'), 
                          style = "display: block; margin-left: auto; margin-right: auto; max-width: 100%; width: 100%; height: auto")),
           column(12, p('These artists grew on you a lot over the past few years:', align = 'center', style = "font-size: 24pt;")),
           displayBiggestRisersEmbed(personChoices[name])
  ) %>%
    return()
}

# Displays biggest risers
displayBiggestRisersEmbed <- function(name) {
  eval(parse(text = paste0('biggestRisers', name))) -> df
  lapply(1:2, function(i) {
    column(12,
           tags$iframe(
             src=paste0("https://open.spotify.com/embed/artist/", getArtistID(df$Artist[i * 4 - 3]), "?utm_source=generator"),
             width="24.75%",
             height="352",
             frameBorder="0",
             allowfullscreen="",
             allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
             loading="lazy"),
           tags$iframe(
             src=paste0("https://open.spotify.com/embed/artist/", getArtistID(df$Artist[i * 4 - 2]), "?utm_source=generator"),
             width="24.75%",
             height="352",
             frameBorder="0",
             allowfullscreen="",
             allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
             loading="lazy"),
           tags$iframe(
             src=paste0("https://open.spotify.com/embed/artist/", getArtistID(df$Artist[i * 4 - 1]), "?utm_source=generator"),
             width="24.75%",
             height="352",
             frameBorder="0",
             allowfullscreen="",
             allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
             loading="lazy"),
           tags$iframe(
             src=paste0("https://open.spotify.com/embed/artist/", getArtistID(df$Artist[i * 4]), "?utm_source=generator"),
             width="24.75%",
             height="352",
             frameBorder="0",
             allowfullscreen="",
             allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
             loading="lazy")
    )
  })
}

# Displays top songs playlists
displayTopSongPlaylists <- function() {
  
  lapply(personChoices, function(i) {
    conditionalPanel(condition = paste0("input.totalsTopSongsPersonSelect == '", i, "'"),
                     column(12,
                            tags$iframe(
                              style="border-radius:12px",
                              src=paste0("https://open.spotify.com/embed/playlist/", 
                                         playlists[playlists$Person == i & playlists$Type == 'Top Songs',]$ID[1], 
                                         "?utm_source=generator"),
                              width="100%",
                              height="352",
                              frameBorder="0",
                              allowfullscreen="",
                              allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
                              loading="lazy"
                            )
                            )
                     )
  })
  
}

# Displays emotional quadrant charts for each split
displaySplitEmotionalQuadrantChart <- function(split, name) {
  
  lapply(unlist(splits[split]), function(i) {
    conditionalPanel(condition = paste0(splitPrefix[split], " == '", i, "'"),
                     column(12,
                            img(src = paste0('charts/splits', str_to_title(split), 'EmotionalQuadrantChart', name, i, '.png'), 
                                style = "display: block; margin-left: auto; margin-right: auto; max-width: 100%; width: 100%; height: auto")
                     )
    )
    
  })
  
}

# Displays metric charts for each split
displayMetricCharts <- function(split, name) {
  
  lapply(unlist(splits[split]), function(i) {
    conditionalPanel(condition = paste0(splitPrefix[split], " == '", i, "'"),
                     lapply(c('Danceability', 'Energy', 'Liveness', 'Valence', 'Speechiness', 'Acousticness', 'Instrumentalness'), function(j) {
                       fluidRow(
                         column(3, p(paste0(j, ':')), 
                                style = "font-size: 24pt; font-family: questrial;", align = 'center'),
                         column(9,
                                img(src = paste0('charts/splits', str_to_title(split), j, 'Histogram', name, i, '.png'), 
                                    style = "display: block; margin-left: auto; margin-right: auto; max-width: 100%; width: 100%; height: auto")
                         )
                       )
                     })
    )
  })
  
}

# Displays album pictures for each split
displaySplitAlbums <- function(split, name) {
  
  lapply(X = unlist(splits[split]), FUN = function(i) {
    conditionalPanel(condition = paste0(splitPrefix[split], " == '", i, "'"),
                     img(
                       src = art[art$From == paste0(split, 's') & art$Type == 'album' & art$Val == i & art$Person == name & art$Ind == 1,'File'],
                       width = "33%",
                       height = 500,
                       style = "margin-left: auto; margin-right: auto;"
                     ),
                     img(
                       src = art[art$From == paste0(split, 's') & art$Type == 'album' & art$Val == i & art$Person == name & art$Ind == 2,'File'],
                       width = "33%",
                       height = 500,
                       style = "margin-left: auto; margin-right: auto;"
                     ),
                     img(
                       src = art[art$From == paste0(split, 's') & art$Type == 'album' & art$Val == i & art$Person == name & art$Ind == 3,'File'],
                       width = "33%",
                       height = 500,
                       style = "margin-left: auto; margin-right: auto;"
                     )
    )
  })
  
}

# Displays splits
displaySplit <- function(split, name) {
  cap_split <- str_to_title(split)
  tabPanel(name,
           htmlOutput(paste0('splits', cap_split, 'Playlist', personChoices[name])),
           displaySplitEmotionalQuadrantChart(split, personChoices[name]),
           displayMetricCharts(split, personChoices[name]),
           column(12, dataTableOutput(paste0('splits', cap_split, 'Song', personChoices[name]))),
           fluidRow(column(4, htmlOutput(paste0('splits', cap_split, 'Artist', personChoices[name], '1'))),
                    column(4, htmlOutput(paste0('splits', cap_split, 'Artist', personChoices[name], '2'))),
                    column(4, htmlOutput(paste0('splits', cap_split, 'Artist', personChoices[name], '3')))),
           column(12, dataTableOutput(paste0('splits', cap_split, 'Artist', personChoices[name]))),
           displaySplitAlbums(split, personChoices[name]),
           column(12, dataTableOutput(paste0('splits', cap_split, 'Album', personChoices[name])))
           ) %>%
    return()
}
