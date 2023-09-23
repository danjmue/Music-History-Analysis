library(tidyverse)
library(spotifyr)

updateAudioFeats <- F

# Loading in audio features for each user's listening history
fread('audioFeats//audioFeatsDM.csv') %>%
  select(-c(key, mode, type, uri, track_href, analysis_url, time_signature, value)) -> audioFeatsDM
fread('audioFeats//audioFeatsAN.csv') %>%
  select(-c(key, mode, type, uri, track_href, analysis_url, time_signature, value)) -> audioFeatsAN
fread('audioFeats//audioFeatsAK.csv') %>%
  select(-c(key, mode, type, uri, track_href, analysis_url, time_signature, value)) -> audioFeatsAK

idList %>%
  select(id, Name) -> idListDM
unique(dataAN[,c(1,3,4)]) %>%
  rename('id' = 'ID') %>%
  mutate(Name = paste(Artist, '~', Song)) %>%
  select(id, Name) -> idListAN
unique(dataAK[,c(1,3,4)]) %>%
  rename('id' = 'ID') %>%
  mutate(Name = paste(Artist, '~', Song)) %>%
  select(id, Name) -> idListAK

# Get audio features for each user
calcAudioFeats <- function(name) {
  
  # Creating csv's for each user's audio features
  if (updateAudioFeats) {
    
    eval(parse(text = paste0('audioFeats', name))) %>%
      inner_join(eval(parse(text = paste0('idList', name))), multiple = 'first') -> dfIDs
    
    eval(parse(text = paste0('years', name))) -> dfBig
    map_df(1:length(dfBig[[1]]), function(i) {
      dfBig[[1]][[i]] %>%
        select(-ID) %>%
        inner_join(dfIDs, by = c('Song' = 'Name')) %>%
        mutate(
          danceability = Plays * danceability,
          energy = Plays * energy,
          loudness = Plays * loudness,
          speechiness = Plays * speechiness,
          acousticness = Plays * acousticness,
          instrumentalness = Plays * instrumentalness,
          liveness = Plays * liveness,
          valence = Plays * valence,
          tempo = Plays * tempo,
          duration = Plays * duration_ms
        ) -> df
      sum(df$Plays) -> n
      data.frame(
        Year = df$Year[1],
        danceability = sum(df$danceability) / n,
        energy = sum(df$energy) / n,
        loudness = sum(df$loudness) / n,
        speechiness = sum(df$speechiness) / n,
        acousticness = sum(df$acousticness) / n,
        instrumentalness = sum(df$instrumentalness) / n,
        liveness = sum(df$liveness) / n,
        valence = sum(df$valence) / n,
        tempo = sum(df$tempo) / n,
        duration = ((sum(df$duration) / n) / 1000) / 60
      ) %>%
        return()
    }) %>%
      write.csv(paste0('audioFeats//yearsFeats', name, '.csv'), row.names = F)
    
    eval(parse(text = paste0('seasons', name))) -> dfBig
    map_df(1:length(dfBig[[1]]), function(i) {
      dfBig[[1]][[i]] %>%
        select(-ID) %>%
        inner_join(dfIDs, by = c('Song' = 'Name')) %>%
        mutate(
          danceability = Plays * danceability,
          energy = Plays * energy,
          loudness = Plays * loudness,
          speechiness = Plays * speechiness,
          acousticness = Plays * acousticness,
          instrumentalness = Plays * instrumentalness,
          liveness = Plays * liveness,
          valence = Plays * valence,
          tempo = Plays * tempo,
          duration = Plays * duration_ms
        ) -> df
      sum(df$Plays) -> n
      data.frame(
        Season = df$Season[1],
        danceability = sum(df$danceability) / n,
        energy = sum(df$energy) / n,
        loudness = sum(df$loudness) / n,
        speechiness = sum(df$speechiness) / n,
        acousticness = sum(df$acousticness) / n,
        instrumentalness = sum(df$instrumentalness) / n,
        liveness = sum(df$liveness) / n,
        valence = sum(df$valence) / n,
        tempo = sum(df$tempo) / n,
        duration = ((sum(df$duration) / n) / 1000) / 60
      ) %>%
        return()
    }) %>%
      write.csv(paste0('audioFeats//seasonsFeats', name, '.csv'), row.names = F)
    
    eval(parse(text = paste0('months', name))) -> dfBig
    map_df(1:length(dfBig[[1]]), function(i) {
      dfBig[[1]][[i]] %>%
        select(-ID) %>%
        inner_join(dfIDs, by = c('Song' = 'Name')) %>%
        mutate(
          danceability = Plays * danceability,
          energy = Plays * energy,
          loudness = Plays * loudness,
          speechiness = Plays * speechiness,
          acousticness = Plays * acousticness,
          instrumentalness = Plays * instrumentalness,
          liveness = Plays * liveness,
          valence = Plays * valence,
          tempo = Plays * tempo,
          duration = Plays * duration_ms
        ) -> df
      sum(df$Plays) -> n
      data.frame(
        Month = df$Month[1],
        danceability = sum(df$danceability) / n,
        energy = sum(df$energy) / n,
        loudness = sum(df$loudness) / n,
        speechiness = sum(df$speechiness) / n,
        acousticness = sum(df$acousticness) / n,
        instrumentalness = sum(df$instrumentalness) / n,
        liveness = sum(df$liveness) / n,
        valence = sum(df$valence) / n,
        tempo = sum(df$tempo) / n,
        duration = ((sum(df$duration) / n) / 1000) / 60
      ) %>%
        return()
    }) %>%
      write.csv(paste0('audioFeats//monthsFeats', name, '.csv'), row.names = F)
    
    eval(parse(text = paste0('monthyears', name))) -> dfBig
    map_df(1:length(dfBig[[1]]), function(i) {
      dfBig[[1]][[i]] %>%
        select(-ID) %>%
        inner_join(dfIDs, by = c('Song' = 'Name')) %>%
        mutate(
          danceability = Plays * danceability,
          energy = Plays * energy,
          loudness = Plays * loudness,
          speechiness = Plays * speechiness,
          acousticness = Plays * acousticness,
          instrumentalness = Plays * instrumentalness,
          liveness = Plays * liveness,
          valence = Plays * valence,
          tempo = Plays * tempo,
          duration = Plays * duration_ms
        ) -> df
      sum(df$Plays) -> n
      data.frame(
        MonthYear = df$MonthYear[1],
        danceability = sum(df$danceability) / n,
        energy = sum(df$energy) / n,
        loudness = sum(df$loudness) / n,
        speechiness = sum(df$speechiness) / n,
        acousticness = sum(df$acousticness) / n,
        instrumentalness = sum(df$instrumentalness) / n,
        liveness = sum(df$liveness) / n,
        valence = sum(df$valence) / n,
        tempo = sum(df$tempo) / n,
        duration = ((sum(df$duration) / n) / 1000) / 60
      ) %>%
        return()
    }) %>%
      write.csv(paste0('audioFeats//monthyearsFeats', name, '.csv'), row.names = F)
    
    eval(parse(text = paste0('schoolperiods', name))) -> dfBig
    map_df(1:length(dfBig[[1]]), function(i) {
      dfBig[[1]][[i]] %>%
        select(-ID) %>%
        inner_join(dfIDs, by = c('Song' = 'Name')) %>%
        mutate(
          danceability = Plays * danceability,
          energy = Plays * energy,
          loudness = Plays * loudness,
          speechiness = Plays * speechiness,
          acousticness = Plays * acousticness,
          instrumentalness = Plays * instrumentalness,
          liveness = Plays * liveness,
          valence = Plays * valence,
          tempo = Plays * tempo,
          duration = Plays * duration_ms
        ) -> df
      sum(df$Plays) -> n
      data.frame(
        SchoolPeriod = df$SchoolPeriod[1],
        danceability = sum(df$danceability) / n,
        energy = sum(df$energy) / n,
        loudness = sum(df$loudness) / n,
        speechiness = sum(df$speechiness) / n,
        acousticness = sum(df$acousticness) / n,
        instrumentalness = sum(df$instrumentalness) / n,
        liveness = sum(df$liveness) / n,
        valence = sum(df$valence) / n,
        tempo = sum(df$tempo) / n,
        duration = ((sum(df$duration) / n) / 1000) / 60
      ) %>%
        return()
    }) %>%
      write.csv(paste0('audioFeats//schoolperiodsFeats', name, '.csv'), row.names = F)
    
    eval(parse(text = paste0('hours', name))) -> dfBig
    map_df(1:length(dfBig[[1]]), function(i) {
      dfBig[[1]][[i]] %>%
        select(-ID) %>%
        inner_join(dfIDs, by = c('Song' = 'Name')) %>%
        mutate(
          danceability = Plays * danceability,
          energy = Plays * energy,
          loudness = Plays * loudness,
          speechiness = Plays * speechiness,
          acousticness = Plays * acousticness,
          instrumentalness = Plays * instrumentalness,
          liveness = Plays * liveness,
          valence = Plays * valence,
          tempo = Plays * tempo,
          duration = Plays * duration_ms
        ) -> df
      sum(df$Plays) -> n
      data.frame(
        Hour = df$Hour[1],
        danceability = sum(df$danceability) / n,
        energy = sum(df$energy) / n,
        loudness = sum(df$loudness) / n,
        speechiness = sum(df$speechiness) / n,
        acousticness = sum(df$acousticness) / n,
        instrumentalness = sum(df$instrumentalness) / n,
        liveness = sum(df$liveness) / n,
        valence = sum(df$valence) / n,
        tempo = sum(df$tempo) / n,
        duration = ((sum(df$duration) / n) / 1000) / 60
      ) %>%
        return()
    }) %>%
      write.csv(paste0('audioFeats//hoursFeats', name, '.csv'), row.names = F)

  }
  
  # loading in audio feats for specified user
  read.csv(paste0('audioFeats//yearsFeats', name, '.csv')) -> yearsFeats
  read.csv(paste0('audioFeats//seasonsFeats', name, '.csv')) -> seasonsFeats
  read.csv(paste0('audioFeats//monthsFeats', name, '.csv')) -> monthsFeats
  read.csv(paste0('audioFeats//monthyearsFeats', name, '.csv')) -> monthyearsFeats
  read.csv(paste0('audioFeats//schoolperiodsFeats', name, '.csv')) -> schoolperiodsFeats
  read.csv(paste0('audioFeats//hoursFeats', name, '.csv')) -> hoursFeats
  
  list(
    yearsFeats,
    seasonsFeats,
    monthsFeats,
    monthyearsFeats,
    schoolperiodsFeats,
    hoursFeats
  ) %>%
    return()
}

calcAudioFeats('DM') -> audioFeatBreakdownDM
calcAudioFeats('AN') -> audioFeatBreakdownAN
calcAudioFeats('AK') -> audioFeatBreakdownAK

# Function to create plots of each user's audio features for each split
getAudioFeatPlots <- function(name) {
  
  eval(parse(text = paste0('audioFeatBreakdown', name))) -> df
  
  metrics <- list('danceability', 'energy', 'liveness', 'valence', 'speechiness', 'acousticness', 'instrumentalness', 'tempo', 'loudness', 'duration')
  
  df[[1]] %>%
    pivot_longer(c(danceability:energy, speechiness:valence)) %>%
    ggplot(aes(Year, value, color = name, group = name)) +
    geom_line(linewidth = 2) +
    scale_color_manual(labels = c('Acousticness', 'Danceability', 'Energy', 'Instrumentalness', 'Liveness', 'Speechiness', 'Valence'), 
                       values = c('brown3', 'darkorange', 'darkgoldenrod1', 'chartreuse4', 'cyan3', 'blue3', 'darkorchid2'),
                       name = element_blank()) +
    theme(axis.text = element_text(size=12),
          axis.title = element_text(size=20,face="bold"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = 'azure2'),
          panel.grid.minor = element_line(colour = 'azure2')) -> yearFeatPlot
  
  df[[2]] %>%
    pivot_longer(c(danceability:energy, speechiness:valence)) %>%
    ggplot(aes(Season, value, color = name, group = name)) +
    geom_line(linewidth = 2) +
    scale_color_manual(labels = c('Acousticness', 'Danceability', 'Energy', 'Instrumentalness', 'Liveness', 'Speechiness', 'Valence'), 
                       values = c('brown3', 'darkorange', 'darkgoldenrod1', 'chartreuse4', 'cyan3', 'blue3', 'darkorchid2'),
                       name = element_blank()) +
    theme(axis.text = element_text(size=12),
          axis.title = element_text(size=20,face="bold"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = 'azure2'),
          panel.grid.minor = element_line(colour = 'azure2')) -> seasonFeatPlot
  
  df[[3]] %>%
    pivot_longer(c(danceability:energy, speechiness:valence)) %>%
    ggplot(aes(Month, value, color = name, group = name)) +
    geom_line(linewidth = 2) +
    scale_color_manual(labels = c('Acousticness', 'Danceability', 'Energy', 'Instrumentalness', 'Liveness', 'Speechiness', 'Valence'), 
                       values = c('brown3', 'darkorange', 'darkgoldenrod1', 'chartreuse4', 'cyan3', 'blue3', 'darkorchid2'),
                       name = element_blank()) +
    theme(axis.text = element_text(size=12),
          axis.title = element_text(size=20,face="bold"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = 'azure2'),
          panel.grid.minor = element_line(colour = 'azure2')) -> monthFeatPlot
  
  df[[4]] %>%
    pivot_longer(c(danceability:energy, speechiness:valence)) %>%
    ggplot(aes(MonthYear, value, color = name, group = name)) +
    geom_line(linewidth = 2) +
    scale_color_manual(labels = c('Acousticness', 'Danceability', 'Energy', 'Instrumentalness', 'Liveness', 'Speechiness', 'Valence'), 
                       values = c('brown3', 'darkorange', 'darkgoldenrod1', 'chartreuse4', 'cyan3', 'blue3', 'darkorchid2'),
                       name = element_blank()) +
    theme(axis.text = element_text(size=12),
          axis.title = element_text(size=20,face="bold"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = 'azure2'),
          panel.grid.minor = element_line(colour = 'azure2')) -> monthyearFeatPlot
  
  df[[5]] %>%
    pivot_longer(c(danceability:energy, speechiness:valence)) %>%
    ggplot(aes(SchoolPeriod, value, color = name, group = name)) +
    geom_line(linewidth = 2) +
    scale_color_manual(labels = c('Acousticness', 'Danceability', 'Energy', 'Instrumentalness', 'Liveness', 'Speechiness', 'Valence'), 
                       values = c('brown3', 'darkorange', 'darkgoldenrod1', 'chartreuse4', 'cyan3', 'blue3', 'darkorchid2'),
                       name = element_blank()) +
    theme(axis.text = element_text(size=12),
          axis.title = element_text(size=20,face="bold"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = 'azure2'),
          panel.grid.minor = element_line(colour = 'azure2')) -> schoolperiodFeatPlot
  
  df[[6]] %>%
    pivot_longer(c(danceability:energy, speechiness:valence)) %>%
    ggplot(aes(Hour, value, color = name, group = name)) +
    geom_line(linewidth = 2) +
    scale_color_manual(labels = c('Acousticness', 'Danceability', 'Energy', 'Instrumentalness', 'Liveness', 'Speechiness', 'Valence'), 
                       values = c('brown3', 'darkorange', 'darkgoldenrod1', 'chartreuse4', 'cyan3', 'blue3', 'darkorchid2'),
                       name = element_blank()) +
    theme(axis.text = element_text(size=12),
          axis.title = element_text(size=20,face="bold"),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = 'azure2'),
          panel.grid.minor = element_line(colour = 'azure2')) -> hourFeatPlot
  
  list(
    yearFeatPlot,
    seasonFeatPlot,
    monthFeatPlot,
    monthyearFeatPlot,
    schoolperiodFeatPlot,
    hourFeatPlot
  ) %>%
    return()
  
}

getAudioFeatPlots('DM') -> audioFeatPlotsDM
getAudioFeatPlots('AN') -> audioFeatPlotsAN
getAudioFeatPlots('AK') -> audioFeatPlotsAK
