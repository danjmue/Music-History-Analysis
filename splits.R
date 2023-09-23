library(tidyverse)
library(spotifyr)
library(data.table)

updateSplits <- F
loadSplits <- F

# Generating dataframes of top songs, artists, and albums for each split and writing them to csv's
if (updateSplits) {

  byYear <- function(df, name) {
    
    df %>%
      group_by(Year, Song, Artist) %>%
      summarise(
        ID = last(ID),
        Plays = n()
      ) %>%
      mutate(
        Song = paste(Artist, '~', Song)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(ID, Year, Song, Plays) -> songs
    
    df %>%
      group_by(Year, Artist) %>%
      summarise(
        Plays = n()
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(Year, Artist, Plays) -> artists
    
    df %>%
      group_by(Year, Album) %>%
      summarise(
        Artist = getmode(Artist),
        Plays = n()
      ) %>%
      mutate(
        Album = paste(Artist, '~', Album)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(Year, Album, Plays) -> albums
    
    write.csv(songs, paste0('splits//yearSongs', name, '.csv'), row.names = F)
    write.csv(artists, paste0('splits//yearArtists', name, '.csv'), row.names = F)
    write.csv(albums, paste0('splits//yearAlbums', name, '.csv'), row.names = F)
    
  }
  
  byYear(dataDM, 'DM') -> yearsDM
  byYear(dataAN, 'AN') -> yearsAN
  byYear(dataAK, 'AK') -> yearsAK
  
  bySeason <- function(df, name) {
    
    df %>%
      group_by(Season, Song, Artist) %>%
      summarise(
        ID = last(ID),
        Plays = n()
      ) %>%
      mutate(
        Song = paste(Artist, '~', Song)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(ID, Season, Song, Plays) -> songs
    
    df %>%
      group_by(Season, Artist) %>%
      summarise(
        Plays = n()
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(Season, Artist, Plays) -> artists
    
    df %>%
      group_by(Season, Album) %>%
      summarise(
        Artist = getmode(Artist),
        Plays = n()
      ) %>%
      mutate(
        Album = paste(Artist, '~', Album)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(Season, Album, Plays) -> albums
    
    write.csv(songs, paste0('splits//seasonSongs', name, '.csv'), row.names = F)
    write.csv(artists, paste0('splits//seasonArtists', name, '.csv'), row.names = F)
    write.csv(albums, paste0('splits//seasonAlbums', name, '.csv'), row.names = F)
    
  }
  
  bySeason(dataDM, 'DM') -> seasonsDM
  bySeason(dataAN, 'AN') -> seasonsAN
  bySeason(dataAK, 'AK') -> seasonsAK
  
  byMonth <- function(df, name) {
    
    df %>%
      group_by(Month, Song, Artist) %>%
      summarise(
        ID = last(ID),
        Plays = n()
      ) %>%
      mutate(
        Song = paste(Artist, '~', Song)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(ID, Month, Song, Plays) -> songs
    
    df %>%
      group_by(Month, Artist) %>%
      summarise(
        Plays = n()
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(Month, Artist, Plays) -> artists
    
    df %>%
      group_by(Month, Album) %>%
      summarise(
        Artist = getmode(Artist),
        Plays = n()
      ) %>%
      mutate(
        Album = paste(Artist, '~', Album)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(Month, Album, Plays) -> albums
    
    write.csv(songs, paste0('splits//monthSongs', name, '.csv'), row.names = F)
    write.csv(artists, paste0('splits//monthArtists', name, '.csv'), row.names = F)
    write.csv(albums, paste0('splits//monthAlbums', name, '.csv'), row.names = F)
    
  }
  
  byMonth(dataDM, 'DM') -> monthsDM
  byMonth(dataAN, 'AN') -> monthsAN
  byMonth(dataAK, 'AK') -> monthsAK
  
  byMonthYear <- function(df, name) {
    
    df %>%
      group_by(MonthYear, Song, Artist) %>%
      summarise(
        ID = last(ID),
        Plays = n()
      ) %>%
      mutate(
        Song = paste(Artist, '~', Song)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(ID, MonthYear, Song, Plays) -> songs
    
    df %>%
      group_by(MonthYear, Artist) %>%
      summarise(
        Plays = n()
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(MonthYear, Artist, Plays) -> artists
    
    df %>%
      group_by(MonthYear, Album) %>%
      summarise(
        Artist = getmode(Artist),
        Plays = n()
      ) %>%
      mutate(
        Album = paste(Artist, '~', Album)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(MonthYear, Album, Plays) -> albums
    
    write.csv(songs, paste0('splits//monthyearSongs', name, '.csv'), row.names = F)
    write.csv(artists, paste0('splits//monthyearArtists', name, '.csv'), row.names = F)
    write.csv(albums, paste0('splits//monthyearAlbums', name, '.csv'), row.names = F)
    
  }
  
  byMonthYear(dataDM, 'DM') -> monthyearsDM
  byMonthYear(dataAN, 'AN') -> monthyearsAN
  byMonthYear(dataAK, 'AK') -> monthyearsAK
  
  bySchoolPeriod <- function(df, name) {
    
    df %>%
      group_by(SchoolPeriod, Song, Artist) %>%
      summarise(
        ID = last(ID),
        Plays = n()
      ) %>%
      filter(SchoolPeriod != '') %>%
      mutate(
        Song = paste(Artist, '~', Song)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(ID, SchoolPeriod, Song, Plays) -> songs
    
    df %>%
      group_by(SchoolPeriod, Artist) %>%
      summarise(
        Plays = n()
      ) %>%
      filter(SchoolPeriod != '') %>%
      arrange(
        desc(Plays)
      ) %>%
      select(SchoolPeriod, Artist, Plays) -> artists
    
    df %>%
      group_by(SchoolPeriod, Album) %>%
      summarise(
        Artist = getmode(Artist),
        Plays = n()
      ) %>%
      filter(SchoolPeriod != '') %>%
      mutate(
        Album = paste(Artist, '~', Album)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(SchoolPeriod, Album, Plays) -> albums
    
    write.csv(songs, paste0('splits//schoolperiodSongs', name, '.csv'), row.names = F)
    write.csv(artists, paste0('splits//schoolperiodArtists', name, '.csv'), row.names = F)
    write.csv(albums, paste0('splits//schoolperiodAlbums', name, '.csv'), row.names = F)
    
  }
  
  bySchoolPeriod(dataDM, 'DM') -> schoolperiodsDM
  bySchoolPeriod(dataAN, 'AN') -> schoolperiodsAN
  bySchoolPeriod(dataAK, 'AK') -> schoolperiodsAK
  
  byHour <- function(df, name) {
    
    df %>%
      group_by(Hour, Song, Artist) %>%
      summarise(
        ID = last(ID),
        Plays = n()
      ) %>%
      mutate(
        Song = paste(Artist, '~', Song)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(ID, Hour, Song, Plays) -> songs
    
    df %>%
      group_by(Hour, Artist) %>%
      summarise(
        Plays = n()
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(Hour, Artist, Plays) -> artists
    
    df %>%
      group_by(Hour, Album) %>%
      summarise(
        Artist = getmode(Artist),
        Plays = n()
      ) %>%
      mutate(
        Album = paste(Artist, '~', Album)
      ) %>%
      arrange(
        desc(Plays)
      ) %>%
      select(Hour, Album, Plays) -> albums
    
    write.csv(songs, paste0('splits//hourSongs', name, '.csv'), row.names = F)
    write.csv(artists, paste0('splits//hourArtists', name, '.csv'), row.names = F)
    write.csv(albums, paste0('splits//hourAlbums', name, '.csv'), row.names = F)
    
  }
  
  byHour(dataDM, 'DM') -> hoursDM
  byHour(dataAN, 'AN') -> hoursAN
  byHour(dataAK, 'AK') -> hoursAK
  
}

# Loading the split csv's then saving them as RData
if (loadSplits) {
  
  loadYear <- function(name) {
    
    fread(paste0('splits//yearSongs', name, '.csv')) -> songs
    fread(paste0('splits//yearArtists', name, '.csv')) -> artists
    fread(paste0('splits//yearAlbums', name, '.csv')) -> albums
    
    lapply(yearList, function(year) {
      songs %>%
        filter(Year == year) %>%
        return()
    }) -> songs
    
    lapply(yearList, function(year) {
      artists %>%
        filter(Year == year) %>%
        return()
    }) -> artists
    
    lapply(yearList, function(year) {
      albums %>%
        filter(Year == year) %>%
        return()
    }) -> albums
    
    list(
      songs, artists, albums
    ) %>%
      return()
    
  }
  
  loadYear('DM') -> yearsDM
  loadYear('AN') -> yearsAN
  loadYear('AK') -> yearsAK
  
  loadSeason <- function(name) {
    
    fread(paste0('splits//seasonSongs', name, '.csv')) -> songs
    fread(paste0('splits//seasonArtists', name, '.csv')) -> artists
    fread(paste0('splits//seasonAlbums', name, '.csv')) -> albums
    
    lapply(seasonList, function(season) {
      songs %>%
        filter(Season == season) %>%
        return()
    }) -> songs
    
    lapply(seasonList, function(season) {
      artists %>%
        filter(Season == season) %>%
        return()
    }) -> artists
    
    lapply(seasonList, function(season) {
      albums %>%
        filter(Season == season) %>%
        return()
    }) -> albums
    
    list(
      songs, artists, albums
    ) %>%
      return()
    
  }
  
  loadSeason('DM') -> seasonsDM
  loadSeason('AN') -> seasonsAN
  loadSeason('AK') -> seasonsAK
  
  loadMonth <- function(name) {
    
    fread(paste0('splits//monthSongs', name, '.csv'), colClasses = c(rep('character', 3), 'numeric')) -> songs
    fread(paste0('splits//monthArtists', name, '.csv'), colClasses = c(rep('character', 2), 'numeric')) -> artists
    fread(paste0('splits//monthAlbums', name, '.csv'), colClasses = c(rep('character', 2), 'numeric')) -> albums
    
    lapply(monthList$num, function(month) {
      songs %>%
        filter(Month == month) %>%
        return()
    }) -> songs
    
    lapply(monthList$num, function(month) {
      artists %>%
        filter(Month == month) %>%
        return()
    }) -> artists
    
    lapply(monthList$num, function(month) {
      albums %>%
        filter(Month == month) %>%
        return()
    }) -> albums
    
    list(
      songs, artists, albums
    ) %>%
      return()
    
  }
  
  loadMonth('DM') -> monthsDM
  loadMonth('AN') -> monthsAN
  loadMonth('AK') -> monthsAK
  
  loadMonthyear <- function(name) {
    
    fread(paste0('splits//monthyearSongs', name, '.csv')) -> songs
    fread(paste0('splits//monthyearArtists', name, '.csv')) -> artists
    fread(paste0('splits//monthyearAlbums', name, '.csv')) -> albums
    
    lapply(monthyearList, function(monthyear) {
      songs %>%
        filter(MonthYear == monthyear) %>%
        return()
    }) -> songs
    
    lapply(monthyearList, function(monthyear) {
      artists %>%
        filter(MonthYear == monthyear) %>%
        return()
    }) -> artists
    
    lapply(monthyearList, function(monthyear) {
      albums %>%
        filter(MonthYear == monthyear) %>%
        return()
    }) -> albums
    
    list(
      songs, artists, albums
    ) %>%
      return()
    
  }
  
  loadMonthyear('DM') -> monthyearsDM
  loadMonthyear('AN') -> monthyearsAN
  loadMonthyear('AK') -> monthyearsAK
  
  loadSchoolperiod <- function(name) {
    
    fread(paste0('splits//schoolperiodSongs', name, '.csv')) -> songs
    fread(paste0('splits//schoolperiodArtists', name, '.csv')) -> artists
    fread(paste0('splits//schoolperiodAlbums', name, '.csv')) -> albums
    
    lapply(schoolperiodList, function(schoolperiod) {
      songs %>%
        filter(SchoolPeriod == schoolperiod) %>%
        return()
    }) -> songs
    
    lapply(schoolperiodList, function(schoolperiod) {
      artists %>%
        filter(SchoolPeriod == schoolperiod) %>%
        return()
    }) -> artists
    
    lapply(schoolperiodList, function(schoolperiod) {
      albums %>%
        filter(SchoolPeriod == schoolperiod) %>%
        return()
    }) -> albums
    
    list(
      songs, artists, albums
    ) %>%
      return()
    
  }
  
  loadSchoolperiod('DM') -> schoolperiodsDM
  loadSchoolperiod('AN') -> schoolperiodsAN
  loadSchoolperiod('AK') -> schoolperiodsAK
  
  loadHour <- function(name) {
    
    start <- Sys.time()
    fread(paste0('splits//hourSongs', name, '.csv'), colClasses = c(rep('character', 3), 'numeric')) -> songs
    fread(paste0('splits//hourArtists', name, '.csv'), colClasses = c(rep('character', 2), 'numeric')) -> artists
    fread(paste0('splits//hourAlbums', name, '.csv'), colClasses = c(rep('character', 2), 'numeric')) -> albums
    print(Sys.time() - start)
    
    lapply(hourList$val, function(hour) {
      songs %>%
        filter(Hour == hour) %>%
        return()
    }) -> songs
    
    lapply(hourList$val, function(hour) {
      artists %>%
        filter(Hour == hour) %>%
        return()
    }) -> artists
    
    lapply(hourList$val, function(hour) {
      albums %>%
        filter(Hour == hour) %>%
        return()
    }) -> albums
    
    list(
      songs, artists, albums
    ) %>%
      return()
    
  }
  
  loadHour('DM') -> hoursDM
  loadHour('AN') -> hoursAN
  loadHour('AK') -> hoursAK
  
  save(
    yearsDM, yearsAN, yearsAK,
    seasonsDM, seasonsAN, seasonsAK,
    monthsDM, monthsAN, monthsAK,
    monthyearsDM, monthyearsAN, monthyearsAK,
    schoolperiodsDM, schoolperiodsAN, schoolperiodsAK,
    hoursDM, hoursAN, hoursAK,
    file = 'splits.RData'
  )
  
}

load('splits.RData')
