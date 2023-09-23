options(dplyr.summarise.inform = FALSE)

# Information to use the Spotify API for developers
Sys.setenv(SPOTIFY_CLIENT_ID = '--------------------------------')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '------------------------------')
response <- POST(
  "https://accounts.spotify.com/api/token",
  config = authenticate(user = Sys.getenv("SPOTIFY_CLIENT_ID"), 
                        password = Sys.getenv("SPOTIFY_CLIENT_SECRET")),
  body = list(grant_type = "client_credentials"), 
  encode = "form"
)
content(response)$access_token -> access_token

font_add_google('Questrial', family = 'questrial')
showtext_auto()

# csv file containing the spotify ID's for tracks from Apple Music data
read.csv('ID List.csv') %>%
  mutate(Name = paste(Artist, '~', Song)) -> idList

# csv file containing the names and spotify ID's for every playlist on the shiny app
fread('playlists.csv') -> playlists

source('helperFunctions.R')
source('loadCleanData.R')
source('displayFunctions.R')
source('universalVariables.R')

updateMainRanks <- F

# Updating the top song rankings for each user
if (updateMainRanks) {
  
  getTopSongs <- function(df) {
    
    df %>%
      group_by(Song, Artist) %>%
      summarise(
        Plays = n(),
        `First Listen` = min(Date),
        ID = getmode(ID)
      ) %>%
      arrange(desc(Plays)) %>%
      return()
    
  }
  
  getTopSongs(dataDM) -> topSongsDM
  getTopSongs(dataAN) -> topSongsAN
  getTopSongs(dataAK) -> topSongsAK
  
  topSongsDM %>%
    mutate(Name = paste(Artist, '~', Song)) %>%
    left_join(select(idList, Name, id), by = 'Name') %>%
    mutate(ID = ifelse(is.na(id), ID, id)) %>%
    select(Song, Artist, Plays, `First Listen`, ID) -> topSongsDM
  
  write.csv(topSongsDM, 'mainRanks/topSongsDM.csv', row.names = F)
  write.csv(topSongsAN, 'mainRanks/topSongsAN.csv', row.names = F)
  write.csv(topSongsAK, 'mainRanks/topSongsAK.csv', row.names = F)
  
  getTopArtists <- function(df) {
    
    df %>%
      group_by(Artist) %>%
      summarise(
        Plays = n(),
        Songs = n_distinct(Song),
        `First Listen` = min(Date)
      ) %>%
      mutate(
        `Plays Per Song` = Plays / Songs
      ) %>%
      arrange(desc(Plays)) %>%
      return()
    
  }
  
  getTopArtists(dataDM) -> topArtistsDM
  getTopArtists(dataAN) -> topArtistsAN
  getTopArtists(dataAK) -> topArtistsAK
  
  write.csv(topArtistsDM, 'mainRanks/topArtistsDM.csv', row.names = F)
  write.csv(topArtistsAN, 'mainRanks/topArtistsAN.csv', row.names = F)
  write.csv(topArtistsAK, 'mainRanks/topArtistsAK.csv', row.names = F)
  
  getTopArtistRates <- function(df) {
    
    df %>%
      filter(Songs > 9) %>%
      select(Artist, Songs, `Plays Per Song`) %>%
      return()
    
  }
  
  getTopArtistRates(topArtistsDM) -> topArtistRatesDM
  getTopArtistRates(topArtistsAN) -> topArtistRatesAN
  getTopArtistRates(topArtistsAK) -> topArtistRatesAK
  
  write.csv(topArtistRatesDM, 'mainRanks/topArtistRatesDM.csv', row.names = F)
  write.csv(topArtistRatesAN, 'mainRanks/topArtistRatesAN.csv', row.names = F)
  write.csv(topArtistRatesAK, 'mainRanks/topArtistRatesAK.csv', row.names = F)
  
  getTopAlbums <- function(df) {
    
    df %>%
      filter(Album != '') %>%
      group_by(Album) %>%
      summarise(
        Plays = n(),
        Songs = n_distinct(Song),
        Artist = getmode(Artist),
        `First Listen` = min(Date)
      ) %>%
      mutate(
        `Plays Per Song` = Plays / Songs
      ) %>%
      arrange(desc(Plays)) %>%
      return()
    
  }
  
  getTopAlbums(dataDM) -> topAlbumsDM
  getTopAlbums(dataAN) -> topAlbumsAN
  getTopAlbums(dataAK) -> topAlbumsAK
  
  write.csv(topAlbumsDM, 'mainRanks/topAlbumsDM.csv', row.names = F)
  write.csv(topAlbumsAN, 'mainRanks/topAlbumsAN.csv', row.names = F)
  write.csv(topAlbumsAK, 'mainRanks/topAlbumsAK.csv', row.names = F)
  
  getTopAlbumRates <- function(df) {
    
    df %>%
      filter(Songs > 1) %>%
      select(Artist, Album, Songs, `Plays Per Song`) %>%
      return()
    
  }
  
  getTopAlbumRates(topAlbumsDM) -> topAlbumRatesDM
  getTopAlbumRates(topAlbumsAN) -> topAlbumRatesAN
  getTopAlbumRates(topAlbumsAK) -> topAlbumRatesAK
  
  write.csv(topAlbumRatesDM, 'mainRanks/topAlbumRatesDM.csv', row.names = F)
  write.csv(topAlbumRatesAN, 'mainRanks/topAlbumRatesAN.csv', row.names = F)
  write.csv(topAlbumRatesAK, 'mainRanks/topAlbumRatesAK.csv', row.names = F)
  
  getTopDays <- function(df) {
    
    df %>%
      group_by(Date) %>%
      summarise(Plays = n()) %>%
      arrange(desc(Plays))
    
  }
  
  getTopDays(dataDM) -> topDaysDM
  getTopDays(dataAN) -> topDaysAN
  getTopDays(dataAK) -> topDaysAK
  
  write.csv(topDaysDM, 'mainRanks/topDaysDM.csv', row.names = F)
  write.csv(topDaysAN, 'mainRanks/topDaysAN.csv', row.names = F)
  write.csv(topDaysAK, 'mainRanks/topDaysAK.csv', row.names = F)
  
  getBiggestRisers <- function(df) {
    
    df %>%
      group_by(Artist, Year) %>%
      summarise(
        Plays = n()
      ) %>%
      arrange(Year) %>%
      pivot_wider(names_from = Year, values_from = Plays, values_fill = 0) -> df
    
    rowSums(df[,2:ncol(df)]) -> df$Total
    
    df %>%
      filter(Total > 100) -> df
    
    prop.table(as.matrix(df[,2:(ncol(df) - 1)]), 1) -> df[,2:(ncol(df) - 1)]
    
    df %>%
      arrange(desc(`2022`)) %>%
      head(8) %>%
      select(Artist) -> df
    
    lapply(df$Artist, function(i) {
      if (!file.exists(paste0('www\\', i, '.jpg'))) {
        getArtistID(i) -> id
        z <- tempfile()
        download.file(get_artist(id$id[1])$images$url[1], z, mode = 'wb')
        writeJPEG(readJPEG(z), paste0('www\\', i, '.jpg'))
        file.remove(z)
      }
    })
    
    return(df)
    
  }
  
  getBiggestRisers(dataDM) -> biggestRisersDM
  getBiggestRisers(dataAN) -> biggestRisersAN
  getBiggestRisers(dataAK) -> biggestRisersAK
  
  write.csv(biggestRisersDM, 'mainRanks/biggestRisersDM.csv', row.names = F)
  write.csv(biggestRisersAN, 'mainRanks/biggestRisersAN.csv', row.names = F)
  write.csv(biggestRisersAK, 'mainRanks/biggestRisersAK.csv', row.names = F)
  
}

# Reading in the saved top song rankings for each user
fread('mainRanks/topSongsDM.csv') -> topSongsDM
fread('mainRanks/topSongsAN.csv') -> topSongsAN
fread('mainRanks/topSongsAK.csv') -> topSongsAK

fread('mainRanks/topArtistsDM.csv') -> topArtistsDM
fread('mainRanks/topArtistsAN.csv') -> topArtistsAN
fread('mainRanks/topArtistsAK.csv') -> topArtistsAK

fread('mainRanks/topArtistRatesDM.csv') -> topArtistRatesDM
fread('mainRanks/topArtistRatesAN.csv') -> topArtistRatesAN
fread('mainRanks/topArtistRatesAK.csv') -> topArtistRatesAK

fread('mainRanks/topAlbumsDM.csv') -> topAlbumsDM
fread('mainRanks/topAlbumsAN.csv') -> topAlbumsAN
fread('mainRanks/topAlbumsAK.csv') -> topAlbumsAK

fread('mainRanks/topAlbumRatesDM.csv') -> topAlbumRatesDM
fread('mainRanks/topAlbumRatesAN.csv') -> topAlbumRatesAN
fread('mainRanks/topAlbumRatesAK.csv') -> topAlbumRatesAK

fread('mainRanks/topDaysDM.csv') -> topDaysDM
fread('mainRanks/topDaysAN.csv') -> topDaysAN
fread('mainRanks/topDaysAK.csv') -> topDaysAK

read.csv('mainRanks/biggestRisersDM.csv') -> biggestRisersDM
read.csv('mainRanks/biggestRisersAN.csv') -> biggestRisersAN
read.csv('mainRanks/biggestRisersAK.csv') -> biggestRisersAK

source('comparisons.R')
source('splits.R')
source('downloadArt.R')
source('audioFeats.R')
