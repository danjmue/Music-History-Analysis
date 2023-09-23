library(rjson)
library(stringi)

updateData <- F

# Loading and cleaning data
if (updateData) {
  
  # Loading in listening history data
  fread('playhistoryDM.csv', encoding = 'UTF-8') %>%
    filter(`Play Duration Milliseconds` > 30000) %>%
    select(`Event ID`, `Album Name`, `Artist Name`, `Event Received Timestamp`, `Song Name`) -> dataDM
  
  fread('playhistoryAN.csv', encoding = 'Latin-1') %>%
    filter(ms_played > 30000) %>%
    drop_na(master_metadata_track_name) %>%
    select(spotify_track_uri, master_metadata_album_album_name, master_metadata_album_artist_name, ts, master_metadata_track_name) -> dataAN
  gsub('spotify:track:', '', dataAN$spotify_track_uri) -> dataAN$spotify_track_uri
  
  fread('playhistoryAK.csv', encoding = 'Latin-1') %>%
    filter(ms_played > 30000) %>%
    drop_na(master_metadata_track_name) %>%
    select(spotify_track_uri, master_metadata_album_album_name, master_metadata_album_artist_name, ts, master_metadata_track_name) -> dataAK
  gsub('spotify:track:', '', dataAK$spotify_track_uri) -> dataAK$spotify_track_uri
  
  # Creating columns for different categories to sort songs by
  createVariables <- function(df) {
    
    colnames(df)[1:5] <- c('ID', 'Album', 'Artist', 'TS', 'Song')
    df[df$Song != '',] -> df
    df[df$Song != 'Apple Music 1',] -> df
    
    df$Date <- substr(df$TS, 1, 10)
    df$Time <- substr(df$TS, 12, 19)
    df$Hour <- substr(df$Time, 1, 2)
    df$Day <- substr(df$Date, 6, 10)
    df$Month <- substr(df$Day, 1, 2)
    df$Year <- substr(df$Date, 1, 4)
    df$MonthYear <- substr(df$Date, 1, 7)
    df$Season <- ifelse(df$Day >= '03-20' & df$Day < '06-21', 'Spring',
                        ifelse(df$Day >= '06-21' & df$Day < '09-23', 'Summer',
                               ifelse(df$Day >= '09-23' & df$Day < '12-21', 'Fall', 'Winter')))
    df$SchoolPeriod <- ''
    df[df$Date >= '2015-09-08' & df$Date <= '2016-06-16', 'SchoolPeriod'] <- '8th Grade'
    df[df$Date > '2016-06-16' & df$Date < '2016-09-06', 'SchoolPeriod'] <- '8th-9th Summer'
    df[df$Date >= '2016-09-06' & df$Date <= '2017-06-15', 'SchoolPeriod'] <- '9th Grade'
    df[df$Date > '2017-06-15' & df$Date < '2017-09-05', 'SchoolPeriod'] <- '9th-10th Summer'
    df[df$Date >= '2017-09-05' & df$Date <= '2018-06-15', 'SchoolPeriod'] <- '10th Grade'
    df[df$Date > '2018-06-15' & df$Date < '2018-09-04', 'SchoolPeriod'] <- '10th-11th Summer'
    df[df$Date >= '2018-09-04' & df$Date <= '2019-06-14', 'SchoolPeriod'] <- '11th Grade'
    df[df$Date > '2019-06-14' & df$Date < '2019-09-02', 'SchoolPeriod'] <- '11th-12th Summer'
    df[df$Date >= '2019-09-02' & df$Date <= '2020-03-13', 'SchoolPeriod'] <- '12th Grade'
    df[df$Date > '2020-03-13' & df$Date < '2020-08-31', 'SchoolPeriod'] <- '12th-Freshman Summer'
    df[df$Date >= '2020-08-31' & df$Date <= '2021-04-29', 'SchoolPeriod'] <- 'Freshman Year'
    df[df$Date > '2021-04-29' & df$Date < '2021-08-30', 'SchoolPeriod'] <- 'Freshman-Sophomore Summer'
    df[df$Date >= '2021-08-30' & df$Date <= '2022-04-28', 'SchoolPeriod'] <- 'Sophomore Year'
    df[df$Date > '2022-04-28' & df$Date < '2022-08-29', 'SchoolPeriod'] <- 'Sophomore-Junior Summer'
    df %>%
      select(-TS) -> df
    df %>%
      filter(Date >= '2016-01-01' & Date < '2023-01-01') -> df
    
    return(df)
    
  }
  
  createVariables(dataDM) -> dataDM
  createVariables(dataAN) -> dataAN
  createVariables(dataAK) -> dataAK
  
  # Fixing data entries
  fixData <- function(df, am) {
    
    if (am) {
      iconv(df$Album, from = 'UTF-8', to = "ASCII//TRANSLIT") -> df$Album
      iconv(df$Artist, from = 'UTF-8', to = "ASCII//TRANSLIT") -> df$Artist
      iconv(df$Song, from = 'UTF-8', to = "ASCII//TRANSLIT") -> df$Song
    } else {
      iconv(df$Album, to = "ASCII//TRANSLIT") -> df$Album
      iconv(df$Artist, to = "ASCII//TRANSLIT") -> df$Artist
      iconv(df$Song, to = "ASCII//TRANSLIT") -> df$Song
    }
    
    df[df$Artist != 'Kurt Vile',] -> df
    df[df$Album == 'The Water (S)', 'Album'] <- 'The Water[s]'
    df[df$Album == 'Wave[S]', 'Album'] <- 'Wave[s]'
    df[df$Album == 'Elephant In the Room', 'Album'] <- 'Elephant in the Room'
    df[df$Album == 'DAMN. COLLECTORS EDITION.', 'Album'] <- 'DAMN.'
    df[df$Album == 'White Ceilings', 'Album'] <- 'white ceilings'
    df[df$Song == 'Whitegold', 'Song'] <- 'whitegold'
    df[df$Song == 'Prblms', 'Song'] <- 'PRBLMS'
    
    df[df$Song == 'Runaway (feat. Pusha T)', 'Artist'] <- 'Kanye West'
    
    df[df$Album == '?' & df$Artist == 'Ed Sheeran', 'Album'] <- 'Divide'
    
    gsub('B4\\.Da\\.\\$\\$', 'B4.DA.$$', df$Album) -> df$Album
    gsub(' - LUV vs\\. The World 2', '', df$Album) -> df$Album
    
    gsub('Zac Flewids', 'Isaac Zale', df$Artist) -> df$Artist
    
    gsub(' \\(Deluxe\\)', '', df$Album) -> df$Album
    gsub(' \\(Deluxe Version\\)', '', df$Album) -> df$Album
    gsub(' \\(Deluxe Edition\\)', '', df$Album) -> df$Album
    gsub(' \\(Expanded Edition\\)', '', df$Album) -> df$Album
    gsub(' \\(Expanded Version\\)', '', df$Album) -> df$Album
    gsub(' \\(Bonus Track Version\\)', '', df$Album) -> df$Album
    gsub(' - EP', '', df$Album) -> df$Album
    
    return(df)
    
  }
  
  fixData(dataDM, T) -> dataDM
  fixData(dataAN, F) -> dataAN
  fixData(dataAK, F) -> dataAK
  
  # Fetching albums for each song
  getAlbums <- function(df) {
    
    df[df$Album == '',c('Artist', 'Song')] -> fixes
    df %>%
      group_by(Artist, Song) %>%
      summarise(
        Album = getmode(Album)
      ) -> albumsList
    left_join(fixes, albumsList, by = c('Artist', 'Song')) -> fixes
    fixes -> df[df$Album == '',c('Artist', 'Song', 'Album')]
    
    return(df)
    
  }
  
  getAlbums(dataDM) -> dataDM
  getAlbums(dataAN) -> dataAN
  getAlbums(dataAK) -> dataAK
  
  # Writing clean data
  fwrite(dataDM, 'dataCleanDM.csv')
  fwrite(dataAN, 'dataCleanAN.csv')
  fwrite(dataAK, 'dataCleanAK.csv')
  
}

fread('dataCleanDM.csv', colClasses = c('Hour' = 'character', 'Month' = 'character')) -> dataDM
fread('dataCleanAN.csv', colClasses = c('Hour' = 'character', 'Month' = 'character')) -> dataAN
fread('dataCleanAK.csv', colClasses = c('Hour' = 'character', 'Month' = 'character')) -> dataAK

