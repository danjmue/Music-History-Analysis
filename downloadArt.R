library(tidyverse)
library(spotifyr)
library(jpeg)

updateArt <- F

# Download art and save its data into RData
if (updateArt) {
  
  # Function to download the art
  downloadAllArt <- function() {
    
    names <- c('DM', 'AN', 'AK')
    splits <- c('years', 'seasons', 'months', 'monthyears', 'schoolperiods', 'hours')
    do.call(paste0, expand.grid(splits, names)) -> splitNames
    
    lapply(splitNames, function(dfName) {
      eval(parse(text = dfName)) -> dfBig
      lapply(1:length(dfBig[[1]]), function(i) {
        dfBig[[1]][i] %>% 
          as.data.frame() %>%
          head(n = 3) -> df
        if (nrow(df) > 0) {
          lapply(1:3, function(j) {
            
            paste0(str_sub(dfName, 1, str_length(dfName) - 2),
                   str_sub(dfName, str_length(dfName) - 1, str_length(dfName)),
                   as.character(df[1,2]), 'song', j) -> fileID
            
            df$Song[j] -> songName
            gsub('/ ', '', songName) -> songName
            gsub(':', '', songName) -> songName
            gsub('\\?', '', songName) -> songName
            gsub('"', '', songName) -> songName
            gsub(' \\| ', ' ', songName) -> songName
            if (!file.exists(paste0('www\\', fileID, '.jpg'))) {
              if (str_sub(dfName, str_length(dfName) - 1, str_length(dfName)) == 'DM') {
                getSongID(df$Song[j]) -> id
                z <- tempfile()
                download.file(get_track(id$id[1])$album$images$url[1], z, mode = 'wb')
                writeJPEG(readJPEG(z), paste0('www\\', fileID, '.jpg'))
                file.remove(z)
              } else {
                z <- tempfile()
                download.file(get_track(df$ID[j])$album$images$url[1], z, mode = 'wb')
                writeJPEG(readJPEG(z), paste0('www\\', fileID, '.jpg'))
                file.remove(z)
              }
            }
            return(data.frame(Name = df$Song[j], File = paste0(fileID, '.jpg'), From = str_sub(dfName, 1, str_length(dfName) - 2), 
                              Person = str_sub(dfName, str_length(dfName) - 1, str_length(dfName)), 
                              Val = as.character(df[1,2]), Type = 'song', Ind = j, Id = ''))
          })
        }
      }) -> dfBig
      return(dfBig)
    }) %>% unlist(recursive = F) %>% bind_rows() -> songArt
    
    lapply(splitNames, function(dfName) {
      eval(parse(text = dfName)) -> dfBig
      lapply(1:length(dfBig[[2]]), function(i) {
        dfBig[[2]][i] %>%
          as.data.frame() %>%
          head(n = 3) -> df
        if (nrow(df) > 0) {
          lapply(1:3, function(j) {
            
            paste0(str_sub(dfName, 1, str_length(dfName) - 2),
                   str_sub(dfName, str_length(dfName) - 1, str_length(dfName)),
                   as.character(df[1,1]), 'artist', j) -> fileID
            
            df$Artist[j] -> artistName
            gsub(':', '', artistName) -> artistName
            if (!file.exists(paste0('www\\', fileID, '.jpg'))) {
              getArtistID(df$Artist[j]) -> id
              z <- tempfile()
              download.file(get_artist(id$id[1])$images$url[1], z, mode = 'wb')
              writeJPEG(readJPEG(z), paste0('www\\', fileID, '.jpg'))
              file.remove(z)
            }
            return(data.frame(Name = df$Artist[j], File = paste0(fileID, '.jpg'), From = str_sub(dfName, 1, str_length(dfName) - 2), 
                              Person = str_sub(dfName, str_length(dfName) - 1, str_length(dfName)),
                              Val = as.character(df[1,1]), Type = 'artist', Ind = j, Id = getArtistID(df$Artist[j])))
          })
        }
      }) %>%
        rename('Id' = 'id') -> dfBig
      return(dfBig)
    }) %>% unlist(recursive = F) %>% bind_rows() -> artistArt
    
    lapply(splitNames, function(dfName) {
      eval(parse(text = dfName)) -> dfBig
      lapply(1:length(dfBig[[3]]), function(i) {
        dfBig[[3]][i] %>%
          as.data.frame() %>%
          head(n = 3) -> df
        if (nrow(df) > 0) {
          lapply(1:3, function(j) {
            
            paste0(str_sub(dfName, 1, str_length(dfName) - 2),
                   str_sub(dfName, str_length(dfName) - 1, str_length(dfName)),
                   as.character(df[1,1]), 'album', j) -> fileID
            
            df$Album[j] -> albumName
            gsub('/ ', '', albumName) -> albumName
            gsub(':', '', albumName) -> albumName
            gsub('\\?', '', albumName) -> albumName
            gsub('\\*', '', albumName) -> albumName
            gsub('"', '', albumName) -> albumName
            if (!file.exists(paste0('www\\', fileID, '.jpg'))) {
              getAlbumID(df$Album[j]) -> id
              z <- tempfile()
              download.file(get_album(id$id[1])$images$url[1], z, mode = 'wb')
              writeJPEG(readJPEG(z), paste0('www\\', fileID, '.jpg'))
              file.remove(z)
            }
            return(data.frame(Name = df$Album[j], File = paste0(fileID, '.jpg'), From = str_sub(dfName, 1, str_length(dfName) - 2), 
                              Person = str_sub(dfName, str_length(dfName) - 1, str_length(dfName)),
                              Val = as.character(df[1,1]), Type = 'album', Ind = j, Id = ''))
          })
        }
      }) -> dfBig
      return(dfBig)
    }) %>% unlist(recursive = F) %>% bind_rows() -> albumArt
    
    rbind(
      songArt,
      artistArt,
      albumArt
    ) %>%
      return()
    
  }
  
  downloadAllArt() %>% distinct() -> art
  
  save(art, file = 'artData.RData')
  
}

load('artData.RData')