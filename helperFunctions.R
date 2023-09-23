# Gets the mode from a set of elements
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Removes weird characters from a string
stripDownString <- function(str) {
  gsub("[^[:alnum:] ]", "", str) -> str
  return(str)
}

# Given an artist and name, searches spotify for a song and returns its Spotify ID
getSongID <- function(name) {
  gsub(" \\(.*", "", name) -> name
  gsub(" -.*", "", name) -> name
  gsub(" /.*", "", name) -> name
  gsub("/.*", "", name) -> name
  gsub("\\|.*", "", name) -> name
  gsub("\\ l .*", "", name) -> name
  gsub("Sh*t", 'Shit', name, fixed = T) -> name
  gsub(" pt", ' part', name, fixed = T) -> name
  str_split(name, ' ~ ') -> narrowDown
  narrowDown[[1]][1] -> narrowArtist
  str_split(narrowArtist, ',')[[1]][1] -> narrowArtist
  narrowDown[[1]][2] -> narrowSong
  stripDownString(narrowArtist) -> narrowArtist
  stripDownString(narrowSong) -> narrowSong
  gsub("Remix.*", "", narrowSong, ignore.case = T) -> narrowSong
  trimws(narrowArtist) -> narrowArtist
  trimws(narrowSong) -> narrowSong
  str_split(name, ' ~ ') -> songSplit
  str_split(songSplit[[1]][1], ',')[[1]][1] -> songSplit[[1]][1]
  stripDownString(songSplit[[1]][1]) -> songSplit[[1]][1]
  stripDownString(songSplit[[1]][2]) -> songSplit[[1]][2]
  gsub("AAP Rocky", "A$AP Rocky", songSplit[[1]][1]) -> songSplit[[1]][1]
  query <- paste0('artist: ', songSplit[[1]][1], ' track: ', songSplit[[1]][2])
  if (query == "artist: Swae Lee track: 42") {
    query <- "artist: Swae Lee track: \"42\""
  } else if (query == "artist: Playboi Carti track: RIP") {
    query <- "artist: Playboi Carti track: R.I.P."
  } else if (query == 'artist: Kendrick Lamar track: ADHD') {
    query <- 'artist: Kendrick Lamar track: A.D.H.D.'
  } else if (query == "artist: MIKNNA track: On Sight") {
    query <- "artist: Free Nationals track: On Sight"
  }
  search_spotify(query, 'track', market = 'US', limit = 50, authorization = access_token) -> songs
  lapply(songs$artists, function(df) {
    as.character(df$name) -> df$name
    return(df)
  }) -> songs$artists
  songs %>%
    unnest_longer(artists) -> songs
  iconv(songs$name, from = 'UTF-8', to = 'ASCII//TRANSLIT') -> songs$name
  iconv(songs$artists$name, from = 'UTF-8', to = 'ASCII//TRANSLIT') -> songs$artists$name
  gsub(",.*", "", songs$artists$name) -> songs$artists$name
  stripDownString(songs$artists$name) -> songs$artists$name
  gsub(" \\(.*", "", songs$name) -> songs$name
  gsub(" -.*", "", songs$name) -> songs$name
  gsub(" /.*", "", songs$name) -> songs$name
  gsub("/.*", "", songs$name) -> songs$name
  gsub("\\|.*", "", songs$name) -> songs$name
  gsub(" FEAT.*", "", songs$name, ignore.case = T) -> songs$name
  gsub("Remix.*", "", songs$name, ignore.case = T) -> songs$name
  gsub("Sh*t", 'Shit', songs$name, fixed = T) -> songs$name
  gsub(" pt", ' part', songs$name, fixed = T) -> songs$name
  stripDownString(songs$name) -> songs$name
  trimws(songs$name) -> songs$name
  songs %>%
    filter(tolower(artists$name) == tolower(narrowArtist)) -> songs
  songs %>%
    filter(tolower(name) == tolower(narrowSong)) -> songs
  songs %>%
    head(1) %>%
    select(id) %>%
    return()
}

# Given an artist searches spotify for an artist and returns its Spotify ID
getArtistID <- function(name) {
  search_spotify(name, type = 'artist', authorization = access_token) -> artists
  artists %>%
    head(1) %>%
    select(id) %>%
    return()
}

# Given an artist and name, searches spotify for an album and returns its Spotify ID
getAlbumID <- function(name) {
  str_split(name, ' ~ ') -> narrowDown
  narrowDown[[1]][1] -> narrowArtist
  narrowDown[[1]][2] -> narrowAlbum
  gsub('Vol\\.', 'Volume', narrowAlbum) -> narrowAlbum
  gsub(" \\(.*", "", narrowAlbum) -> narrowAlbum
  stripDownString(narrowAlbum) -> narrowAlbum
  query <- paste0('artist: ', narrowDown[[1]][1], ' album: ', narrowDown[[1]][2])
  search_spotify(query, type = 'album', authorization = access_token) -> albums
  albums %>%
    unnest_longer(artists) -> albums
  iconv(albums$name, from = 'UTF-8', to = 'ASCII//TRANSLIT') -> albums$name
  iconv(albums$artists$name, from = 'UTF-8', to = 'ASCII//TRANSLIT') -> albums$artists$name
  gsub('Vol\\.', 'Volume', albums$name) -> albums$name
  gsub(" \\(.*", "", albums$name) -> albums$name
  stripDownString(albums$name) -> albums$name
  albums %>%
    filter(tolower(name) == tolower(narrowAlbum)) -> albums
  albums %>%
    head(1) %>%
    select(id) %>%
    return()
}
