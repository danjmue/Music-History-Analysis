# Function to get shared songs between two users
getSharedSongs <- function(df1, df2) {
  
  df1 %>%
    filter(Plays > 25) %>%
    rename('Plays1' = 'Plays') %>%
    select(Song, Artist, `Plays1`) -> mergeDF1
  
  df2 %>%
    filter(Plays > 25) %>%
    rename('Plays2' = 'Plays') %>%
    select(Song, Artist, `Plays2`) -> mergeDF2
  
  mergeDF1 %>%
    inner_join(mergeDF2) %>%
    mutate(`Combined Plays` = `Plays1` + `Plays2`) %>%
    arrange(desc(`Combined Plays`)) %>% 
    return()
  
}

# Function to get shared artists between two users
getSharedArtists <- function(df1, df2) {
  
  df1 %>%
    filter(Plays > 250) %>%
    rename('Plays1' = 'Plays') %>%
    select(Artist, `Plays1`) -> mergeDF1
  
  df2 %>%
    filter(Plays > 250) %>%
    rename('Plays2' = 'Plays') %>%
    select(Artist, `Plays2`) -> mergeDF2
  
  mergeDF1 %>%
    inner_join(mergeDF2) %>%
    mutate(`Combined Plays` = `Plays1` + `Plays2`) %>%
    arrange(desc(`Combined Plays`)) %>% 
    return()
  
}

# Function to get shared albums between two users
getSharedAlbums <- function(df1, df2) {
  
  df1 %>%
    filter(Plays > 150) %>%
    rename('Plays1' = 'Plays') %>%
    select(Album, `Plays1`) -> mergeDF1
  
  df2 %>%
    filter(Plays > 150) %>%
    rename('Plays2' = 'Plays') %>%
    select(Album, `Plays2`) -> mergeDF2
  
  mergeDF1 %>%
    inner_join(mergeDF2) %>%
    mutate(`Combined Plays` = `Plays1` + `Plays2`) %>%
    arrange(desc(`Combined Plays`)) %>% 
    return()
  
}
