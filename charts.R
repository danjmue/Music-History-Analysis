updateCharts <- F

# Generates charts then saves them as images
if (updateCharts) {
  
  # Charts for the home page
  lapply(personChoices, function(person) {
    
    df <- switch(person,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    df %>%
      group_by(MonthYear) %>%
      summarise(Songs = n()) %>%
      ggplot(aes(MonthYear, Songs)) +
      geom_col(aes(fill = Songs)) +
      scale_x_discrete(breaks = c('2017-01', '2018-01', '2019-01', '2020-01', '2021-01', '2022-01'),
                       labels = 2017:2022) +
      scale_fill_gradient(low = '#FFFFFF', high = '#72ECFA') +
      labs(title = 'Songs Per Month', x = element_blank(), y = 'Plays') +
      theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
            axis.title = element_text(family = 'questrial', color = 'white', size = 35),
            axis.text = element_text(family = 'questrial', color = 'white', size = 20),
            legend.title = element_text(family = 'questrial', color = 'white', size = 35),
            legend.text = element_text(family = 'questrial', color = 'white', size = 20),
            legend.key.size = unit(1.0, 'cm'),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = '#121212'),
            panel.grid.minor = element_line(color = '#121212'),
            plot.background = element_rect(fill = '#121212', colour = '#121212'),
            legend.background = element_rect(fill = '#121212')
      ) -> plot
      ggsave(filename = paste0('www/charts/homeSongsPerDayChart', person, '.png'), plot, width = 2000, height = 1000, units = 'px')
    
  })
  lapply(personChoices, function(person) {
    
    
    df <- switch(person,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    df %>%
      group_by(Hour, Weekday = lubridate::wday(Date, label = T)) %>%
      summarise(Plays = n()) %>%
      ggplot(aes(Weekday, Hour, fill = Plays)) +
      geom_tile(color = 'black') +
      scale_y_discrete(breaks = hourList$val, labels = hourList$label, limits = rev) +
      scale_fill_gradient(low = '#8072FA', high = '#FA8072') +
      labs(title = 'Plays Per Week') +
      theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
            axis.title = element_text(family = 'questrial', color = 'white', size = 35),
            axis.text = element_text(family = 'questrial', color = 'white', size = 20),
            legend.title = element_text(family = 'questrial', color = 'white', size = 35),
            legend.text = element_text(family = 'questrial', color = 'white', size = 20),
            legend.key.size = unit(1.0, 'cm'),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = '#121212'),
            panel.grid.minor = element_line(color = '#121212'),
            plot.background = element_rect(fill = '#121212', colour = '#121212'),
            legend.background = element_rect(fill = '#121212')
      ) -> plot
      ggsave(filename = paste0('www/charts/homeWeeklySongChart', person, '.png'), plot, width = 2000, height = 1000, units = 'px')
    
  })
  lapply(personChoices, function(person) {
    
    
    df <- switch(person,
                 'DM' = topSongsDM,
                 'AN' = topSongsAN,
                 'AK' = topSongsAK)
    
    af <- switch(person,
                 'DM' = audioFeatsDM,
                 'AN' = audioFeatsAN,
                 'AK' = audioFeatsAK)
    
    df %>%
      head(100) %>%
      inner_join(af, by = c('ID' = 'id')) %>%
      ggplot(aes(valence, energy, color = Plays)) +
      geom_point(size = 1.5) +
      geom_vline(xintercept = 0.5, color = 'white') +
      geom_hline(yintercept = 0.5, color = 'white') +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
      scale_color_gradient(low = '#8072FA', high = '#fac472') +
      annotate('text', 0.25 / 2, 0.95, label = "Angry", size = 15, family = 'questrial', color = 'white') +
      annotate('text', 1.75 / 2, 0.95, label = "Happy", size = 15, family = 'questrial', color = 'white') +
      annotate('text', 1.75 / 2, 0.05, label = "Peaceful", size = 15, family = 'questrial', color = 'white') +
      annotate('text', 0.25 / 2, 0.05, label = "Depressing", size = 15, family = 'questrial', color = 'white') +
      labs(title= 'Emotional Quadrant for Top 100 Songs', x= "Valence", y= "Energy") +
      theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
            axis.title = element_text(family = 'questrial', color = 'white', size = 35),
            axis.text = element_text(family = 'questrial', color = 'white', size = 20),
            legend.title = element_text(family = 'questrial', color = 'white', size = 35),
            legend.text = element_text(family = 'questrial', color = 'white', size = 20),
            legend.key.size = unit(1.0, 'cm'),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = '#121212'),
            panel.grid.minor = element_line(color = '#121212'),
            plot.background = element_rect(fill = '#121212', colour = '#121212'),
            legend.background = element_rect(fill = '#121212')
      ) -> plot
      ggsave(filename = paste0('www/charts/homeSongsEmotionalChart', person, '.png'), plot, width = 2000, height = 1000, units = 'px')
    
  })
  lapply(personChoices, function(person) {
    
    topSongs <- switch(person,
                       'DM' = topSongsDM,
                       'AN' = topSongsAN,
                       'AK' = topSongsAK)
    
    topSongs %>%
      head(10) -> topSongs
    
    df <- switch(person,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    inner_join(df, topSongs[,c('Artist', 'Song')]) -> df
    
    df %>%
      arrange(Date) %>%
      mutate(Plays = 1) %>%
      group_by(Artist, Song, MonthYear) %>%
      summarise(CumPlays = sum(Plays)) %>%
      as.data.frame() %>%
      group_by(Artist, Song) %>%
      mutate(CumPlays = cumsum(CumPlays)) %>%
      ungroup() -> df
    
    df %>%
      ggplot(aes(x = MonthYear, y = CumPlays, group = Song, color = Song)) +
      geom_line(linewidth = 0.75) +
      scale_color_manual(values = c("brown", "red", "darkgoldenrod3", "darkolivegreen3", "chartreuse4",
                                    "cyan3", "cornflowerblue", "blue3", "darkmagenta", "hotpink1")) +
      labs(title = 'Top 10 Songs', x = element_blank(), y = 'Plays') +
      scale_x_discrete(breaks = c('2017-01', '2018-01', '2019-01', '2020-01', '2021-01', '2022-01'),
                       labels = 2017:2022) +
      theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
            axis.title = element_text(family = 'questrial', color = 'white', size = 35),
            axis.text.y = element_text(family = 'questrial', color = 'white', size = 20),
            axis.text.x = element_text(family = 'questrial', color = 'white', size = 20, angle = 45, vjust = 1, hjust = 1),
            legend.title = element_text(family = 'questrial', color = 'white', size = 30),
            legend.text = element_text(family = 'questrial', color = 'white', size = 15),
            legend.key.size = unit(0.5, 'cm'),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = '#121212'),
            panel.grid.minor = element_line(color = '#121212'),
            plot.background = element_rect(fill = '#121212', colour = '#121212'),
            legend.key = element_rect(fill = '#121212'),
            legend.background = element_rect(fill = '#121212')
      ) -> plot
      ggsave(filename = paste0('www/charts/homeCumTopSongsChart', person, '.png'), plot, width = 2000, height = 1000, units = 'px')
    
  })
  lapply(personChoices, function(person) {
    
    
    topArtists <- switch(person,
                         'DM' = topArtistsDM,
                         'AN' = topArtistsAN,
                         'AK' = topArtistsAK)
    
    topArtists %>%
      head(10) -> topArtists
    
    df <- switch(person,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    inner_join(df, topArtists[,'Artist']) -> df
    
    df %>%
      arrange(Date) %>%
      mutate(Plays = 1) %>%
      group_by(Artist, MonthYear) %>%
      summarise(CumPlays = sum(Plays)) %>%
      as.data.frame() %>%
      group_by(Artist) %>%
      mutate(CumPlays = cumsum(CumPlays)) %>%
      ungroup() -> df
    
    df %>%
      ggplot(aes(x = MonthYear, y = CumPlays, group = Artist, color = Artist)) +
      geom_line(linewidth = 0.75) +
      scale_color_manual(values = c("brown", "red", "darkgoldenrod3", "darkolivegreen3", "chartreuse4",
                                    "cyan3", "cornflowerblue", "blue3", "darkmagenta", "hotpink1")) +
      labs(title = 'Top 10 Artists', x = element_blank(), y = 'Plays') +
      scale_x_discrete(breaks = c('2017-01', '2018-01', '2019-01', '2020-01', '2021-01', '2022-01'),
                       labels = 2017:2022) +
      theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
            axis.title = element_text(family = 'questrial', color = 'white', size = 35),
            axis.text.y = element_text(family = 'questrial', color = 'white', size = 20),
            axis.text.x = element_text(family = 'questrial', color = 'white', size = 20, angle = 45, vjust = 1, hjust = 1),
            legend.title = element_text(family = 'questrial', color = 'white', size = 30),
            legend.text = element_text(family = 'questrial', color = 'white', size = 15),
            legend.key.size = unit(0.5, 'cm'),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = '#121212'),
            panel.grid.minor = element_line(color = '#121212'),
            plot.background = element_rect(fill = '#121212', colour = '#121212'),
            legend.key = element_rect(fill = '#121212'),
            legend.background = element_rect(fill = '#121212')
      ) -> plot
    ggsave(filename = paste0('www/charts/homeCumTopArtistsChart', person, '.png'), plot, width = 2000, height = 1000, units = 'px')
    
  })
  lapply(personChoices, function(person) {
    
    
    topAlbums <- switch(person,
                        'DM' = topAlbumsDM,
                        'AN' = topAlbumsAN,
                        'AK' = topAlbumsAK)
    
    topAlbums %>%
      head(10) -> topAlbums
    
    df <- switch(person,
                 'DM' = dataDM,
                 'AN' = dataAN,
                 'AK' = dataAK)
    
    inner_join(df, topAlbums[,c('Artist', 'Album')]) -> df
    
    df %>%
      arrange(Date) %>%
      mutate(Plays = 1) %>%
      group_by(Artist, Album, MonthYear) %>%
      summarise(CumPlays = sum(Plays)) %>%
      as.data.frame() %>%
      group_by(Artist, Album) %>%
      mutate(CumPlays = cumsum(CumPlays)) %>%
      ungroup() -> df
    
    df %>%
      ggplot(aes(x = MonthYear, y = CumPlays, group = Album, color = Album)) +
      geom_line(linewidth = 0.75) +
      scale_color_manual(values = c("brown", "red", "darkgoldenrod3", "darkolivegreen3", "chartreuse4",
                                    "cyan3", "cornflowerblue", "blue3", "darkmagenta", "hotpink1")) +
      labs(title = 'Top 10 Albums', x = element_blank(), y = 'Plays') +
      scale_x_discrete(breaks = c('2017-01', '2018-01', '2019-01', '2020-01', '2021-01', '2022-01'),
                       labels = 2017:2022) +
      theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
            axis.title = element_text(family = 'questrial', color = 'white', size = 35),
            axis.text.y = element_text(family = 'questrial', color = 'white', size = 20),
            axis.text.x = element_text(family = 'questrial', color = 'white', size = 20, angle = 45, vjust = 1, hjust = 1),
            legend.title = element_text(family = 'questrial', color = 'white', size = 30),
            legend.text = element_text(family = 'questrial', color = 'white', size = 15),
            legend.key.size = unit(0.5, 'cm'),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = '#121212'),
            panel.grid.minor = element_line(color = '#121212'),
            plot.background = element_rect(fill = '#121212', colour = '#121212'),
            legend.key = element_rect(fill = '#121212'),
            legend.background = element_rect(fill = '#121212')
      ) -> plot
    ggsave(filename = paste0('www/charts/homeCumTopAlbumsChart', person, '.png'), plot, width = 2000, height = 1000, units = 'px')
    
  })
  
  # Emotional quadrant charts for the splits panel
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('years', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(split, function(j) {
      type <- j$Year[1]
      j %>%
        left_join(ids, by = c('Song' = 'Name')) %>%
        filter(!is.na(id)) %>%
        mutate(ID = id) %>%
        head(100) %>%
        select(ID, Year, Song, Plays) %>%
        inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
        ggplot(aes(valence, energy, color = Plays)) +
        geom_point(size = 1.5) +
        geom_vline(xintercept = 0.5, color = 'white') +
        geom_hline(yintercept = 0.5, color = 'white') +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_color_gradient(low = '#8072FA', high = '#fac472') +
        annotate('text', 0.25 / 2, 0.95, label = "Angry", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.95, label = "Happy", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.05, label = "Peaceful", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 0.25 / 2, 0.05, label = "Depressing", size = 15, family = 'questrial', color = 'white') +
        labs(title= 'Emotional Quadrant for Top 100 Songs', x= "Valence", y= "Energy") +
        theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
              axis.title = element_text(family = 'questrial', color = 'white', size = 35),
              axis.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.title = element_text(family = 'questrial', color = 'white', size = 35),
              legend.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.key.size = unit(1.0, 'cm'),
              panel.background = element_blank(),
              panel.grid.major = element_line(color = '#121212'),
              panel.grid.minor = element_line(color = '#121212'),
              plot.background = element_rect(fill = '#121212', colour = '#121212'),
              legend.background = element_rect(fill = '#121212')
        ) -> plot
      ggsave(filename = paste0('www/charts/splitsYearEmotionalQuadrantChart', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
      })
  })
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('seasons', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(split, function(j) {
      type <- j$Season[1]
      j %>%
        left_join(ids, by = c('Song' = 'Name')) %>%
        filter(!is.na(id)) %>%
        mutate(ID = id) %>%
        head(100) %>%
        select(ID, Season, Song, Plays) %>%
        inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
        ggplot(aes(valence, energy, color = Plays)) +
        geom_point(size = 1.5) +
        geom_vline(xintercept = 0.5, color = 'white') +
        geom_hline(yintercept = 0.5, color = 'white') +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_color_gradient(low = '#8072FA', high = '#fac472') +
        annotate('text', 0.25 / 2, 0.95, label = "Angry", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.95, label = "Happy", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.05, label = "Peaceful", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 0.25 / 2, 0.05, label = "Depressing", size = 15, family = 'questrial', color = 'white') +
        labs(title= 'Emotional Quadrant for Top 100 Songs', x= "Valence", y= "Energy") +
        theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
              axis.title = element_text(family = 'questrial', color = 'white', size = 35),
              axis.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.title = element_text(family = 'questrial', color = 'white', size = 35),
              legend.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.key.size = unit(1.0, 'cm'),
              panel.background = element_blank(),
              panel.grid.major = element_line(color = '#121212'),
              panel.grid.minor = element_line(color = '#121212'),
              plot.background = element_rect(fill = '#121212', colour = '#121212'),
              legend.background = element_rect(fill = '#121212')
        ) -> plot
      ggsave(filename = paste0('www/charts/splitsSeasonEmotionalQuadrantChart', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
    })
  })
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('months', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(split, function(j) {
      type <- j$Month[1]
      j %>%
        left_join(ids, by = c('Song' = 'Name')) %>%
        filter(!is.na(id)) %>%
        mutate(ID = id) %>%
        head(50) %>%
        select(ID, Month, Song, Plays) %>%
        inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
        ggplot(aes(valence, energy, color = Plays)) +
        geom_point(size = 1.5) +
        geom_vline(xintercept = 0.5, color = 'white') +
        geom_hline(yintercept = 0.5, color = 'white') +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_color_gradient(low = '#8072FA', high = '#fac472') +
        annotate('text', 0.25 / 2, 0.95, label = "Angry", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.95, label = "Happy", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.05, label = "Peaceful", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 0.25 / 2, 0.05, label = "Depressing", size = 15, family = 'questrial', color = 'white') +
        labs(title= 'Emotional Quadrant for Top 100 Songs', x= "Valence", y= "Energy") +
        theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
              axis.title = element_text(family = 'questrial', color = 'white', size = 35),
              axis.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.title = element_text(family = 'questrial', color = 'white', size = 35),
              legend.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.key.size = unit(1.0, 'cm'),
              panel.background = element_blank(),
              panel.grid.major = element_line(color = '#121212'),
              panel.grid.minor = element_line(color = '#121212'),
              plot.background = element_rect(fill = '#121212', colour = '#121212'),
              legend.background = element_rect(fill = '#121212')
        ) -> plot
      ggsave(filename = paste0('www/charts/splitsMonthEmotionalQuadrantChart', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
    })
  })
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('monthyears', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(split, function(j) {
      type <- j$MonthYear[1]
      j %>%
        left_join(ids, by = c('Song' = 'Name')) %>%
        filter(!is.na(id)) %>%
        mutate(ID = id) %>%
        head(25) %>%
        select(ID, MonthYear, Song, Plays) %>%
        inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
        ggplot(aes(valence, energy, color = Plays)) +
        geom_point(size = 1.5) +
        geom_vline(xintercept = 0.5, color = 'white') +
        geom_hline(yintercept = 0.5, color = 'white') +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_color_gradient(low = '#8072FA', high = '#fac472') +
        annotate('text', 0.25 / 2, 0.95, label = "Angry", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.95, label = "Happy", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.05, label = "Peaceful", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 0.25 / 2, 0.05, label = "Depressing", size = 15, family = 'questrial', color = 'white') +
        labs(title= 'Emotional Quadrant for Top 100 Songs', x= "Valence", y= "Energy") +
        theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
              axis.title = element_text(family = 'questrial', color = 'white', size = 35),
              axis.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.title = element_text(family = 'questrial', color = 'white', size = 35),
              legend.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.key.size = unit(1.0, 'cm'),
              panel.background = element_blank(),
              panel.grid.major = element_line(color = '#121212'),
              panel.grid.minor = element_line(color = '#121212'),
              plot.background = element_rect(fill = '#121212', colour = '#121212'),
              legend.background = element_rect(fill = '#121212')
        ) -> plot
      ggsave(filename = paste0('www/charts/splitsMonthyearEmotionalQuadrantChart', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
    })
  })
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('schoolperiods', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(split, function(j) {
      type <- j$SchoolPeriod[1]
      j %>%
        left_join(ids, by = c('Song' = 'Name')) %>%
        filter(!is.na(id)) %>%
        mutate(ID = id) %>%
        head(50) %>%
        select(ID, SchoolPeriod, Song, Plays) %>%
        inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
        ggplot(aes(valence, energy, color = Plays)) +
        geom_point(size = 1.5) +
        geom_vline(xintercept = 0.5, color = 'white') +
        geom_hline(yintercept = 0.5, color = 'white') +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_color_gradient(low = '#8072FA', high = '#fac472') +
        annotate('text', 0.25 / 2, 0.95, label = "Angry", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.95, label = "Happy", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.05, label = "Peaceful", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 0.25 / 2, 0.05, label = "Depressing", size = 15, family = 'questrial', color = 'white') +
        labs(title= 'Emotional Quadrant for Top 100 Songs', x= "Valence", y= "Energy") +
        theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
              axis.title = element_text(family = 'questrial', color = 'white', size = 35),
              axis.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.title = element_text(family = 'questrial', color = 'white', size = 35),
              legend.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.key.size = unit(1.0, 'cm'),
              panel.background = element_blank(),
              panel.grid.major = element_line(color = '#121212'),
              panel.grid.minor = element_line(color = '#121212'),
              plot.background = element_rect(fill = '#121212', colour = '#121212'),
              legend.background = element_rect(fill = '#121212')
        ) -> plot
      ggsave(filename = paste0('www/charts/splitsSchoolperiodEmotionalQuadrantChart', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
    })
  })
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('hours', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(split, function(j) {
      type <- j$Hour[1]
      j %>%
        left_join(ids, by = c('Song' = 'Name')) %>%
        filter(!is.na(id)) %>%
        mutate(ID = id) %>%
        head(50) %>%
        select(ID, Hour, Song, Plays) %>%
        inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
        ggplot(aes(valence, energy, color = Plays)) +
        geom_point(size = 1.5) +
        geom_vline(xintercept = 0.5, color = 'white') +
        geom_hline(yintercept = 0.5, color = 'white') +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_color_gradient(low = '#8072FA', high = '#fac472') +
        annotate('text', 0.25 / 2, 0.95, label = "Angry", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.95, label = "Happy", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 1.75 / 2, 0.05, label = "Peaceful", size = 15, family = 'questrial', color = 'white') +
        annotate('text', 0.25 / 2, 0.05, label = "Depressing", size = 15, family = 'questrial', color = 'white') +
        labs(title= 'Emotional Quadrant for Top 100 Songs', x= "Valence", y= "Energy") +
        theme(plot.title = element_text(family = 'questrial', color = 'white', size = 50, hjust = 0.5),
              axis.title = element_text(family = 'questrial', color = 'white', size = 35),
              axis.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.title = element_text(family = 'questrial', color = 'white', size = 35),
              legend.text = element_text(family = 'questrial', color = 'white', size = 20),
              legend.key.size = unit(1.0, 'cm'),
              panel.background = element_blank(),
              panel.grid.major = element_line(color = '#121212'),
              panel.grid.minor = element_line(color = '#121212'),
              plot.background = element_rect(fill = '#121212', colour = '#121212'),
              legend.background = element_rect(fill = '#121212')
        ) -> plot
      ggsave(filename = paste0('www/charts/splitsHourEmotionalQuadrantChart', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
    })
  })
  
  # Audio feature histograms for the splits panel
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('years', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(names(afColors), function (j) {
      lapply(split, function(k) {
        type <- k$Year[1]
        k %>%
          left_join(ids, by = c('Song' = 'Name')) %>%
          filter(!is.na(id)) %>%
          mutate(ID = id) %>%
          head(100) %>%
          select(ID, Year, Song, Plays) %>%
          inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
          ggplot(aes(eval(parse(text = j)), fill = afColors[j])) +
          geom_density() +
          xlim(0, 1) +
          geom_vline(aes(xintercept = mean(eval(parse(text = j)))), linetype = 'dashed', color = 'white') +
          geom_text(aes(x = mean(eval(parse(text = j))) + .1, y = 2.5, label = paste('Average:', round(mean(eval(parse(text = j))), 3)), 
                        family = 'questrial'), color = 'white', size = 7.5) +
          labs(title= element_blank(), x= element_blank(), y= element_blank()) +
          theme(axis.text.x = element_text(family = 'questrial', color = 'white', size = 20),
                axis.text.y = element_blank(),
                legend.position = 'none',
                panel.background = element_blank(),
                panel.grid.major = element_line(color = '#121212'),
                panel.grid.minor = element_line(color = '#121212'),
                plot.background = element_rect(fill = '#121212', colour = '#121212')
          ) -> plot
        ggsave(filename = paste0('www/charts/splitsYear', str_to_title(j), 'Histogram', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
      })
    })
  })
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('seasons', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(names(afColors), function (j) {
      lapply(split, function(k) {
        type <- k$Season[1]
        k %>%
          left_join(ids, by = c('Song' = 'Name')) %>%
          filter(!is.na(id)) %>%
          mutate(ID = id) %>%
          head(100) %>%
          select(ID, Season, Song, Plays) %>%
          inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
          ggplot(aes(eval(parse(text = j)), fill = afColors[j])) +
          geom_density() +
          xlim(0, 1) +
          geom_vline(aes(xintercept = mean(eval(parse(text = j)))), linetype = 'dashed', color = 'white') +
          geom_text(aes(x = mean(eval(parse(text = j))) + .1, y = 2.5, label = paste('Average:', round(mean(eval(parse(text = j))), 3)), 
                        family = 'questrial'), color = 'white', size = 7.5) +
          labs(title= element_blank(), x= element_blank(), y= element_blank()) +
          theme(axis.text.x = element_text(family = 'questrial', color = 'white', size = 20),
                axis.text.y = element_blank(),
                legend.position = 'none',
                panel.background = element_blank(),
                panel.grid.major = element_line(color = '#121212'),
                panel.grid.minor = element_line(color = '#121212'),
                plot.background = element_rect(fill = '#121212', colour = '#121212')
          ) -> plot
        ggsave(filename = paste0('www/charts/splitsSeason', str_to_title(j), 'Histogram', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
      })
    })
  })
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('months', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(names(afColors), function (j) {
      lapply(split, function(k) {
        type <- k$Month[1]
        k %>%
          left_join(ids, by = c('Song' = 'Name')) %>%
          filter(!is.na(id)) %>%
          mutate(ID = id) %>%
          head(100) %>%
          select(ID, Month, Song, Plays) %>%
          inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
          ggplot(aes(eval(parse(text = j)), fill = afColors[j])) +
          geom_density() +
          xlim(0, 1) +
          geom_vline(aes(xintercept = mean(eval(parse(text = j)))), linetype = 'dashed', color = 'white') +
          geom_text(aes(x = mean(eval(parse(text = j))) + .1, y = 2.5, label = paste('Average:', round(mean(eval(parse(text = j))), 3)), 
                        family = 'questrial'), color = 'white', size = 7.5) +
          labs(title= element_blank(), x= element_blank(), y= element_blank()) +
          theme(axis.text.x = element_text(family = 'questrial', color = 'white', size = 20),
                axis.text.y = element_blank(),
                legend.position = 'none',
                panel.background = element_blank(),
                panel.grid.major = element_line(color = '#121212'),
                panel.grid.minor = element_line(color = '#121212'),
                plot.background = element_rect(fill = '#121212', colour = '#121212')
          ) -> plot
        ggsave(filename = paste0('www/charts/splitsMonth', str_to_title(j), 'Histogram', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
      })
    })
  })
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('monthyears', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(names(afColors), function (j) {
      lapply(split, function(k) {
        type <- k$MonthYear[1]
        k %>%
          left_join(ids, by = c('Song' = 'Name')) %>%
          filter(!is.na(id)) %>%
          mutate(ID = id) %>%
          head(100) %>%
          select(ID, MonthYear, Song, Plays) %>%
          inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
          ggplot(aes(eval(parse(text = j)), fill = afColors[j])) +
          geom_density() +
          xlim(0, 1) +
          geom_vline(aes(xintercept = mean(eval(parse(text = j)))), linetype = 'dashed', color = 'white') +
          geom_text(aes(x = mean(eval(parse(text = j))) + .1, y = 2.5, label = paste('Average:', round(mean(eval(parse(text = j))), 3)), 
                        family = 'questrial'), color = 'white', size = 7.5) +
          labs(title= element_blank(), x= element_blank(), y= element_blank()) +
          theme(axis.text.x = element_text(family = 'questrial', color = 'white', size = 20),
                axis.text.y = element_blank(),
                legend.position = 'none',
                panel.background = element_blank(),
                panel.grid.major = element_line(color = '#121212'),
                panel.grid.minor = element_line(color = '#121212'),
                plot.background = element_rect(fill = '#121212', colour = '#121212')
          ) -> plot
        ggsave(filename = paste0('www/charts/splitsMonthyear', str_to_title(j), 'Histogram', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
      })
    })
  })
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('schoolperiods', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(names(afColors), function (j) {
      lapply(split, function(k) {
        type <- k$SchoolPeriod[1]
        k %>%
          left_join(ids, by = c('Song' = 'Name')) %>%
          filter(!is.na(id)) %>%
          mutate(ID = id) %>%
          head(100) %>%
          select(ID, SchoolPeriod, Song, Plays) %>%
          inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
          ggplot(aes(eval(parse(text = j)), fill = afColors[j])) +
          geom_density() +
          xlim(0, 1) +
          geom_vline(aes(xintercept = mean(eval(parse(text = j)))), linetype = 'dashed', color = 'white') +
          geom_text(aes(x = mean(eval(parse(text = j))) + .1, y = 2.5, label = paste('Average:', round(mean(eval(parse(text = j))), 3)), 
                        family = 'questrial'), color = 'white', size = 7.5) +
          labs(title= element_blank(), x= element_blank(), y= element_blank()) +
          theme(axis.text.x = element_text(family = 'questrial', color = 'white', size = 20),
                axis.text.y = element_blank(),
                legend.position = 'none',
                panel.background = element_blank(),
                panel.grid.major = element_line(color = '#121212'),
                panel.grid.minor = element_line(color = '#121212'),
                plot.background = element_rect(fill = '#121212', colour = '#121212')
          ) -> plot
        ggsave(filename = paste0('www/charts/splitsSchoolperiod', str_to_title(j), 'Histogram', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
      })
    })
  })
  lapply(personChoices, function(i) {
    eval(parse(text = paste0('hours', i)))[[1]] -> split
    eval(parse(text = paste0('idList', i))) -> ids
    eval(parse(text = paste0('audioFeats', i))) -> afs
    lapply(names(afColors), function (j) {
      lapply(split, function(k) {
        type <- k$Hour[1]
        k %>%
          left_join(ids, by = c('Song' = 'Name')) %>%
          filter(!is.na(id)) %>%
          mutate(ID = id) %>%
          head(100) %>%
          select(ID, Hour, Song, Plays) %>%
          inner_join(afs, by = c('ID' = 'id'), multiple = 'first') %>%
          ggplot(aes(eval(parse(text = j)), fill = afColors[j])) +
          geom_density() +
          xlim(0, 1) +
          geom_vline(aes(xintercept = mean(eval(parse(text = j)))), linetype = 'dashed', color = 'white') +
          geom_text(aes(x = mean(eval(parse(text = j))) + .1, y = 2.5, label = paste('Average:', round(mean(eval(parse(text = j))), 3)), 
                        family = 'questrial'), color = 'white', size = 7.5) +
          labs(title= element_blank(), x= element_blank(), y= element_blank()) +
          theme(axis.text.x = element_text(family = 'questrial', color = 'white', size = 20),
                axis.text.y = element_blank(),
                legend.position = 'none',
                panel.background = element_blank(),
                panel.grid.major = element_line(color = '#121212'),
                panel.grid.minor = element_line(color = '#121212'),
                plot.background = element_rect(fill = '#121212', colour = '#121212')
          ) -> plot
        ggsave(filename = paste0('www/charts/splitsHour', str_to_title(j), 'Histogram', i, type, '.png'), plot, width = 2000, height = 1000, units = 'px')
      })
    })
  })
  
}

# More chart generating functions
# Charts for song breakdown panel
songTimeBarGraph <- function(artist, name, person) {
  
  df <- switch(person,
               'DM' = dataDM,
               'AN' = dataAN,
               'AK' = dataAK)
  
  df %>%
    filter(Artist == artist & Song == name) %>%
    group_by(Hour) %>%
    summarise(Count = n()) -> songData
  
  songData %>%
    right_join(
      data.frame(
        Hour = hourList$val
      )
    ) %>%
    mutate(Count = ifelse(is.na(Count), 0, Count)) -> songData
  
  ggplot(songData, aes(Hour, Count, fill = '#72ECFA')) +
    coord_polar(theta = 'x', start = -0.1308996939, direction = 1) +
    geom_bar(stat = 'identity') +
    scale_x_discrete(breaks = hourList$val, labels = hourList$label) +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(family = 'questrial', color = 'white', size = 28),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = '#444444'),
          plot.background = element_rect(fill = '#121212', colour = '#121212'),
          legend.position = 'none'
          ) -> plot
  
  return(plot)

}
songAudioFeatSpiderChart <- function(artist, name, person) {
  
  df <- switch(person,
               'DM' = topSongsDM,
               'AN' = topSongsAN,
               'AK' = topSongsAK)
  
  af <- switch(person,
               'DM' = audioFeatsDM,
               'AN' = audioFeatsAN,
               'AK' = audioFeatsAK)
  
  df %>%
    filter(Artist == artist & Song == name) %>%
    as.data.frame() %>%
    inner_join(af, by = c('ID' = 'id'), multiple = 'first') %>%
    select(danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence) -> df
  
  names(df) <- str_to_title(names(df))
  
  df %>%
    pivot_longer(
      cols = c(Danceability, Energy, Speechiness, Acousticness, Instrumentalness, Liveness, Valence)
    ) -> df

  df %>%
    ggplot(aes(factor(name, level = c('Acousticness', 'Energy', 'Liveness', 'Instrumentalness', 'Danceability', 'Valence', 'Speechiness')), 
               value, fill = '#8072FA')) +
    coord_polar(theta = 'x', start = 0, direction = 1) +
    geom_bar(stat = 'identity') +
    ylim(-0.25, 1) +
    scale_fill_manual(values = '#8072FA') +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(family = 'questrial', color = 'white', size = 28),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = '#444444'),
          plot.background = element_rect(fill = '#121212', colour = '#121212'),
          legend.position = 'none'
          #plot.margin=unit(c(0,5,0,5), "cm")
    ) -> plot
  
  return(plot)
  
}
songYearlyChart <- function(artist, name, person) {
  
  df <- switch(person,
               'DM' = dataDM,
               'AN' = dataAN,
               'AK' = dataAK)
  
  df %>%
    filter(Artist == artist & Song == name) -> df
  
  df %>%
    ggplot(aes(Year)) +
    geom_bar(fill = '#C472FA') +
    scale_x_continuous(breaks = 2016:2022, labels = 2016:2022, limits = c(2015.5, 2022.5)) +
    labs(y = 'Plays') +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(family = 'questrial', color = 'white', size = 55),
      axis.text = element_text(family = 'questrial', color = 'white', size = 40),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = '#121212'),
      panel.grid.minor = element_line(color = '#121212'),
      plot.background = element_rect(fill = '#121212', colour = '#121212'),
      legend.key = element_rect(fill = '#121212'),
      legend.background = element_rect(fill = '#121212')
    )
  
}
songCumChart <- function(artist, name, person) {
  
  df <- switch(person,
               'DM' = dataDM,
               'AN' = dataAN,
               'AK' = dataAK)
  
  df %>%
    filter(Artist == artist & Song == name) %>%
    arrange(Date) %>%
    mutate(Plays = 1) %>%
    select(MonthYear, Plays) %>%
    right_join(data.frame(MonthYear = monthyearList)) %>%
    mutate(Plays = ifelse(is.na(Plays), 0, Plays)) %>%
    group_by(MonthYear) %>%
    summarise(CumPlays = sum(Plays)) %>%
    as.data.frame() %>%
    mutate(CumPlays = cumsum(CumPlays)) %>%
    ungroup() -> df
  
  df %>%
    ggplot(aes(x = MonthYear, y = CumPlays)) +
    geom_line(group = 1, color = '#A8FA72', linewidth = 2) +
    labs(y = 'Total Plays') +
    scale_x_discrete(breaks = c('2016-01', '2017-01', '2018-01', '2019-01', '2020-01', '2021-01', '2022-01'),
                     labels = 2016:2022) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(family = 'questrial', color = 'white', size = 55),
      axis.text = element_text(family = 'questrial', color = 'white', size = 40),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = '#121212'),
      panel.grid.minor = element_line(color = '#121212'),
      plot.background = element_rect(fill = '#121212', colour = '#121212'),
      legend.key = element_rect(fill = '#121212'),
      legend.background = element_rect(fill = '#121212')
    ) %>%
    return()
  
}

# Charts for artist breakdown panel
artistTimeBarGraph <- function(artist, person) {
  
  df <- switch(person,
               'DM' = dataDM,
               'AN' = dataAN,
               'AK' = dataAK)
  
  df %>%
    filter(Artist == artist) %>%
    group_by(Hour) %>%
    summarise(Count = n()) -> artistData
  
  artistData %>%
    right_join(
      data.frame(
        Hour = hourList$val
      )
    ) %>%
    mutate(Count = ifelse(is.na(Count), 0, Count)) -> artistData
  
  ggplot(artistData, aes(Hour, Count, fill = '#72ECFA')) +
    coord_polar(theta = 'x', start = -0.1308996939, direction = 1) +
    geom_bar(stat = 'identity') +
    scale_x_discrete(breaks = hourList$val, labels = hourList$label) +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(family = 'questrial', color = 'white', size = 28),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = '#444444'),
          plot.background = element_rect(fill = '#121212', colour = '#121212'),
          legend.position = 'none'
    ) -> plot
  
  return(plot)
  
}
artistCumChart <- function(artist, person) {
  
  df <- switch(person,
               'DM' = dataDM,
               'AN' = dataAN,
               'AK' = dataAK)
  
  df %>%
    filter(Artist == artist) %>%
    arrange(Date) %>%
    mutate(Plays = 1) %>%
    select(MonthYear, Plays) %>%
    right_join(data.frame(MonthYear = monthyearList)) %>%
    mutate(Plays = ifelse(is.na(Plays), 0, Plays)) %>%
    group_by(MonthYear) %>%
    summarise(CumPlays = sum(Plays)) %>%
    as.data.frame() %>%
    mutate(CumPlays = cumsum(CumPlays)) %>%
    ungroup() -> df
  
  df %>%
    ggplot(aes(x = MonthYear, y = CumPlays)) +
    geom_line(group = 1, color = '#A8FA72', linewidth = 2) +
    labs(y = 'Total Plays') +
    scale_x_discrete(breaks = c('2016-01', '2017-01', '2018-01', '2019-01', '2020-01', '2021-01', '2022-01'),
                     labels = 2016:2022) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(family = 'questrial', color = 'white', size = 55),
      axis.text = element_text(family = 'questrial', color = 'white', size = 40),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = '#121212'),
      panel.grid.minor = element_line(color = '#121212'),
      plot.background = element_rect(fill = '#121212', colour = '#121212'),
      legend.key = element_rect(fill = '#121212'),
      legend.background = element_rect(fill = '#121212')
    ) %>%
    return()
  
}
artistTopSongsChart <- function(artist, person) {
  
  df <- switch(person,
               'DM' = dataDM,
               'AN' = dataAN,
               'AK' = dataAK)
  
  df %>%
    filter(Artist == artist) %>%
    group_by(Song) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(10) %>%
    as.data.frame() %>%
    select(Song) %>%
    merge(monthyearList) %>%
    rename('MonthYear' = 'y') -> top10
  
  df %>%
    filter(Artist == artist) %>%
    filter(Song %in% top10$Song) %>%
    mutate(Plays = 1) %>%
    select(Song, MonthYear, Plays) %>%
    right_join(top10) %>%
    mutate(Plays = ifelse(is.na(Plays), 0, Plays)) %>%
    group_by(Song, MonthYear) %>%
    summarise(CumPlays = sum(Plays)) %>%
    as.data.frame() %>%
    group_by(Song) %>%
    mutate(CumPlays = cumsum(CumPlays)) %>%
    ungroup() %>%
    filter(!is.na(Song)) -> df
  
  df %>%
    ggplot(aes(x = MonthYear, y = CumPlays, group = Song, color = Song)) +
    geom_line(linewidth = 0.75) +
    scale_color_manual(values = c("brown", "red", "darkgoldenrod3", "darkolivegreen3", "chartreuse4",
                                  "cyan3", "cornflowerblue", "blue3", "darkmagenta", "hotpink1")) +
    labs(y = 'Plays') +
    scale_x_discrete(breaks = c('2016-01', '2017-01', '2018-01', '2019-01', '2020-01', '2021-01', '2022-01'),
                     labels = 2016:2022) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(family = 'questrial', color = 'white', size = 55),
          axis.text = element_text(family = 'questrial', color = 'white', size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = '#121212'),
          panel.grid.minor = element_line(color = '#121212'),
          plot.background = element_rect(fill = '#121212', colour = '#121212'),
          legend.title = element_blank(),
          legend.text = element_text(family = 'questrial', color = 'white', size = 35),
          legend.key.size = unit(0.5, 'cm'),
          legend.key = element_rect(fill = '#121212'),
          legend.background = element_rect(fill = '#121212')
    ) %>%
    return()
  
}
artistStackChart <- function(artist, person) {
  
  df <- switch(person,
               'DM' = dataDM,
               'AN' = dataAN,
               'AK' = dataAK)
  
  df %>%
    filter(Artist == artist) %>%
    filter(Album != '') %>%
    filter(!grepl(Album, pattern = 'Single')) %>%
    group_by(Album) %>%
    filter(n() > 25) %>%
    ungroup() -> artistData
  
  ggplot(artistData, aes(x = Year, fill = Album)) +
    geom_bar(position="stack") +
    scale_x_continuous(breaks = 2016:2022, labels = 2016:2022, limits = c(2015.5, 2022.5)) +
    labs(y = 'Plays') +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(family = 'questrial', color = 'white', size = 55),
          axis.text = element_text(family = 'questrial', color = 'white', size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = '#121212'),
          panel.grid.minor = element_line(color = '#121212'),
          plot.background = element_rect(fill = '#121212', colour = '#121212'),
          legend.title = element_blank(),
          legend.text = element_text(family = 'questrial', color = 'white', size = 35),
          legend.key.size = unit(0.5, 'cm'),
          legend.key = element_rect(fill = '#121212'),
          legend.background = element_rect(fill = '#121212')
          ) %>%
    return()
  
}

# Charts for album breakdown panel
albumTimeBarGraph <- function(artist, name, person) {
  
  df <- switch(person,
               'DM' = dataDM,
               'AN' = dataAN,
               'AK' = dataAK)
  
  df %>%
    filter(Artist == artist & Album == name) %>%
    group_by(Hour) %>%
    summarise(Count = n()) -> albumData
  
  albumData %>%
    right_join(
      data.frame(
        Hour = hourList$val
      )
    ) %>%
    mutate(Count = ifelse(is.na(Count), 0, Count)) -> albumData
  
  ggplot(albumData, aes(Hour, Count, fill = '#72ECFA')) +
    coord_polar(theta = 'x', start = -0.1308996939, direction = 1) +
    geom_bar(stat = 'identity') +
    scale_x_discrete(breaks = hourList$val, labels = hourList$label) +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(family = 'questrial', color = 'white', size = 28),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = '#444444'),
          plot.background = element_rect(fill = '#121212', colour = '#121212'),
          legend.position = 'none'
    ) -> plot
  
  return(plot)
  
}
albumCumChart <- function(artist, name, person) {
  
  df <- switch(person,
               'DM' = dataDM,
               'AN' = dataAN,
               'AK' = dataAK)
  
  df %>%
    filter(Artist == artist & Album == name) %>%
    arrange(Date) %>%
    mutate(Plays = 1) %>%
    select(MonthYear, Plays) %>%
    right_join(data.frame(MonthYear = monthyearList)) %>%
    mutate(Plays = ifelse(is.na(Plays), 0, Plays)) %>%
    group_by(MonthYear) %>%
    summarise(CumPlays = sum(Plays)) %>%
    as.data.frame() %>%
    mutate(CumPlays = cumsum(CumPlays)) %>%
    ungroup() -> df
  
  df %>%
    ggplot(aes(x = MonthYear, y = CumPlays)) +
    geom_line(group = 1, color = '#A8FA72', linewidth = 2) +
    labs(y = 'Total Plays') +
    scale_x_discrete(breaks = c('2016-01', '2017-01', '2018-01', '2019-01', '2020-01', '2021-01', '2022-01'),
                     labels = 2016:2022) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(family = 'questrial', color = 'white', size = 55),
      axis.text = element_text(family = 'questrial', color = 'white', size = 40),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = '#121212'),
      panel.grid.minor = element_line(color = '#121212'),
      plot.background = element_rect(fill = '#121212', colour = '#121212'),
      legend.key = element_rect(fill = '#121212'),
      legend.background = element_rect(fill = '#121212')
    ) %>%
    return()
  
}
albumTopSongsChart <- function(artist, name, person) {
  
  df <- switch(person,
               'DM' = dataDM,
               'AN' = dataAN,
               'AK' = dataAK)
  
  df %>%
    filter(Artist == artist & Album == name) %>%
    group_by(Song) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(10) %>%
    as.data.frame() %>%
    select(Song) %>%
    merge(monthyearList) %>%
    rename('MonthYear' = 'y') -> top10
  
  df %>%
    filter(Artist == artist & Album == name) %>%
    filter(Song %in% top10$Song) %>%
    mutate(Plays = 1) %>%
    select(Song, MonthYear, Plays) %>%
    right_join(top10) %>%
    mutate(Plays = ifelse(is.na(Plays), 0, Plays)) %>%
    group_by(Song, MonthYear) %>%
    summarise(CumPlays = sum(Plays)) %>%
    as.data.frame() %>%
    group_by(Song) %>%
    mutate(CumPlays = cumsum(CumPlays)) %>%
    ungroup() %>%
    filter(!is.na(Song)) -> df
  
  df %>%
    ggplot(aes(x = MonthYear, y = CumPlays, group = Song, color = Song)) +
    geom_line(linewidth = 0.75) +
    scale_color_manual(values = c("brown", "red", "darkgoldenrod3", "darkolivegreen3", "chartreuse4",
                                  "cyan3", "cornflowerblue", "blue3", "darkmagenta", "hotpink1")) +
    labs(y = 'Plays') +
    scale_x_discrete(breaks = c('2016-01', '2017-01', '2018-01', '2019-01', '2020-01', '2021-01', '2022-01'),
                     labels = 2016:2022) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(family = 'questrial', color = 'white', size = 55),
          axis.text = element_text(family = 'questrial', color = 'white', size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = '#121212'),
          panel.grid.minor = element_line(color = '#121212'),
          plot.background = element_rect(fill = '#121212', colour = '#121212'),
          legend.title = element_blank(),
          legend.text = element_text(family = 'questrial', color = 'white', size = 35),
          legend.key.size = unit(0.5, 'cm'),
          legend.key = element_rect(fill = '#121212'),
          legend.background = element_rect(fill = '#121212')
    ) %>%
    return()
  
}
