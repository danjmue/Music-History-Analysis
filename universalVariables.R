# Declaring variables to be used throughout the remaining code

personList <- c('Aalaynah', 'Aditya', 'Daniel')

yearList <- 2016:2022

seasonList <- c('Spring', 'Summer', 'Fall', 'Winter')

monthList <- data.frame(
  num = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'),
  name = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
) %>%
  arrange(num)

monthyearList <- sort(do.call(paste0, expand.grid(yearList, '-', monthList$num)))

schoolperiodList <- c('8th Grade', '8th-9th Summer', '9th Grade', '9th-10th Summer', '10th Grade', '10th-11th Summer', '11th Grade',
                   '11th-12th Summer', '12th Grade', '12th-Freshman Summer', 'Freshman Year', 'Freshman-Sophomore Summer', 'Sophomore Year', 
                   'Sophomore-Junior Summer')

hourList <- data.frame(
  val = c(paste0(0, 0:9), 10:23),
  label = c(paste(c(12, 1:11), 'AM'), paste(c(12, 1:11), 'PM'))
)

personChoices <- c('AN', 'AK', 'DM')
names(personChoices) <- personList

monthChoices <- monthList$num
names(monthChoices) <- monthList$name

hourChoices <- hourList$val
names(hourChoices) <- hourList$label

splits <- list(
  as.list(yearList),
  as.list(seasonList),
  as.list(monthList$num),
  as.list(monthyearList),
  as.list(schoolperiodList),
  as.list(hourList$val)
)
names(splits) <- c('year', 'season', 'month', 'monthyear', 'schoolperiod', 'hour')

splitPrefix <- list(
  'year' = "input.splitsYearSelect",
  'season' = "input.splitsSeasonSelect",
  'month' = "input.splitsMonthSelect",
  'monthyear' = "input.splitsMonthyearySelect + '-' + input.splitsMonthyearmSelect",
  'schoolperiod' = "input.splitsSchoolperiodSelect",
  'hour' = "input.splitsHourSelect"
)

afColors <- list('#FA72A8', '#FA8072', '#FAC472', '#72FA80', '#72ECFA', '#8072FA', '#C472FA')
names(afColors) <- c('danceability', 'energy', 'liveness', 'valence', 'speechiness', 'acousticness', 'instrumentalness')
