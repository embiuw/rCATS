#' \code{readDiary} Reading csv file of CATS diary data
#' @param filename Name of file to be read (can be \code{file.choose()} to interactivelo select file)
#' @details Reads standard diary data collected by CATS data loggers, saved as a csv file
#' @returns Returns a tibble with all data stored in file
#' @family CATS data reading and manipulation functions
#' @author Martin Biuw
#' @examples
#' dat <- readDiary(file.choose())
#'
#' @imports tidyverse
#' @imports readr
#' #' @export

readDiary <- function(filename=file.choose()) {
  require(tidyverse)
  diary <- readr::read_csv(filename)
  names(diary) <- unlist(lapply(names(diary), function(x) {
    paste(unlist(strsplit(x, ' '))[c(1,2)], collapse='_')
  }))
  names(diary) <- gsub('_NA', '', names(diary))
  names(diary) <- gsub('(', '', names(diary), fixed=T)
  names(diary) <- gsub(')', '', names(diary), fixed=T)
  names(diary) <- gsub('[', '', names(diary), fixed=T)
  names(diary) <- gsub(']', '', names(diary), fixed=T)
  names(diary) <- gsub('.', '', names(diary), fixed=T)
  
  diary$DT_UTC <- as.POSIXct(strptime(paste(diary$Date_UTC, 
                                            diary$Time_UTC), '%d.%m.%Y %H:%M:%S'),
                             tz='UTC')
  sampFreq <- round(length(diary$DT_UTC)/length(unique(diary$DT_UTC)))
  diary 
}


#' \code{readGps} Reading csv file of CATS gps data
#' @param filename Name of file to be read (can be \code{file.choose()} to interactivelo select file)
#' @details Reads standard gps data collected by CATS data loggers, saved as a csv file
#' @returns Returns a tibble with all data stored in file
#' @family CATS data reading and manipulation functions
#' @author Martin Biuw
#' @examples
#' gps <- readGPS(file.choose())
#'
#' @imports tidyverse
#' @imports readr
#' @export

readGPS <- function(filename=file.choose()) {
  require(tidyverse)
  gps <- readr::read_csv(filename)
  names(gps) <- unlist(lapply(names(gps), function(x) {
    paste(unlist(strsplit(x, ' '))[c(1,2)], collapse='_')
  }))
  names(gps) <- gsub('_NA', '', names(gps))
  names(gps) <- gsub('(', '', names(gps), fixed=T)
  names(gps) <- gsub(')', '', names(gps), fixed=T)
  
  gps$DT_UTC <- as.POSIXct(strptime(paste(gps$Date_GPS, 
                                            gps$Time_GPS), '%d.%m.%Y %H:%M:%S'),
                             tz='UTC')
  gps
}


readCATS <- function(folder='choose', 
                     pars=c('Accelerometer_X', 'Accelerometer_Y', 'Accelerometer_Z', 'Gyroscope_X', 'Gyroscope_Y', 'Gyroscope_Z', 'Depth'),
                     date.format=NA, plotting=T) {
  
  if(folder=='choose') {
    cats.dir <- rstudioapi::selectDirectory()
  } else {
    cats.dir <- setwd(folder)
  }
  
  
  contents <- dir(cats.dir)
  
  FileName <- paste(cats.dir, contents[grep('.txt', dir(cats.dir))], sep='\\')
  metadata <- readLines(FileName)
  
  StartTime <- metadata[grep('first_entry', metadata)]
  StartTZ <- unlist(strsplit(StartTime, '(', fixed=T))[2]
  StartTZ <- gsub(')', '', StartTZ, fixed=T)
  
  StartTime <- gsub('first_entry=', '', StartTime)
  StartTime <- gsub(paste(' (', StartTZ, ')', sep=''), '', StartTime, fixed=T)
  StartTime <- as.POSIXct(strptime(StartTime, '%d.%m.%Y %H:%M:%S'), tz=StartTZ)

  EndTime <- metadata[grep('last_entry', metadata)]
  EndTime <- gsub('last_entry=', '', EndTime)
  EndTime <- gsub(paste(' (', StartTZ, ')', sep=''), '', EndTime, fixed=T)
  EndTime <- as.POSIXct(strptime(EndTime, '%d.%m.%Y %H:%M:%S'), tz=StartTZ)
  
  activated <- match('[activated sensors]', metadata)+1
  activated <- c(activated:length(metadata))
  
  parnames <-metadata[activated][grep('name=', metadata[activated])]
  parnames <- unlist(lapply(parnames, function(x) unlist(strsplit(x, '='))[2]))

  temps <- agrep('Temp', parnames)
  if(length(temps)>1) temps <- setdiff(temps, agrep('(depth)', parnames))
  if(length(temps)>0) parnames <- parnames[-temps]
  
  parnames <- unlist(lapply(parnames, function(x) unlist(strsplit(x, ' (', fixed=T))[1]))
  parnames <- unlist(lapply(parnames, function(x) unlist(gsub('.', '', x, fixed=T))[1]))
  parnames <- unlist(lapply(parnames, function(x) unlist(gsub(' ', '_', x, fixed=T))[1]))
  
  intervals <- metadata[activated][grep('interval=', metadata[activated])]
  intervals <- as.numeric(unlist(lapply(intervals, function(x) unlist(strsplit(x, '='))[2])))
  
  skip.pars <- unlist(lapply(c('GPS', 'Light_intensity', 'Hydrophone'), function(x) grep(x, parnames)))
  
  if(length(skip.pars)>0) {
    parnames <- parnames[-skip.pars]
    intervals <- intervals[-skip.pars]
  }
  
  intervals <- intervals[c(1:length(parnames))]
    
  max.samplerate <- max(intervals)
  
  main.header <- list(FileName=FileName,
                      StartTime=StartTime,
                      EndTime=EndTime)
  
  
  diary.files <- contents[grep('.csv', contents)]
  if(length(grep('gps', diary.files))>0) diary.files <- diary.files[-grep('gps', diary.files)]
  
  diary <- readDiary(paste(cats.dir, diary.files[1], sep='/'))
  varnames <- readLines(paste(cats.dir, diary.files[1], sep='/'), n=1)
  varnames <- unlist(strsplit(varnames, ','))
  varnames <- varnames[unlist(lapply(parnames, function(x) agrep(x, varnames)))]
  varnames <- unique(varnames)
  
  units <- unlist(lapply(varnames, function(x) tail(unlist(strsplit(x, ' ')), 1)))
  units <- unique(units[grep('[', units, fixed=T)])
  
  data.list <- list()
  
  for(d in 1:length(diary.files)) {
    if(d>1)   diary <- readDiary(paste(cats.dir, diary.files[d], sep='/'))
    for(i in 1:length(parnames)) {
      vars <- grep(parnames[i], varnames)
      unit <- units[i]
      for(j in 1:length(vars)) {
        if(length(vars)==3) {
          varname <- paste(parnames[i], c('X', 'Y', 'Z')[j], sep='_')
        } else {
          varname <- varnames[grep(parnames[i], varnames)]
          varname <- names(diary)[which.min(adist(varname, names(diary)))]
        }  
        varnum <- match(varname, names(diary))
        varname <- gsub('Accelerometer', 'Acceleration', varname)
        subsample <- (max.samplerate/intervals)[i]
        var.header <- list(as.character(unit), 
                           as.character(nrow(diary)/subsample),
                           as.character(1/intervals[i]),
                           as.character(vars[j]))
        names(var.header) <- c('Units:',
                               'Data size:',
                               'Interval (sec):',
                               'Colour:')
        data <- diary[[varnum]]
        data <- data[seq(1, length(data), by=subsample)]
        
        if(d==1) {
          data.list[[vars[j]]] <- list(header=var.header,
                                       data=data)
          names(data.list)[vars[j]] <- varname
        } else {
          data.list[[vars[j]]]$data <- c(data.list[[vars[j]]]$data, data)
        }  
      }
    }
  }
  
  for(i in 1:length(data.list)) {
    data.list[[i]]$header[[2]] <- length(data.list[[i]]$data)
  }
  list(main.header=main.header, data=data.list)
}
