readDiary <- function(filename) {
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
 diary 
}
