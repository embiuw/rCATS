#' \code{readDiary} Reading csv file of CATS diary data
#' @param filename NAme of file to be read (can be \code{file.choose()} to interactivelo select file)
#' @details Reads standard diary data collected by CATS data loggers, saved as a csv file
#' @returns Returns a tibble with all data stored in file
#' @family CATS data reading and manipulation functions
#' @author Martin Biuw
#' @examples
#' dat <- readDiary(file.choose())
#'
#' @imports tidyverse
#' @export

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
