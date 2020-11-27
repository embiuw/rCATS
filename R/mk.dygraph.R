mk.dygraph <- function(data=cats, dive=NA, theMain='', 
                       channels=c('Acceleration_X', 'Acceleration_Y', 'Acceleration_Z', 'Depth'),
                       filtered=F, subsample=c(10, 10, 10, 1)) {
  require(dygraphs)
  require(RColorBrewer)
  require(fields)
  require(xts)
  require(htmltools)
  
  options(digits.secs = 3)

  CustomAxisLabel <- 'function (ms) {
                          var d = new Date(ms);
  return Dygraph.zeropad(d.getHours()) + ":" +
  Dygraph.zeropad(d.getMinutes()) + ":" +
  Dygraph.zeropad(d.getSeconds());
}'
  CustomValueFormat = 'function (ms) {
  var d = new Date(ms);
  return Dygraph.zeropad(d.getHours()) + ":" +
  Dygraph.zeropad(d.getMinutes()) + ":" +
  Dygraph.zeropad(d.getSeconds()) + "." +
  Dygraph.zeropad(d.getMilliseconds());
  }'
  
  start.time <- data$main.header$StartTime

  dive.t <- start.time + (c(0:(length(data$data$Depth$data)-1)) * as.numeric(data$data$Depth$header[[3]]))
  subs <- subsample[match('Depth', channels)]
  if(subs>1) {
    subs <- seq(1, length(data$data$Depth$data), by=subs)
  } else {
    subs <- c(1:length(data$data$Depth$data))
  }  
  dive.d <- data$data$Depth$data[subs]
  dive.p <- data$data$Depth$phase[subs]
  dive.n <- data$data$Depth$dive[subs]
  d.p <- paste(dive.n, dive.p, sep='')
  d.p[which(d.p=='NANA')] <- NA
  un.dp <- unique(d.p)
  un.dp <- un.dp[which(!is.na(un.dp))]
  
  if(!is.na(dive)) {
    d.start <- dive.t[match(c(1:max(dive.n, na.rm=T)), dive.n)]
    d.end <- dive.t[rev(length(dive.n)-match(c(max(dive.n, na.rm=T):1), rev(dive.n)))+1]
    b.start <- dive.t[match(un.dp[grep('B', un.dp)], d.p)]
    a.start <- dive.t[match(un.dp[grep('A', un.dp)], d.p)]
    des.end <- dive.t[(length(d.p)-match(un.dp[grep('D', un.dp)], rev(d.p)))+1]
  } else {
    d.start <- head(dive.t, 1)
    d.end <- tail(dive.t, 1)
    b.start <- d.start
    a.start <- d.end
    des.end <- d.end
  }
  
  ts.list <- list()
  for(i in 1:length(channels)) {
    ch <- which.min(adist(channels[i], names(data$data)))
    ch.interval <- as.numeric(data$data[[ch]]$header[[3]]) 
    ch.length <- as.numeric(data$data[[ch]]$header[[2]]) 
    if(subsample[i]>1) {
      subs <- seq(1, ch.length, by=subsample[i])
    } else {
      subs <- c(1:ch.length)
    }  
    Times <- start.time+((c(1:ch.length)-1)*ch.interval)[subs]
    if(!is.na(dive)) {
      if(length(dive)==1) {
        which.dive <- which(data$data[[ch]]$dive[subs] %in% dive)
      } else {
        first.dive <- which(data$data[[ch]]$dive[subs] %in% dive[1])
        last.dive <- which(data$data[[ch]]$dive[subs] %in% tail(dive, 1))
        which.dive <- range(c(first.dive, last.dive))
      }
    } else {
      which.dive <- c(1:(length(data$data[[ch]]$data[subs])))
    }
    if(length(grep('Acceleration', channels[i]))==1) {
      if(filtered) {
        ts.list[[i]] <- as.xts(data.frame(data$data[[ch]]$Static[subs][which.dive], 
                                          data$data[[ch]]$Dynamic[subs][which.dive]), 
                            order.by=Times[which.dive])
      } else {
        ts.list[[i]] <- as.xts(data$data[[ch]]$data[subs][which.dive], order.by=Times[which.dive])
      }
    } else {
      if(length(grep('Depth', channels[i]))==1) {
        ts.list[[i]] <- as.xts(-data$data[[ch]]$data[subs][which.dive], order.by=Times[which.dive])
      } else {
        ts.list[[i]] <- as.xts(data$data[[ch]]$data[subs][which.dive], order.by=Times[which.dive])
      }  
    }  
  }  
  
  names(ts.list) <- channels
  
##  theCol <- rev(tim.colors(length(channels)))
  theCol <- brewer.pal(8, 'Dark2')[c(1:length(channels))]
  
  plot.height <- (0.95*dev.size("px")[2])/length(channels)

  add_shades <- function(x, starts, ends, ...) {
    for( i in 1:length(starts) ) {
      x <- dyShading(x, from = starts[i] , to = ends[i], ... )
    }
    x
  }
  
  dyg.text <- 'browsable(tagList('
  for(i in 1:(length(ts.list)-1)) {
    y.lab <- names(ts.list)[i]
    if(dim(ts.list[[i]])[2]>1) {
      dyg <- paste('dygraph(ts.list[[', i, 
                   ']], group="A", main=theMain, height=plot.height) %>% dyOptions(colors=c("grey", "', 
                   theCol[i], 
                   '")) %>% dyAxis("x", axisLabelFormatter=CustomAxisLabel, valueFormatter=CustomValueFormat) %>% dyAxis("y", label="',
                   y.lab, '") %>% dyLegend(show="never") %>% dyEvent(d.start) %>% dyEvent(d.end) %>% add_shades(d.start, des.end, "#B3E2CD") %>% add_shades(des.end, a.start, "#FDCDAC") %>% add_shades(a.start, d.end, "#CBD5E8"),', sep='')
      dyg.text <- paste(dyg.text, dyg, sep='')
    } else {
      dyg <- paste('dygraph(ts.list[[', i, 
                   ']], group="A", main=theMain, height=plot.height) %>% dyOptions(colors="', 
                   theCol[i], 
                   '") %>% dyAxis("x", axisLabelFormatter=CustomAxisLabel, valueFormatter=CustomValueFormat) %>% dyAxis("y", label="',
                   y.lab, '") %>% dyLegend(show="never") %>% dyEvent(d.start) %>% dyEvent(d.end) %>% add_shades(d.start, des.end, "#B3E2CD") %>% add_shades(des.end, a.start, "#FDCDAC") %>% add_shades(a.start, d.end, "#CBD5E8"),', sep='')
      dyg.text <- paste(dyg.text, dyg, sep='')
    }
  }  
  y.lab <- tail(names(ts.list),1)

  if(dim(ts.list[[length(ts.list)]])[2]>1) {
    dyg.text <- paste(dyg.text, 'dygraph(ts.list[[length(ts.list)]], group="A", main=theMain, height=plot.height) %>% dyOptions(colors=c("grey", "',
                      tail(theCol, 1), 
                      '")) %>%dyAxis("x", axisLabelFormatter=CustomAxisLabel, valueFormatter=CustomValueFormat) %>% dyAxis("y", label="',
                      y.lab, '") %>% dyLegend(show="never") %>% add_shades(d.start, des.end, "#B3E2CD") %>% dyEvent(d.start) %>% dyEvent(d.end) %>% add_shades(d.start, des.end, "#8DD3C7") %>% add_shades(des.end, a.start, "#FDCDAC") %>% add_shades(a.start, d.end, "#CBD5E8") %>% dyRangeSelector()))')
  } else {  
    dyg.text <- paste(dyg.text, 'dygraph(ts.list[[length(ts.list)]], group="A", main=theMain, height=plot.height) %>% dyOptions(colors="',
                    tail(theCol, 1), 
                    '") %>%dyAxis("x", axisLabelFormatter=CustomAxisLabel, valueFormatter=CustomValueFormat) %>% dyAxis("y", label="',
                    y.lab, '") %>% dyLegend(show="never") %>% add_shades(d.start, des.end, "#B3E2CD") %>% dyEvent(d.start) %>% dyEvent(d.end) %>% add_shades(d.start, des.end, "#8DD3C7") %>% add_shades(des.end, a.start, "#FDCDAC") %>% add_shades(a.start, d.end, "#CBD5E8") %>% dyRangeSelector()))')
  }  
  eval(parse(text=dyg.text))
}
