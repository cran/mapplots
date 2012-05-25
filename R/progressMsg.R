progressMsg <-
function (value, min=0, max=1, round = 0) 
{
    if (!is.finite(value) || value < min || value > max) 
      return()
    if(value==min) {
      assign('.t0',Sys.time(),.GlobalEnv)
      assign('.pc',0,.GlobalEnv)
    } else {
      if(!exists('.t0')) assign('.t0',Sys.time(),.GlobalEnv)
      if(!exists('.pc')) assign('.pc',0,.GlobalEnv)   
      pc <- round(100 * (value - min)/(max - min), round)
      if (pc == .pc) 
          return()
      dT <- as.numeric(difftime(Sys.time(), .t0, units = "secs"))
      rT <- (100 - pc) * dT/pc
      remaining <- ifelse(rT > 86400, paste(round(rT/86400, 
          2), "days"), ifelse(rT > 3600, paste(round(rT/3600, 
          2), "hours"), ifelse(rT > 60, paste(round(rT/60, 
          2), "minutes"), paste(round(rT, 0), "seconds"))))
      cat(paste("\r",pc, "% completed ", remaining, 
          " remaining  ", sep = ""))
      flush.console()
      .pc <<- pc
    }
    if(value==max) {
      rm('.pc','.t0',envir=.GlobalEnv)
      cat('\n')
    }
}
