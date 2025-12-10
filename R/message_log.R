#' Title
#' @description
#' Generate a diagnostic message from its arguments just like
#'  [message](library/base/help/warning) except that it logs it to a file in
#'  the directory "logs" that will have the PID number of the process
#'  and extension warning.log,
#'  e.g. NNNNNNN_warning.log
#'
#' @param ...  characters to add to the message
#' @param isWarning if TRUE then a warning message is created and both a
#' warning.log and message.log file is updated.
#'
#' @returns files names with pid and warning.log and message.log.
#' @export
#'
#' @examples
#' # message_log("my nice ", "message in file with PID=", Sys.getpid())
message_log <- function(..., isWarning=F) {
  # Write to logfile
  logdir <- "logs"
  ts <- get_cache("timeCounterSteps")
  if(is.null(ts)){
    set_cache("timeCounterSteps", Sys.time())
    ts <- get_cache("timeCounterSteps")
  }
  tsnow <- Sys.time()
  elapsed <- difftime(tsnow ,ts)

  bullet <- "\U0001F7E2 "

  if(isWarning) bullet <- "\U0001F534 "


  if(!dir.exists(logdir)) dir.create(logdir)
  logfile <-  file.path(logdir, paste0(Sys.getpid(),"_Message.log.html"))
  warnfile <- file.path(logdir, paste0(Sys.getpid(),"_Warning.log.html"))

  if( !file.exists(logfile) ){
    cat("<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'><title>Log</title><style>body{font-family:Segoe UI,Roboto,Helvetica,Arial,sans-serif;background:#121212;color:#e0e0e0;padding:20px;}h1{color:#8ab4f8;}pre{background:#1e1e1e;padding:15px;border-radius:6px;box-shadow:0 0 5px rgba(0,0,0,0.6);white-space:pre-wrap;word-wrap:break-word;font-family:Consolas,'Courier New',monospace;}</style></head><body><h1>Log File</h1><div class='timestamp'>LOG Generated: ",  format(Sys.time(), "%A, %d %B %Y %H:%M:%S"), "</div>", file=logfile)
  }

  if( !file.exists(warnfile) ){
    cat("<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'><title>Log</title><style>body{font-family:Segoe UI,Roboto,Helvetica,Arial,sans-serif;background:#121212;color:#e0e0e0;padding:20px;}h1{color:#8ab4f8;}pre{background:#1e1e1e;padding:15px;border-radius:6px;box-shadow:0 0 5px rgba(0,0,0,0.6);white-space:pre-wrap;word-wrap:break-word;font-family:Consolas,'Courier New',monospace;}</style></head><body><h1>Log File</h1><div class='timestamp'>LOG Generated: ",  format(Sys.time(), "%A, %d %B %Y %H:%M:%S"), "</div>", file=warnfile)
  }

  cat("<pre> ",bullet, format(Sys.time(), "%Y-%m-%d %H:%M:%S: "), "-", paste(..., concatenate=" "),
      "</pre>\n",
      file = logfile ,
      append = TRUE)

  if(isWarning) {
    cat("<pre> ", bullet, format(Sys.time(), "%Y-%m-%d %H:%M:%S: "), "-",
        paste(..., concatenate=" "), "</pre>\n", file = warnfile, append = TRUE)

    message(bullet,cli::col_cyan(format(Sys.time(), "%Y-%m-%d %H:%M:%S - "),
                             # " elapsed: ",
                             # format(round(elapsed, 3)),
                             cli::style_bold("WARNING: ")) , ...)
  } else {
    message(bullet, cli::col_green(format(Sys.time(), "%Y-%m-%d %H:%M:%S - ")
                           # " elapsed: ",
                           # format(round(elapsed, 3))
                           ) ,cli::col_black(...))
  }

}



