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
  if(!dir.exists(logdir)) dir.create(logdir)
  logfile <-  file.path(logdir, paste0(Sys.getpid(),"_Message.log"))
  warnfile <- file.path(logdir, paste0(Sys.getpid(),"_Warning.log"))

  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = logfile , append = TRUE)
  if(isWarning) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = warnfile, append = TRUE)
    warning(...)
  } else {
    message(...)
  }

}
