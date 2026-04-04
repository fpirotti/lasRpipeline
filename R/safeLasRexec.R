safe_lasR_exec <- function(pipeline,
                           ctg,
                           check_every = 2,        # seconds
                           min_free_gb = 2,        # abort if below this
                           verbose = TRUE) {

  # --- helper: free memory in GB ---
  get_free_gb <- function() {
    if (.Platform$OS.type == "windows") {
      out <- system("wmic OS get FreePhysicalMemory", intern = TRUE)
      kb <- as.numeric(gsub("[^0-9]", "", out[2]))
      return((kb * 1024) / 1024^3)
    } else {
      free_kb <- as.numeric(
        system("grep MemAvailable /proc/meminfo | awk '{print $2}'", intern = TRUE)
      )
      return((free_kb * 1024) / 1024^3)
    }
  }

  aborted <- FALSE

  # lasR does not provide async execution, so we monitor in a parallel R session
  # We'll use a child process via callr

  message_log("Running lasR with memory watchdog (threshold ", min_free_gb, " GB)...", verbose=verbose)

  # run lasR::exec in a background R process
  p <- callr::r_bg(
    func = function(pipeline, ctg) {
      lasR::exec(pipeline, on = ctg)
    },
    args = list(pipeline = pipeline, ctg = ctg)
  )

  # --- watchdog loop ---
  while (p$is_alive()) {
    Sys.sleep(check_every)

    free_gb <- get_free_gb()

    message_log(" Free RAM: ", round(free_gb, 2), " GB", verbose = verbose)

    if (free_gb < min_free_gb) {
      aborted <- TRUE
      message_log("RAM dropped below threshold (", free_gb, " GB). Aborting run...", isWarning = TRUE)
      p$kill()
      break
    }
  }

  if (aborted) {
    stop("lasR execution aborted due to low memory.", call. = FALSE)
  }

  # join and return result if successful
  result <- p$get_result()
  if (verbose) message_log("lasR execution completed successfully.", verbose=verbose)
  result
}
