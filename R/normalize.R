#' normalize
#' @description
#' Adds height a above ground of points, with heuristics on existing temp files
#' in the project
#'
#' @param ifiles file or list of files to process
#' @param odir output directory, default = outdir/norm
#' @param force force rewrite of files in output
#' @param verbose verbose output messages in logs
#' @param concurrent_files default = NULL - if NULL it will calculate how many
#' concurrent LAS files to process by trying to check the RAM and number of cores
#' and divide it by the maximum size of LAS files.
#'
#' @returns list of normalized files with full path, that can be used for
#' successive processing.
#' @export
#'
#' @examples
#' # none
normalize <- function(ifiles, odir="outdir/norm", force=FALSE, verbose=TRUE,
                      concurrent_files=NULL){

  if(verbose) message_log("## Starting function ", paste(cli::style_bold(sys.call()[[1]]), collapse="", sep="") )

  files2process <- ifiles
  if(!dir.exists(odir)) {
    if(verbose) message_log("Creating output directory: ", cli::style_underline(odir) )
    dir.create(odir, recursive = TRUE)
  } else {
    files2process <- list_files_still_to_process(ifiles, odir)
  }

  if(length(files2process)==0 ) {
    if(verbose) message_log("All files processed: remove files in ",
                            cli::style_underline(odir)," if you want to reprocess this stage " )

    f1 <-  list.files(odir, pattern= get_cache("laspattern"),full.names = TRUE )

    return(f1)
  }

  if(hasHAGinfo(ifiles)){
    if (!ask_user(paste0("Height Above Ground is already available for this file - continuing will ",
                        cli::style_bold("OVERWRITE"),
                        " the existing values. Press YES if you want to continue.") )) {
      return(NULL)
    }
  }


  if(verbose) message_log("Processing: ", cli::style_underline(length(files2process)), " files." )

  pipeline <- lasR::reader() +  lasR::remove_attribute("HAG") +
                                lasR::hag() +
                                lasR::write_las(ofile = file.path(odir,"*.laz") )


  if(is.null(concurrent_files))
    concurrent_files <- ifelse(length(files2process) > lasR::half_cores(),
                             lasR::half_cores(),
                             length(files2process) )

  message_log("Executing.... if you get errors, consider decreasing the
              number of parallel files or use 'concurrent_files=1' to get
              non-multi-threaded processing")

  if(concurrent_files>1){
    lasR::set_parallel_strategy( lasR::concurrent_files(concurrent_files)  )
  } else {
    lasR::set_parallel_strategy( lasR::sequential()  )
  }

  processed <- lasR::exec(pipeline, on = files2process, progress=TRUE)

  files2process <- list_files_still_to_process(ifiles, odir)

  if(length(files2process)!=0 ) {
    if(verbose) message_log(cli::style_bold(length(files2process)), " files NOT processed:   e.g. ",
                            cli::style_underline(files2process[[1]])," - check problems... " )



    return(character(0))
  }

  f1 <-  list.files(odir, pattern= get_cache("laspattern"),full.names = TRUE )
  return(f1)
}
