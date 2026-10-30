#' normalize
#' @description
#' Adds height a above ground of points, with heuristics on existing temp files
#' in the project
#'
#' @param ifiles file or list of files to process
#' @param odir output directory, default = outdir/norm
#' @param force force rewrite of files in output
#' @param verbose verbose output messages in logs
#'
#' @returns list of normalized files with full path, that can be used for
#' successive processing.
#' @export
#'
#' @examples
#' # none
normalize <- function(ifiles, odir="outdir/norm", force=FALSE, verbose=TRUE ){

   message_log("## Starting function ", paste(cli::style_bold(sys.call()[[1]]), collapse="", sep=""), verbose=verbose )

  files2process <- ifiles
  if(!dir.exists(odir)) {
    if(verbose) message_log("Creating output directory: ", cli::style_underline(odir) )
    dir.create(odir, recursive = TRUE)
  } else {
    files2process <- list_files_still_to_process(ifiles, odir)
  }

  if(length(files2process)==0 ) {
    message_log("All files processed: remove files in ",
                            cli::style_underline(odir)," if you want to reprocess this stage ", verbose=verbose )

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


  message_log("Processing: ", cli::style_underline(length(files2process)), " files.", verbose=verbose )

  pipeline <- lasR::reader() +  lasR::remove_attribute("HAG") +
                                lasR::hag() +
                                lasR::write_las(ofile = file.path(odir,"*.laz") )




  message_log("Executing.... if you get errors, consider decreasing the
              number of  files in ncores  to get
              non-multi-threaded processing", verbose=verbose)



  processed <- lasR::exec(pipeline, on = files2process )

  files2process <- list_files_still_to_process(ifiles, odir)

  if(length(files2process)!=0 ) {
     message_log(cli::style_bold(length(files2process)), " files NOT processed:   e.g. ",
                            cli::style_underline(files2process[[1]])," - check problems... ",
                 isWarning = TRUE,
                 verbose=verbose )



    return(character(0))
  }

  f1 <-  list.files(odir, pattern= get_cache("laspattern"),full.names = TRUE )

  message_log("## Finished function ", paste(cli::style_bold(sys.call()[[1]]), collapse="", sep=""), verbose=verbose )

  return(f1)
}
