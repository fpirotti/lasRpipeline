#' normalize
#' @description
#' Adds height a above ground of points, with heuristics on existing temp files
#' in the project
#'
#' @param f file or list of files to process
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
normalize <- function(f, odir="outdir/norm", force=FALSE, verbose=TRUE){

  if(verbose) message_log("Starting function ", cli::style_bold(sys.call()[[1]]) )

  files2process <- f
  if(!dir.exists(odir)) {
    if(verbose) message_log("Creating output directory: ", cli::style_underline(odir) )
    dir.create(odir, recursive = TRUE)
  } else {
    files2process <- list_files_still_to_process(f, odir)
  }

  if(length(files2process)==0 ) {
    if(verbose) message_log("All files processed: remove files in ",
                            cli::style_underline(odir)," to reprocess this stage " )

    f1 <-  list.files(odir, pattern= get_cache("laspattern"),full.names = TRUE )

    return(f1)
  }


  if(verbose) message_log("Processing: ", cli::style_underline(length(files2process)), " files." )

  pipeline <- lasR::reader() + lasR::hag() +
                               lasR::write_las(ofile = file.path(odir,"*.laz") )
  processed <- lasR::exec(pipeline, on = files2process, progress=TRUE)

  files2process <- list_files_still_to_process(f, odir)

  if(length(files2process)!=0 ) {
    if(verbose) message_log(cli::style_bold(length(files2process)), " files NOT processed:   e.g. ",
                            cli::style_underline(files2process[[1]])," - check problems... " )



    return(character(0))
  }

  f1 <-  list.files(odir, pattern= get_cache("laspattern"),full.names = TRUE )
  return(f1)
}
