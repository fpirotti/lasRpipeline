normalize <- function(f, odir="outdir/norm", force=FALSE, verbose=TRUE){
  if(force || )
  pipeline <- lasR::reader() + lasR::hag() +
                               lasR::write_las(ofile = file.path(odir,".laz") )
  exec(pipeline, on = f)
}
