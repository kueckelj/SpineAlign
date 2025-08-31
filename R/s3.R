

columna <- function(){

  obj <- list(
    ap_ann = list(),
    ap_img = NULL,
    ap_img_path = NA,
    lat_ann = list(),
    lat_img = NULL,
    lat_img_path = NA,
    lat_or = NULL,
    exam = NULL,
    source = NULL
  )

  class(obj) <- "columna"

  return(obj)

}





