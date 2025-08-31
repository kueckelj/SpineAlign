
empty_coords <- data.frame(col = numeric(), row = numeric())

image_suffix_opts <- c("jpg", "jpeg", "png", "tiff")

rgx <- list(
  "img_ap" = stringr::str_c(paste0("ap\\.", image_suffix_opts, "$"), collapse = "|"),
  "img_lat" = stringr::str_c(paste0("lat\\.", image_suffix_opts, "$"), collapse = "|")
)

style_ab <- "height: 33px; font-size: 14px; padding: 6px; text-align: center;"

styles <- list(
  row_sep_bottom = "border-bottom: 1px solid #ddd; padding-bottom: 8px; margin-bottom: 8px;"
)
