
Columna <- function(){

  if(local_launch()){ shiny::addResourcePath("www", "inst/app/www") }

  shiny::shinyApp(
    ui = ColumnaUI(),
    server = function(input, output, session) {
      ColumnaServer(input, output, session)
    })

}

compute_bb <- function(poly, limits, buffer = 0.5, square = FALSE){

  bb <-
    purrr::imap(
      .x = poly,
      .f = function(var, name){

        r <- range(var)
        d <- diff(r)
        r[1] <- max(c(1, r[1]-d*buffer))
        r[2] <- min(c(r[2]+d*buffer, limits[name]))

        return(r)

      })

  if(square){

    dc <- diff(bb$col)
    dr <- diff(bb$row)

    if(dc > dr){

      mid <- max(bb$row)-dr/2
      bb$row <- c(mid-dc/2, mid+dc/2)

    } else if(dr > dc){

      mid <- max(bb$col)-dc/2
      bb$col <- c(mid-dr/2, mid+dr/2)

    }

  }

  return(bb)

}

contains_image <- function(cobj, type){

  EBImage::is.Image(cobj[[paste0(type, "_img")]])

}

create_columna <- function(path_exam){

  cobj <- columna()

  cobj$ap_img_path <- get_path_img(path_exam, type = "ap")
  cobj$lat_img_path <- get_path_img(path_exam, type = "lat")
  cobj$exam <- basename(path_exam)
  cobj$source <- dirname(path_exam)

  return(cobj)

}


df_to_img <- function(df){

  reshape2::acast(df, formula = col ~ row, value.var = "value", fill = 0) %>%
  EBImage::Image(colormode = "Grayscale")

}

disable_action_buttons <- function(prefix, suffix, value, session){

  manage_action_buttons(
    prefix = prefix,
    suffix = suffix,
    value = TRUE,
    session = session
  )

}

divide <- function(x){ x[1]/x[2] }

enable_action_buttons <- function(prefix, suffix, value, session){

  manage_action_buttons(
    prefix = prefix,
    suffix = suffix,
    value = FALSE,
    session = session
  )

}

flash_button <- function(id) {
  shinyjs::runjs(sprintf("
    var btn = $('#%s');
    btn.removeClass('flash-steelblue unflash');
    void btn[0].offsetWidth;   // restart animation
    btn.addClass('flash-steelblue');
  ", id))
}




get_image <- function(cobj, type){

  img <- cobj[[paste0(type, "_img")]]

  if(!EBImage::is.Image(img)){

    msg <- glue::glue("No {toupper(type)} image for exam {basename(cobj$exam)}.")
    if(shiny::isRunning()){

      shiny::showNotification(ui = msg, type = "error")

    } else {

      warning(msg)

    }

  }

  return(img)

}

get_path_img <- function(folder, type = c("ap", "lat"), warn = FALSE){

  type <- match.arg(type, c("ap", "lat"))

  exam <- basename(folder)
  files <- list.files(folder, full.names = TRUE)

  out <- stringr::str_subset(files, pattern = rgx[[paste0("img_", type)]])

  if(length(out) == 0){

    out <- NA

    if(warn){

      shiny::showNotification(
        ui = glue::glue("No {toupper(type)} image found for {exam}."),
        type = "warning"
      )

    }

  } else if(length(out) > 1){

    out <- out[1]

    if(warn){

      shiny::showNotification(
        ui = glue::glue("More than one {toupper(type)}-image found for {exam}. Using {basename(out)}."),
        type = "warning"
      )

    }

  }

  return(out)

}


img_limits <- function(img){

  l <- dim(img)[1:2]
  names(l) <- c("col", "row")
  return(l)

}

img_to_df <- function(img){

  reshape2::melt(
    data = as.matrix(img),
    varnames = c("col", "row"),
    value.name = "value"
    )

}



is_inside_poly <- function(cursor, poly){

  sp::point.in.polygon(
    point.x = cursor[1],
    point.y = cursor[2],
    pol.x = poly$col,
    pol.y = poly$row
  ) != 0

}

is_complete <- function(cobj){

  FALSE

}

#' @export
launchColumna <- function(){

  shiny::runApp(
    appDir = Columna(),
    launch.browser = local_launch()
    )

}

last_annotation_index <- function(annotations){

  if(length(annotations) == 0){

    out <- 0

  } else {

    out <-
      names(annotations) %>%
      stringr::str_extract(pattern = "\\d*$") %>%
      as.numeric() %>%
      max()

  }

  return(out)

}

local_launch <- function(session = NULL){

  if(!is.null(session)){

    hostname <- session$clientData$url_hostname

    grepl("localhost", hostname) || grepl("^127\\.0\\.0\\.1$", hostname)

  } else {

    dir.exists("/Users/heilandr/lab/projects/SpineAlign")

  }

}

make_pretty_name <- function(x){

  stringr::str_split(x, pattern = "_", n = stringr::str_count(x, pattern = "_")+1)[[1]] %>%
    purrr::map_chr(.f = stringr::str_to_title) %>%
    stringr::str_c(collapse = " ")

}

manage_action_buttons <- function(prefix, suffix, value, session){

  button_ids <- paste0(prefix, "_", suffix)

  for(button_id in button_ids){

    shiny::updateActionButton(
      session = session,
      inputId = button_id,
      disabled = value
    )

  }

}

mask_image <- function(img, polys){

  img_to_df(img) %>%
  mask_image_df(polys = polys) %>%
  df_to_img()

}

mask_image_df <- function(df, polys){

  in_poly <- vector(mode = "logical", length = nrow(df))
  for(poly in polys){

    in_poly <-
      sp::point.in.polygon(
        point.x = df$col,
        point.y = df$row,
        pol.x = poly$col,
        pol.y = poly$row
      ) != 0 | in_poly

  }

  df$value <- dplyr::if_else(in_poly, 1, 0)

  return(df)

}


plot_frame <- function(x = NA, y = NA, xlim, ylim, asp, type = "n", bg = NA, axes = FALSE, ...){

  par(mar = c(4, 4, 1, 1), bg = bg)

  plot(
    x = x,
    y = y,
    xlim = xlim,
    ylim = ylim,
    xaxs = "i",
    yaxs = "i",
    asp = asp,
    axes = axes,
    xlab = "",
    ylab = "",
    type = type,
    ...
  )

}

plot_xray <- function(img, xlim, ylim, asp, ...){

  plot_frame(xlim = xlim, ylim = ylim, asp = asp, ...)

  rasterImage(
    image = as.raster(img),
    xleft = 1,
    ybottom = dim(img)[2],
    xright = dim(img)[1],
    ytop = 1,
    interpolate = FALSE
  )

}

ggplot_xray <- function(df){

  ggplot2::ggplot(data = df) +
    ggplot2::geom_raster(
      mapping =
    )

}

read_columna <- function(path_exam, images = c("ap", "lat")){

  path_columna <- file.path(path_exam, "columna.RDS")

  cobj <- readRDS(path_columna)

  if("ap" %in% images){ cobj <- set_xray(cobj, type = "ap") }

  if("lat" %in% images){ cobj <- set_xray(cobj, type = "lat") }

  return(cobj)

}

set_xray <- function(cobj, type){

  img_path <- cobj[[paste0(type, "_img_path")]]

  if(is.character(img_path) & file.exists(img_path)){

    img <- read_xray(img_path)

  } else {

    msg <- glue::glue("No {toupper(type)} image for exam {cobj$exam}.")

    if(shiny::isRunning()){

      shiny::showNotification(ui = msg, type = "warning")

    } else {

      warning(msg)

    }

    img <- NULL

  }

  cobj[[paste0(type, "_img")]] <- img

  return(cobj)

}

read_xray <- function(path_img){

  if(is.na(path_img)){

    img <- NULL

  } else if(!stringr::str_detect(path_img, pattern = "\\.dcm$")){

    img  <-
      EBImage::readImage(path_img) %>%
      EBImage::channel("gray")

  } else {

    stop("DCM not supported yet.")

  }

  # 2D Image from EBImage or NULL
  return(img)

}

resource_file <- function(file){

  ifelse(local_launch(), file.path("www", file), file)

}

save_columna <- function(cobj, dir_source = NULL, rm_img = FALSE){

  if(is.null(dir_source)){ dir_source <- cobj$source }

  if(isTRUE(rm_img)){ cobj$ap_img <- NULL; cobj$lat_img <- NULL }

  saveRDS(cobj, file = file.path(dir_source, cobj$exam, "columna.RDS"))

}


showModalAnnotatePoly <- function(){

  shiny::showModal(
    ui = shiny::modalDialog(
      size = "l",
      title = "Annotate Outline",
      footer = shiny::tagList(),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          style = "border-right: 1px solid #ddd;",
          shiny::fluidRow(
            style = styles$row_sep_bottom,
            shiny::column(
              width = 12,
              shinyWidgets::radioGroupButtons(
                inputId = "ann_main",
                label = "Main Label:",
                choices = purrr::set_names(
                  x = names(labeling_system),
                  nm = purrr::map_chr(names(labeling_system), make_pretty_name)
                ),
                selected = NA
              ),
              shinyWidgets::radioGroupButtons(
                inputId = "ann_visibility",
                label = "Visibility:",
                choices = c("Clear" = "clear", "Occluded" = "occluded"),
                selected = NA
              ),
              shiny::uiOutput("help_text_main")
            )
          ),
          shiny::uiOutput("ann_name"),
          shiny::uiOutput("ann_location"),
          shiny::uiOutput("ann_status"),
          shiny::uiOutput("ann_status_type")
        ),
        shiny::column(
          width = 4,
          shiny::plotOutput(outputId = "plot_ann_poly")
        )
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(width = 3),
        shiny::column(
          width = 3,
          align = "center",
          shiny::actionButton("ann_confirm", label = "Confirm", disabled = TRUE, width = "100%")
        ),
        shiny::column(
          width = 3,
          align = "center",
          shiny::actionButton("close_modal_annotate_poly", label = "Close", width = "100%")
        ),
        shiny::column(width = 4)
      )
    )
  )

}

showModalMasking <- function(){

  shiny::showModal(
    ui = shiny::modalDialog(
      title = "Masking of Selected Annotations",
      size = "l",
      shiny::fluidRow(
        shiny::column(
          style = "border-right: 1px solid #ddd;",
          width = 6,
          shiny::h4(shiny::strong("Original X-Ray")),
          shiny::plotOutput("plot_xray_modal")
        ),
        shiny::column(
          width = 6,
          shiny::h4(shiny::strong("Masking")),
          shiny::plotOutput("plot_xray_mask")
        )
      )
    )
  )

}

showModalPolySelected <- function(name){

  shiny::showModal(
    ui = shiny::modalDialog(
      title = stringr::str_replace(name, pattern = "ann_", replacement = "Annotation "),
      shiny::verbatimTextOutput("description_poly_selected"),
      footer = shiny::fluidRow(
        shiny::column(
          width = 6,
          align = "center",
          shiny::actionButton("ann_delete", "Delete"),
        ),
        shiny::column(
          width = 6,
          align = "center",
          shiny::actionButton("close_modal_poly_selected", "Close")
        )
      )
    )
  )

}

status_exam <- function(cobj) {

  output <- c("ap" = NA, "lat" = NA)

  output["ap"] <- status_image_ann(cobj, "ap")
  output["lat"] <- status_image_ann(cobj, "lat")

  return(output)

}

status_image_ann <- function(cobj, type){

  ann <- cobj[[paste0(type, "_ann")]]
  path <- cobj[[paste0(type, "_img_path")]]

  if(is.na(path)){

    out <- "Missing"

  } else {

    if(FALSE){

      out <- "Complete"

    } else if(identical(x = ann, y = list())) {

      out <- "Empty"

    } else {

      out <- "In Progress"

    }

  }

  return(out)

}

unflash_button <- function(id) {
  shinyjs::runjs(sprintf("
    var btn = $('#%s');
    btn.removeClass('flash-steelblue');
  ", id))
}

validate_exam_folder <- function(folder){

  exam_files <- list.files(folder, full.names = FALSE)

  tests <- logical()

  tests[1] <- any(stringr::str_detect(exam_files, rgx$img_ap))
  tests[2] <- any(stringr::str_detect(exam_files, rgx$img_lat))

  any(tests)

}

zoom_in <- function(frame, fct = 0.1){

  dst <- diff(frame)*fct
  round(c(frame[1]+dst, frame[2]-dst))

}


