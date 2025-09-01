
ColumnaServer <- function(input, output, session){

  lwd <- 3.5

  # UI - Server Side --------------------------------------------------------

  roots <- c(project = ifelse(F, "/Users/heilandr/lab/projects/SpineAlign", "~"))

  # allow access to the filesystem
  shinyFiles::shinyDirChoose(
    input,
    id = "dir_source",
    roots = roots,
    session = session,
    defaultRoot = "project"
  )

  output$ann_location <- shiny::renderUI({

    shiny::req(req_location())

    choices <- labeling_system[[input$ann_main]][["location"]]$choices[[input$ann_name]]

    shiny::tagList(
      shiny::fluidRow(
        style = styles$row_sep_bottom,
        shiny::column(
          width = 12,
          shinyWidgets::radioGroupButtons(
            inputId = "ann_location",
            label = "Location:",
            choices = purrr::set_names(
              x = choices,
              nm = purrr::map_chr(choices, .f = make_pretty_name)
            ),
            selected = NA,
            justified = FALSE
          ),
          shiny::uiOutput("help_text_location")
        )
      )
    )

  })

  output$ann_name <- shiny::renderUI({

    shiny::req(req_name())

    shiny::tagList(
      shiny::fluidRow(
        style = styles$row_sep_bottom,
        shiny::column(
          width = 12,
          shinyWidgets::radioGroupButtons(
            inputId = "ann_name",
            label = "Name:",
            choices = purrr::set_names(
              x = labeling_system[[input$ann_main]][["name"]]$choices,
              nm = purrr::map_chr(labeling_system[[input$ann_main]][["name"]]$choices, .f = make_pretty_name)
            ),
            selected = NA,
            justified = FALSE
          ),
          shiny::uiOutput("help_text_name")
        )
      )
    )

  })

  output$ann_status <- shiny::renderUI({

    shiny::req(req_status())

    shiny::tagList(
      shiny::fluidRow(
        style = styles$row_sep_bottom,
        shiny::column(
          width = 12,
          shinyWidgets::radioGroupButtons(
            inputId = "ann_status",
            label = "Status:",
            choices = purrr::set_names(
              x = labeling_system[[input$ann_main]][["status"]]$choices,
              nm = purrr::map_chr(labeling_system[[input$ann_main]][["status"]]$choices, .f = make_pretty_name)
            ),
            selected = NA,
            justified = FALSE
          ),
          shiny::uiOutput("help_text_status")
        )
      )
    )

  })

  output$ann_status_type <- shiny::renderUI({

    shiny::req(req_status_type())

    choices <- labeling_system[[input$ann_main]][["status_type"]]$choices[[input$ann_status]]

    shiny::tagList(
      shiny::fluidRow(
        style = styles$row_sep_bottom,
        shiny::column(
          width = 12,
          shinyWidgets::radioGroupButtons(
            inputId = "ann_status_type",
            label = "Status Type:",
            choices = purrr::set_names(
              x = choices,
              nm = purrr::map_chr(choices, .f = make_pretty_name)
            ),
            selected = NA,
            justified = FALSE
          ),
          shiny::uiOutput("help_text_status_type")
        )
      )
    )

  })

  output$annotation_selectors <- shiny::renderUI({

    shiny::tagList(
      purrr::map(
        .x = names(labeling_system),
        .f = ~ moduleAnnotationSelectorUI(id = .x)
      )
    )

  })

  output$help_text_location <- shiny::renderUI({

    if(!shiny::isTruthy(input$ann_location)){

      shiny::helpText("Please select the location.")

    }

  })


  output$help_text_main <- shiny::renderUI({

    if(!shiny::isTruthy(input$ann_main) | !shiny::isTruthy(input$ann_visibility)){

      shiny::helpText("Please select the main label and the visibility.")

    }

  })

  output$help_text_name <- shiny::renderUI({

    if(!shiny::isTruthy(input$ann_name)){

      shiny::helpText("Please select the name.")

    }

  })

  output$help_text_status <- shiny::renderUI({

    if(!shiny::isTruthy(input$ann_status)){

      shiny::helpText("Please select the status.")

    }

  })

  output$help_text_status_type <- shiny::renderUI({

    if(!shiny::isTruthy(input$ann_status_type)){

      shiny::helpText("Please select the status type.")

    }

  })

  # Reactive Values ---------------------------------------------------------

  #asp <- shiny::reactiveVal(value = NULL)

  cobj <- shiny::reactiveVal(value = NULL)

  drawing <- shiny::reactiveVal(value = FALSE)

  dir_source <- shiny::reactiveVal(value = NULL)
  dirs_exams <- shiny::reactiveVal(value = NULL)
  names_exams <- shiny::reactiveVal(value = NULL)

  poly_hover <- shiny::reactiveVal(value = "")
  poly_modal_open <- shiny::reactiveVal(value = FALSE)
  poly_selected <- shiny::reactiveVal(value = NULL)

  ranges <- shiny::reactiveVal(value = NULL)

  selected_index <- shiny::reactiveVal(value = NULL)
  selected_exam <- shiny::reactiveVal(value = NULL)

  table_exams <- shiny::reactiveVal(value = NULL)

  xlim <- shiny::reactiveVal(value = NULL)
  ylim <- shiny::reactiveVal(value = NULL)

  # ------

  poly <- shiny::reactiveValues(
    coords = data.frame(col = numeric(), row = numeric()),
    closed = FALSE
  )

  # Module Outputs ----------------------------------------------------------

  ann_bone <-
    moduleAnnotationSelectorServer(
      id = "bone",
      annotations = shiny::reactive({ purrr::keep(annotations(), ~ .x$ann_main == "bone") })
    )

  ann_degeneration <-
    moduleAnnotationSelectorServer(
      id = "degeneration",
      annotations = shiny::reactive({ purrr::keep(annotations(), ~ .x$ann_main == "degeneration") })
    )

  ann_hardware <-
    moduleAnnotationSelectorServer(
      id = "hardware",
      annotations = shiny::reactive({ purrr::keep(annotations(), ~ .x$ann_main == "hardware") })
    )

  ann_neural_foramen <-
    moduleAnnotationSelectorServer(
      id = "neural_foramen",
      annotations = shiny::reactive({ purrr::keep(annotations(), ~ .x$ann_main == "neural_foramen") })
    )


  # Reactive (Expressions) --------------------------------------------------

  annotations <- shiny::reactive({ cobj()[[paste0(xray_view(), "_ann")]] })

  annotations_selected <- shiny::reactive({

    list(
      ann_bone(),
      ann_degeneration(),
      ann_hardware(),
      ann_neural_foramen()
    ) %>%
      purrr::map(.f = ~ purrr::discard(.x, .p = is.null)) %>%
      purrr::flatten()

    })

  asp <- shiny::reactive({ as.numeric(dist(cframe()$x)/dist(cframe()$y)) })

  # current frame
  cframe <- shiny::reactive({ shiny::req(ranges()); ranges()[[n_zoom()]] })

  cursor_pos <- shiny::reactive({ c(input$hover$x, input$hover$y) })
  cursor_rest <- shiny::debounce(r = cursor_pos, millis = 150)

  height <- shiny::reactive({ dim(xray_img()[2] )})

  min_dist <- shiny::reactive({ max(diff(cframe()$x), diff(cframe()$y))*0.01 })

  last_index <- shiny::reactive({ last_annotation_index(annotations()) })

  n_zoom <- shiny::reactive({ length(ranges()) })

  req_name <- shiny::reactive({

    (shiny::isTruthy(input$ann_main) & shiny::isTruthy(input$ann_visibility)) &&
    "name" %in% names(labeling_system[[input$ann_main]])

  })

  req_location <- shiny::reactive({

    (shiny::isTruthy(input$ann_main) & shiny::isTruthy(input$ann_visibility)) &&
    "location" %in% names(labeling_system[[input$ann_main]]) &&
    input$ann_name %in% names(labeling_system[[input$ann_main]]$location$choices)

  })

  req_status <- shiny::reactive({

    (shiny::isTruthy(input$ann_main) & shiny::isTruthy(input$ann_visibility)) &&
    shiny::isTruthy(input$ann_name) &&
    "status" %in% names(labeling_system[[input$ann_main]])

  })

  req_status_type <- shiny::reactive({

    (shiny::isTruthy(input$ann_main) & shiny::isTruthy(input$ann_visibility)) &&
    "status_type" %in% names(labeling_system[[input$ann_main]]) &&
    input$ann_status %in% names(labeling_system[[input$ann_main]]$status_type$choices)

  })

  width <- shiny::reactive({ dim(xray_img())[1] })

  xray_img <- shiny::reactive({

    shiny::req(cobj())

    EBImage::normalize(get_image(cobj(), xray_view()))

    })

  xray_img_proc <- shiny::reactive({

    EBImage::normalize(
      object = xray_img(),
      inputRange = quantile(as.numeric(xray_img()), input$quantile_clip)
      )^input$gamma

  })

  xray_view <- shiny::reactive({ input$xray_view })


  # Observers ---------------------------------------------------------------

  shiny::observe({

    tests <- vector(mode = "logical", length = 6)

    tests[1] <- shiny::isTruthy(input$ann_main)
    tests[2] <- shiny::isTruthy(input$ann_visibility)
    tests[3] <- ifelse(req_name(), shiny::isTruthy(input$ann_name), TRUE)
    tests[4] <- ifelse(req_location(), shiny::isTruthy(input$ann_location), TRUE)
    tests[5] <- ifelse(req_status(), shiny::isTruthy(input$ann_status), TRUE)
    tests[6] <- ifelse(req_status_type(), shiny::isTruthy(input$ann_status_type), TRUE)

    if(all(tests)){

      shiny::updateActionButton(
        session = session,
        inputId = "ann_confirm",
        disabled = FALSE
      )

      flash_button("ann_confirm")

    } else {

      shiny::updateActionButton(
        session = session,
        inputId = "ann_confirm",
        disabled = TRUE
      )

      unflash_button("ann_confirm")

    }

  })

  shiny::observe({

    shiny::updateActionButton(
      session = session,
      inputId = "show_masking",
      disabled = length(annotations_selected())==0
    )

  })

  # Observe Events ----------------------------------------------------------

  shiny::observeEvent(input$ann_confirm, {

    ann_req <- paste0("ann_", names(labeling_system[[input$ann_main]]))

    out <-
      list(
        poly = tibble::tibble(poly$coords),
        ann_main = input$ann_main,
        ann_visibility = input$ann_visibility,
        ann_name = if(req_name()) input$ann_name, # can be NULL
        ann_location = if(req_location()) input$ann_location, # can be NULL
        ann_status = if(req_status()) input$ann_status, # can be NULL
        ann_status_type = if(req_status_type()) input$ann_status_type # can be NULL
      )

    cobjX <- cobj()
    cobjX[[paste0(xray_view(), "_ann")]][[paste0("ann_", last_index()+1)]] <- out

    cobj({ cobjX })

    save_columna(cobj(), dir_source = dir_source(), rm_img = TRUE)

    shiny::removeModal()

    shiny::updateActionButton(
      session = session,
      inputId = "ann_confirm",
      disabled = TRUE
    )

    disable_action_buttons(
      prefix = "poly",
      suffix = c("backwards", "reset", "close", "annotate"),
      session = session
    )

    poly$coords <- empty_coords
    poly$closed <- FALSE
    unflash_button("poly_annotate")

  })

  shiny::observeEvent(input$ann_delete, {

    cobjX <- cobj()
    cobjX[[paste0(xray_view(), "_ann")]][[poly_selected()]] <- NULL
    cobj({ cobjX })

    save_columna(cobj(), dir_source = dir_source(), rm_img = TRUE)

    poly_hover({ "" })
    poly_selected({ NULL })
    poly_modal_open({ FALSE })
    shiny::removeModal()

  })

  shiny::observeEvent(input$brush, {

    shiny::req(input$brush)

    shiny::updateActionButton(
      session = session,
      inputId = "zoom_out",
      disabled = FALSE
    )

    b <- input$brush

    x1 <- max(1, b$xmin)
    x2 <- min(width(), b$xmax)
    y1 <- max(1, b$ymin)
    y2 <- min(height(), b$ymax)

    nr <- list()
    nr$x <- sort(c(x1, x2))
    nr$y <- rev(sort(c(y1, y2)))  # reversed for top-down display

    r <- ranges()
    r[[n_zoom()+1]] <- nr
    ranges({ r })

  })

  shiny::observeEvent(input$close_modal_annotate_poly, {

    shiny::removeModal()

  })

  shiny::observeEvent(input$close_modal_poly_selected, {

    poly_modal_open({ FALSE })
    poly_selected({ NULL })
    shiny::removeModal()

  })

  shiny::observeEvent(cobj(), {

    shiny::req(selected_index(), table_exams())
    idx <- selected_index()
    df <- table_exams()

    session$sendCustomMessage("dt_preserve_scroll", "exams_tbl")

    DT::replaceData(
      proxy = exams_proxy,
      data = df[, c("Name", "AP", "LAT")],
      resetPaging = TRUE,
      rownames = FALSE,
      clearSelection = "none"
    )

    DT::selectRows(exams_proxy, idx)

  })

  shiny::observeEvent(cursor_rest(), {

    shiny::req(
      !drawing() &
      length(annotations_selected()) != 0 &
      !poly_modal_open()
    )

    if(shiny::isTruthy(cursor_rest())){

      polys_hover <-
        purrr::keep(
          .x = annotations_selected(),
          .p = ~ is_inside_poly(cursor_rest(), poly = .x$poly)
        )

    } else {

      polys_hover <- list()

    }


    if(length(polys_hover) == 0){

      poly_hover({ "" })

    } else {

      poly_hover({ names(polys_hover[1]) })

    }

  })

  shiny::observeEvent(input$dblclick, {

    # Opt 1: (De-) Activate drawing
    if(!shiny::isTruthy(poly_hover())){

      if(!drawing()){

        drawing({ TRUE })
        poly$closed <- FALSE

        disable_action_buttons(
          prefix = "poly",
          suffix = c("backwards", "close", "reset"),
          session = session
        )

        shinyjs::runjs("$('#plot_interaction').addClass('cursor-pencil');")

      } else {

        drawing({ FALSE })

        if(nrow(poly$coords) > 2){

          enable_action_buttons(
            prefix = "poly",
            suffix = c("backwards", "close", "reset"),
            session = session
          )

        }

        shinyjs::runjs("$('#plot_interaction').removeClass('cursor-pencil');")

      }

      # Opt 2:
    } else {

      poly_selected({ poly_hover() })

    }

  })

  shiny::observeEvent(input$dir_source, {

    shiny::req(input$dir_source)

    dir_source({ shinyFiles::parseDirPath(roots = roots, selection = input$dir_source) })

  })

  shiny::observeEvent(dir_source(), {

    shiny::req(dir_source())

    shiny::showNotification(
      ui = "Checking source folder for valid exams.",
      type = "message",
      duration = 5
    )

    valid <-
      purrr::keep(list.files(dir_source(), full.names = TRUE), validate_exam_folder) %>%
      normalizePath()

    tbl <-
      tibble::tibble(
        path_exam = valid,
        path_columna = file.path(valid, "columna.RDS"),
        Name = basename(valid)
      )

    # check columna file status
    pb <- confuns::create_progress_bar(nrow(tbl))

    source_check <- purrr::map(
      .x = 1:nrow(tbl),
      .f = function(i){

        pb$tick()

        pe <- tbl$path_exam[i]
        pc <- tbl$path_columna[i]

        if(file.exists(pc)){

          cobj <- read_columna(pe, images = NULL)

        } else {

          cobj <- create_columna(pe)
          save_columna(cobj)

        }

        status_exam(cobj)

      }
    )

    tbl[["AP"]] <- purrr::map_chr(.x = source_check, .f = ~ .x["ap"])
    tbl[["LAT"]] <- purrr::map_chr(.x = source_check, .f = ~ .x["lat"])

    table_exams({ tbl })

    selected_index({ 1L })

    session$sendCustomMessage("dt_preserve_scroll", "exams_tbl")

    DT::replaceData(
      proxy = exams_proxy,
      data = tbl[, c("Name", "AP", "LAT")],
      resetPaging = FALSE,
      rownames = FALSE,
      clearSelection = "none"
      )

    DT::selectRows(exams_proxy, selected_index())

    shiny::updateActionButton(
      session = session,
      inputId = "exam_next",
      disabled = FALSE
    )

  })

  shiny::observeEvent(input$exam_next, {

    selected_index({ selected_index()+1 })

  })

  shiny::observeEvent(input$exam_prev, {

    selected_index({ selected_index()-1 })

  })

  shiny::observeEvent(input$exams_tbl_rows_selected, {

    idx <- as.integer(input$exams_tbl_rows_selected)

    if(!identical(x = idx, y = selected_index())){

      selected_index({ idx })

    }

  })

  shiny::observeEvent(input$hover, {

    req(drawing())
    hv <- input$hover
    append <- TRUE
    if(nrow(poly$coords) > 0){

      last <- tail(poly$coords, 1)

      if(sqrt((hv$x - last$col)^2 + (hv$y - last$row)^2) < min_dist()){

        append <- FALSE

      }
    }

    if(append){

      poly$coords <- rbind(poly$coords, data.frame(col = hv$x, row = hv$y))

    }

  })

  shiny::observeEvent(n_zoom(), {

    if(n_zoom() == 1){

      shiny::updateActionButton(
        session = session,
        inputId = "zoom_out",
        disabled = TRUE
      )

    } else {

      shiny::updateActionButton(
        session = session,
        inputId = "zoom_out",
        disabled = FALSE
      )

    }

  })

  shiny::observeEvent(input$poly_annotate, {

    showModalAnnotatePoly()

  })

  shiny::observeEvent(input$poly_backwards, {

    n_coords <- nrow(poly$coords)
    keep <- round(n_coords*0.95)
    poly$coords <- poly$coords[1:keep,]

    if(poly$closed){

      poly$closed <- FALSE
      unflash_button("poly_annotate")
      shiny::updateActionButton(
        session = session,
        inputId = "poly_annotate",
        disabled = TRUE
      )

    }

  })

  shiny::observeEvent(input$poly_close, {

    poly$closed <- TRUE

    shiny::updateActionButton(
      session = session,
      inputId = "poly_annotate",
      disabled = FALSE
    )

    shinyjs::enable("poly_annotate")
    flash_button("poly_annotate")

  })

  shiny::observeEvent(input$poly_reset, {

    poly$coords <- empty_coords

    if(poly$closed){

      poly$closed <- FALSE
      unflash_button("poly_annotate")

    }

    disable_action_buttons(
      prefix = "poly",
      suffix = c("backwards", "reset", "close", "annotate"),
      session = session
    )

  })

  shiny::observeEvent(poly_selected(), {

    showModalPolySelected(name = poly_selected())

  })

  shiny::observeEvent(input$reset_contrast_adjustments, {

    shiny::updateSliderInput(
      session = session,
      inputId = "quantile_clip",
      value = c(0,1)
    )

    shiny::updateSliderInput(
      session = session,
      inputId = "gamma",
      value = 1
    )

  })

  shiny::observeEvent(selected_index(), {

    shiny::req(table_exams(), selected_index())

    idx <- selected_index()

    if(!identical(x = idx, y = input$exams_tbl_rows_selected)){

      DT::selectRows(exams_proxy, idx)

    }

    # save old
    if(!is.null(cobj())){ save_columna(cobj(), dir_source = dir_source(), rm_img = TRUE) }

    # read in new
    cobj({

      read_columna(
        path_exam = table_exams()[["path_exam"]][idx],
        images = c("ap", "lat")
      )

    })

    # reset drawing
    drawing({ FALSE })
    poly$coords <- empty_coords
    poly$closed <- FALSE

    disable_action_buttons(
      prefix = "poly",
      suffix = c("backwards", "close", "reset", "annotate"),
      session = session
    )

    # enable/disable
    if(selected_index() == nrow(table_exams())){

      shiny::updateActionButton(
        session = session,
        inputId = "exam_next",
        disabled = TRUE
      )

    } else {

      shiny::updateActionButton(
        session = session,
        inputId = "exam_next",
        disabled = FALSE
      )

    }

    if(selected_index() == 1){

      shiny::updateActionButton(
        session = session,
        inputId = "exam_prev",
        disabled = TRUE
      )

    } else {

      shiny::updateActionButton(
        session = session,
        inputId = "exam_prev",
        disabled = FALSE
      )

    }

  })

  shiny::observeEvent(input$show_masking, {

    showModalMasking()

  })

  shiny::observeEvent(xray_img(), {

    shiny::updateSliderInput(
      session = session,
      inputId = "quantile_clip",
      value = c(0,1)
    )

    shiny::updateSliderInput(
      session = session,
      inputId = "gamma",
      value = 1
    )

    # reset zoom
    nr <- list()
    nr$x <- c(1, dim(xray_img())[1])
    nr$y <- c(dim(xray_img())[2], 1)
    ranges({ list(nr) })

  })

  shiny::observeEvent(input$zoom_in, {

    r <- ranges()
    r[[n_zoom()+1]] <- purrr::map(cframe(), zoom_in)
    ranges({ r })

  })

  shiny::observeEvent(input$zoom_out, {

    # reset zoom
    ranges({ ranges()[1:(n_zoom()-1)] })

  })

  # Outputs - Plot ----------------------------------------------------------

  output$plot_ann_poly <- shiny::renderPlot({

    shiny::req(poly$closed)

    bb <- compute_bb(poly$coords, limits = img_limits(xray_img()), square = TRUE)

    plot_xray(
      img = xray_img_proc(),
      xlim = bb$col,
      ylim = rev(sort(bb$row)),
      asp = 1
    )

    polypath(
      x = poly$coords$col,
      y = poly$coords$row,
      border = "orange",
      col = ifelse("Fill" %in% input$drawing_opts, ggplot2::alpha("orange", 0.15), NA),
      lwd = lwd
    )

  })

  output$plot_annotations <- shiny::renderPlot({

    shiny::req(annotations_selected())

    plot_frame(xlim = cframe()$x, ylim = cframe()$y, asp = asp())

    for(nm in names(annotations_selected())){

      ann <- annotations_selected()[[nm]]
      poly_highlight <- nm == poly_hover()

      color <- pal[ann$ann_main]

      polypath(
        x = ann$poly$col,
        y = ann$poly$row,
        border = color,
        col = ggplot2::alpha(color, ifelse(poly_highlight, 0.66, 0.33)),
        lwd = ifelse(poly_highlight, lwd*2, lwd)
      )

    }

  }, bg = "transparent")

  output$plot_drawing <- shiny::renderPlot({

    shiny::req(nrow(poly$coords) > 1)

    plot_frame(xlim = cframe()$x, ylim = cframe()$y, asp = asp())

    if(!poly$closed){

      lines(poly$coords$col, poly$coords$row, lwd = lwd, col = "orange")

    }

    if("Points" %in% input$drawing_opts){

      points(poly$coords$col, poly$coords$row, pch = 16, cex = lwd/3, col = "orange")

    }

    polypath(
      x = poly$coords$col,
      y = poly$coords$row,
      border = ifelse(poly$closed, "orange", NA),
      col = ifelse("Fill" %in% input$drawing_opts, ggplot2::alpha("orange", 0.15), NA),
      lwd = lwd
    )

  }, bg = "transparent")

  output$plot_interaction <- shiny::renderPlot({

    plot_frame(xlim = cframe()$x, ylim = cframe()$y, asp = asp())

  }, bg = "transparent")

  output$plot_xray <- shiny::renderPlot({

    shiny::validate(
      shiny::need(
        expr = shiny::isTruthy(cobj()),
        message = "No exam selected."
      )
    )

    shiny::validate(
      shiny::need(
        expr = xray_img_proc(),
        message = glue::glue("No {toupper(xray_view())} image.")
      )
    )

    plot_xray(
      img = xray_img_proc(),
      xlim = cframe()$x,
      ylim = cframe()$y,
      asp = asp()
      )

  })

  output$plot_xray_mask <- shiny::renderPlot({

    plot_xray(
      img = mask_image(
        img = xray_img(),
        polys = purrr::map(
          .x = annotations_selected(),
          .f = ~ .x$poly
        )
      ),
      xlim = ranges()[[1]]$x,
      ylim = ranges()[[1]]$y,
      asp = divide(xray_img())
    )

  })

  output$plot_xray_modal <- shiny::renderPlot({

    plot_xray(
      img = xray_img(),
      xlim = ranges()[[1]]$x,
      ylim = ranges()[[1]]$y,
      asp = divide(xray_img())
    )

    if(length(annotations_selected()) != 0){

      for(nm in names(annotations_selected())){

        ann <- annotations_selected()[[nm]]

        color <- pal[ann$ann_main]

        polypath(
          x = ann$poly$col,
          y = ann$poly$row,
          border = color,
          col = ggplot2::alpha(color, 0.33),
          lwd = lwd
        )

      }

    }

  })

  # Outputs - Table ---------------------------------------------------------

  output$exams_tbl <- DT::renderDT({

    shiny::validate(
      shiny::need(
        expr = shiny::isTruthy(dir_source()),
        message = "Please choose a folder."
      )
    )

    DT::datatable(
      data.frame(Name = character(), `AP` = character(), `LAT` = character()),
      escape = FALSE, rownames = FALSE,
      selection = "single",
      extensions = "Scroller",
      options = list(
        dom = "t",
        paging = TRUE,
        scrollY = "55vh", scrollCollapse = TRUE,
        deferRender = TRUE,
        scroller = TRUE,
        stateSave = TRUE,
        stateDuration = -1
      )
    )
  }, server = TRUE)

  exams_proxy <- DT::dataTableProxy("exams_tbl")


  # Outputs - Text ----------------------------------------------------------

  output$dir_source_chosen <- shiny::renderText({

    shiny::validate(
      shiny::need(
        expr = shiny::isTruthy(dir_source()),
        message = "No source folder chosen..."
      )
    )

    dir_source()

  })

  output$description_poly_selected <- shiny::renderText({

    shiny::req(poly_selected())

    purrr::keep(annotations()[[poly_selected()]], is.character) %>%
      purrr::imap_chr(.x = ., function(content, name){

        paste0(
          if(name != "ann_main") "\n",
          stringr::str_remove(name, "ann_") %>% stringr::str_to_title(),
          ": ",
          make_pretty_name(content)
        )

      }) %>%
      unname()

  })

  # Debug -------------------------------------------------------------------

  shiny::observeEvent(input$test, {

  }, ignoreInit = TRUE)

  #observe({ dir_source("data_dev/raw/BUU_LSPINE_2k") })

}



