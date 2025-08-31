
moduleAnnotationSelectorUI <- function(id){

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(
      shiny::column(
        style = "overflow: hidden;",
        width = 12,
        shiny::fluidRow(
          style = styles$row_sep_bottom,
          shiny::column(
            width = 12,
            shiny::h5(shiny::strong(paste0(make_pretty_name(id), ":"))),
            shiny::uiOutput(outputId = ns("annotations"))
          )
        )
      )
    )
  )

}


moduleAnnotationSelectorServer <- function(id, annotations = list()){

  ns <- shiny::NS(id)
  idp <- make_pretty_name(id)
  id_abb <- unname(abbreviate(idp))

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){


      output$annotations <- shiny::renderUI({

        shiny::validate(
          shiny::need(
            expr = shiny::isTruthy(choices()),
            message = glue::glue("No {idp} annotations.")
          )
        )

        shinyWidgets::checkboxGroupButtons(
          inputId = ns("annotations"),
          label = NULL,
          choices = choices(),
          direction = "horizontal",
          selected = NULL,
          justified = FALSE
        )

      })

      annotations_use <- shiny::reactive({

        purrr::keep(.x = annotations(), .p = ~ .x$ann_main == id)

        })

      choices <- shiny::reactive({

        ann_ids <- names(annotations_use())

        ann_names <-
          purrr::imap_chr(
            .x = unname(annotations_use()),
            .f = function(ann, idx){

              if(is.null(ann$ann_name)){

                paste0(id_abb, " ", idx)

              } else {

                make_pretty_name(ann$ann_name)

              }

            })

        if(length(ann_ids) != 0){

          purrr::set_names(x = ann_ids, nm = ann_names)

        } else {

          NULL

        }

      })

      module_output <- shiny::reactive({

        if(length(input$annotations) != 0){

          annotations()[input$annotations]

        } else {

          NULL

        }


      })

      return(module_output)

    }
  )

}
