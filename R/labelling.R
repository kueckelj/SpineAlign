

c_opt <- function(..., .dep = FALSE, .mult = FALSE, .req = FALSE, .sel = NA){

  x <- c(...)

  attr(x, which = "dep") <- .dep
  attr(x, which = "mult") <- .mult
  attr(x, which = "req") <- .req

  return(x)

}

labels <- c(
  "bone",
  "degeneration",
  "hardware",
  "neural_foramen"
  )

other_bones <- c("femoral_head", "os_sacrum")
vertebra_hr <- c(paste0("c", 1:7), paste0("t", 1:12), paste0("l", 1:5), paste0("s", 1:5))
vertebra_lr <- paste0("vertebra_", c("cervical", "thoracic", "lumbar", "sacral"))

labeling_system <- list(
  bone = list(
    name = list(
      choices = c("vertebra", other_bones),
      mult = FALSE
    ),
    location = list(
      choices = list(
        vertebra = c("cervical", "thoracic", "lumbar", "sacral")
      )
    ),
    status = list(
      choices = c("intact", "dysplastic", "fractured"),
      mult = FALSE
    ),
    status_type = list(
      choices = list(
        fractured = c("burst", "compression", "chance", "dislocated", "unspecified")
        ),
      mult = TRUE
    )
  ),
  degeneration = list(
    name = list(
      choices = c("sclerosis", "osteophyte", "pseudocyst"),
      req = TRUE
    )
  ),
  hardware = list(
    name = list(
      choices = c("cage", "interbody_spacer", "rod",  "pedicle_screw", "plate"),
      req = TRUE
      ),
    status = list(
      choices = c("intact", "broken"),
      req = TRUE,
      sel = "intact"
      )
  ),
  neural_foramen = NULL
)

pal <- c(
  bone                = "#39FF14",
  degeneration        = "#FFFF33",
  hardware            = "#FF1744",
  neural_foramen      = "#00F5FF"
)

make_annotation_input <- function(main, sub, input = NULL){

  id <- paste0("ann_", main, "_", sub)

  info <- labeling_system[[main]][[sub]]

  choices <-
    purrr::set_names(
      x = as.character(info),
      nm = stringr::str_to_title(info)
      )


  check_icon <- list(
    yes = icon("square-check"),
    no = icon("square")
  )

  if(attr(info, "mult")){

    shinyWidgets::checkboxGroupButtons(
      inputId = id,
      label = stringr::str_to_title(sub),
      choices = choices,
      check_icon = check_icon,
      selected = input[[id]]
    )

  } else if(!attr(info, "mult")){

    shinyWidgets::radioGroupButtons(
      inputId = id,
      label = stringr::str_to_title(sub),
      choices = choices,
      check_icon = check_icon,
      selected = if(is.null(input[[id]])) character(0) else input[[id]]
    )

  }

}


htmlAnnotationPicker <- function(main, input){



}



