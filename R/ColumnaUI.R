
ColumnaUI <- function(){

  shinydashboard::dashboardPage(

    # header
    shinydashboard::dashboardHeader(
      title = shiny::HTML("Columna")
    ),

    # sidebar
    shinydashboard::dashboardSidebar(
      collapsed = FALSE,

      shinydashboard::sidebarMenu(
        id = "sidebar_menu",
        shinydashboard::menuItem(
          tabName = "tab_annotate",
          text = "Annotate",
          icon = shiny::icon("pencil")
        ),
        shiny::br(),
        shiny::br(),
        shinyFiles::shinyDirButton(
          id = "dir_source",
          label = "Choose a Folder:" ,
          title = "Please select a folder:",
          multiple = FALSE
        ),
        #shiny::actionButton("test", "Test"),
        shiny::br()
      )
    ),

    # body
    shinydashboard::dashboardBody(

      # tags
      shiny::tags$script(
        shiny::HTML("
          Shiny.addCustomMessageHandler('dt_preserve_scroll', function(id) {
            var box = $('#' + id);
            var body = box.find('.dataTables_scrollBody');
            var pos = body.scrollTop();
            // On next draw, restore
            box.find('table.dataTable').on('draw.dt.dtps', function() {
              body.scrollTop(pos);
              box.find('table.dataTable').off('draw.dt.dtps');
            });
          });
        ")
        ),
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML("
          html, body, .container-fluid { height: 100%; }

          :root { --page-bottom-pad: 7.5vh; }

          body { padding-bottom: calc(var(--page-bottom-pad) + env(safe-area-inset-bottom)); }

          .row-full { min-height: calc(100vh - var(--page-bottom-pad)); display: flex; }

          /* Make columns flex containers so children can stretch */
          .row-full > [class^='col-'] { display: flex; flex-direction: column; min-height: 0; }

          /* Keep some inner spacing, but no visible borders */
          .col-left, .col-center, .col-right { padding: 8px; }

          .col-container {
            background-color: white;
            border-radius: 5px;
            box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
            display: flex;
            flex-direction: column;
            padding: 1.5%;
            width: 100%;
            margin: 0 !important;

            /* Make it fill the column height and allow children to shrink */
            flex: 1 1 auto;
            min-height: 0;
          }

          /* Header with bottom line */
          #center_header {
            flex: 0 0 auto;
            margin-bottom: 8px;
            padding-bottom: 4px;
            border-bottom: 1px solid #ddd;
          }

          /* Top section */
          #center_top {
            flex: 0 0 80%;
            min-height: 0;
            display: flex;
            flex-direction: column;
            overflow: visible;
          }

          /* Bottom with top line */
          #center_bottom {
            flex: 0 0 20%;
            margin-top: 8px;
            display: flex;
            flex-direction: column;
            border-top: 1px solid #ddd;
            padding-top: 4px;
          }

          /* Make plots + tools rail sit side-by-side inside #center_top */
          .multiple-plots-wrapper {
            display: flex;
            align-items: stretch;
            width: 100%;
            height: 100%;
            gap: 8px;                 /* space between plot and rail */
          }

          /* Plot area takes remaining space */
          .multiple-plots {
            position: relative;
            flex: 1 1 auto;           /* <-- grow to fill */
            height: 100%;
            max-width: 100%;
            aspect-ratio: 1 / 1;      /* keep square canvas */
            margin: 0 !important;
            padding: 0 !important;
            overflow: hidden;
          }

          /* Stack all plot layers */
          .multiple-plots > * {
            position: absolute;
            inset: 0;
            width: 100%;
            height: 100%;
          }

          /* Right-side tools rail */
          .tools-rail {
            flex: 0 0 72px;           /* fixed rail width; tweak as you like */
            display: flex;
            flex-direction: column;
            justify-content: space-between;
            align-items: stretch;
            position: relative,
            overflow: visible
          }

          /* group containers inside the rail */
          .tools-rail .rail-top,
          .tools-rail .rail-bottom {
            display: flex;
            flex-direction: column;
            gap: 8px;
          }

          /* make rail buttons fill rail width neatly */
          .tools-rail .btn { width: 100%; }

          .dropdown-menu {
            min-width: max-content !important; /* grow to fit content */
          }

          /* Pencil cursor class (SVG data URL with hotspot) */
        .cursor-pencil {
              cursor: url('data:image/svg+xml;utf8,<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"32\" height=\"32\" viewBox=\"0 0 32 32\"><path fill=\"%23000\" d=\"M2 30l6-2 18-18-4-4L4 24l-2 6z\"/><path fill=\"%23ffa726\" d=\"M22 10l-4-4 2-2 4 4-2 2z\"/></svg>') 1 31, crosshair;
            }

        ")
        ),
        shiny::tags$style(
          shiny::HTML("
            /* Flash into steelblue and stay */
            @keyframes flash-steelblue {
              0%   { background-color: #a7c8e8; }
              100% { background-color: steelblue; color: white; }
            }

            .flash-steelblue {
              animation: flash-steelblue 0.8s ease forwards;
            }

            /* Revert to default (remove custom background) */
            .unflash {
              background-color: inherit !important;
              color: inherit !important;
            }
          ")
          )
      ),

      # busy spinner
      shinybusy::add_busy_spinner(spin = "cube-grid", color = "tomato", timeout = 1500),

      # java script options
      shinyjs::useShinyjs(),

      # tabs
      shinydashboard::tabItems(


        # Annotate ----------------------------------------------------------------
        shinydashboard::tabItem(
          tabName = "tab_annotate",

          shiny::fluidRow(
            class = "row-full",
            # source
            shiny::column(
              class = "col-left",
              width = 3,
              align = "left",
              shiny::div(
                class = "col-container",
                shiny::div(
                  id = "center_header",
                  shiny::h4(shiny::strong("Source"))
                ),
                shiny::div(
                  id = "center_top",
                  DT::DTOutput("exams_tbl"),
                ),
                shiny::div(
                  id = "center_bottom",
                  shiny::fluidRow(
                    shiny::column(
                      width = 12,
                      align = "center",
                      shiny::splitLayout(
                        cellWidths = "50%",
                        shiny::actionButton(inputId = "exam_prev", label = shiny::tagList(shiny::icon("arrow-left"), "Previous"), disabled = TRUE),
                        shiny::actionButton(inputId = "exam_next", label = shiny::tagList("Next", shiny::icon("arrow-right")), disabled = TRUE)
                      )
                    )
                  )
                )
              )
            ),

            # images
            shiny::column(
              class = "col-center",
              width = 7,
              align = "left",
              shiny::div(
                class = "col-container",
                # HEADER: headline row
                shiny::div(
                  id = "center_header",
                  shiny::div(
                    width = 12,
                    shiny::h4(shiny::strong("X-Ray Annotation Viewer"))
                  )
                ),
                shiny::div(
                  id = "center_top",
                  shiny::div(
                    class = "multiple-plots-wrapper",

                    # LEFT: plot stack (unchanged)
                    shiny::div(
                      class = "multiple-plots",
                      style = "background-color: white;",
                      shiny::plotOutput("plot_xray", height = "100%", width = "100%"),
                      shiny::plotOutput("plot_annotations", height = "100%", width = "100%"),
                      shiny::plotOutput("plot_drawing", height = "100%", width = "100%"),
                      shiny::plotOutput(
                        outputId = "plot_interaction",
                        height = "100%",
                        brush = shiny::brushOpts(id = "brush", delayType = "debounce", resetOnNew = TRUE),
                        dblclick = shiny::dblclickOpts(id = "dblclick"),
                        hover = shiny::hoverOpts(id = "hover", delay = 75, delayType = "throttle", clip = TRUE)
                      )
                    ),

                    # RIGHT: tools rail
                    shiny::div(
                      class = "tools-rail",

                      # Top group: zoom buttons
                      shiny::div(
                        class = "rail-top",
                        shiny::actionButton(
                          inputId = "zoom_in",
                          label = shiny::tagList(shiny::icon("magnifying-glass-plus"), "In"),
                          width = "100%", style = style_ab
                        ),
                        shiny::actionButton(
                          inputId = "zoom_out",
                          label = shiny::tagList(shiny::icon("magnifying-glass-minus"), "Out"),
                          width = "100%", style = style_ab, disabled = TRUE
                        ),
                        shinyWidgets::dropdownButton(
                          icon   = shiny::icon("gear"),
                          circle = FALSE, inline = TRUE, size = "sm", up = FALSE, width = "300px",
                          shiny::h4(shiny::strong("Contrast Adjustments:")),
                          shiny::sliderInput("quantile_clip", "Quantile Clip:", min = 0, max = 1, step = 0.01, value = c(0, 1)),
                          shiny::sliderInput("gamma", "Gamma:", min = 0.5, max = 5, step = 0.01, value = 1),
                          shiny::actionButton("reset_contrast_adjustments", "Reset")
                        )
                      )
                    )
                  )
                ),
                # BOTTOM: auto height controls
                shiny::div(
                  id = "center_bottom",
                  shiny::fluidRow(
                    shiny::column(
                      width = 3,
                      shiny::h5(shiny::strong("X-Ray View:")),
                      shinyWidgets::radioGroupButtons(
                        inputId = "xray_view", label = NULL,
                        choices = c("AP" = "ap", "LAT" = "lat"),
                        selected = "lat",
                        justified = TRUE
                      )
                    ),

                    shiny::column(
                      width = 2,
                      shiny::h5(shiny::strong("Drawing Options:")),
                      shinyWidgets::checkboxGroupButtons(
                        inputId = "drawing_opts",
                        label = NULL,
                        choices = c("Fill", "Points"),
                        selected = NA,
                        justified = TRUE,
                        width = "100%"
                      )
                    ),
                    shiny::column(
                      width = 4,
                      shiny::h5(shiny::strong("Edit Outline:")),
                      shiny::splitLayout(
                        cellWidths = "33%",
                        shiny::actionButton(
                          inputId = "poly_reset",
                          label = shiny::tagList(shiny::icon("rotate-left"), "Reset"),
                          disabled = TRUE,
                          width = "100%"
                        ),
                        shiny::actionButton(
                          inputId = "poly_backwards",
                          label = shiny::tagList(shiny::icon("step-backward"), "Backwards"),
                          disabled = TRUE,
                          width = "100%"
                        ),
                        shiny::actionButton(
                          inputId = "poly_close",
                          label = shiny::tagList(shiny::icon("check"), "Close"),
                          disabled = TRUE,
                          width = "100%"
                        )
                      )
                    ),
                    shiny::column(
                      width = 2,
                      shiny::h5(shiny::strong("Finish Outline:"), style = "visibility: hidden;"),
                      shiny::actionButton("poly_annotate", "Annotate", disabled = TRUE, width = "100%")
                    )
                  )
                )

              )
            ),

            # labels
            shiny::column(
              class = "col-right",
              width = 2,
              shiny::div(
                class = "col-container",
                shiny::div(
                  id = "center_header",
                  shiny::h4(shiny::strong("Annotations"))
                ),
                shiny::div(
                  id = "center_top",
                  style = "overflow: hidden;",
                  shiny::uiOutput(outputId = "annotation_selectors")
                ),
                shiny::div(
                  id = "center_bottom",
                  shiny::fluidRow(
                    shiny::column(
                      width = 12,
                      align = "center",
                      shiny::br(),
                      shiny::actionButton(
                        inputId = "show_masking",
                        label = "Show Masking",
                        disabled = TRUE,
                        width = "50%"
                      )
                    )
                  )
                )
              )
            )
          )

        )

      )

    )

  )

}


