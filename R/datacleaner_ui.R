

# UI ----------------------------------------------------------------------
# //-------------------------------------------------------------------------




datacleanr_ui <- function(request){

    # ns <- shiny::NS(id)


    # Panel texts ----------------------------

    text_grouping_side_panel <- shiny::tagList(shiny::h4(shiny::tags$strong("Overview")),
                                               shiny::br(),
                                               shiny::p("Select grouping variables for subsequent cleaning."),
                                               shiny::br(),
                                               shiny::p("Choose if you want to view an overview with or without grouping."))

    # text_filtering_side_panel <- shiny::tagList(
    #     # shiny::p(
    #     # shiny::tags$b("Add/Remove"),
    #     # "text boxes and add unquoted filter statements."),
    #     shiny::p(shiny::tags$b("Add/Remove"),
    #              "filter statements as necessary. These are passed to",
    #              shiny::tags$b("base::subset()"), "."),
    #     shiny::p("Use", shiny::tags$b("single quotes"), "for values of character/factor variables."),
    #     shiny::tags$p("For example, valid statements for filtering",
    #                   shiny::tags$b("iris"),
    #                   "are:"),
    #     shiny::tags$ol(
    #         shiny::tags$li(shiny::tags$small("Species == 'setosa'")),
    #         shiny::tags$li(shiny::tags$small("Species %in% c('setosa','versicolor')"))),
    #
    #     shiny::br(),
    #     shiny::p("Click",
    #              shiny::tags$b("'Apply'"),
    #              "when you're ready, and",
    #              shiny::tags$b("'Reset'"),
    #              "to start from scratch."))
    #
    #
    # text_annotate_side_panel <- shiny::tagList(
    #     shiny::p("After selecting points by",
    #              shiny::tags$b("clicking/lasso-selecting"),
    #              "the",
    #              shiny::tags$b("last selection"),
    #              "can be annotated with a text label.",
    #              shiny::br(),
    #              "These labels are collected and provided as an additional column",
    #              shiny::tags$b("'.annotation'"),
    #              "in the table to the right and outputted via the",
    #              shiny::tags$b("Extraction Tab.")),
    #     shiny::br(),
    #
    #     shiny::p("The annotation can be updated or removed by deleting all characters in the input box and clicking the button again.",
    #              "Note, that the most-recent selection can be deleted with a ",
    #              shiny::tags$b("double-click on the plot."),
    #              "This also removes the respective annotations."))
    #
    #
    # text_plot_main_panel <- shiny::tagList(
    #     shiny::p("Select at least ",
    #              shiny::tags$b("X and Y"),
    #              " variables and click",
    #              shiny::tags$b("'Plot!'."),
    #              "The",
    #              shiny::tags$b("Z"),
    #              "variable adjusts point size.",
    #              "The legend entries correspond with the rownumbers on the table to the left.",
    #              "Individual items (i.e. groups) can be",
    #              shiny::tags$b("hidden"),
    #              "by clicking on the legend.",
    #              "Double-clicking one item hides all others (speeding up selections), and a subsequent double-click displays all data."),
    #     shiny::p("Note, that",
    #              shiny::tags$b("'Plot!'"),
    #              "must be clicked after any variable input has been",
    #              shiny::tags$b("clicked or changed."),
    #              shiny::br(),
    #              shiny::br(),
    #              "To mark and exclude outliers, ",
    #              shiny::tags$b("click or lasso/box select"),
    #              "individual points.",
    #              shiny::tags$b("Double-click"),
    #              "on the plot area to remove the last selection.",
    #              shiny::br(),
    #              "The plot's",
    #              shiny::tags$b("control bar"),
    #              "allows to",
    #              shiny::tags$b("zoom and reset views")),
    #     shiny::br(),
    #
    #     shiny::p("Selected points appear in the",
    #              shiny::tags$b(" table below"),
    #              "and can be annotated with the tool to the left."))


    navbarPageWithInputs("datacleanr",
                         id = "nav",

                         # TAB GROUPING ------------
                         shiny::tabPanel("Overview & Set-up",
                                         value = "grouping",
                                         icon = shiny::icon("layer-group"),

                                         # panel set-up
                                         shiny::sidebarLayout(

                                             sidebarPanel = shiny::sidebarPanel(
                                                 text_grouping_side_panel,
                                                 module_ui_group_select(id = "group"),
                                                 module_ui_checkbox(id = "grouptick",
                                                                    cond_id = "group-groupvar"),
                                                 shiny::br(),
                                                 shiny::actionButton("gobutton",
                                                                     "Start",
                                                                     icon = shiny::icon("rocket"),
                                                                     class = "btn-info"),
                                                 width = 3
                                             ), #/sidebarPanel

                                             mainPanel = shiny::mainPanel(
                                                 module_ui_summary(id = "summary")
                                             ) #/mainPanel
                                         ) #/sidebarLayout


                         ), # /tabPanel

                         # TAB FILTERING -----------
                         shiny::tabPanel("Filtering",
                                         value = "filtering",
                                         icon = shiny::icon("sliders-h"),


                                         shiny::sidebarLayout(
                                             sidebarPanel = shiny::sidebarPanel(
                                                 shiny::h4(shiny::tags$strong("Filter Statements")),

                                                 shiny::actionLink("help-filter",
                                                                   "Click for Help",
                                                                   icon = shiny::icon("question-circle")),

                                                 shiny::br(),
                                                 shiny::br(),

                                                 shiny::actionButton("addbutton",
                                                                     "Add",
                                                                     icon = shiny::icon("plus-circle")),
                                                 shiny::actionButton("removebutton",
                                                                     "Remove",
                                                                     icon = shiny::icon("trash")),
                                                 shiny::br(),
                                                 # text_filtering_side_panel,
                                                 shiny::br(),

                                                 module_ui_df_filter("check"),

                                                 shiny::br(),

                                                 shiny::tagList( shiny::actionButton(inputId = "apply_filter",
                                                                                     label = "Apply",
                                                                                     icon = shiny::icon("check-double"),
                                                                                     class = "btn-info"),
                                                                 shiny::actionButton(inputId = "reset_filter",
                                                                                     label = "Reset",
                                                                                     icon = shiny::icon("undo"),
                                                                                     class = "btn-danger"),

                                                                 shiny::br(),
                                                                 shiny::br(),
                                                                 # shiny::hr(style = "border-top: 1px solid #A0A0A0;"),

                                                                 shiny::h4(shiny::tags$strong("Data Overview")),
                                                                 shiny::br(),
                                                                 module_ui_group_selector_table("df-filter-tab")),


                                                 width = 3
                                             ), # /sidebarLayout

                                             mainPanel = shiny::mainPanel(
                                                 shiny::textOutput('show_inputs'),###
                                                 shiny::verbatimTextOutput("outDF"),
                                                 shiny::h2("Filtering statements"),
                                                 module_ui_filter_str(1),
                                                 shiny::tags$div(id = 'placeholder')
                                             ) #/mainPanel
                                         ) #/sidebarLayout
                         ), #/tabPanel

                         # TAB VIS -----------------
                         shiny::tabPanel("Visualization",
                                         value = "visu",
                                         icon = shiny::icon("chart-area"),
                                         shiny::sidebarLayout(

                                             sidebarPanel = shiny::sidebarPanel(width = 4,
                                                                                shiny::h4(shiny::tags$strong("Data Overview")),
                                                                                shiny::br(),
                                                                                module_ui_group_selector_table("df"),

                                                                                shiny::br(),
                                                                                # shiny::hr(style = "border-top: 1px solid #A0A0A0;"),

                                                                                shiny::h4(shiny::tags$strong("Annotate last selection")),
                                                                                shiny::actionLink("help-annotator",
                                                                                                  "Click for Help",
                                                                                                  icon = shiny::icon("question-circle")),
                                                                                shiny::br(),
                                                                                module_ui_text_annotator("annotator"),

                                                                                shiny::br(),
                                                                                module_ui_histograms("plotvars")
                                             ),

                                             mainPanel = shiny::mainPanel(width = 8,
                                                                          module_ui_plot_selectorcontrols("selectors"),
                                                                          shiny::actionLink("help-plot",
                                                                                            "Click for Help",
                                                                                            icon = shiny::icon("question-circle")),
                                                                          module_ui_plot_selectable("plot"),
                                                                          module_ui_lowercontrol_btn("lwrcontrol"),
                                                                          module_ui_plot_annotation_table("dt"))
                                         ) #/sidebarLayout
                         ) #/tabPanel


                         ,
                         # TAB EXTRACT -------------
                         shiny::tabPanel("Extraction",
                                         value = "extract",
                                         icon = shiny::icon("file-export"),
                                         shiny::sidebarLayout(



                                             sidebarPanel = shiny::sidebarPanel(width = 4,
                                                                                shiny::h4(shiny::tags$strong("Reproducible Recipe")),
                                                                                shiny::br(),
                                                                                shiny::p("All commands and operations in previous tabs are translated to code on the right, ensuring reproducibility"),
                                                                                shiny::br(""),
                                                                                shiny::checkboxInput("overwrite",
                                                                                                     label = "Concise code?",
                                                                                                     value = FALSE)),

                                             mainPanel = shiny::mainPanel(width = 8,
                                                                          module_ui_extract_code("extract")))
                         ), #/tabPanel



                         # HANDLER BUTTONS TOP ------------------
                         inputs = list(shiny::actionButton("done",
                                                           "Done",
                                                           icon = shiny::icon("check-circle"),
                                                           class = "btn-success"),
                                       shiny::actionButton("cancel",
                                                           "Cancel",
                                                           icon = shiny::icon("window-close"),
                                                           class = "btn-secondary")
                                       # shiny::bookmarkButton("bookmark",
                                                             # label = "Save",
                                                             # icon = shiny::icon("save"))

                                       # miniUI::miniTitleBarButton(ns("done"),
                                       #                                      "Done",
                                       #                                      primary = TRUE),
                                       #
                                       #           miniUI::miniTitleBarCancelButton(inputId = ns("cancel"),
                                       #                                            label = "Cancel",
                                       #                                            primary = FALSE)
                                       #
                         ), #inputs




                         shiny::tags$style(type = "text/css", "body {padding-top: 70px;}"),
                         position = "fixed-top"


    ) #/navbarPageWithInputs


}
