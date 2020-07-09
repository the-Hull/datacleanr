# MODULE UI ----
#' UI Module: DT for annotation
#'
#' @param id Character string
#'
module_ui_plot_annotation_table <- function(id) {
    ns <- shiny::NS(id)


    shiny::tagList(
        DT::DTOutput(ns('dtannotation'))
    )

}

#------------------------------------------------------------------------------#
# MODULE SERVER ----

#' Server Module: DT for annotation

#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param df df used for plotting
#' @param sel_points numeric, vector of .dcrkeys selected in plot
#'
#' @return df with .dcrkeys and annotations
#'
module_server_plot_annotation_table <- function(input, output, session, df, sel_points){
    ns = session$ns






    table_dat <- df$df$data[df$df$data$.dcrkey %in% sel_points, ]

    if(nrow(table_dat)>0){
        table_dat$.annotation <- ""
    }


    str(table_dat)



    disable_cols <- which(colnames(table_dat) != ".annotation")
    #print(paste("disable", disable_cols))


    columns2hide <- base::match(".dcrkey", colnames(table_dat))


    shiny::observe({

        output$dtannotation <- DT::renderDT(table_dat,
                                            options = list(columnDefs = list(list(visible=FALSE, targets=columns2hide))),
                                            editable = list(target = "column",
                                                            disable = list(columns = disable_cols)))

    })

    annotations <- NULL


    # edit all cells
    shiny::observeEvent(input$dtannotation_cell_edit, {

        if(length(sel_points > 0)){
            table_dat <<- DT::editData(table_dat, input$dtannotation_cell_edit, 'dtannotation')
            #print("table was edited")
            annotations <- table_dat[ ,c(".dcrkey", ".annotation")]
            #print(annotations)

        }
    })



    shiny::observeEvent({
        # plotly::event_data("plotly_doubleclick", source = "scatterselect", priority = "event")
        plotly::event_data("plotly_deselect", source = "scatterselect", priority = "event")

        # 1}, {
    }, {

        # req(table_dat)
        if(nrow(table_dat) > 0){

            #print("cleared annotations")
            # table_dat <<- table_dat[0, ]
            table_dat$.annotation <<- ""
            #print("this is new table dat")
            #print(table_dat)

            # annotation <<- NULL

        }


    })

    # annotations <- shiny::reactiveVal({table_dat[ ,c(".dcrkey", ".annotation")]})
    # #print(annotations())



    return(annotations)





}
