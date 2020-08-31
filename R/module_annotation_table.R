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
#' @param dframe df used for plotting
#' @param sel_points numeric, vector of .dcrkeys selected in plot
#'
#' @return df with .dcrkeys and annotations
#'
module_server_plot_annotation_table <- function(input, output, session, dframe, sel_points){
    ns = session$ns



    # table_dat <- df$df$data
    # table_dat$.annotation <- ""

    # note_vec <- character(length(sel_points$df$keys))


    # table_dat <- shiny::reactiveVal(cbind(df$df$data[df$df$data$.dcrkey %in% sel_points$df$keys, ],
    # ".annotation" =  character(length(sel_points$df$keys))))
    # table_dat <- shiny::isolate(shiny::reactiveVal(cbind(df$df$data[df$df$data$.dcrkey %in% sel_points$df$keys, ],
    #                                       ".annotation" =  character(length(sel_points$df$keys)))))


    # str(df$df$data)
    # str(table_dat)

    # table_dat <- dframe()[dframe()$.dcrkey %in% sel_points$df$keys, ]

    # use index to subset data from original df!
    table_dat <- dframe[as.numeric(sel_points$df$keys), ]

    # table_dat$.annotation <-

    if(length(sel_points$df$keys) >= 1){
        table_dat <- dplyr::left_join(table_dat,
                                      sel_points$df[ , c("keys", ".annotation", "selection_count")],
                                      by = c(".dcrkey" = "keys"))
    } else {

        table_dat <- cbind(dplyr::ungroup(table_dat[0, ]),
                           sel_points$df[0, -grep("keys",
                                                  colnames(sel_points$df)
                                                  )
                                         ]
                           )
    }
    # if(length(sel_points$df$keys) < 1){
    #     table_dat$.annotation <- character(0)
    # }
    # if(length(sel_points$df$keys) >= 1){
    #     table_dat$.annotation <- ""
    # }


    # disable_cols <- which(colnames(table_dat) != ".annotation")
    #print(paste("disable", disable_cols))


    # columns2hide <- base::match(".dcrkey", colnames(table_dat))
    columns2hide <- c(grep("[.]dcr", colnames(table_dat)),
                      grep("selection_count", colnames(table_dat)))
    columns2sort <- grep("selection_count", colnames(table_dat))





        output$dtannotation <- DT::renderDT(table_dat,
                                            options = list(columnDefs = list(list(visible=FALSE,
                                                                                  targets=columns2hide)),
                                                           order = list(list(columns2sort, "desc"))))
                                            # editable = list(target = "column")
                                            # )



    # cell_edit <- shiny::reactiveVal()
    # edit all cells
    # shiny::observeEvent(input$dtannotation_cell_edit, {
    #
    #
    #
    #     if(length(sel_points$df$keys) > 0){
    #         table_dat <<- DT::editData(table_dat, input$dtannotation_cell_edit, 'dtannotation')
    #         # table_dat(DT::editData(shiny::isolate(table_dat()), input$dtannotation_cell_edit, 'dtannotation'))
    #         print("table was edited")
    #
    #         # print(table_dat)
    #         # print(head(table_dat()))
    #         #print(annotations)
    #
    #     }
    # })


    # if(nrow(table_dat) > 0){
    #
    #     annotations <- table_dat[ ,c(".dcrkey", ".annotation")]
    #     print("testing")
    #
    # }



    # shiny::observeEvent(
    #     # plotly::event_data("plotly_doubleclick", source = "scatterselect", priority = "input")
    #     plotly::event_data("plotly_deselect", source = "scatterselect", priority = "input")
    #     # 1
    #     # 1}, {
    # , {
    #
    #
    #     print("some stuff!")
    #     # req(table_dat)
    #     if(nrow(table_dat) >= 1){
    #         table_dat$.annotation <<- ""
    #     } else if(nrow(table_dat) < 1){
    #         table_dat$.annotation <<- character(0)
    #
    #     }
    #
    #     # table_dat<<- table_dat[FALSE, ]
    #
    #
    #     print("oddly here")
    #
    # })

    # annotations <- shiny::reactiveVal({table_dat[ ,c(".dcrkey", ".annotation")]})
    # #print(annotations())



    # return(annotations)


    # clear on dbl click
    # shiny::observeEvent(plotly::event_data("plotly_doubleclick", source = "scatterselect", priority = "input"),
    #                     {
    #                         req(nrow(sel_points$df) > 0)
    #                         print("CLEARED TABLE VALS")
    #                         drop_ind <- which(sel_points$df$selection_count == max(sel_points$df$selection_count, na.rm = TRUE))
    #                         # get corresponding keys
    #                         keys_to_clear <- sel_points$df$keys[drop_ind]
    #                         print(keys_to_clear)
    #                         # set rows in table_dat to ""
    #                         print(table_dat$.dcrkeys %in% keys_to_clear)
    #                         table_dat$.annotation[table_dat$.dcrkeys %in% keys_to_clear] <- ""
    #
    #                     })

    # shiny::observeEvent(plotly::event_data("plotly_deselect", source = "scatterselect", priority = "event"),
    #                     {
    #
    #                         # shiny::validate(shiny::need(nrow(sel_points$df) > 0,
    #                         #                             label = "need selected data"))
    #
    #                         if(shiny::req(nrow(sel_points$df) > 0)){
    #
    #                             print("CLEARED TABLE VALS")
    #                             drop_ind <- which(sel_points$df$selection_count == max(sel_points$df$selection_count, na.rm = TRUE))
    #                             # get corresponding keys
    #                             keys_to_clear <- sel_points$df$keys[drop_ind]
    #                             print(keys_to_clear)
    #                             # set rows in table_dat to ""
    #                             print(table_dat$.dcrkey %in% keys_to_clear)
    #                             table_dat$.annotation[table_dat$.dcrkey %in% keys_to_clear] <- ""
    #
    #                             View(table_dat)
    #
    #                         }
    #
    #
    #                     })

}
