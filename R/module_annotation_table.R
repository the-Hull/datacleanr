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





        table_dat <- dplyr::bind_cols(table_dat,
                                      sel_points$df[ ,c(".annotation", "selection_count")])


#
#         table_dat <- dplyr::left_join(table_dat,
#                                       sel_points$df[ , c("keys", ".annotation", "selection_count")],
#                                       by = c(".dcrkey" = "keys"))



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
    # columns2hide <- c(grep("[.]dcr", colnames(table_dat)),
                      # grep("selection_count", colnames(table_dat)))
    columns2hide <- c( grep(".dcrindex", colnames(table_dat)),
                      grep("selection_count", colnames(table_dat)))
    columns2sort <- grep("selection_count", colnames(table_dat))





        output$dtannotation <- DT::renderDT(table_dat,
                                            options = list(scrollX = TRUE,
                                                           columnDefs = list(list(visible=FALSE,
                                                                                  targets=columns2hide)),
                                                           order = list(list(columns2sort, "desc"))))
                                            # editable = list(target = "column")
                                            # )




}
