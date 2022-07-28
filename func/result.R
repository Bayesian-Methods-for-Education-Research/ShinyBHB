# Result.R presents an overview of the result.
library(xtable) 

# Print the model
observeEvent(c(input$long, input$t1, input$td1, input$tf1, input$y1, input$x1, input$x2, input$select_method, input$select_mode, input$add_2, input$add_3, input$delete_2, input$delete_3), {
    if (!is.null(input$x1) && !is.null(input$y1)) {
        removeUI('#result_model > .equ', multiple = TRUE, immediate = TRUE)
        removeUI('#result_model > .level', multiple = TRUE, immediate = TRUE)
        get_model('result_model')
    }
})

output$cycle_selected <- renderUI({
    tagList(
        input$w1,
        tags$script(paste0('renderMathInElement(document.getElementById("cycle_selected"), {delimiters: [{left: "$", right: "$", display: false}]});')),
    )
})

output$ic <- renderUI({
    req(info$ic)
    digits <- if (abs(info$ic[1]) >= 1e6) 0 else 2
    tex('\\textup{mean} = ', round(info$ic[3, 1], digits), ',\\ \\textup{s.e.} = ', round(info$ic[3, 2], digits))
})

output$ic2 <- renderUI({
    req(info$ic_sch)
    digits <- if (abs(info$ic_sch[1]) >= 1e6) 0 else 2
    tex('\\textup{mean} = ', round(info$ic_sch[3, 1], digits), ',\\ \\textup{s.e.} = ', round(info$ic_sch[3, 2], digits))
})



row_all <- reactiveVal()
dat_table <- reactiveVal()


observeEvent(c(info$est, info$var), {
    req(info$var)
    print(info$est)
    param_l = c()
    for (i in seq(1,length(info$var))){
        param_l <- c(param_l, info$var[i])
    }
    row_all(unique(do.call(c, lapply(var, function(var) {
        grep(paste(paste0('(^', param_l, '$)'), collapse = '|'), rownames(info$est), value = T)
    }))))

    output$rhat_cycle <- renderUI({
        rhat_cycle_max <- max(info$est[row_all(), 'Rhat'], na.rm = T)
        tagList(
            paste0('$\\max(\\hat{R}, \\text{cycle}=', input$w1, '):', toString(format(rhat_cycle_max, scientific = F)), '$'),
            tags$script(paste0('renderMathInElement(document.getElementById("rhat_cycle"), {delimiters: [{left: "$", right: "$", display: false}]});')),
        )
        
    })
    
    req(length(row_all()) > 0)
    dat_table_ <- info$est[row_all(), , drop = F]
    print(dat_table_)
    rownames_new = c()
    for (i in 1 : length(rownames(dat_table_))) {
        for (print_form in names(info$var)){
            if (gsub("\\\\", "", info$var[print_form]) == rownames(dat_table_)[i]) {
                rownames_new <- c(rownames_new, substring(print_form, 2))
                break
            }
        }
    }
    rownames(dat_table_) = rownames_new
    dat_table_[, 9] <- round(dat_table_[, 9],0)
    print(dat_table_)
    dat_table(dat_table_)
    
    
    output$table <- renderDT({
        req(length(row_all()) > 0)
        dat_table_ = isolate(dat_table())
        colnames(dat_table_)[colnames(dat_table_) == "n_eff"] <- paste0("n_eff<div class='n_eff' data-tooltip='", description['n_eff'], "' style='display: inline'>  <i class='fas fa-info-circle'></i></div>")
        colnames(dat_table_)[colnames(dat_table_) == "Rhat"] <- paste0('Rhat<div class="Rhat" data-tooltip="', description['Rhat'], '" style="display: inline">  <i class="fas fa-info-circle"></i></div>')
        
        rownames(dat_table_) = paste('<div id="', rownames(dat_table_), '">$\\', rownames(dat_table_), '$</div> <script>renderMathInElement(document.getElementById("', rownames(dat_table_), '"), {delimiters: [{left: "$", right: "$", display: false}]});</script>', sep='')
        print(dat_table_)
        datatable(dat_table_, escape = FALSE, options = DToption) %>%
            formatRound(c(1:(ncol(info$est) - 2), ncol(info$est)), 3, mark = '')
    })
})

# three export methods
output$download_csv <- downloadHandler(
    filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
        dat_table_ = isolate(dat_table())
        rownames(dat_table_) <- lapply(rownames(dat_table_), function(s) gsub("text\\{([^}]*)\\}(.*)", "\\1\\2", s))
        write.csv(dat_table_, file, row.names = TRUE)
    }
)
output$download_xls <- downloadHandler(
    filename = function() {
        paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
        dat_table_ = isolate(dat_table())
        rownames(dat_table_) <- lapply(rownames(dat_table_), function(s) gsub("text\\{([^}]*)\\}(.*)", "\\1\\2", s))
        write.xlsx(as.data.frame(dat_table_), file, row.names = TRUE)
    }
)
output$download_tex <- downloadHandler(
    filename = function() {
        paste("data-", Sys.Date(), ".tex", sep="")
    },
    content = function(file) {
        dat_table_ = isolate(dat_table())
        rownames(dat_table_) = paste("$\\", rownames(dat_table_), "$", sep='')
        print(xtable(dat_table_), sanitize.rownames.function = identity, file=file)
    }
)