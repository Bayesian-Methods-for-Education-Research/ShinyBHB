# equr1 <- reactiveVal()
# 
# output$equr1 <- renderUI({
#     latex('\\begin{aligned}', equr1(), '\\end{aligned}')
#     if (!is.null(input$x2)) { # level-2
#         
#     }
# })
library(xtable) 

observeEvent(c(input$long, input$t1, input$td1, input$tf1, input$y1, input$x1, input$x2, input$select_method, input$select_mode, input$add_2, input$add_3, input$delete_2, input$delete_3), {
    if (!is.null(input$x1) && !is.null(input$y1)) {
        removeUI('#result_model > .equ', multiple = TRUE, immediate = TRUE)
        removeUI('#result_model > .level', multiple = TRUE, immediate = TRUE)
        get_model('result_model')
        
        # removeUI('#result_model > .equ', multiple = TRUE, immediate = TRUE)
        # 
        # insertUI(
        #     selector = "#result_model",
        #     where = "afterBegin",
        #     ui = uiOutput('result_model_main', class = "equ"),
        # )
        # 
        # if (!is.null(input$x2)) { # level-2
        #     output$result_model_main <- renderUI({
        #         x1 = isolate(input$x1)
        #         x2 = isolate(input$x2)
        #         rhs = c('\\beta_{0j}', if (length(x1) > 0)
        #             paste0('\\beta_{', 1:length(x1), 'j}', x1, '_{ij}')
        #             else
        #                 NULL)
        #         major = paste0('$', input$y1, '_{ij}=', paste0(rhs, collapse = '+'), '+R_{ij}$')
        #         
        #         tagList(
        #             major,
        #             tags$script(paste0('renderMathInElement(document.getElementById("result_model_main"), {delimiters: [{left: "$", right: "$", display: false}]});')),
        #         )
        #     })
        #     
        #     for (i in 0: length(input$x1)) {
        #         insertUI(
        #             selector = "#result_model",
        #             where = "beforeEnd",
        #             ui = uiOutput(paste0('result_model_second_', i), class = "equ"),
        #         )
        #     }
        #     
        #     lapply(0: length(input$x1), function (i) {
        #         output[[paste0('result_model_second_', i)]] <- renderUI({
        #             x2 = isolate(input$x2)
        # 
        #             major = paste0('$\\beta_{', i, 'j}=\\gamma_{', i, '0}')
        #             for (j in 1 : length(x2)) {
        #                 if (exclude()[i + 1, j] == 0) {
        #                     major = paste0(major, "+\\gamma_{", i, j, "}", x2[j], "_j")
        #                 }
        #             }
        #             if (exclude()[i + 1, length(x2) + 1] == 0) {
        #                 major = paste0(major, "+U_{", i, "j}")
        #             }
        #             major = paste0(major, "$")
        #             
        #             tagList(
        #                 major,
        #                 tags$script(paste0('renderMathInElement(document.getElementById("result_model_second_', i, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
        #             )
        #         })
        #     })
        # }
        # else { # level-1
        #     output$result_model_main <- renderUI({
        #         x1 = isolate(input$x1)
        #         rhs = c('\\beta_{0}', if (length(x1) > 0)
        #             paste0('\\beta_{', 1:length(x1), '}', x1, '_{i}')
        #             else
        #                 NULL)
        #         major = paste0('$', input$y1, '_{i}=', paste0(rhs, collapse = '+'), '+R_{i}$')
        #         
        #         tagList(
        #             major,
        #             tags$script(paste0('renderMathInElement(document.getElementById("result_model_main"), {delimiters: [{left: "$", right: "$", display: false}]});')),
        #         )
        #     })
        # }
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

# table_rendered <- reactiveValues()
observeEvent(c(info$est, info$var), {
    req(info$var)
    param_l = c()
    for (i in seq(1,length(info$var))){
        param_l <- c(param_l, info$var[i])
    }
    row_all(unique(do.call(c, lapply(var, function(var) {
        grep(paste(paste0('(^', param_l, '$)'), collapse = '|'), rownames(info$est), value = T)
    }))))
    # req(length(row_all()) > 0)
    # dat_table <- info$est[row_all(), , drop = F]
    # print(dat_table)
    # tfile <- tempfile(fileext = ".csv")
    # write.csv(dat_table, tfile)
    # file.copy("out.csv", tfile)
    # print(as.data.frame(dat_table))
    # write.xlsx(as.data.frame(dat_table), "test.xlsx")
    # latex(dat_table, file="test.txt")
    output$rhat_cycle <- renderUI({
        rhat_cycle_max <- max(info$est[row_all(), 'Rhat'], na.rm = T)
        tagList(
            paste0('$max(\\hat{R}, cycle=', input$w1, '):', toString(format(rhat_cycle_max, scientific = F)), '$'),
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
    
    
    #addTooltip(session, 'ic', "https://avehtari.github.io/modelselection/CV-FAQ.html", placement = "right", trigger = "hover", options = NULL)
    
    # runjs("
    #        $(document).on('shiny:value', function(event) {
    #           if (event.name === 'table') {
    #             console.log(event)
    #             console.log(document.getElementsByClassName('dt-right sorting'));
    #             var element = document.getElementsByClassName('dt-right sorting')[22];
    #             console.log(element);
    #             element.id = 'neff';
    #           }
    #         });
    #       ")
    
    output$table <- renderDT({
        req(length(row_all()) > 0)
        dat_table_ = isolate(dat_table())
        colnames(dat_table_)[colnames(dat_table_) == "n_eff"] <- paste0("n_eff<div class='n_eff' data-tooltip='", description['n_eff'], "' style='display: inline'>  <i class='fas fa-question-circle'></i></div>")
        colnames(dat_table_)[colnames(dat_table_) == "Rhat"] <- paste0('Rhat<div class="Rhat" data-tooltip="', description['Rhat'], '" style="display: inline">  <i class="fas fa-question-circle"></i></div>')
        
        rownames(dat_table_) = paste('<div id="', rownames(dat_table_), '">$\\', rownames(dat_table_), '$</div> <script>renderMathInElement(document.getElementById("', rownames(dat_table_), '"), {delimiters: [{left: "$", right: "$", display: false}]});</script>', sep='')
        print(dat_table_)
        datatable(dat_table_, escape = FALSE, options = DToption) %>%
            formatRound(c(1:(ncol(info$est) - 2), ncol(info$est)), 3, mark = '')
    })
    
    # table_rendered$a <- TRUE
    
})

# observeEvent(table_rendered$a, {
#     req(table_rendered$a)
#     
#     runjs('console.log(document.getElementsByClassName("dt-right sorting")); var element = document.getElementsByClassName("dt-right sorting")[22]; console.log(element); element.id = "neff";')
# })





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
        #dat_table_ <- diag(paste("$\\", rownames(dat_table_), "$"))
        rownames(dat_table_) = paste("$\\", rownames(dat_table_), "$", sep='')
        print(xtable(dat_table_), sanitize.rownames.function = identity, file=file)
        
        #lapply(xtable(as.data.frame(dat_table)), write, file, append=TRUE, ncolumns=1000)
    }
)

# output$table_test <- renderUI({
#     # req(input$param)
#     # req(length(row()) > 0)
#     # 
#     # dat_table <- info$est[row(), , drop = F]
#     # rownames(dat_table) = paste("$\\", rownames(dat_table), "$", sep="")
#     # includeHTML(gsub(" ", "", paste0("table.html?dat=", toString(dat_table))))
#     includeHTML(paste0("table.html"))
# })
    






