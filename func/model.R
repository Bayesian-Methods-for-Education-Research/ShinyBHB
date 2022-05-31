exclude <- reactiveVal(0)
# exclude_init_2[i, j]: /sigma_{(i - 1), j}
# exclude_init_2[i, j_max]: /U_{(i - 1)}
exclude_2 <- reactiveVal(0)
# exclude_init_2[i, j, k]: /delta_{(i - 1), (j - 1), k}
# exclude_init_2[i, j, k_max]: V_{(i - 1), (j - 1)}
model_mode_BDB <- reactiveVal("others")
model_mode <- reactiveVal("noninf")

init_line_layer_3 <- function (i, j) {
    hei_max = dim(exclude_2())[3]
    lapply(1: hei_max, function (k) {
        id = paste0('term_3_', i, '_', j, '_', k)
        
        if (k < hei_max) {
            if (layer() == 'two' && input$long) {
                latex_component = paste0('\\delta_{', i, j, k, '}', input$x2[k], '_j')
            }
        }
        else {
            if (layer() == 'two' && input$long) {
                latex_component = paste0('V_{', i, j, 'j}')
            }
        }
        
        output[[id]] <- renderUI({
            if (i == 0 && j == 0 && k == hei_max) {
                latex_component = paste0('$', latex_component, '$')
            }
            else if (exclude_2()[i + 1, j + 1, k] == 0) {
                latex_component = paste0('$\\color{blue}', latex_component, '$')
            } else {
                latex_component = paste0('$\\color{lightgray}\\sout{', latex_component, '}$') # or use \\cancel{} for incline
            }
            
            tagList(
                latex_component,
                tags$script(paste0('renderMathInElement(document.getElementById("', id, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
            )
        })
        
        output[[paste0(id, '_before')]] <- renderUI({
            if (k == 1) {
                if (layer() == 'two' && input$long) {
                    latex_component_before = paste0('\\gamma_{', i, j, 'j}=\\delta_{', i, j, '0}+')
                }
            }
            else {
                latex_component_before = paste0('+')
            }
            tagList(
                paste0('$', latex_component_before, '$'),
                tags$script(paste0('renderMathInElement(document.getElementById("', id, '_before', '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
            )
        })
        
        onclick(id, {
            if (i != 0 || j != 0 || k != hei_max) {
                if (exclude_2()[i + 1, j + 1, k] == 0) {
                    output[[id]] <- renderUI({
                        tagList(
                            paste0('$\\color{lightgray}\\sout{', latex_component, '}$'),
                            tags$script(paste0('renderMathInElement(document.getElementById("', id, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                        )
                    })
                    temp = exclude_2()
                    temp[i + 1, j + 1, k] <- 1
                    exclude_2(temp)
                }
                else {
                    output[[id]] <- renderUI({
                        tagList(
                            paste0('$\\color{blue}', latex_component, '$'),
                            tags$script(paste0('renderMathInElement(document.getElementById("', id, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                        )
                    })
                    temp = exclude_2()
                    temp[i + 1, j + 1, k] <- 0
                    exclude_2(temp)
                }
            }
        })
    })
}

observeEvent(c(input$long, input$t1, input$td1, input$tf1, input$y1, input$x1, input$x2, input$select_method, input$select_mode, input$add_2, input$add_3, input$delete_2, input$delete_3), {
    if (!is.null(input$x1) && !is.null(input$y1)) { # need to delete
        
        s_i = list(one=1, two=2, three=3)
        layer_actual = s_i[[layer()]]
        if (input$long) {
            layer_actual = layer_actual + 1
        }
        
        # check if all variables are ready
        if (layer() == 'one' && !input$long) {
            req(input$y1)
            req(input$x1)
        }
        else if (layer() == 'one' && input$long) {
            req(input$y1)
            req(input$t1)
            req(input$tf1)
            req(input$td1)
            req(input$x1)
        }
        else if (layer() == 'two' && !input$long) {
            req(input$y1)
            req(input$x1)
            req(input$x2)
        }
        else if (layer() == 'two' && input$long) {
            req(input$y1)
            req(input$t1)
            req(input$tf1)
            req(input$td1)
            req(input$x1)
            req(input$x2)
        }
        
        # clear layout
        #removeUI('#estimation_panel > br', multiple = TRUE)
        #removeUI('#head-level-1', multiple = TRUE, immediate = TRUE)
        
        
        if (layer_actual >= 3) { # level-3 equation
            # get former layout info for better update
            row_max_former = dim(exclude_2())[1]
            col_max_former = dim(exclude_2())[2]
            hei_max_former = dim(exclude_2())[3]
            # init the exclusion matrix
            if (input$long) {
                row_max = 1 + strtoi(input$tf1) + length(input$td1)
                col_max = length(input$x1) + 1
                hei_max = length(input$x2) + 1
            }
            else {
                row_max = 1 + length(input$x1)
                col_max = length(input$x2) + 1
            }
            exclude_init_2 = array(0, c(row_max, col_max, hei_max))
            for (i in 1 : row_max) {
                j = 1
                for (k in 1 : (hei_max - 1)) {
                    exclude_init_2[i, j, k] <- 1
                }
                
                for (j in 2 : col_max) {
                    for (k in 1 : hei_max) {
                        exclude_init_2[i, j, k] <- 1
                    }
                }
                
            }
            exclude_2(exclude_init_2)
            
            if (is.null(row_max_former)){
                insertUI(
                    selector = "#head-prior",
                    where = "beforeBegin",
                    ui = tags$div(
                        id = "head-level-3",
                        tags$label('Level-3'),
                    ),
                    immediate = TRUE
                )
            }

            # level-3 equation
            # make layouts: add missing selection blocks, and delete unnecessary blocks
            for (i in 0: (row_max - 1)) {
                for (j in 0: (col_max - 1)) {
                    line_id = paste0('term_3_', i, '_', j)
                    if (is.null(row_max_former) || (i + 1 > row_max_former) || (j + 1) > col_max_former) {
                        if (j == 0) {
                            if (i == 0) {
                                front = paste0('#head-level-3')
                            }
                            else {
                                front = paste0('#term_3_', i - 1, '_', col_max - 1)
                            }
                        }
                        else {
                            front = paste0('#term_3_', i, '_', j - 1)
                        }
                        
                        insertUI(
                            selector = front,
                            where = "afterEnd",
                            ui = tags$div(
                                id = line_id
                            ),
                            immediate = TRUE
                        )
                    }
                    
                    
                    for (k in 1: hei_max) {
                        id = paste0('term_3_', i, '_', j, '_', k)
                        
                        if (is.null(row_max_former) || (i + 1 > row_max_former) || (j + 1 > col_max_former) || (k > hei_max_former)) {
                            if (k == 1) {
                                insertUI(
                                    selector = paste0('#', line_id),
                                    where = "afterBegin",
                                    ui = uiOutput(paste0(id, '_before'), inline = TRUE),
                                    immediate = TRUE
                                )
                            }
                            else {
                                insertUI(
                                    selector = paste0('#', 'term_3_', i, '_', j, '_', k - 1),
                                    where = "afterEnd",
                                    ui = uiOutput(paste0(id, '_before'), inline = TRUE),
                                    immediate = TRUE
                                )
                            }
                            
                            insertUI(
                                selector = paste0('#', id , '_before'),
                                where = "afterEnd",
                                ui = uiOutput(id, inline = TRUE),
                                immediate = TRUE
                            )
                        }
                    }
                }
            }
            if (!is.null(row_max_former)) {
                lapply(0: (row_max_former - 1), function (i) {
                    lapply(0: (col_max_former - 1), function (j) {
                        if (i > row_max - 1 || j > col_max - 1) {
                            removeUI(paste0('#term_3_', i, '_', j), immediate = TRUE, multiple = TRUE)
                        }
                        else {
                            lapply(1: hei_max_former, function (k) {
                                if (j > col_max) {
                                    removeUI(paste0('#term_3_', i, '_', j, '_', k), immediate = TRUE, multiple = TRUE)
                                    removeUI(paste0('#term_3_', i, '_', j, '_', k, '_before'), immediate = TRUE, multiple = TRUE)
                                }
                            }) 
                        }
                    })
                })
            }
            # fill in selection blocks
            lapply(0: (row_max - 1), function (i) {
                init_line_layer_3(i, 0)
            })
        }
        else { # clear level-3 equation
            row_max_former = dim(exclude_2())[1]
            col_max_former = dim(exclude_2())[2]
            hei_max_former = dim(exclude_2())[3]
            if (!is.null(row_max_former)) {
                lapply(0: (row_max_former - 1), function (i) {
                    lapply(0: (col_max_former - 1), function (j) {
                        removeUI(paste0('#term_3_', i, '_', j), immediate = TRUE, multiple = TRUE)
                    })
                })
            }
            exclude_2(0)
            removeTooltip(session, "hyperprior_nu")
            removeTooltip(session, "hyperprior_nu_2")
            removeUI('#head-level-3', multiple = TRUE, immediate = TRUE)
        }
        
        
        
        
        
        if (layer_actual >= 2) { # level-2 equation
            # get former layout info for better update
            row_max_former = nrow(exclude())
            col_max_former = ncol(exclude())
            # init the exclusion matrix
            if (input$long) {
                row_max = 1 + strtoi(input$tf1) + length(input$td1)
                col_max = length(input$x1) + 1
            }
            else {
                row_max = 1 + length(input$x1)
                col_max = length(input$x2) + 1
            }
            exclude_init = matrix(0, nrow = row_max, ncol = col_max)
            for (i in 1 : row_max) {
                for (j in 1 : col_max - 1) {
                    exclude_init[i, j] <- 1
                }
            }
            exclude(exclude_init)
            
            if (is.null(row_max_former)){
                insertUI(
                    selector = "#equ1",
                    where = "afterEnd",
                    ui = tags$div(
                        id = "head-level-2",
                        tags$label('Level-2'),
                    ),
                    immediate = TRUE
                )
            }
            
            # level-2 equation
            # make layouts: add missing selection blocks, and delete unnecessary blocks
            for (i in 0: (row_max - 1)) {
                line_id = paste0('term_2_', i)
                if (is.null(row_max_former) || (i + 1 > row_max_former)){
                    if (i == 0) {
                        front = paste0('#head-level-2')
                    }
                    else {
                        front = paste0('#term_2_', i - 1)
                    }
                    insertUI(
                        selector = front,
                        where = "afterEnd",
                        ui = tags$div(
                            id = line_id
                        ),
                        immediate = TRUE
                    )
                }
                
                for (j in 1: col_max) {
                    id = paste0('term_2_', i, '_', j)
                    
                    if (is.null(row_max_former) || (i + 1 > row_max_former) || (j > col_max_former)) {
                        if (j == 1) {
                            insertUI(
                                selector = paste0('#', line_id),
                                where = "afterBegin",
                                ui = uiOutput(paste0(id, '_before'), inline = TRUE),
                                immediate = TRUE
                            )
                        }
                        else {
                            insertUI(
                                selector = paste0('#', 'term_2_', i, '_', j - 1),
                                where = "afterEnd",
                                ui = uiOutput(paste0(id, '_before'), inline = TRUE),
                                immediate = TRUE
                            )
                        }
                        
                        insertUI(
                            selector = paste0('#', id , '_before'),
                            where = "afterEnd",
                            ui = uiOutput(id, inline = TRUE),
                            immediate = TRUE
                        )
                    }
                }
            }
            if (!is.null(row_max_former)) {
                lapply(0: (row_max_former - 1), function (i) {
                    if (i >= row_max) {
                        removeUI(paste0('#term_2_', i), immediate = TRUE, multiple = TRUE)
                    }
                    else {
                        lapply(1: col_max_former, function (j) {
                            if (j > col_max) {
                                removeUI(paste0('#term_2_', i, '_', j), immediate = TRUE, multiple = TRUE)
                                removeUI(paste0('#term_2_', i, '_', j, '_before'), immediate = TRUE, multiple = TRUE)
                            }
                        }) 
                    }
                })
            }
            # fill in selection blocks
            lapply(0: (row_max - 1), function (i) {
                lapply(1: col_max, function (j) {
                    id = paste0('term_2_', i, '_', j)
                    
                    if (j < col_max) {
                        if (layer() == 'one' && input$long) {
                            latex_component = paste0('\\gamma_{', i, j, '}', input$x1[j], '_i')
                        }
                        else if (layer() == 'two' && !input$long) {
                            latex_component = paste0('\\gamma_{', i, j, '}', input$x2[j], '_j')
                        }
                        else if (layer() == 'two' && input$long) {
                            latex_component = paste0('\\gamma_{', i, j, 'j}', input$x1[j], '_{ij}')
                        }
                    }
                    else {
                        if (layer() == 'one' && input$long) {
                            latex_component = paste0('U_{', i, 'i}')
                        }
                        else if (layer() == 'two' && !input$long) {
                            latex_component = paste0('U_{', i, 'j}')
                        }
                        else if (layer() == 'two' && input$long) {
                            latex_component = paste0('U_{', i, 'ij}')
                        }
                    }
                    
                    output[[id]] <- renderUI({
                        if (i == 0 && j == col_max) {
                            latex_component = paste0('$', latex_component, '$')
                        }
                        else if (exclude()[i + 1, j] == 0) {
                            latex_component = paste0('$\\color{blue}', latex_component, '$')
                        } else {
                            latex_component = paste0('$\\color{lightgray}\\sout{', latex_component, '}$') # or use \\cancel{} for incline
                        }
                        
                        tagList(
                            latex_component,
                            tags$script(paste0('renderMathInElement(document.getElementById("', id, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                        )
                    })
                    
                    output[[paste0(id, '_before')]] <- renderUI({
                        if (j == 1) {
                            if (layer() == 'one' && input$long) {
                                latex_component_before = paste0('\\beta_{', i, 'i}=\\gamma_{', i, '0}+')
                            }
                            else if (layer() == 'two' && !input$long) {
                                latex_component_before = paste0('\\beta_{', i, 'j}=\\gamma_{', i, '0}+')
                            }
                            else if (layer() == 'two' && input$long) {
                                latex_component_before = paste0('\\beta_{', i, 'ij}=\\gamma_{', i, '0j}+')
                            }
                        }
                        else {
                            latex_component_before = paste0('+')
                        }
                        tagList(
                            paste0('$', latex_component_before, '$'),
                            tags$script(paste0('renderMathInElement(document.getElementById("', id, '_before', '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                        )
                    })
                    
                    onclick(id, {
                        if (i != 0 || j != col_max) {
                            if (exclude()[i + 1, j] == 0) {
                                output[[id]] <- renderUI({
                                    tagList(
                                        paste0('$\\color{lightgray}\\sout{', latex_component, '}$'),
                                        tags$script(paste0('renderMathInElement(document.getElementById("', id, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                                    )
                                })
                                temp = exclude()
                                temp[i + 1, j] <- 1
                                exclude(temp)
                                
                                if (layer_actual == 3 && j != col_max) {
                                    temp = exclude_2()
                                    for (k in 1: hei_max) {
                                        id_3 = paste0('term_3_', i, '_', j, '_', k)
                                        output[[id_3]] <- renderUI({
                                            tagList(
                                                '',
                                                tags$script(paste0('renderMathInElement(document.getElementById("', id_3, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                                            )
                                        })
                                        output[[paste0(id_3, '_before')]] <- renderUI({
                                            tagList(
                                                '',
                                                tags$script(paste0('renderMathInElement(document.getElementById("', id_3, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                                            )
                                        })
                                        temp[i + 1, j + 1, k] <- 1
                                    }
                                    exclude_2(temp)
                                }
                            }
                            else {
                                output[[id]] <- renderUI({
                                    tagList(
                                        paste0('$\\color{blue}', latex_component, '$'),
                                        tags$script(paste0('renderMathInElement(document.getElementById("', id, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                                    )
                                })
                                temp = exclude()
                                temp[i + 1, j] <- 0
                                exclude(temp)
                                
                                if (layer_actual == 3 && j != col_max) {
                                    temp = exclude_2()
                                    temp[i + 1, j + 1, hei_max] <- 0
                                    exclude_2(temp)
                                    init_line_layer_3(i, j)
                                }
                            }
                        }
                    })
                    
                    # if (j == col_max) {
                    #     insertUI(
                    #         selector = paste0('#', id),
                    #         where = "afterEnd",
                    #         ui = br(),
                    #     )
                    # }
                })
            })
        }
        else { # clear level-2 equation
            row_max_former = nrow(exclude())
            col_max_former = ncol(exclude())
            if (!is.null(row_max_former)) {
                lapply(0: (row_max_former - 1), function (i) {
                    removeUI(paste0('#term_2_', i), immediate = TRUE, multiple = TRUE)
                })
            }
            exclude(0)
            removeUI('#head-level-2', multiple = TRUE, immediate = TRUE)
        }
        
        
        if (layer_actual >= 1) { # level-1 equation
            # insertUI(
            #     selector = "#equ1",
            #     where = "beforeBegin",
            #     ui = tags$div(
            #         id = "head-level-1",
            #         tags$label('Level-1'),
            #     ),
            #     immediate = TRUE
            # )
            
            output$equ1 <- renderUI({
                tf1 = strtoi(input$tf1)
                if (layer() == 'one' && !input$long) {
                    rhs = c('\\beta_{0}', if (length(input$x1) > 0)
                        paste0('\\beta_{', 1:length(input$x1), '}', input$x1, '_{i}')
                        else
                            NULL)
                    major = paste0('$', input$y1, '_{i}=', paste0(rhs, collapse = '+'), '+R_{i}$')
                }
                else if (layer() == 'one' && input$long) {
                    rhs = c('\\beta_{0i}', paste0('\\beta_{1i}', input$t1, '_{ti}'))
                    if (tf1 >= 2) {
                        rhs = c(rhs, paste0('\\beta_{2i}', input$t1, '_{ti}^2'))
                    }
                    if (tf1 >= 3) {
                        rhs = c(rhs, paste0('\\beta_{3i}', input$t1, '_{ti}^3'))
                    }
                    rhs = c(rhs, paste0('\\beta_{', (1 + tf1):(length(input$td1) + tf1), 'i}', input$td1, '_{ti}'), paste0('R_{ti}'))
                    major = paste0('$', input$y1, '_{ti}=', paste0(rhs, collapse = '+'), '$')
                }
                else if (layer() == 'two' && !input$long) {
                    rhs = c('\\beta_{0j}', paste0('\\beta_{', 1:length(input$x1), 'j}', input$x1, '_{ij}'), 'R_{ij}')
                    major = paste0('$', input$y1, '_{ij}=', paste0(rhs, collapse = '+'), '$')
                }
                else if (layer() == 'two' && input$long) {
                    rhs = c('\\beta_{0ij}', paste0('\\beta_{1ij}', input$t1, '_{tij}'))
                    if (tf1 >= 2) {
                        rhs = c(rhs, paste0('\\beta_{2ij}', input$t1, '_{tij}^2'))
                    }
                    if (tf1 >= 3) {
                        rhs = c(rhs, paste0('\\beta_{3ij}', input$t1, '_{tij}^3'))
                    }
                    rhs = c(rhs, paste0('\\beta_{', (1 + tf1):(length(input$td1) + tf1), 'ij}', input$td1, '_{tij}'), paste0('R_{tij}'))
                    major = paste0('$', input$y1, '_{tij}=', paste0(rhs, collapse = '+'), '$')
                }
                
                tagList(
                    major,
                    tags$script(paste0('renderMathInElement(document.getElementById("equ1"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                )
            })
            
            # insertUI(
            #     selector = "#equ1",
            #     where = "afterEnd",
            #     ui = br(),
            # )
        }
        
        
        # Update hyperpriors
        updateHyperprior <- function(component, label) {
            if (component == "sigma_beta" & model_mode_BDB() == "others") {
                mapping = list('\\textup{Fixed Value}' = 'fix_value')
            }
            else {
                mapping = hyperprior[[component]]
            }
            
            output[[paste0("hyperprior_", component)]] <- renderUI({
                tagList(
                    if (label == "") {
                        "$$"
                    }
                    else {
                        div(
                            div(style="display:inline-block", class="hyperprior_label", inline(selectizeInput(paste0('dist_', component), paste0("$", label, "\\sim$"), mapping, options = katex, width = "600px"))),
                            div(style="display:inline-block", class="hyperprior_input", numericInput(inputId=paste0(component, "_a"), label="$a$", value = 0.0, width = "30%")),
                            div(style="display:inline-block", class="hyperprior_input", numericInput(inputId=paste0(component, "_b"), label="$b$", value = 10, width = "30%"))
                        )
                    },

                    tags$script(paste0('renderMathInElement(document.getElementById("hyperprior_', component, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                )
            })
            observeEvent(input[[paste0("dist_", component)]], {
                ending = ""
                hyperprior_component_new = mapping
                if (component != 'Omega') {
                    names = c()
                    if (input[[paste0('dist_', component)]] == "inv_gamma") {
                        ending = '^2'
                        
                        for (latex_form in names(mapping)) {
                            if (mapping[[latex_form]] != "inv_gamma") {
                                names <- c(names, paste0(label, "\\sim", latex_form))
                            }
                            else {
                                names <- c(names, latex_form)
                            }
                        }
                    }
                    
                    else {
                        for (latex_form in names(mapping)) {
                            if (mapping[[latex_form]] != "inv_gamma") {
                                names <- c(names, latex_form)
                            }
                            else {
                                names <- c(names, paste0(label, "^2\\sim", latex_form))
                            }
                        }
                    }

                    names(hyperprior_component_new) <- names
                }
                
                
                output[[paste0("hyperprior_", component)]] <- renderUI({
                    tagList(
                        if (label == "") {
                            "$$"
                        }
                        else {
                            div(
                                div(style="display:inline-block;", class="hyperprior_label", selectizeInput(paste0('dist_', component), paste0("$", label, ending, "\\sim$"), selected = input[[paste0('dist_', component)]], hyperprior_component_new, options = katex, width = "300px")),
                                if (hyperprior[[paste0(component, "_a")]][input[[paste0('dist_', component)]]] != "0") {
                                    div(style="display:inline-block",
                                        div(style="display:inline-block", class="hyperprior_input", numericInput(inputId=paste0(component, "_a"), label=paste0("$", hyperprior[[paste0(component, "_a")]][input[[paste0('dist_', component)]]], "$"), value = 0.0, width = "100px")),
                                        bsTooltip(id = paste0(component, "_a"), title = hyperprior$help_text_a[[input[[paste0('dist_', component)]]]], placement = "bottom", trigger = "hover", options = NULL)
                                    )
                                },
                                div(style="display:inline-block", class="hyperprior_input", numericInput(inputId=paste0(component, "_b"), label=paste0("$", hyperprior[[paste0(component, "_b")]][input[[paste0('dist_', component)]]], "$"), value = 10, width = "100px")),
                                bsTooltip(id = paste0(component, "_b"), title = hyperprior$help_text_b[[input[[paste0('dist_', component)]]]], placement = "bottom", trigger = "hover", options = NULL)
                            )
                        },
                        tags$script(paste0('renderMathInElement(document.getElementById("hyperprior_', component, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                    )
                })
            })
        }
        
        # updateHyperprior("sigma_beta", "\\sigma_\\beta")
        # if (model_mode_BDB() == 'BDB'){
        #     updateHyperprior("sigma_0_beta", "\\sigma_{0_\\beta}")
        # }
        # else {
        #     updateHyperprior("sigma_0_beta", "")
        # }
        # if (!is.null(input$x2)) {
        #     if (model_mode_BDB() == 'BDB'){
        #         updateHyperprior("tau", "\\tau_S")
        #     }
        #     else {
        #         updateHyperprior("tau", "\\tau_U")
        #     }
        # }
        # else {
        #     updateHyperprior("tau", "")
        # }
        # if (!is.null(input$x2)) {
        #     if (model_mode_BDB() == 'BDB'){
        #         updateHyperprior("Omega", "\\Omega_S")
        #     }
        #     else {
        #         updateHyperprior("Omega", "\\Omega_U")
        #     }
        # }
        # else {
        #     updateHyperprior("Omega", "")
        # }
        
        if (input$long) {
            long = "long"
        }
        else {
            long = "cross"
        }
        updateHyperprior("sigma_beta", hyperprior_formula[["sigma_beta"]][[long]][[layer()]][[model_mode_BDB()]])
        updateHyperprior("sigma_0_beta", hyperprior_formula[["sigma_0_beta"]][[long]][[layer()]][[model_mode_BDB()]])
        updateHyperprior("tau", hyperprior_formula[["tau"]][[long]][[layer()]][[model_mode_BDB()]])
        updateHyperprior("Omega", hyperprior_formula[["Omega"]][[long]][[layer()]][[model_mode_BDB()]])
        updateHyperprior("tau_2", hyperprior_formula[["tau_2"]][[long]][[layer()]][[model_mode_BDB()]])
        updateHyperprior("Omega_2", hyperprior_formula[["Omega_2"]][[long]][[layer()]][[model_mode_BDB()]])
        updateHyperprior("sigma_R", "\\sigma_R")
        
        if (layer_actual >= 2 && model_mode_BDB() == 'BDB') {
            output$hyperprior_nu <- renderUI({
                if (layer_actual >= 3) {
                    label = "$\\nu_U=$"
                }
                else {
                    label = "$\\nu=$"
                }
                tagList(
                    inline(numericInput(inputId="nu", label=label, value = 1, width = "40%")),
                    tags$script(paste0('renderMathInElement(document.getElementById("hyperprior_nu"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                )
            })
            addTooltip(session, "hyperprior_nu", "This parameter controls the degree of borrowing and must be positive. 1 indicates weak borrowing.", placement = "left", trigger = "hover", options = NULL)
            observeEvent(input$nu, {
                req(input$nu)
                if (input$nu >= 0) {
                    runjs("document.getElementById('nu').style.color = 'black';")
                    enable('run')
                }
                else {
                    runjs("document.getElementById('nu').style.color = 'red';")
                    disable('run')
                }

            })
        }
        else {
            output$hyperprior_nu <- renderUI({
                tagList(
                    "$$",
                    tags$script(paste0('renderMathInElement(document.getElementById("hyperprior_nu"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                )
            })
            removeTooltip(session, "hyperprior_nu")
        }

        if (layer_actual >= 3 && model_mode_BDB() == 'BDB') {
            output$hyperprior_nu_2 <- renderUI({
                tagList(
                    inline(numericInput(inputId="nu_2", label="$\\nu_V=$", value = 1, width = "40%")),
                    tags$script(paste0('renderMathInElement(document.getElementById("hyperprior_nu_2"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                )
            })
            addTooltip(session, "hyperprior_nu_2", "This parameter controls the degree of borrowing and must be positive. 1 indicates weak borrowing.", placement = "left", trigger = "hover", options = NULL)
            observeEvent(input$nu_2, {
                req(input$nu_2)
                if (input$nu_2 >= 0) {
                    runjs("document.getElementById('nu_2').style.color = 'black';")
                    enable('run')
                }
                else {
                    runjs("document.getElementById('nu_2').style.color = 'red';")
                    disable('run')
                }
                
            })
        }
        else {
            output$hyperprior_nu_2 <- renderUI({
                tagList(
                    "$$",
                    tags$script(paste0('renderMathInElement(document.getElementById("hyperprior_nu_2"), {delimiters: [{left: "$", right: "$", display: false}]});')),
                )
            })
            removeTooltip(session, "hyperprior_nu_2")
        }

    }
})


#

observeEvent(input$select_method, {
    if (input$select_method == "no_bor") {
        updateSelectInput(session, 'select_mode', choices = c("Noninformative" = "noninf"), selected = "noninf")
    }
    else if (input$select_method == "static_bor") {
        updateSelectInput(session, 'select_mode', choices = c("Pooling" = "pooling", "Power priors" = "power_prior"), selected = "pooling")
    }
    else if (input$select_method == "dynamic_bor") {
        updateSelectInput(session, 'select_mode', choices = c("BDB" = "BDB"), selected = "BDB")
    }
})

observeEvent(input$select_mode, {
    if (input$select_mode == "noninf") {
        model_mode('noninf')
        model_mode_BDB('others')
        
        removeUI(
            selector = "#hyperprior_pp_weight", immediate = TRUE
        )
        removeTooltip(session, "hyperprior_pp_weight")
    }
    else if (input$select_mode == "BDB") {
        model_mode('BDB')
        model_mode_BDB('BDB')
        
        removeUI(
            selector = "#hyperprior_pp_weight", immediate = TRUE
        )
        removeTooltip(session, "hyperprior_pp_weight")
    }
    else if (input$select_mode == "pooling") {
        model_mode('pooling')
        model_mode_BDB('others')
        
        removeUI(
            selector = "#hyperprior_pp_weight", immediate = TRUE
        )
        removeTooltip(session, "hyperprior_pp_weight")
    }
    else if (input$select_mode == "power_prior") {
        model_mode('pp')
        model_mode_BDB('others')
        
        insertUI(
            selector = "#estimation_param",
            where = "beforeEnd",
            ui = uiOutput("hyperprior_pp_weight"),
            immediate = TRUE
        )
        output$hyperprior_pp_weight <- renderUI({
            tagList(
                inline(numericInput(inputId="pp_weight", label="Power parameter $\\alpha$", value = 0.5, width = "40%")),
                tags$script(paste0('renderMathInElement(document.getElementById("hyperprior_pp_weight"), {delimiters: [{left: "$", right: "$", display: false}]});')),
            )
        })
        
        addTooltip(session, "hyperprior_pp_weight", "This parameter controls the degree of borrowing and must be between 0 and 1", placement = "right", trigger = "hover", options = NULL)
    }
})

observeEvent(input$pp_weight, {
    req(input$pp_weight)
    if (input$pp_weight >= 0 && input$pp_weight <= 1) {
        runjs("document.getElementById('pp_weight').style.color = 'black';")
        enable('run')
    }
    else {
        runjs("document.getElementById('pp_weight').style.color = 'red';")
        disable('run')
    }
    
})

observeEvent(input$iter, {
    req(input$iter)
    updateNumericInput(session, 'warmup', value = as.integer(input$iter) / 2)
})

# Update priors
formula.component <- c("prior_beta", "prior_mu_beta", "prior_U", "prior_sigma_U", "prior_S", "prior_U_2", "prior_sigma_U_2", "prior_S_2", "prior_R")
lapply(formula.component, function (component) {
    output[[component]] <- renderUI({
        if (input$long) {
            long = "long"
        }
        else {
            long = "cross"
        }
        tagList(
            paste0('$', formula[[component]][[long]][[layer()]][[model_mode_BDB()]], '$'),
            tags$script(paste0('renderMathInElement(document.getElementById("', component, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
        )
    })
})
