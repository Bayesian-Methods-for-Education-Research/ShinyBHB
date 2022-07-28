# server.R includes the algorithms of BDB and other borrowing methods, and the process of taking inputs and running data.

library(shiny)
null <- character(0)
DToption = list(scrollX = T)


read.data <- function(path) {
    ext <- file_ext(path)
    data <- if (ext == 'csv')
        read.csv(path)
    else if (ext == 'dta')
        read_dta(path)
    else if (ext == 'sav')
        read_sav(path)
    else if (ext == 'xlsx')
        read.xlsx(path)
    else
        read.table(path, header = T)
    for (var in colnames(data))
        if (is.character(data[[var]]))
            data[[var]] <- factor(data[[var]])
    data
}

real <- function(x) {
    if (is.null(x)) 0. else as.numeric(x)
}

math <- function(x) {
    setNames(x, sapply(x, function(s) {
        if (s %in% c('beta', 'sigma'))
            paste0('\\', s)
        else
            s
    }))
}

show <- function(...) {
    runjs(paste0('document.getElementById("fit").innerHTML += "', ..., '\\n";'))
}



shinyServer(function(input, output, session) {
    ## file to write progress
    tfile <- tempfile(fileext = ".txt")
    
    ## reactive values
    r <- reactiveValues(
        progress_mtime = -1,
        progress = "",
        running = FALSE
    )
    
    count <- 0
    
    tex <- function(...) {
        count <<- count + 1
        id <- paste0('katex-', count)
        tagList(
            span(id = id),
            tags$script(HTML(paste0('katex.render("\\\\displaystyle{', gsub('\\', '\\\\', paste0(...), fixed = T),
                                    '}", document.getElementById("', id, '"));')))
        )
    }
    
    latex <- function(...) {
        count <<- count + 1
        id <- paste0('katex-', count)
        tagList(
            div(id = id),
            tags$script(HTML(paste0('katex.render("', gsub('\\', '\\\\', paste0(...), fixed = T),
                                    '", document.getElementById("', id, '"), {displayMode: true});')))
        )
    }
    
    source('func/data.R', local = T)
    source('func/model.R', local = T)
    source('func/diagnostics.R', local = T)
    source('func/result.R', local = T)
    
    insertUI(
        selector = "#core-label",
        where = "afterEnd",
        ui = inline(
            HTML(paste0("<div class='core' data-tooltip='", parallel::detectCores(), " is the number of cores your system has. We recommend specifying the number of cores to be the same as the number of chains.", "' style='display: inline;'>  <i class='fas fa-info-circle'></i></div>"))
        )
    )
    
    fit <- reactiveVal()
    info <- reactiveValues()
    
    ## print progress
    output$progress <- renderText({
        req(r$progress)
        paste(r$progress, collapse = "\n")
    })
    
    ## observe status of bg process
    observe({
        req(r$bg_process, r$poll)
        ## keep re-executing observer as
        ## long as bg process is running
        invalidateLater(millis = 1000, session)
        ## read current progress if file is modified
        mtime <- file.info(tfile)$mtime
        if(mtime > r$progress_mtime) {
            r$progress <- readLines(tfile)
            r$progress_mtime <- mtime
        }
        ## extract draws when bg process is finished
        if(!r$bg_process$is_alive() && !r$killed) {
            fit(r$bg_process$get_result())
            r$poll <- FALSE  ## stop polling bg process
            
            runjs('document.getElementById("fit").innerHTML += "Summarizing...\\n";')
            
            info$est <- summary(fit())$summary
            info$ic <- loo(fit())$estimates
            info$ic_sch <- if (layer() == 'two') loo(fit(), 'log_lik_sch')$estimates else NULL
            info$rhat <- max(info$est[, 'Rhat'], na.rm = T)
            
            cycles <- relevel(factor(data1()[[input$z1]]), input$w1)
            info$cycle <- unique(levels(cycles))
            cycle_index = setNames(1:length(info$cycle), info$cycle)
            options(max.print=999999)
            
            if (layer() == 'one' && !input$long) {
                info$var <- c('\\sigma_R'='sigma_r')
                if (model_mode() == 'BDB'){
                    for (i in 1 : (length(input$x1) + 1)) {
                        info$var[paste0('\\beta_', i-1)] = paste0('beta\\[', cycle_index, ',', i, '\\]')
                    }
                }
                else {
                    for (i in 1 : (length(input$x1) + 1)) {
                        info$var[paste0('\\beta_', i-1)] = paste0('beta\\[', i, '\\]')
                    }
                }
            }
            else if (layer() == 'two' && !input$long) {
                info$var <- c('\\sigma_R'='sigma_r')
                ind_cur <- 1
                for (i in 0: length(input$x1)) {
                    info$var[paste0('\\gamma_{', i, '0}')] = paste0('beta\\[', ind_cur, '\\]')
                    ind_cur <- ind_cur + 1
                    for (j in 1: length(input$x2)) {
                        if (!exclude()[i + 1, j]) {
                            info$var[paste0('\\gamma_{', i, j, '}')] = paste0('beta\\[', ind_cur, '\\]')
                            ind_cur <- ind_cur + 1
                        }
                    }
                }
                ind_cur_i <- 1
                for (i in 0: length(input$x1)) {
                    if (!exclude()[i + 1, length(input$x2) + 1]) {
                        info$var[paste0('\\text{Var}(U_{', i, 'j})')] = paste0('sigma_u\\[', ind_cur_i, ',', ind_cur_i, '\\]')
                        if (i != length(input$x1)) {
                            ind_cur_j <- ind_cur_i + 1
                            for (j in (i + 1): length(input$x1)) {
                                if (!exclude()[j + 1, length(input$x2) + 1]) {
                                    info$var[paste0('\\text{Cov}(U_{', i, 'j},U_{', j, 'j})')] = paste0('sigma_u\\[', ind_cur_i, ',', ind_cur_j, '\\]')
                                    ind_cur_j <- ind_cur_j + 1
                                }
                            }
                        }
                        
                        ind_cur_i <- ind_cur_i + 1
                    }
                }
            }
            else if (layer() == 'one' && input$long) {
                info$var <- c('\\sigma_R'='sigma_r')
                ind_cur <- 1
                for (i in 0: (strtoi(input$tf1) + length(input$td1))) {
                    info$var[paste0('\\gamma_{', i, '0}')] = paste0('beta\\[', ind_cur, '\\]')
                    ind_cur <- ind_cur + 1
                    for (j in 1: length(input$x1)) {
                        if (!exclude()[i + 1, j]) {
                            info$var[paste0('\\gamma_{', i, j, '}')] = paste0('beta\\[', ind_cur, '\\]')
                            ind_cur <- ind_cur + 1
                        }
                    }
                }
                ind_cur_i <- 1
                for (i in 0: (strtoi(input$tf1) + length(input$td1))) {
                    if (!exclude()[i + 1, length(input$x1) + 1]) {
                        info$var[paste0('\\text{Var}(U_{', i, 'i})')] = paste0('Sigma_u\\[', ind_cur_i, ',', ind_cur_i, '\\]')
                        if (i != strtoi(input$tf1) + length(input$td1)) {
                            ind_cur_j <- ind_cur_i + 1
                            for (j in (i + 1): (strtoi(input$tf1) + length(input$td1))) {
                                if (!exclude()[j + 1, length(input$x1) + 1]) {
                                    info$var[paste0('\\text{Cov}(U_{', i, 'i},U_{', j, 'i})')] = paste0('Sigma_u\\[', ind_cur_i, ',', ind_cur_j, '\\]')
                                    ind_cur_j <- ind_cur_j + 1
                                }
                            }
                        }
                        
                        ind_cur_i <- ind_cur_i + 1
                    }
                }
            }
           else if (layer() == 'two' && input$long) {
                info$var <- c('\\sigma_R'='sigma_r')
                ind_cur <- 1
                for (i in 0: (strtoi(input$tf1) + length(input$td1))) {
                    info$var[paste0('\\delta_{', i, '00}')] = paste0('beta\\[', ind_cur, '\\]')
                    ind_cur <- ind_cur + 1
                    for (k in 1: length(input$x2)) {
                        if (!exclude_2()[i + 1, 1, k]) {
                            info$var[paste0('\\delta_{', i, '0', k, '}')] = paste0('beta\\[', ind_cur, '\\]')
                            ind_cur <- ind_cur + 1
                        }
                    }
                        
                    for (j in 1: length(input$x1)) {
                        if (!exclude()[i + 1, j]) {
                            info$var[paste0('\\delta_{', i, j, '0}')] = paste0('beta\\[', ind_cur, '\\]')
                            ind_cur <- ind_cur + 1
                            for (k in 1: length(input$x2)) {
                                if (!exclude_2()[i + 1, j + 1, k]) {
                                    info$var[paste0('\\delta_{', i, j, k, '}')] = paste0('beta\\[', ind_cur, '\\]')
                                    ind_cur <- ind_cur + 1
                                }
                            }
                        }
                    }
                }
                
                ind_cur_i <- 1
                for (i in 0: (strtoi(input$tf1) + length(input$td1))) {
                    if (!exclude()[i + 1, length(input$x1) + 1]) {
                        info$var[paste0('\\text{Var}(U_{', i, 'ij})')] = paste0('Sigma_u\\[', ind_cur_i, ',', ind_cur_i, '\\]')
                        if (i != strtoi(input$tf1) + length(input$td1)) {
                            ind_cur_j <- ind_cur_i + 1
                            for (j in (i + 1): (strtoi(input$tf1) + length(input$td1))) {
                                if (!exclude()[j + 1, length(input$x1) + 1]) {
                                    info$var[paste0('\\text{Cov}(U_{', i, 'ij},U_{', j, 'ij})')] = paste0('Sigma_u\\[', ind_cur_i, ',', ind_cur_j, '\\]')
                                    ind_cur_j <- ind_cur_j + 1
                                }
                            }
                        }
                        
                        ind_cur_i <- ind_cur_i + 1
                    }
                }
                
                ind_cur_i <- 1
                for (i in 0: (strtoi(input$tf1) + length(input$td1))) {
                    for (j in 0: length(input$x1)) {
                        if (!exclude_2()[i + 1, j + 1, length(input$x2) + 1]) {
                            info$var[paste0('\\text{Var}(V_{', i, j, 'j})')] = paste0('Sigma_v\\[', ind_cur_i, ',', ind_cur_i, '\\]')
                            
                            ind_cur_j <- ind_cur_i + 1
                            n_i <- i
                            n_j <- j
                            while (TRUE) {
                                n_j <- n_j + 1
                                if (n_j == length(input$x1) + 1) {
                                    n_j <- 0
                                    n_i <- n_i + 1
                                }
                                if (n_i == strtoi(input$tf1) + length(input$td1) + 1) {
                                    break
                                }
                                if (!exclude_2()[n_i + 1, n_j + 1, length(input$x2) + 1]) {
                                    info$var[paste0('\\text{Cov}(V_{', i, j, 'j},V_{', n_i, n_j, 'j})')] = paste0('Sigma_v\\[', ind_cur_i, ',', ind_cur_j, '\\]')
                                    ind_cur_j <- ind_cur_j + 1
                                }
                            }
                            
                            ind_cur_i <- ind_cur_i + 1
                        }
                    }
                }
            } 
                      runjs('document.getElementById("fit").innerHTML += "Done!\\n";')
            updateActionButton(session, 'run', 'Run')
            enable('run')
            shinyjs::removeClass("run", "red")
            r$running <- FALSE;
        }
    })
    
    observeEvent(input$run, {
        if (r$running) {
            r$bg_process$kill_tree();
            r$killed <- TRUE;
            runjs('document.getElementById("fit").innerHTML += "Killed!\\n";')
            updateActionButton(session, 'run', 'Run')
            enable('run')
            shinyjs::removeClass("run", "red")
            r$running <- FALSE;
            return ()
        }
        
        
        r$progress = ""
        
        updateActionButton(session, 'run', 'Running')
        disable('run')
        runjs('document.getElementById("fit").innerHTML = "Compiling...\\n";')
        
        chain <- as.integer(input$chain)
        core <- as.integer(input$core)
        warmup <- as.integer(input$warmup)
        iter <- max(as.integer(input$iter), warmup + 1)
        thin <- as.integer(input$thin)
        
        cycles <- relevel(factor(data1()[[input$z1]]), input$w1)
        cycle <- as.integer(cycles)
        C <- max(cycle)
        data <- data1()[order(cycle), ]
        cycle <- sort(cycle)
        
        getExpression <- function(component) {
            if (component == 'sigma_beta_std') {
                if (input[['dist_sigma_beta']] == 'inv_gamma') {
                    return('sqrt(sigma_beta_std)')
                }
                else {
                    return('sigma_beta_std')
                }
            }
            
            if (component == 'nu') {
                if (is.null(input[['nu']])) {
                    return ("")
                }
                else {
                    return (as.character(input[['nu']]))
                }
            }
            
              if (component == 'nu_2') {
                if (is.null(input[['nu_2']])) {
                    return ("")
                }
                else {
                    return (as.character(input[['nu_2']]))
                }
            }

            if (is.null(input[[paste0("dist_", component)]])) {
                return ("")
            }
            
            
            if (input[[paste0("dist_", component)]] == 'fix_value') {
                return(as.character(input[[paste0(component, '_b')]]))
            }

            result = paste0(input[[paste0("dist_", component)]], '(')
            if (hyperprior$Omega_a[[input[[paste0("dist_", component)]]]] == '0') {
                if (input[[paste0("dist_", component)]] != "lkj_corr_cholesky") {
                    result = paste0(result, '0, ')
                }
            }
            else {
                result = paste0(result, input[[paste0(component, '_a')]], ', ')
            }
            
            if (hyperprior$Omega_b[[input[[paste0("dist_", component)]]]] == '0') {
                result = paste0(result, '0)')
            }
            else {
                result = paste0(result, input[[paste0(component, '_b')]], ')')
            }
            

            return(result)
        }
        
        
        
        if (layer() == 'two') {
            if (input$long) {
                v1 <- c('1', paste0('I(', input$t1, '^', 1:as.integer(input$tf1), ')'), input$td1)
                v2 <- c('1', input$x1)
                v3 <- input$x2
                fe <- '~1'
                for (i1 in seq_len(length(v1))) {
                    x1 <- v1[i1]
                    for (i2 in seq_len(length(v2))) {
                        x2 <- v2[i2]
                        if ((i1 != 1 || i2 != 1) && (i2 == 1 || !exclude()[i1, i2 - 1]))
                            fe <- paste0(fe, '+', paste0('I(', x1, '*', x2, ')'))
                        for (i3 in seq_len(length(v3))) {
                            x3 <- v3[i3]
                            if (!exclude_2()[i1, i2, i3])
                                fe <- paste0(fe, '+', paste0('I(', x1, '*', x2, '*', x3, ')'))
                        }
                    }
                }
                print(fe)
                data.fe <- model.matrix(as.formula(fe), data)
                
                re1 <- '~1'
                for (i1 in 2:length(v1))
                    if (!exclude()[i1, length(v2)])
                        re1 <- paste0(re1, '+', v1[i1])
                print(re1)
                data.re1 <- model.matrix(as.formula(re1), data)
                
                re2 <- '~1'
                for (i1 in seq_len(length(v1))) {
                    x1 <- v1[i1]
                    for (i2 in seq_len(length(v2))) {
                        x2 <- v2[i2]
                        if (i1 == 1 && i2 == 1) next
                        if (!exclude_2()[i1, i2, length(v3) + 1])
                            re2 <- paste0(re2, '+', paste0('I(', x1, '*', x2, ')'))
                    }
                }
                print(re2)
                data.re2 <- model.matrix(as.formula(re2), data)
                
                sch <- as.integer(factor(paste0(sprintf('%09d', cycle), '*** --- ***', data[[input$g2]])))
                stu <- as.integer(factor(paste0(sprintf('%09d', cycle), '*** --- ***', data[[input$id1]], '*** --- ***', data[[input$g2]])))
                
                cycle.sch <- (data.frame(cycle = cycle, id = sch) %>%
                                  group_by(id) %>%
                                  summarize(cycle = median(cycle)))$cycle
                sid <- (data.frame(sid = sch, id = stu) %>%
                            group_by(id) %>%
                            summarize(sid = median(sid)))$sid
                dat <- list(S = nrow(data), SS = sum(cycle == 1), N = max(stu), NN = max(stu[cycle == 1]),
                            M = max(sch), MM = max(sch[cycle == 1]), C = C,
                            K = ncol(data.fe), K1 = ncol(data.re1), K2 = ncol(data.re2),
                            x = data.fe, x1 = data.re1, x2 = data.re2, y = data[[input$y1]],
                            a = c(1, rep(input$pp_weight, C - 1)),
                            id = stu, sid = sid, cycle = cycle.sch)
                
                model.str <- read_file(paste0('model/long_two_', model_mode(), '.stan')) %>%
                    str_replace('SIGMA_BETA_STD', getExpression('sigma_beta_std')) %>%
                    str_replace('SIGMA_BETA', getExpression('sigma_beta')) %>%
                    str_replace('SIGMA_0_BETA', getExpression('sigma_0_beta')) %>%
                    str_replace('NU', getExpression('nu')) %>%
                    str_replace('NU', getExpression('nu')) %>%
                    str_replace('TAU', getExpression('tau')) %>%
                    str_replace('OMEGA', getExpression('Omega')) %>%
                    str_replace('NU2', getExpression('nu_2')) %>%
                    str_replace('NU2', getExpression('nu_2')) %>%
                    str_replace('TAU2', getExpression('tau_2')) %>%
                    str_replace('OMEGA2', getExpression('Omega_2')) %>%
                    str_replace('SIGMA_R', getExpression('sigma_R'))
            }
            else {
                fe <- '~1'
                for (i2 in seq_len(length(input$x2)))
                    if (!exclude()[1, i2])
                        fe <- paste0(fe, '+', input$x2[i2])
                for (i1 in seq_len(length(input$x1))) {
                    x1 <- input$x1[i1]
                    fe <- paste0(fe, '+', x1)
                    for (i2 in seq_len(length(input$x2)))
                        if (!exclude()[i1 + 1, i2])
                            fe <- paste0(fe, '+I(', x1, '*', input$x2[i2], ')')
                }
                print(fe)
                data.fe <- model.matrix(as.formula(fe), data)
                
                re <- '~1'
                for (i1 in seq_len(length(input$x1)))
                    if (!exclude()[i1 + 1, length(input$x2) + 1])
                        re <- paste0(re, '+', input$x1[i1])
                data.re <- model.matrix(as.formula(re), data)
                
                sch <- as.integer(factor(paste0(sprintf('%09d', cycle), '*** --- ***', data[[input$g2]])))
                cycle.sch <- (data.frame(cycle = cycle, id = sch) %>%
                                  group_by(id) %>%
                                  summarize(cycle = median(cycle)))$cycle
                dat <- list(N = nrow(data), NN = sum(cycle == 1), M = ncol(data.fe), MM = ncol(data.re),
                            S = max(sch), SS = max(sch[cycle == 1]), C = C,
                            x = data.fe, xx = data.re, y = data[[input$y1]],
                            a = c(1, rep(input$pp_weight, C - 1)),
                            sch = sch, cycle = cycle, cycle_sch = cycle.sch,
                            nu1 = 0.001, nu2 = 0.001, nu = 10)

                model.str <- read_file(paste0('model/two_', model_mode(), '.stan')) %>%
                    str_replace('SIGMA_BETA_STD', getExpression('sigma_beta_std')) %>%
                    str_replace('SIGMA_BETA', getExpression('sigma_beta')) %>%
                    str_replace('SIGMA_0_BETA', getExpression('sigma_0_beta')) %>%
                    str_replace('NU', getExpression('nu')) %>%
                    str_replace('NU', getExpression('nu')) %>%
                    str_replace('TAU', getExpression('tau')) %>%
                    str_replace('OMEGA', getExpression('Omega')) %>%
                    str_replace('SIGMA_R', getExpression('sigma_R'))
            }
            
        } else {
            if (input$long) {
                v1 <- c(paste0('I(', input$t1, '^', 1:as.integer(input$tf1), ')'), input$td1)
                fe <- '~1'
                for (i2 in seq_len(length(input$x1)))
                    if (!exclude()[1, i2])
                        fe <- paste0(fe, '+', input$x1[i2])
                for (i1 in seq_len(length(v1))) {
                    x1 <- v1[i1]
                    fe <- paste0(fe, '+', x1)
                    for (i2 in seq_len(length(input$x1)))
                        if (!exclude()[i1 + 1, i2])
                            fe <- paste0(fe, '+I(', x1, '*', input$x1[i2], ')')
                }
                print(fe)
                data.fe <- model.matrix(as.formula(fe), data)
                
                re <- '~1'
                for (i1 in seq_len(length(v1)))
                    if (!exclude()[i1 + 1, length(input$x1) + 1])
                        re <- paste0(re, '+', v1[i1])
                print(re)
                data.re <- model.matrix(as.formula(re), data)
                
                stu <- as.integer(factor(paste0(sprintf('%09d', cycle), '*** --- ***', data[[input$id1]])))
                cycle.stu <- (data.frame(cycle = cycle, id = stu) %>%
                                  group_by(id) %>%
                                  summarize(cycle = median(cycle)))$cycle
                dat <- list(S = nrow(data), SS = sum(cycle == 1), N = max(stu), NN = max(stu[cycle == 1]),
                            C = C, K = ncol(data.fe), K1 = ncol(data.re),
                            x = data.fe, x1 = data.re, y = data[[input$y1]],
                            a = c(1, rep(input$pp_weight, C - 1)),
                            id = stu, cycle = cycle.stu)

                model.str <- read_file(paste0('model/long_one_', model_mode(), '.stan')) %>%
                    str_replace('SIGMA_BETA_STD', getExpression('sigma_beta_std')) %>%
                    str_replace('SIGMA_BETA', getExpression('sigma_beta')) %>%
                    str_replace('SIGMA_0_BETA', getExpression('sigma_0_beta')) %>%
                    str_replace('NU', getExpression('nu')) %>%
                    str_replace('NU', getExpression('nu')) %>%
                    str_replace('TAU', getExpression('tau')) %>%
                    str_replace('OMEGA', getExpression('Omega')) %>%
                    str_replace('SIGMA_R', getExpression('sigma_R'))
            }
            else {
                data.fe <- model.matrix(~ ., data[, input$x1, drop = F])
                dat <- list(N = nrow(data.fe), M = ncol(data.fe), C = C, NN = sum(cycle == 1),
                            x = data.fe, y = data[[input$y1]], cycle = cycle,
                            a = c(1, rep(input$pp_weight, C - 1)))

                model.str <- read_file(paste0('model/one_', model_mode(), '.stan')) %>%
                    str_replace('SIGMA_BETA_STD', getExpression('sigma_beta_std')) %>%
                    str_replace('SIGMA_BETA', getExpression('sigma_beta')) %>%
                    str_replace('SIGMA_0_BETA', getExpression('sigma_0_beta')) %>%
                    str_replace('SIGMA_R', getExpression('sigma_R'))
            }
        }
        runjs('document.getElementById("fit").innerHTML += "Sampling...\\n";')
        
        r$bg_process <- callr::r_bg(
            func = function(model.str, dat, chain, warmup, iter, core, thin) {
                stan.model <- rstan::stan_model(model_code = model.str)
                
                rstan::sampling(stan.model, dat, chains = chain, warmup = warmup, iter = iter, cores = core, thin = thin)
            },
            args = list(model.str = model.str, dat = dat, chain = chain, warmup = warmup, iter = iter, core = core, thin = thin),
            stdout = tfile,
            stderr = tfile,
            supervise = TRUE,
        )
        r$poll <- TRUE
        r$killed <- FALSE
        r$running <- TRUE
        updateActionButton(session, 'run', 'Stop')
        enable('run')
        shinyjs::addClass("run", "red")
    })
})