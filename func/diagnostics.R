get_model <- function (div_name) {
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
  
  if (layer_actual >= 1) {
    insertUI(
      selector = paste0("#", div_name),
      where = "beforeEnd",
      ui = tags$div(
        class = "level",
        tags$label('Level-1'),
      ),
    )
    
    insertUI(
      selector = paste0("#", div_name),
      where = "beforeEnd",
      ui = uiOutput(paste0(div_name, '_1'), class = "equ"),
    )
    
    output[[paste0(div_name, '_1')]] <- renderUI({
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
        tags$script(paste0('renderMathInElement(document.getElementById("', div_name, '_1', '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
      )
    })
  }
  
  if (layer_actual >= 2) { # level-2
    insertUI(
      selector = paste0("#", div_name),
      where = "beforeEnd",
      ui = tags$div(
        class = "level",
        tags$label('Level-2'),
      ),
    )
    
    if (input$long) {
      row_max = 1 + strtoi(input$tf1) + length(input$td1)
      col_max = length(input$x1) + 1
    }
    else {
      row_max = 1 + length(input$x1)
      col_max = length(input$x2) + 1
    }
    
    for (i in 0: (row_max - 1)) {
      insertUI(
        selector = paste0("#", div_name),
        where = "beforeEnd",
        ui = uiOutput(paste0(div_name, '_2_', i), class = "equ"),
      )
    }
    
    lapply(0: (row_max - 1), function (i) {
      output[[paste0(div_name, '_2_', i)]] <- renderUI({
        if (layer() == 'one' && input$long) {
          major = paste0('\\beta_{', i, 'i}=\\gamma_{', i, '0}')
        }
        else if (layer() == 'two' && !input$long) {
          major = paste0('\\beta_{', i, 'j}=\\gamma_{', i, '0}')
        }
        else if (layer() == 'two' && input$long) {
          major = paste0('\\beta_{', i, 'ij}=\\gamma_{', i, '0j}')
        }
        
        for (j in 1: (col_max - 1)){
          if (exclude()[i + 1, j] == 0) {
            if (layer() == 'one' && input$long) {
              major = paste0(major, '+\\gamma_{', i, j, '}', input$x1[j], '_i')
            }
            else if (layer() == 'two' && !input$long) {
              major = paste0(major, '+\\gamma_{', i, j, '}', input$x2[j], '_j')
            }
            else if (layer() == 'two' && input$long) {
              major = paste0(major, '+\\gamma_{', i, j, 'j}', input$x1[j], '_{ij}')
            }
          }
        }
        
        j = col_max
        if (exclude()[i + 1, j] == 0) {
          if (layer() == 'one' && input$long) {
            major = paste0(major, '+U_{', i, 'i}')
          }
          else if (layer() == 'two' && !input$long) {
            major = paste0(major, '+U_{', i, 'j}')
          }
          else if (layer() == 'two' && input$long) {
            major = paste0(major, '+U_{', i, 'ij}')
          }
        }
        
        major = paste0('$', major, "$")
        
        tagList(
          major,
          tags$script(paste0('renderMathInElement(document.getElementById("', div_name, '_2_', i, '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
        )
      })
    })
  }
  
  if (layer_actual >= 3) { # level-3
    insertUI(
      selector = paste0("#", div_name),
      where = "beforeEnd",
      ui = tags$div(
        class = "level",
        tags$label('Level-3'),
      ),
    )
    
    row_max = 1 + strtoi(input$tf1) + length(input$td1)
    col_max = length(input$x1) + 1
    hei_max = length(input$x2) + 1
    
    insertUI(
      selector = paste0("#", div_name),
      where = "beforeEnd",
      ui = uiOutput(paste0(div_name, '_3'), class = "equ"),
    )
    
    output[[paste0(div_name, '_3')]] <- renderUI({
      major = ""
      for (i in 0: (row_max - 1)) {
        j_list = c(0)
        for (j in 1: (col_max - 1)) {
          if (exclude()[i + 1, j] == 0) {
            j_list = c(j_list, j)
          }
        }
        
        for (j in j_list) {
          major = paste0(major, '\\gamma_{', i, j, 'j}=\\delta_{', i, j, '0}')
          
          
          for (k in 1: (hei_max - 1)){
            if (exclude_2()[i + 1, j + 1, k] == 0) {
              major = paste0(major, '+\\delta_{', i, j, k, '}', input$x2[k], '_j')
            }
          }
          
          k = hei_max
          if (exclude_2()[i + 1, j + 1, k] == 0) {
            major = paste0(major, '+V_{', i, j, 'j}')
          }
          
          major = paste0(major, '\\\\')
          
        }
      }
            
      major = paste0('$', major, "$")
      
      tagList(
        major,
        tags$script(paste0('renderMathInElement(document.getElementById("', div_name, '_3', '"), {delimiters: [{left: "$", right: "$", display: false}]});')),
      )
    })
    
  }
  
}

observeEvent(c(input$long, input$t1, input$td1, input$tf1, input$y1, input$x1, input$x2, input$select_method, input$select_mode, input$add_2, input$add_3, input$delete_2, input$delete_3), {
  if (!is.null(input$x1) && !is.null(input$y1)) {
    removeUI('#diagnostics_model > .equ', multiple = TRUE, immediate = TRUE)
    removeUI('#diagnostics_model > .level', multiple = TRUE, immediate = TRUE)
    get_model('diagnostics_model')
  }
})



getLatex <- function (row_name) {
  for (t in names(info$var)) {
    if (info$var[[t]] == row_name) {
      res = t
      break
    }
  }
  substr(res, 2, 1000000L)
}

observeEvent(info$var, {
  updateSelectizeInput(session, 'param', choices = null, selected = null)
  req(info$var)
  updateSelectizeInput(session, 'param', choices = info$var, selected = info$var[1], options = katex)
})

#observeEvent(info$cycle, {
#    updateSelectizeInput(session, 'param_cycle', selected = 0,
#                         choices = setNames(0:length(info$cycle), c('(All)', info$cycle)))
#})

output$ic <- renderUI({
  req(info$ic)
  digits <- if (abs(info$ic[3, 1]) >= 1000) 0 else 2
  tex('\\textup{mean} = ', round(info$ic[3, 1], digits), ',\\ \\textup{s.e.} = ', round(info$ic[3, 2], digits))
})

output$rhat <- renderUI({
  req(info$rhat)
  tex(format(info$rhat, scientific = F))
})



row <- reactiveVal()

observeEvent(c(input$param), {
  # req(input$param)
  # var <- if (input$param == '(All)')
  #   info$var
  # else
  #   # else
  #   input$param
  # 
  # cycle2index = setNames(1:length(info$cycle), info$cycle)
  # index = cycle2index[input$w1]
  # cycle <- paste0('\\[', index, ',')
  
  # cycle <- if (input$param_cycle == '0')
  #     '\\['
  #     paste0('\\[', input$param_cycle, ',')
  
  row(unique(do.call(c, lapply(var, function(var) {
    grep(paste(paste0('(^', input$param, '$)'), collapse = '|'), rownames(info$est), value = T)
  }))))
  
  output$rhat_param <- renderUI({
    tex(format(info$est[row(), 'Rhat'], scientific = F))
  })
  
  #addTooltip(session, 'rhat_param', "R-hat is a convergence diagnostic, which compares the between- and within-chain estimates for model parameters and other univariate quantities of interest. If chains have not mixed well (ie, the between- and within-chain estimates don't agree), R-hat is larger than 1. It is recommended to run at least four chains and only using the sample if R-hat is less than 1.05.", placement = "right", trigger = "hover", options = NULL)
})


plotDens = function(cur_row, param_name) {
  stan_dens(fit(), cur_row) + xlab(TeX(paste0("$\\", param_name, "$"))) + ylab("Density") + 
    theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
}
output$dens <- renderPlot({
  req(input$param)
  req(length(row()) > 0)
  # fit_tmp <- fit()
  # names(fit_tmp) <- paste("$\\", names(fit_tmp), "$", sep="")
  # row_tmp <- paste("$\\", row(), "$", sep="")
  plotDens(row(), getLatex(input$param))
})
output$export_dens <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".zip", sep="")
  },
  content = function(file) {
    fs <- c()
    curdir <- getwd()
    tmpdir <- tempdir()
    setwd(tempdir())
    
    for (i in names(info$var)) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      path <- paste0("dens_", substr(i, 2, 100000L), ".png")
      fs <- c(fs, path)
      
      cur_row <- unique(do.call(c, lapply(var, function(var) {
        grep(paste(paste0('(^', info$var[[i]], '$)'), collapse = '|'), rownames(info$est), value = T)
      })))
      ggsave(path, plot = plotDens(cur_row, substr(i, 2, 100000L)), device = device)
    }
    
    res = zip(zipfile=file, files=fs)
    setwd(curdir)
    res
  },
  contentType = "application/zip"
)

plotTrace = function(cur_row, param_name) {
  stan_trace(fit(), cur_row) + xlab("Iteration") + ylab(TeX(paste0("$\\", param_name, "$"))) + 
    theme(text = element_text(size = 20), legend.title = element_text(size = 20), axis.text.y = element_text(size = 20))
}
output$trace <- renderPlot({
  req(input$param)
  req(length(row()) > 0)
  plotTrace(row(), getLatex(input$param))
})
output$export_trace <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".zip", sep="")
  },
  content = function(file) {
    fs <- c()
    curdir <- getwd()
    tmpdir <- tempdir()
    setwd(tempdir())
    
    for (i in names(info$var)) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      path <- paste0("trace_", substr(i, 2, 100000L), ".png")
      fs <- c(fs, path)
      
      cur_row <- unique(do.call(c, lapply(var, function(var) {
        grep(paste(paste0('(^', info$var[[i]], '$)'), collapse = '|'), rownames(info$est), value = T)
      })))
      ggsave(path, plot = plotTrace(cur_row, substr(i, 2, 100000L)), device = device)
    }
    
    res = zip(zipfile=file, files=fs)
    setwd(curdir)
    res
  },
  contentType = "application/zip"
)

plotAc = function(cur_row) {
  stan_ac(fit(), cur_row) + xlab("Lag") + ylab("Autocorrelation") +
    theme(text = element_text(size = 20), legend.title = element_text(size = 16))
}
output$ac <- renderPlot({
  req(input$param)
  req(length(row()) > 0)
  plotAc(row())
})
output$export_ac <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".zip", sep="")
  },
  content = function(file) {
    fs <- c()
    curdir <- getwd()
    tmpdir <- tempdir()
    setwd(tempdir())

    for (i in names(info$var)) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      path <- paste0("ac_", substr(i, 2, 100000L), ".png")
      fs <- c(fs, path)
      
      cur_row <- unique(do.call(c, lapply(var, function(var) {
        grep(paste(paste0('(^', info$var[[i]], '$)'), collapse = '|'), rownames(info$est), value = T)
      })))
      ggsave(path, plot = plotAc(cur_row), device = device)
    }

    res = zip(zipfile=file, files=fs)
    setwd(curdir)
    res
  },
  contentType = "application/zip"
)

# output$export_ac <- downloadHandler(
#   filename = function() {
#     paste("data-", Sys.Date(), ".png", sep="")
#   },
#   content = function(file) {
#     device <- function(..., width, height) {
#       grDevices::png(..., width = width, height = height,
#                      res = 300, units = "in")
#     }
#     ggsave(file, plot = plotAc(), device = device)
#   }
# )

