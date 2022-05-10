layer <- reactiveVal('one')

get_selection_params <- function() {
    params = c('z1', 'y1', 'x1')
    if (input$long) {
        params = c(params, 't1', 'td1', 'id1')
    }
    if (layer() == 'three') {
        params = c(params, 'g3', 'x3', 'g2', 'x2')
    }
    else if (layer() == 'two') {
        params = c(params, 'g2', 'x2')
    }
    
    return (params)
}

update_selections <- function() {
    var1 <- isolate(var1())
    
    params = get_selection_params()
    for (param in params) {
        selection = c(isolate(input[[param]]))
        others = c()
        for (param_others in params) {
            if (param != param_others) {
                others = c(others, isolate(input[[param_others]]))
            }
        }
        updateSelectInput(session, param, choices = var1[!(var1 %in% others)], selected = selection[!(selection %in% others)])
    }
}

data1 <- eventReactive(input$path1, {
    req(input$path1)
    
    # reset all the selected parameters
    params_to_reset = c(get_selection_params(), 'w1')
    for (param in params_to_reset) {
        updateSelectInput(session, param, choices = null, selected = null)
    }
    
    # load new data
    read.data(input$path1$datapath)
})

var1 <- eventReactive(data1(), {
    gsub('_', '\\\\_', colnames(data1()))
})

observe({
    tryCatch({
        cycle <- grep(var.z, var1(), ignore.case = T) # find matching cycle description in var list and return index
        z <- if (length(cycle) > 0) cycle[1] else 1
        y <- if (z == 1) 2 else 1
        updateSelectInput(session, 'z1', choices = var1(), selected = var1()[z])
        #updateSelectInput(session, 'y1', choices = c(var1()[-z], c('Intercept')), selected = var1()[y])
        updateSelectInput(session, 'y1', choices = var1()[-z], selected = var1()[y])
        updateSelectInput(session, 'x1', choices = var1()[-c(z, y)], selected = NULL)
    }, error = function(e) {
        updateSelectInput(session, 'z1', choices = null)
        updateSelectInput(session, 'y1', choices = null)
        updateSelectInput(session, 'x1', choices = null, selected = null)
    })
})

output$table1 <- renderDT({
    data1()
}, options = DToption)

observeEvent(input$z1, {
    var1 <- isolate(var1())
    data1 <- isolate(data1())
    
    update_selections()
    w1 <- as.character(sort(unique(data1[[isolate(input$z1)]])))
    updateSelectInput(session, 'w1', choices = w1, selected = w1[length(w1)])
})

observeEvent(c(input$t1, input$td1, input$id1, input$y1, input$x1, input$g2, input$x2, input$g3, input$x3), {
    update_selections()
})

observeEvent(input$add_2, {
    insertUI(
        selector = "#add_2",
        where = "beforeBegin",
        ui = tags$div(
            id = "level-2",
            
            div(id = "level_2_tag_div",
                div(id = "level_2_tag",
                    if (input$long){
                        h4(tags$b('Group Level'))
                    }
                    else {
                        h4(tags$b('Level-2'))
                    }
                )
            ),
            
            selectizeInput('g2', 'Group ID:', NULL, options = katex.tt),
            if (input$long){
                selectizeInput('x2', 'Group Level Independent Variable:', NULL, multiple = T, options = katex.tt)
            }
            else {
                selectizeInput('x2', 'Level-2 Independent Variable:', NULL, multiple = T, options = katex.tt)
            },
            actionButton("delete_2", "Delete Level-2"),
            #actionButton("add_3", "Add Level-3"),
        )
    )
    removeUI(
        selector = "#add_2",
    )
    if (input$long){
        updateSelectInput(session, "x1", label = 'Independent Variable:')
    }
    else {
        updateSelectInput(session, "x1", label = 'Level-1 Independent Variable:')
    }
    insertUI(
        selector = "#looic",
        where = "beforeEnd",
        ui = tags$div(
            id = "looic-2",
            inline(tags$label('Level-2 LOOIC:')),
            uiOutput('ic2', class='number', inline = T)
        )
    )
    
    layer('two')
    var1 <- isolate(var1())
    z1 <- isolate(input$z1)
    y1 <- isolate(input$y1)
    x1 <- isolate(input$x1)
    updateSelectInput(session, 'g2', choices = var1[!(var1 %in% c(z1, y1, x1))], selected = NULL)
    updateSelectInput(session, 'x2', choices = var1[!(var1 %in% c(z1, y1, x1))], selected = NULL)
})

observeEvent(input$add_3, {
    insertUI(
        selector = "#add_3",
        where = "beforeBegin",
        ui = tags$div(
            id = "level-3",
            h4(tags$b('Level-3')),
            selectizeInput('g3', 'Group ID:', NULL, options = katex.tt),
            selectizeInput('x3', 'Independent Variable:', NULL, multiple = T, options = katex.tt),
            actionButton("delete_3", "Delete Level-3"),
        )
    )
    removeUI(
        selector = "#add_3",
    )
    removeUI(
        selector = "#delete_2",
    )
    
    layer('three')
    var1 <- isolate(var1())
    z1 <- isolate(input$z1)
    y1 <- isolate(input$y1)
    x1 <- isolate(input$x1)
    x2 <- isolate(input$x2)
    g2 <- isolate(input$g2)
    updateSelectInput(session, 'g3', choices = var1[!(var1 %in% c(z1, y1, x1, x2, g2))], selected = NULL)
    updateSelectInput(session, 'x3', choices = var1[!(var1 %in% c(z1, y1, x1, x2, g2))], selected = NULL)
})

observeEvent(input$delete_2, {
    insertUI(
        selector = "#level-2",
        where = "beforeBegin",
        ui = actionButton("add_2", "Add Level-2"),
    )
    updateSelectInput(session, 'g2', choices = NULL, selected = NULL)
    updateSelectInput(session, 'x2', choices = NULL, selected = NULL)
    removeUI(
        selector = "#level-2",
    )
    
    updateSelectInput(session, "x1", label = 'Independent Variable:')
    removeUI(
        selector = "#looic-2",
    )
    
    layer('one')
    var1 <- isolate(var1())
    z1 <- isolate(input$z1)
    y1 <- isolate(input$y1)
    x1 <- isolate(input$x1)
    updateSelectInput(session, 'x1', choices = var1[!(var1 %in% c(z1, y1))], selected = x1)
})

observeEvent(input$delete_3, {
    insertUI(
        selector = "#level-2",
        where = "beforeEnd",
        ui = actionButton("add_3", "Add Level-3"),
    )
    insertUI(
        selector = "#level-2",
        where = "beforeEnd",
        ui = actionButton("delete_2", "Delete Level-2"),
    )
    updateSelectInput(session, 'g3', choices = NULL, selected = NULL)
    updateSelectInput(session, 'x3', choices = NULL, selected = NULL)
    removeUI(
        selector = "#level-3",
    )
    
    layer('two')
    var1 <- isolate(var1())
    z1 <- isolate(input$z1)
    y1 <- isolate(input$y1)
    x1 <- isolate(input$x1)
    x2 <- isolate(input$x2)
    g2 <- isolate(input$g2)
    updateSelectInput(session, 'x1', choices = var1[!(var1 %in% c(z1, y1, x2, g2))], selected = x1)
    updateSelectInput(session, 'x2', choices = var1[!(var1 %in% c(z1, y1, x1, g2))], selected = x2)
})

observeEvent(input$long, {
    if (input$long) {
        removeUI(
            selector = "#level_1_tag",
            immediate = TRUE
        )
        removeUI(
            selector = "#level_2_tag",
            immediate = TRUE
        )
        insertUI(
            selector = "#level_2_tag_div",
            where = "beforeEnd",
            ui = tags$div(
                id = "level_2_tag",
                h4(tags$b('Group Level'))
            ),
            immediate = TRUE
        )
        updateSelectInput(session, "x1", label = 'Independent Variable:')
        updateSelectInput(session, "x2", label = 'Group Level Independent Variable:')
        
        removeUI(
            selector = "#longitudinal_time",
            immediate = TRUE
        )
        
        insertUI(
            selector = "#longitudinal_param",
            where = "beforeEnd",
            ui = tags$div(
                id = "longitudinal_time",
                selectizeInput('t1', 'Time Variable:', NULL, options = katex.tt),
                selectizeInput('tf1', 'Functional Form of Time Variable:', c("linear" = 1, "quadratic" = 2, "cubic" = 3), selected = "linear"),
                selectizeInput('x1', 'Time Invariant Variable:', NULL, multiple = T, options = katex.tt),
                selectizeInput('td1', 'Time Varying Variable:', NULL, multiple = T, options = katex.tt),
                selectizeInput('id1', 'Individual ID:', NULL, options = katex.tt),
            ),
            immediate = TRUE
        )
        update_selections()
    }
    else {
        removeUI(
            selector = "#level_1_tag",
            immediate = TRUE
        )
        insertUI(
            selector = "#level_1_tag_div",
            where = "beforeEnd",
            ui = tags$div(
                id = "level_1_tag",
                h4(tags$b('Level-1'))
            ),
            immediate = TRUE
        )
        removeUI(
            selector = "#level_2_tag",
            immediate = TRUE
        )
        insertUI(
            selector = "#level_2_tag_div",
            where = "beforeEnd",
            ui = tags$div(
                id = "level_2_tag",
                h4(tags$b('Level-2'))
            ),
            immediate = TRUE
        )
        
        if (layer() == 'one') {
            updateSelectInput(session, "x1", label = 'Independent Variable:')
        }
        else {
            updateSelectInput(session, "x1", label = 'Level-1 Independent Variable:')
            updateSelectInput(session, "x2", label = 'Level-2 Independent Variable:')
        }
        
        
        removeUI(
            selector = "#longitudinal_time",
            immediate = TRUE
        )
        
        insertUI(
            selector = "#longitudinal_param",
            where = "beforeEnd",
            ui = tags$div(
                id = "longitudinal_time",
                selectizeInput('x1', 'Independent Variable:', NULL, multiple = T, options = katex.tt),
            ),
            immediate = TRUE
        )
        update_selections()
    }
})
