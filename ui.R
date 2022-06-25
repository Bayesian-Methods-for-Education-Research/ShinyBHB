#ui.R clarifies the basic layout of the app, including the tabs and items inside every page.


library(shiny)
library(shinyBS)

navbarPage('Bayesian Historical Borrowing', theme = shinytheme('flatly'),
    tabPanel('Data',
        useShinyjs(),
        tags$head(
            tags$link(rel = 'stylesheet', href = 'ui.css'),
            tags$link(rel = 'stylesheet', href = 'katex.min.css'),
            tags$script(defer = NA, src = 'katex.min.js'),
            tags$script(defer = NA, src = 'auto-render.min.js'),
            tags$script('document.addEventListener("DOMContentLoaded", function() {
                             renderMathInElement(document.body, {
                                 delimiters: [{left: "$", right: "$", display: false}]
                             });
                         })'),
            tags$script('$(document).ready(function(){
                  var objDiv = document.getElementById("progress");
                  // create an observer instance
                  var observer = new MutationObserver(function(mutations) {
                    objDiv.scrollTop = objDiv.scrollHeight - objDiv.clientHeight;
                  });
                  // configuration of the observer
                  var config = {childList: true};
                  // observe objDiv
                  observer.observe(objDiv, config);
                })'),
            tags$style(".tooltip{width:200px;} 
                        .selectize-input{width:100%;} 
                        button.red {background-color: red;} 
                        .hyperprior_input input {height: 38px;} 
                        
                        div.hyperprior_label {position: relative; top: 13px;} 
                        span.number {position: relative; bottom: 7px;}
                        span#rhat_param {bottom: 5px !important;}
                        label#core-label {
                            display: inline;
                        }
                        .dataTables_filter { display: none; }
                        [data-tooltip]:before {
                            /* needed - do not touch */
                            content: attr(data-tooltip);
                            position: absolute;
                            opacity: 0;
                            
                            /* customizable */
                            transition: all 0.15s ease;
                            padding: 10px;
                            color: white;
                            border-radius: 10px;
                            box-shadow: 2px 2px 1px silver;    
                        }
                        
                        [data-tooltip]:hover:before {
                            /* needed - do not touch */
                            opacity: 1;
                            z-index: 2;
                            /* customizable */
                            background: black;
                            margin-top: 30px;
                            width: 300px;
                            text-align: left;
                        }
                        
                        .core[data-tooltip]:hover:before {
                            margin-left: -50px;
                        }
                        
                        .LOOIC[data-tooltip]:hover:before {
                            margin-left: -50px;
                        }
                        
                        .n_eff[data-tooltip]:hover:before {
                            margin-left: -150px;
                        }
                        
                        .Rhat[data-tooltip]:hover:before {
                            margin-left: -250px;
                        }
                        
                        [data-tooltip]:not([data-tooltip-persistent]):before {
                            pointer-events: none;
                        }
                        /*.dataTables_scrollBody {
                            position: absolute !important;
                        }
                        .dataTables_scrollHead {
                            position: absolute !important;
                        }
                        .dataTables_scrollHead {
                            overflow: visible !important;
                        }*/
                        .dataTables_scrollHead {
                            position: static !important;
                        }
                        .dataTables_scrollBody {
                            position: static !important;
                        }
                        .col-sm-6 {
                            position: static !important;
                        }
                       ")
        ),

        sidebarLayout(
            sidebarPanel(
                fileInput('path1', 'Data:',
                          accept = c('.csv', '.dta', '.sav', '.xlsx', '.txt')),
                div(id = "level_1_tag_div",
                    div(id = "level_1_tag",
                        h4(tags$b('Level-1'))
                    )
                ),
                selectizeInput('z1', 'Cycle Variable:', NULL, options = katex.tt),
                selectizeInput('w1', 'Currect Cycle:', NULL, options = katex.tt),
                selectizeInput('y1', 'Dependent Variable:', NULL, options = katex.tt),
                div(id = "longitudinal_param",
                    checkboxInput('long', 'Use Longitudinal Model', value = FALSE, width = NULL),
                    div(id = "longitudinal_time",
                        selectizeInput('x1', 'Independent Variable:', NULL, multiple = T, options = katex.tt)
                    )
                ),
                actionButton("add_2", "Add Level-2"),
            ),
            mainPanel(
                DTOutput('table1')
            )
        )
    ),
    tabPanel('Estimation',
             
        sidebarLayout(
            sidebarPanel(
                div(id = "estimation_param",
                    selectizeInput('select_method', 'Type of Borrowing', c("No borrowing" = "no_bor", "Static borrowing" = "static_bor", "Dynamic borrowing" = "dynamic_bor"), selected = "no_bor"),
                    selectizeInput('select_mode', 'Method', c("Noninformative" = "noninf"), selected = "noninf")
                ),
                fluidRow(
                    column(6,
                           numericInput('chain', 'Number of chains', 4, min = 1),
                           numericInput('core', paste0('Number of cores out of ', parallel::detectCores()), min(4, parallel::detectCores()), min = 1, max = parallel::detectCores()),
                           
                           #bsTooltip('core', paste0(parallel::detectCores(), " is the number of cores your system has."), placement = "right", trigger = "hover", options = NULL),
                           numericInput('thin', 'Thinning interval', 10, min = 1)
                    ),
                    column(6,
                           numericInput('iter', 'Total iterations', 20000, min = 0, step = 500),
                           numericInput('warmup', 'Warmup iterations', 10000, min = 0, step = 500)
                           )
                ),
                actionButton('run', 'Run'),
                hr(),
                verbatimTextOutput('fit'),
                tags$style(type='text/css', '#progress {white-space: pre-wrap; max-height: 400px;}'),
                verbatimTextOutput("progress"),
            ),
            mainPanel(id = "estimation_panel",
                h4(tags$b('Model:')),
                tags$label('Level-1'),
                uiOutput('equ1'),
                div(id = "head-prior", h4(tags$b('Prior Distribution:'))),
                div(id = 'Prior'),
                uiOutput("prior_beta", class = "prior"),
                uiOutput("prior_mu_beta", class = "prior"),
                uiOutput("prior_U_2", class = "prior"),
                uiOutput("prior_sigma_U_2", class = "prior"),
                uiOutput("prior_S_2", class = "prior"),
                uiOutput("prior_U", class = "prior"),
                uiOutput("prior_sigma_U", class = "prior"),
                uiOutput("prior_S", class = "prior"),
                uiOutput("prior_R", class = "prior"),
                
                h4(tags$b('Hyperprior: * ')),
                div(id = 'Hyperprior'),
                uiOutput("hyperprior_sigma_beta", class = "hyperprior"),
                uiOutput("hyperprior_sigma_0_beta", class = "hyperprior"),
                uiOutput("hyperprior_nu_2", class = "hyperprior"),
                uiOutput("hyperprior_tau_2", class = "hyperprior"),
                uiOutput("hyperprior_Omega_2", class = "hyperprior"),
                uiOutput("hyperprior_nu", class = "hyperprior"),
                uiOutput("hyperprior_tau", class = "hyperprior"),
                uiOutput("hyperprior_Omega", class = "hyperprior"),
                uiOutput("hyperprior_sigma_R", class = "hyperprior"),
                
                helpText('* Note that data have been standardized during the computation. Hyperpriors should be specified based on standardized data, and results will be converted back to their original scale by default.'),
                
            ),
        ),
    ),
    tabPanel('Diagnostics',
         sidebarLayout(
             sidebarPanel(width = 4,
                 h4(tags$b('Model:')),
                 div(id="diagnostics_model"),
                 hr(),
                 
                 div(id="rhat_max_div",
                     inline(tags$label('$max(\\hat{R}, cycle=ALL)$')),
                     inline(
                         HTML(paste0("<div class='LOOIC' data-tooltip='", description['max_R'], "' data-container='body' style='display: inline; position: relative; bottom: 6px;'>  <i class='fas fa-info-circle'></i></div>"))
                     ),
                     inline(tags$label(':')),
                     uiOutput('rhat', class='number', inline = T),
                 ),
                 
                 inline(div(selectizeInput('param', 'Parameter:', NULL, options = katex.tt))),
                 
                 div(id="rhat_div",
                     inline(tags$label('$\\hat{R}$')),
                     inline(
                         HTML(paste0("<div class='LOOIC' data-tooltip='", description['param_R'], "' style='display: inline; position: relative; bottom: 6px;'>  <i class='fas fa-info-circle'></i></div>"))
                     ),
                     inline(tags$label(':')),
                     uiOutput('rhat_param', class='number', inline = T)
                 )
                 
             ),
             mainPanel(width = 8,
               tabsetPanel(
                   tabPanel('Density', plotOutput('dens')),
                   tabPanel('Trace', plotOutput('trace')),
                   tabPanel('Autocorrelation', plotOutput('ac'))
               ),
               br(),
               downloadButton("export_dens", "Export Density Plot"),
               downloadButton("export_trace", "Export Trace Plot"),
               downloadButton("export_ac", "Export Autocorrelation Plot")
             )
         )
    ),
    tabPanel('Results',
        sidebarLayout(
            sidebarPanel(width = 4,
                h4(tags$b('Model:')),
                div(id="result_model"),
                hr(),
                uiOutput('rhat_cycle', class='number', inline = T),
                br(),
                div(id="looic",
                    inline(tags$label('LOOIC')),
                    inline(
                        HTML(paste0("<div class='LOOIC' data-tooltip='", description['LOOIC'], "' style='display: inline; position: relative; bottom: 6px;'>  <i class='fas fa-info-circle'></i></div>"))
                    ),
                    inline(tags$label(':')),
                    uiOutput('ic', class='number', inline = T),
                    br(),
                )
            ),
            mainPanel(width = 8, id = "div_test",
                tabsetPanel(
                    tabPanel('Table', DTOutput('table'))
                ),
                downloadButton("download_csv", "Export CSV File"),
                downloadButton("download_xls", "Export XLS File"),
                downloadButton("download_tex", "Export TEX File"),
            )
        )
    ),
    tabPanel('Tutorial',
        #h4(tags$b('Help')),
        #HTML('')
        HTML('<h1><p style="text-align:center">Tutorial Video for Single-Level Analyses</p></h1>
        <p align="center"><iframe frameborder="0" scrolling="no" marginheight="0" marginwidth="0"width="788.54" height="443" type="text/html" src="https://www.youtube.com/embed/PbGkCS22HP8"></iframe>
        <h1><p style="text-align:center">Tutorial Video for Two Level Analyses</p></h1>
        <p align="center"><iframe frameborder="0" scrolling="no" marginheight="0" marginwidth="0"width="788.54" height="443" type="text/html" src=""></iframe>
        <h1><p style="text-align:center">Tutorial Video for Growth Curve Models</p></h1>
        <p align="center"><iframe frameborder="0" scrolling="no" marginheight="0" marginwidth="0"width="788.54" height="443" type="text/html" src=""></iframe>
        ')
    ),
    tabPanel('About',
        includeHTML("html/index.html")
    )
)
