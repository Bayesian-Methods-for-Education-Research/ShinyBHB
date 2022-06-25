lib <- c('shinyjs', 'shinythemes', 'DT', 'magrittr', 'tools', 'haven', 'htmltools',
         'openxlsx', 'readr', 'shinyBS', 'rstan', 'loo', 'dplyr', 'stringr', 'latex2exp')
for (pkg in lib)
    if (!require(pkg, character.only = T)) { # if the pkg is not found
        install.packages(pkg)
        library(pkg, character.only = T)
    }

# use LaTeX in selectizeInput
katex <- list(render = I('{
    item: function(item, escape) {
        return "<div>" + katex.renderToString(item.label) + "</div>";
    },
    option: function(item, escape) {
        return "<div>" + katex.renderToString(item.label) + "</div>";
    }
}'))

# use text LaTeX in selectizeInput
katex.tt <- list(render = I('{
    item: function(item, escape) {
        return "<div>" + katex.renderToString("\\\\texttt{" + item.label + "}") + "</div>";
    },
    option: function(item, escape) {
        return "<div>" + katex.renderToString("\\\\texttt{" + item.label + "}") + "</div>";
    }
}'))

var.z <- paste0('(', paste0(c('time', 'date', 'year', 'month', 'day', 'cycle'), collapse = ')|('), ')')

formula <- list(
  prior_beta=list(cross=list(one=list(others="\\beta\\sim\\textup{Normal}(0,\\sigma_\\beta)",
                                      BDB="\\beta^1,\\beta^2,\\dots{},\\beta^H,\\beta^C\\sim\\textup{Normal}(\\mu_\\beta,\\sigma_\\beta)"),
                             two=list(others="\\gamma\\sim\\textup{Normal}(0,\\sigma_\\gamma)",
                                      BDB="\\gamma^1,\\gamma^2,\\dots{},\\gamma^H,\\gamma^C\\sim\\textup{Normal}(\\mu_\\gamma,\\sigma_\\gamma)")
                             ),
                  long=list(one=list(others="\\gamma\\sim\\textup{Normal}(0,\\sigma_\\gamma)",
                                     BDB="\\gamma^1,\\gamma^2,\\dots{},\\gamma^H,\\gamma^C\\sim\\textup{Normal}(\\mu_\\gamma,\\sigma_\\gamma)"),
                            two=list(others="\\delta\\sim\\textup{Normal}(0,\\sigma_\\delta)",
                                     BDB="\\delta^1,\\delta^2,\\dots{},\\delta^H,\\delta^C\\sim\\textup{Normal}(\\mu_\\delta,\\sigma_\\delta)")
                            )
                  ),
  prior_mu_beta=list(cross=list(one=list(others="",
                                         BDB="\\mu_\\beta\\sim\\textup{Normal}(0,\\sigma_{0_\\beta})"),
                                two=list(others="",
                                         BDB="\\mu_\\gamma\\sim\\textup{Normal}(0,\\sigma_{0_\\gamma})")
                                ),
                     long=list(one=list(others="",
                                        BDB="\\mu_\\gamma\\sim\\textup{Normal}(0,\\sigma_{0_\\gamma})"),
                               two=list(others="",
                                        BDB="\\mu_\\delta\\sim\\textup{Normal}(0,\\sigma_{0_\\delta})")
                               )
                     ),
  prior_U=list(cross=list(one=list(others="",
                                   BDB=""),
                          two=list(others="U\\sim\\textup{Normal}(0, \\Sigma_U)",
                                  BDB="U\\sim\\textup{Normal}(0, \\Sigma_U)")
                         ),
               long=list(one=list(others="U\\sim\\textup{Normal}(0, \\Sigma_U)",
                                  BDB="U\\sim\\textup{Normal}(0, \\Sigma_U)"),
                         two=list(others="U\\sim\\textup{Normal}(0, \\Sigma_U)",
                                  BDB="U\\sim\\textup{Normal}(0, \\Sigma_U)")
                        )
              ),
  prior_sigma_U=list(cross=list(one=list(others="",
                                         BDB=""),
                                two=list(others="\\Sigma_U = \\tau_U\\Omega_U\\tau_U",
                                         BDB="\\Sigma_U^{-1}\\sim\\textup{Wishart}(\\nu,\\nu\\Sigma_S^{-1})")
                                ),
                     long=list(one=list(others="\\Sigma_U = \\tau_U\\Omega_U\\tau_U",
                                        BDB="\\Sigma_U^{-1}\\sim\\textup{Wishart}(\\nu,\\nu\\Sigma_S^{-1})"),
                               two=list(others="\\Sigma_U = \\tau_U\\Omega_U\\tau_U",
                                        BDB="\\Sigma_U^{-1}\\sim\\textup{Wishart}(\\nu_U,\\nu_U\\Sigma_{U_0}^{-1})")
                                )
                     ),
  prior_S=list(cross=list(one=list(others="",
                                   BDB=""),
                          two=list(others="",
                                   BDB="\\Sigma_S = \\tau_S\\Omega_S\\tau_S")
                          ),
               long=list(one=list(others="",
                                  BDB="\\Sigma_S = \\tau_S\\Omega_S\\tau_S"),
                         two=list(others="",
                                  BDB="\\Sigma_{U_0} = \\tau_{U_0}\\Omega_{U_0}\\tau_{U_0}")
                         )
               ),
  prior_U_2=list(cross=list(one=list(others="",
                                     BDB=""),
                            two=list(others="",
                                     BDB="")
                            ),
                 long=list(one=list(others="",
                                    BDB=""),
                           two=list(others="V\\sim\\textup{Normal}(0, \\Sigma_V)",
                                    BDB="V\\sim\\textup{Normal}(0, \\Sigma_V)")
                          )
                ),
  prior_sigma_U_2=list(cross=list(one=list(others="",
                                           BDB=""),
                                  two=list(others="",
                                           BDB="")
                                  ),
                       long=list(one=list(others="",
                                          BDB=""),
                                 two=list(others="\\Sigma_V = \\tau_V\\Omega_V\\tau_V",
                                          BDB="\\Sigma_V^{-1}\\sim\\textup{Wishart}(\\nu_V,\\nu_V\\Sigma_{V_0}^{-1})")
                                 )
                       ),
  prior_S_2=list(cross=list(one=list(others="",
                                     BDB=""),
                            two=list(others="",
                                     BDB="")
                            ),
                 long=list(one=list(others="",
                                    BDB=""),
                           two=list(others="",
                                    BDB="\\Sigma_{V_0} = \\tau_{V_0}\\Omega_{V_0}\\tau_{V_0}")
                          )
                 ),
  prior_R=list(cross=list(one=list(others="R_i\\sim\\textup{Normal}(0,\\sigma_R)",
                                   BDB="R_i\\sim\\textup{Normal}(0,\\sigma_R)"),
                          two=list(others="R_{ij}\\sim\\textup{Normal}(0,\\sigma_R)",
                                   BDB="R_{ij}\\sim\\textup{Normal}(0,\\sigma_R)")
                          ),
               long=list(one=list(others="R_{ti}\\sim\\textup{Normal}(0,\\sigma_R)",
                                  BDB="R_{ti}\\sim\\textup{Normal}(0,\\sigma_R)"),
                         two=list(others="R_{tij}\\sim\\textup{Normal}(0,\\sigma_R)",
                                  BDB="R_{tij}\\sim\\textup{Normal}(0,\\sigma_R)")
                          )
              )
)

hyperprior_formula <- list(
  sigma_beta=list(cross=list(one=list(others="\\sigma_\\beta",
                                      BDB="\\sigma_\\beta"),
                             two=list(others="\\sigma_\\gamma",
                                      BDB="\\sigma_\\gamma")
                            ),
                  long=list(one=list(others="\\sigma_\\gamma",
                                     BDB="\\sigma_\\gamma"),
                            two=list(others="\\sigma_\\delta",
                                     BDB="\\sigma_\\delta")
                           )
                  ),
  sigma_0_beta=list(cross=list(one=list(others="",
                                        BDB="\\sigma_{0_\\beta}"),
                               two=list(others="",
                                        BDB="\\sigma_{0_\\gamma}")
                               ),
                    long=list(one=list(others="",
                                       BDB="\\sigma_{0_\\gamma}"),
                              two=list(others="",
                                       BDB="\\sigma_{0_\\delta}")
                              )
                    ),
  tau=list(cross=list(one=list(others="",
                               BDB=""),
                      two=list(others="\\tau_U",
                               BDB="\\tau_S")
                      ),
           long=list(one=list(others="\\tau_U",
                              BDB="\\tau_S"),
                     two=list(others="\\tau_U",
                              BDB="\\tau_{U_0}")
                    )
           ),
  Omega=list(cross=list(one=list(others="",
                                 BDB=""),
                        two=list(others="\\Omega_U",
                                 BDB="\\Omega_S")
                        ),
             long=list(one=list(others="\\Omega_U",
                                BDB="\\Omega_S"),
                       two=list(others="\\Omega_U",
                                BDB="\\Omega_{U_0}")
                      )
            ),
  tau_2=list(cross=list(one=list(others="",
                                 BDB=""),
                        two=list(others="",
                                 BDB="")
                       ),
             long=list(one=list(others="",
                                BDB=""),
                       two=list(others="\\tau_V",
                                BDB="\\tau_{V_0}")
                       )
             ),
  Omega_2=list(cross=list(one=list(others="",
                                 BDB=""),
                          two=list(others="",
                                   BDB="")
                         ),
              long=list(one=list(others="",
                                 BDB=""),
                        two=list(others="\\Omega_V",
                                 BDB="\\Omega_{V_0}")
                        )
               )
)

hyperprior <- list(
  sigma_beta = list('\\textup{Half-Cauchy}(0,b)' = 'cauchy', '\\textup{Uniform}(a,b)' = 'uniform', '\\textup{Half-Normal}(0, \\sigma_{\\beta_0})' = 'half_normal', '\\textup{Half-t}(\\nu,0,\\sigma_{\\beta_0})' = 'half_t', '\\textup{Inv-Gamma}(a,b)' = 'inv_gamma'),
  sigma_beta_a = list("uniform"="a", "half_normal"="0", "cauchy"="0", "half_t"="\\nu", "inv_gamma"="a", "inv_wishart"="0", "fix_value"="0"),
  sigma_beta_b = list("uniform"="b", "half_normal"="\\sigma_{\\beta_0}", "cauchy"="b", "half_t"="\\sigma_{\\beta_0}", "inv_gamma"="b", "inv_wishart"="v", "fix_value"="\\textup{Fixed Value}"),
  sigma_0_beta = list('\\textup{Fixed Value}' = 'fix_value'),
  sigma_0_beta_a = list("uniform"="a", "half_normal"="0", "cauchy"="0", "half_t"="\\nu", "inv_gamma"="a", "inv_wishart"="0", "fix_value"="0"),
  sigma_0_beta_b = list("uniform"="b", "half_normal"="\\sigma_{0\\beta_0}", "cauchy"="b", "half_t"="\\sigma_{0\\beta_0}", "inv_gamma"="b", "inv_wishart"="v", "fix_value"="\\textup{Fixed Value}"),
  sigma_R = list('\\textup{Half-Cauchy}(0,b)' = 'cauchy', '\\textup{Uniform}(a,b)' = 'uniform', '\\textup{Half-Normal}(0, \\sigma_{R_0})' = 'half_normal', '\\textup{Half-t}(\\nu,0,\\sigma_{R_0})' = 'half_t', '\\textup{Inv-Gamma}(a,b)' = 'inv_gamma'),
  sigma_R_a = list("uniform"="a", "half_normal"="0", "cauchy"="0", "half_t"="\\nu", "inv_gamma"="a", "inv_wishart"="0"),
  sigma_R_b = list("uniform"="b", "half_normal"="\\sigma_{R_0}", "cauchy"="b", "half_t"="\\sigma_{R_0}", "inv_gamma"="b", "inv_wishart"="v"),
  tau = list('\\textup{Half-Cauchy}(0,b)' = 'cauchy', '\\textup{Uniform}(a,b)' = 'uniform', '\\textup{Half-Normal}(0, \\sigma_{\\tau_0})' = 'half_normal', '\\textup{Half-t}(\\nu,0,\\sigma_{\\tau_0})' = 'half_t', '\\textup{Inv-Gamma}(a,b)' = 'inv_gamma'),
  tau_a = list("uniform"="a", "half_normal"="0", "cauchy"="0", "half_t"="\\nu", "inv_gamma"="a", "inv_wishart"="0"),
  tau_b = list("uniform"="b", "half_normal"="\\sigma_{\\tau_0}", "cauchy"="b", "half_t"="\\sigma_{\\tau_0}", "inv_gamma"="b", "inv_wishart"="v"),
  Omega = list("\\textup{LKJcorr}(\\eta)"='lkj_corr_cholesky'),
  Omega_a = list("uniform"="a", "half_normal"="0", "cauchy"="0", "half_t"="\\nu", "inv_gamma"="a", "inv_wishart"="0", "lkj_corr_cholesky"='0'),
  Omega_b = list("uniform"="b", "half_normal"="\\sigma_{R_0}", "cauchy"="b", "half_t"="\\sigma_{R_0}", "inv_gamma"="b", "inv_wishart"="v", "lkj_corr_cholesky"='\\eta'),
  tau_2 = list('\\textup{Half-Cauchy}(0,b)' = 'cauchy', '\\textup{Uniform}(a,b)' = 'uniform', '\\textup{Half-Normal}(0, \\sigma_{\\tau_0})' = 'half_normal', '\\textup{Half-t}(\\nu,0,\\sigma_{\\tau_0})' = 'half_t', '\\textup{Inv-Gamma}(a,b)' = 'inv_gamma'),
  tau_2_a = list("uniform"="a", "half_normal"="0", "cauchy"="0", "half_t"="\\nu", "inv_gamma"="a", "inv_wishart"="0"),
  tau_2_b = list("uniform"="b", "half_normal"="\\sigma_{\\tau_0}", "cauchy"="b", "half_t"="\\sigma_{\\tau_0}", "inv_gamma"="b", "inv_wishart"="v"),
  Omega_2 = list("\\textup{LKJcorr}(\\eta)"='lkj_corr_cholesky'),
  Omega_2_a = list("uniform"="a", "half_normal"="0", "cauchy"="0", "half_t"="\\nu", "inv_gamma"="a", "inv_wishart"="0", "lkj_corr_cholesky"='0'),
  Omega_2_b = list("uniform"="b", "half_normal"="\\sigma_{R_0}", "cauchy"="b", "half_t"="\\sigma_{R_0}", "inv_gamma"="b", "inv_wishart"="v", "lkj_corr_cholesky"='\\eta'),
  help_text_a = list("uniform"="lower bound", "half_normal"="", "cauchy"="", "half_t"="degree of freedom", "inv_gamma"="shape", "inv_wishart"="?", "lkjcorr"='?'),
  help_text_b = list("uniform"="upper bound", "half_normal"="standard deviation", "cauchy"="scale", "half_t"="standard deviation", "inv_gamma"="scale", "inv_wishart"="?", "lkjcorr"='?')
)

description <- list(
  n_eff = "The amount by which autocorrelation within the chains increases uncertainty in estimates can be measured by effective sample size (ESS). Given independent samples, the central limit theorem bounds uncertainty in estimates based on the number of samples N. Given dependent samples, the number of independent samples is replaced with the effective sample size N_eff, which is the number of independent samples with the same estimation power as the N autocorrelated samples.",
  Rhat = "R-hat is a convergence diagnostic, which compares the between- and within-chain estimates for model parameters and other univariate quantities of interest. If chains have not mixed well (ie, the between- and within-chain estimates don't agree), R-hat is larger than 1. It is recommended to run at least four chains and only using the sample if R-hat is less than 1.05.",
  LOOIC = "https://avehtari.github.io/modelselection/CV-FAQ.html",
  max_R = "This is the maximum Rhat across all the parameters in all the cycles for BDB and in current cycle for other borrowing methods.",
  param_R = "This is Rhat for the selected parameter in current cycle."
)

inline <- function(ui) {
  span(class = 'inline', ui)
}

inline.param <- function(ui) {
  span(class = 'inline param', ui)
}


