linreg_class <- setRefClass(Class = "linreg",
                      fields = list(beta = "matrix",
                                    predictions = "matrix",
                                    residuals = "matrix",
                                    df = "numeric",
                                    res_var = "matrix",
                                    beta_var = "matrix",
                                    t_value = "matrix",
                                    p_value = "matrix"))


