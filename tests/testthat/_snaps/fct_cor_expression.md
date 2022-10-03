# cor_expression() gives consistent result

    Code
      cor_expression(data, response = "AUC")
    Output
      # A tibble: 19,177 x 4
         gene         rho  p.value significant
         <chr>      <dbl>    <dbl> <lgl>      
       1 EDA2R     -0.421 1.32e-30 TRUE       
       2 RPS27L    -0.393 1.60e-26 TRUE       
       3 MDM2      -0.367 3.66e-23 TRUE       
       4 AEN       -0.321 8.74e-18 TRUE       
       5 ZMAT3     -0.321 1.02e-17 TRUE       
       6 RPL22L1   -0.317 2.59e-17 TRUE       
       7 DDB2      -0.311 9.18e-17 TRUE       
       8 TNFRSF10B -0.310 1.34e-16 TRUE       
       9 BAX       -0.309 1.65e-16 TRUE       
      10 CCNG1     -0.300 1.17e-15 TRUE       
      # ... with 19,167 more rows

