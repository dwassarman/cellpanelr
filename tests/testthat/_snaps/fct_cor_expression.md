# cor_expression() gives consistent result

    Code
      cor_expression(data, response = "auc")
    Output
      # A tibble: 19,177 x 4
         gene      rho       p.value significant
         <chr>   <dbl>         <dbl> <lgl>      
       1 CLCF1  -0.423 0.00000000167 TRUE       
       2 PLAU   -0.413 0.00000000413 TRUE       
       3 IL6    -0.402 0.0000000119  TRUE       
       4 RRAS2  -0.392 0.0000000298  TRUE       
       5 GLIPR1 -0.381 0.0000000769  TRUE       
       6 BMP1   -0.379 0.0000000909  TRUE       
       7 A4GALT -0.376 0.000000111   TRUE       
       8 NNMT   -0.375 0.000000123   TRUE       
       9 HDDC3   0.369 0.000000197   TRUE       
      10 TKFC    0.369 0.000000200   TRUE       
      # ... with 19,167 more rows

