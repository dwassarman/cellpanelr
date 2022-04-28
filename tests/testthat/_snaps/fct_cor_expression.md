# cor_expression() gives consistent result

    Code
      cor_expression(data, response = "auc")
    Output
      # A tibble: 19,177 x 4
         gene              rho p.value significant
         <chr>           <dbl>   <dbl> <lgl>      
       1 SFTPA1    -0.00000826   1.00  FALSE      
       2 ACAD11    -0.0000128    1.00  FALSE      
       3 MINPP1    -0.0000229    1.00  FALSE      
       4 FGG       -0.0000240    1.00  FALSE      
       5 SLC22A25   0.0000402    1.00  FALSE      
       6 AQP3       0.0000495    0.999 FALSE      
       7 PROC      -0.0000633    0.999 FALSE      
       8 KRTAP11-1  0.0000675    0.999 FALSE      
       9 ABRAXAS2   0.0000835    0.999 FALSE      
      10 KRTAP6-1  -0.0000879    0.999 FALSE      
      # ... with 19,167 more rows

