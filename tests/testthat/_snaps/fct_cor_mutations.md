# cor_mutations() gives consistent result

    Code
      cor_mutations(data, response = "auc")
    Output
      # A tibble: 19,537 x 4
         gene    effect  p.value significant
         <chr>    <dbl>    <dbl> <lgl>      
       1 RIMS1    0.234 0.000357 FALSE      
       2 TTC29    0.287 0.000474 FALSE      
       3 ZNF764  -0.552 0.000514 FALSE      
       4 NUFIP1   0.297 0.000975 FALSE      
       5 NF2     -0.322 0.00102  FALSE      
       6 NLRP12   0.178 0.00108  FALSE      
       7 HLTF     0.294 0.00110  FALSE      
       8 FRG2B    0.299 0.00169  FALSE      
       9 PLEKHA1 -0.797 0.00221  FALSE      
      10 C8orf37  0.298 0.00223  FALSE      
      # ... with 19,527 more rows

