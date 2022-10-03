# cor_mutations() gives consistent result

    Code
      cor_mutations(data, response = "AUC")
    Output
      # A tibble: 19,537 x 5
         gene    effect  p.value    adj.p significant
         <chr>    <dbl>    <dbl>    <dbl> <lgl>      
       1 TP53    0.162  1.23e-65 2.39e-61 TRUE       
       2 RB1     0.0552 1.29e- 8 1.26e- 4 TRUE       
       3 EHD1   -0.0871 6.43e- 6 4.17e- 2 TRUE       
       4 GIGYF1 -0.0530 1.27e- 5 6.17e- 2 FALSE      
       5 PLXNB1 -0.0321 3.02e- 4 6.17e- 1 FALSE      
       6 RPL22  -0.0456 3.10e- 4 6.17e- 1 FALSE      
       7 MT-CYB  0.0345 3.67e- 4 6.17e- 1 FALSE      
       8 ASB11   0.0511 4.09e- 4 6.17e- 1 FALSE      
       9 FAM83F -0.0527 4.30e- 4 6.17e- 1 FALSE      
      10 PYCRL  -0.0496 4.72e- 4 6.17e- 1 FALSE      
      # ... with 19,527 more rows

