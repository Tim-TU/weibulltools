# plot_layout_helper remains stable

    $x_ticks
      [1] 1e-06 2e-06 3e-06 4e-06 5e-06 6e-06 7e-06 8e-06 9e-06 1e-05 2e-05 3e-05
     [13] 4e-05 5e-05 6e-05 7e-05 8e-05 9e-05 1e-04 2e-04 3e-04 4e-04 5e-04 6e-04
     [25] 7e-04 8e-04 9e-04 1e-03 2e-03 3e-03 4e-03 5e-03 6e-03 7e-03 8e-03 9e-03
     [37] 1e-02 2e-02 3e-02 4e-02 5e-02 6e-02 7e-02 8e-02 9e-02 1e-01 2e-01 3e-01
     [49] 4e-01 5e-01 6e-01 7e-01 8e-01 9e-01 1e+00 2e+00 3e+00 4e+00 5e+00 6e+00
     [61] 7e+00 8e+00 9e+00 1e+01 2e+01 3e+01 4e+01 5e+01 6e+01 7e+01 8e+01 9e+01
     [73] 1e+02 2e+02 3e+02 4e+02 5e+02 6e+02 7e+02 8e+02 9e+02 1e+03 2e+03 3e+03
     [85] 4e+03 5e+03 6e+03 7e+03 8e+03 9e+03 1e+04 2e+04 3e+04 4e+04 5e+04 6e+04
     [97] 7e+04 8e+04 9e+04 1e+05 2e+05 3e+05 4e+05 5e+05 6e+05 7e+05 8e+05 9e+05
    [109] 1e+06 2e+06 3e+06 4e+06 5e+06 6e+06 7e+06 8e+06 9e+06 1e+07 2e+07 3e+07
    [121] 4e+07 5e+07 6e+07 7e+07 8e+07 9e+07 1e+08
    
    $x_labels
      [1] "1e-06" "2e-06" "3e-06" " "     " "     " "     " "     " "     " "    
     [10] "1e-05" "2e-05" "3e-05" " "     " "     " "     " "     " "     " "    
     [19] "1e-04" "2e-04" "3e-04" " "     " "     " "     " "     " "     " "    
     [28] "0.001" "0.002" "0.003" " "     " "     " "     " "     " "     " "    
     [37] "0.01"  "0.02"  "0.03"  " "     " "     " "     " "     " "     " "    
     [46] "0.1"   "0.2"   "0.3"   " "     " "     " "     " "     " "     " "    
     [55] "1"     "2"     "3"     " "     " "     " "     " "     " "     " "    
     [64] "10"    "20"    "30"    " "     " "     " "     " "     " "     " "    
     [73] "100"   "200"   "300"   " "     " "     " "     " "     " "     " "    
     [82] "1000"  "2000"  "3000"  " "     " "     " "     " "     " "     " "    
     [91] "10000" "20000" "30000" " "     " "     " "     " "     " "     " "    
    [100] "1e+05" "2e+05" "3e+05" " "     " "     " "     " "     " "     " "    
    [109] "1e+06" "2e+06" "3e+06" " "     " "     " "     " "     " "     " "    
    [118] "1e+07" "2e+07" "3e+07" " "     " "     " "     " "     " "     " "    
    [127] "1e+08"
    
    $y_ticks
     [1] -16.11809560 -13.81551006 -11.51292046  -9.21029037  -6.90725507
     [6]  -4.60014923  -2.97019525  -2.25036733  -1.49993999  -1.03093043
    [11]  -0.36651292  -0.08742157   0.18562676   0.47588500   0.83403245
    [16]   1.09718870   1.52717963   1.93264473   2.22032681   2.44347036
    
    $y_labels
     [1]  0.00001  0.00010  0.00100  0.01000  0.10000  1.00000  5.00000 10.00000
     [9] 20.00000 30.00000 50.00000 60.00000 70.00000 80.00000 90.00000 95.00000
    [17] 99.00000 99.90000 99.99000 99.99900
    

---

    $x_ticks
    NULL
    
    $x_labels
    NULL
    
    $y_ticks
     [1] -5.1993376 -4.7534243 -4.2648908 -3.7190165 -3.0902323 -2.3263479
     [7] -1.6448536 -1.2815516 -0.8416212 -0.5244005  0.0000000  0.2533471
    [13]  0.5244005  0.8416212  1.2815516  1.6448536  2.3263479  3.0902323
    [19]  3.7190165  4.2648908
    
    $y_labels
     [1]  0.00001  0.00010  0.00100  0.01000  0.10000  1.00000  5.00000 10.00000
     [9] 20.00000 30.00000 50.00000 60.00000 70.00000 80.00000 90.00000 95.00000
    [17] 99.00000 99.90000 99.99000 99.99900
    

# plot_prob_helper remains stable

    CDF estimation for methods 'johnson', 'kaplan':
    # A tibble: 134 x 7
       id        x status  rank    prob cdf_estimation_method     q
       <chr> <dbl>  <dbl> <dbl>   <dbl> <chr>                 <dbl>
     1 ID72     94      1     1 0.00967 johnson               -4.63
     2 ID72     94      1    NA 0.0139  kaplan                -4.27
     3 ID71     96      1     2 0.0235  johnson               -3.74
     4 ID71     96      1    NA 0.0278  kaplan                -3.57
     5 ID69     99      1     4 0.0511  johnson               -2.95
     6 ID70     99      1     4 0.0511  johnson               -2.95
     7 ID69     99      1    NA 0.0556  kaplan                -2.86
     8 ID70     99      1    NA 0.0556  kaplan                -2.86
     9 ID68    104      1     5 0.0649  johnson               -2.70
    10 ID68    104      1    NA 0.0694  kaplan                -2.63
    # ... with 124 more rows

---

    CDF estimation for methods 'johnson', 'kaplan':
    # A tibble: 134 x 7
       id        x status  rank    prob cdf_estimation_method     q
       <chr> <dbl>  <dbl> <dbl>   <dbl> <chr>                 <dbl>
     1 ID72     94      1     1 0.00967 johnson               -2.34
     2 ID72     94      1    NA 0.0139  kaplan                -2.20
     3 ID71     96      1     2 0.0235  johnson               -1.99
     4 ID71     96      1    NA 0.0278  kaplan                -1.91
     5 ID69     99      1     4 0.0511  johnson               -1.63
     6 ID70     99      1     4 0.0511  johnson               -1.63
     7 ID69     99      1    NA 0.0556  kaplan                -1.59
     8 ID70     99      1    NA 0.0556  kaplan                -1.59
     9 ID68    104      1     5 0.0649  johnson               -1.51
    10 ID68    104      1    NA 0.0694  kaplan                -1.48
    # ... with 124 more rows

# plot_mod_helper remains stable

    # A tibble: 100 x 7
         x_p    y_p param_val param_label cdf_estimation_method group     q
       <dbl>  <dbl> <list>    <list>      <chr>                 <chr> <dbl>
     1 6700  0.0183 <chr [3]> <chr [3]>   <NA>                  <NA>  -3.99
     2 6798. 0.0190 <chr [3]> <chr [3]>   <NA>                  <NA>  -3.95
     3 6897. 0.0198 <chr [3]> <chr [3]>   <NA>                  <NA>  -3.91
     4 6997. 0.0206 <chr [3]> <chr [3]>   <NA>                  <NA>  -3.87
     5 7100. 0.0214 <chr [3]> <chr [3]>   <NA>                  <NA>  -3.83
     6 7203. 0.0223 <chr [3]> <chr [3]>   <NA>                  <NA>  -3.79
     7 7308. 0.0232 <chr [3]> <chr [3]>   <NA>                  <NA>  -3.75
     8 7415. 0.0241 <chr [3]> <chr [3]>   <NA>                  <NA>  -3.71
     9 7523. 0.0251 <chr [3]> <chr [3]>   <NA>                  <NA>  -3.67
    10 7633. 0.0261 <chr [3]> <chr [3]>   <NA>                  <NA>  -3.63
    # ... with 90 more rows

# plot_mod_mix_helper remains stable

    # A tibble: 200 x 7
         x_p    y_p param_val param_label cdf_estimation_method group     q
       <dbl>  <dbl> <list>    <list>      <chr>                 <chr> <dbl>
     1 6700  0.0183 <chr [2]> <chr [2]>   johnson               group -3.99
     2 6804. 0.0191 <chr [2]> <chr [2]>   johnson               group -3.95
     3 6909. 0.0199 <chr [2]> <chr [2]>   johnson               group -3.91
     4 7013. 0.0207 <chr [2]> <chr [2]>   johnson               group -3.87
     5 7118. 0.0216 <chr [2]> <chr [2]>   johnson               group -3.82
     6 7222. 0.0225 <chr [2]> <chr [2]>   johnson               group -3.78
     7 7327. 0.0234 <chr [2]> <chr [2]>   johnson               group -3.75
     8 7431. 0.0243 <chr [2]> <chr [2]>   johnson               group -3.71
     9 7536. 0.0252 <chr [2]> <chr [2]>   johnson               group -3.67
    10 7640. 0.0262 <chr [2]> <chr [2]>   johnson               group -3.63
    # ... with 190 more rows

# plot_conf_helper_2 remains stable

    # A tibble: 204 x 5
    # Groups:   bound [2]
           x      y bound cdf_estimation_method     q
       <dbl>  <dbl> <chr> <chr>                 <dbl>
     1 6700. 0.0927 Upper johnson               -2.33
     2 6910. 0.0958 Upper johnson               -2.30
     3 7120. 0.0990 Upper johnson               -2.26
     4 7330. 0.102  Upper johnson               -2.23
     5 7540. 0.106  Upper johnson               -2.19
     6 7750. 0.109  Upper johnson               -2.16
     7 7960. 0.113  Upper johnson               -2.12
     8 8170. 0.117  Upper johnson               -2.09
     9 8380  0.121  Upper johnson               -2.05
    10 8590. 0.124  Upper johnson               -2.02
    # ... with 194 more rows

# plot_pop_helper remains stable

    # A tibble: 100 x 6
          x_s    y_s       q param_val param_label group
        <dbl>  <dbl>   <dbl> <list>    <list>      <chr>
     1 26522. 0.734   0.282  <chr [3]> <chr [3]>   1    
     2 19771. 0.628  -0.0115 <chr [3]> <chr [3]>   1    
     3 11143. 0.427  -0.585  <chr [3]> <chr [3]>   1    
     4  1926. 0.0918 -2.34   <chr [3]> <chr [3]>   1    
     5 32021. 0.798   0.471  <chr [3]> <chr [3]>   1    
     6  2143. 0.102  -2.23   <chr [3]> <chr [3]>   1    
     7  1138. 0.0553 -2.87   <chr [3]> <chr [3]>   1    
     8  8286. 0.339  -0.881  <chr [3]> <chr [3]>   1    
     9  9269. 0.371  -0.769  <chr [3]> <chr [3]>   1    
    10 55681. 0.938   1.02   <chr [3]> <chr [3]>   1    
    # ... with 90 more rows

