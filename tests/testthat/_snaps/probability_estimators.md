# snapshots: examples

    # A tibble: 10 x 6
       id    characteristic status  rank   prob method
       <chr>          <dbl>  <dbl> <int>  <dbl> <chr> 
     1 3435           10000      1     1 0.0673 mr    
     2 1203           20000      1     2 0.163  mr    
     3 958X           30000      1     3 0.260  mr    
     4 XX71           40000      1     4 0.356  mr    
     5 abcd           50000      1     5 0.452  mr    
     6 tz46           60000      1     6 0.548  mr    
     7 fl29           70000      1     7 0.644  mr    
     8 AX23           80000      1     8 0.740  mr    
     9 Uy12           90000      1     9 0.837  mr    
    10 kl1a          100000      1    10 0.933  mr    

---

    # A tibble: 10 x 6
       id    characteristic status  rank   prob method
       <chr>          <dbl>  <dbl> <int>  <dbl> <chr> 
     1 3435           10000      1     1 0.0670 mr    
     2 1203           20000      1     2 0.162  mr    
     3 958X           30000      1     3 0.259  mr    
     4 XX71           40000      1     4 0.355  mr    
     5 abcd           50000      1     5 0.452  mr    
     6 tz46           60000      1     6 0.548  mr    
     7 fl29           70000      1     7 0.645  mr    
     8 AX23           80000      1     8 0.741  mr    
     9 Uy12           90000      1     9 0.838  mr    
    10 kl1a          100000      1    10 0.933  mr    

---

    # A tibble: 10 x 6
       id    characteristic status  rank    prob method 
       <chr>          <dbl>  <dbl> <dbl>   <dbl> <chr>  
     1 3435           10000      0 NA    NA      johnson
     2 1203           20000      1  1.1   0.0769 johnson
     3 958X           30000      1  2.2   0.183  johnson
     4 XX71           40000      0 NA    NA      johnson
     5 abcd           50000      0 NA    NA      johnson
     6 tz46           60000      0 NA    NA      johnson
     7 fl29           70000      1  3.96  0.352  johnson
     8 AX23           80000      0 NA    NA      johnson
     9 Uy12           90000      1  6.31  0.578  johnson
    10 kl1a          100000      0 NA    NA      johnson

---

    # A tibble: 10 x 6
       id    characteristic status  rank   prob method
       <chr>          <dbl>  <dbl> <dbl>  <dbl> <chr> 
     1 3435           10000      0    NA NA     nelson
     2 1203           20000      1    NA  0.105 nelson
     3 958X           30000      1    NA  0.210 nelson
     4 XX71           40000      0    NA NA     nelson
     5 abcd           50000      0    NA NA     nelson
     6 tz46           60000      0    NA NA     nelson
     7 fl29           70000      1    NA  0.385 nelson
     8 AX23           80000      0    NA NA     nelson
     9 Uy12           90000      1    NA  0.627 nelson
    10 kl1a          100000      0    NA NA     nelson

---

    # A tibble: 10 x 6
       id    characteristic status  rank   prob method
       <chr>          <dbl>  <dbl> <dbl>  <dbl> <chr> 
     1 3435           10000      0    NA NA     kaplan
     2 1203           20000      1    NA  0.111 kaplan
     3 958X           30000      1    NA  0.222 kaplan
     4 XX71           40000      0    NA NA     kaplan
     5 abcd           50000      0    NA NA     kaplan
     6 tz46           60000      0    NA NA     kaplan
     7 fl29           70000      1    NA  0.417 kaplan
     8 AX23           80000      0    NA NA     kaplan
     9 Uy12           90000      1    NA  0.708 kaplan
    10 kl1a          100000      0    NA NA     kaplan

---

    # A tibble: 23 x 6
       id    characteristic status  rank  prob method
       <chr>          <dbl>  <dbl> <dbl> <dbl> <chr> 
     1 X              10000      1    NA 0.110 kaplan
     2 X              10000      1    NA 0.110 kaplan
     3 X              20000      1    NA 0.227 kaplan
     4 X              20000      1    NA 0.227 kaplan
     5 X              30000      1    NA 0.413 kaplan
     6 X              30000      1    NA 0.413 kaplan
     7 X              30000      1    NA 0.413 kaplan
     8 X              30000      1    NA 0.413 kaplan
     9 X              40000      1    NA 0.481 kaplan
    10 X              50000      1    NA 0.580 kaplan
    # ... with 13 more rows

# snapshots: input with repeating characteristics

    # A tibble: 2 x 6
      id    characteristic status  rank  prob method
      <chr>          <dbl>  <dbl> <int> <dbl> <chr> 
    1 X                  1      1     2 0.708 mr    
    2 X                  1      1     2 0.708 mr    

---

    # A tibble: 4 x 6
      id    characteristic status  rank   prob method 
      <chr>          <dbl>  <dbl> <dbl>  <dbl> <chr>  
    1 X                  1      0    NA NA     johnson
    2 X                  1      1     2  0.386 johnson
    3 X                  1      0    NA NA     johnson
    4 X                  1      1     2  0.386 johnson

---

    # A tibble: 4 x 6
      id    characteristic status  rank  prob method
      <chr>          <dbl>  <dbl> <dbl> <dbl> <chr> 
    1 X                  1      0    NA  NA   kaplan
    2 X                  1      1    NA   0.5 kaplan
    3 X                  1      0    NA  NA   kaplan
    4 X                  1      1    NA   0.5 kaplan

---

    # A tibble: 4 x 6
      id    characteristic status  rank  prob method
      <chr>          <dbl>  <dbl> <dbl> <dbl> <chr> 
    1 X                  1      0    NA    NA nelson
    2 X                  1      1    NA     0 nelson
    3 X                  1      0    NA    NA nelson
    4 X                  1      1    NA     0 nelson

