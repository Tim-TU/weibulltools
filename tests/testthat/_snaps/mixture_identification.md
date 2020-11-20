# mixmod_regression remains stable

    $mod_1
    $coefficients
            eta        beta 
    639.4723620   0.7199644 
    
    $confint
               2.5 %       97.5 %
    eta  379.8689128 1072.5998774
    beta   0.5659361    0.9159139
    
    $loc_sc_params
          mu    sigma 
    6.460643 1.388958 
    
    $loc_sc_confint
             2.5 %   97.5 %
    mu    5.939826 6.977841
    sigma 1.091806 1.766984
    
    $r_squared
    [1] 0.9566145
    
    $data
    # A tibble: 27 x 2
       characteristic status
                <dbl>  <dbl>
     1              2      1
     2              3      1
     3              5      1
     4              8      1
     5             21      1
     6             28      1
     7             31      1
     8             64      1
     9             69      1
    10             76      1
    # ... with 17 more rows
    
    $distribution
    [1] "weibull"
    
    $x_range
    [1]   2 286
    
    attr(,"class")
    [1] "rank_regression"  "model_estimation" "list"            
    
    $mod_2
    $coefficients
           eta       beta 
    311.703804   3.976386 
    
    $confint
              2.5 %     97.5 %
    eta  277.813339 349.343943
    beta   2.982289   5.301848
    
    $loc_sc_params
           mu     sigma 
    5.7420534 0.2514847 
    
    $loc_sc_confint
              2.5 %    97.5 %
    mu    5.6269494 5.8560569
    sigma 0.1886135 0.3353129
    
    $r_squared
    [1] 0.9838667
    
    $data
    # A tibble: 18 x 2
       characteristic status
                <dbl>  <dbl>
     1            298      1
     2            303      1
     3            314      1
     4            317      1
     5            318      1
     6            320      1
     7            327      1
     8            328      1
     9            328      1
    10            348      1
    11            350      1
    12            360      1
    13            369      1
    14            377      1
    15            387      1
    16            392      1
    17            412      1
    18            446      1
    
    $distribution
    [1] "weibull"
    
    $x_range
    [1] 298 446
    
    attr(,"class")
    [1] "rank_regression"  "model_estimation" "list"            
    

# mixmod_em remains stable

    $coefficients
           eta       beta 
    330.341612   4.805297 
    
    $confint
              2.5 %     97.5 %
    eta  306.424099 356.125973
    beta   3.662714   6.304309
    
    $loc_sc_params
           mu     sigma 
    5.8001273 0.2081037 
    
    $loc_sc_confint
              2.5 %    97.5 %
    mu    5.7249701 5.8752845
    sigma 0.1586217 0.2730216
    
    $loc_sc_varcov
                     mu         sigma
    mu     0.0014704329 -0.0002648277
    sigma -0.0002648277  0.0008310922
    
    $logL
    [1] -180.2821
    
    $aic
    [1] 364.5642
    
    $bic
    [1] 368.6851
    
    $data
    # A tibble: 58 x 2
       characteristic status
                <dbl>  <dbl>
     1              2      1
     2             28      1
     3             67      0
     4            119      1
     5            179      0
     6            236      1
     7            282      1
     8            317      1
     9            348      1
    10            387      1
    # ... with 48 more rows
    
    $distribution
    [1] "weibull"
    
    attr(,"class")
    [1] "ml_estimation"    "model_estimation" "list"            

---

    $coefficients
           eta       beta 
    69.6664711  0.8234136 
    
    $confint
              2.5 %     97.5 %
    eta  35.9075306 135.164327
    beta  0.5392857   1.257237
    
    $loc_sc_params
          mu    sigma 
    4.243719 1.214457 
    
    $loc_sc_confint
             2.5 %   97.5 %
    mu    3.580947 4.906491
    sigma 0.795395 1.854305
    
    $loc_sc_varcov
                   mu       sigma
    mu     0.11434898 -0.02213261
    sigma -0.02213261  0.06876796
    
    $logL
    [1] -72.80019
    
    $aic
    [1] 149.6004
    
    $bic
    [1] 153.7213
    
    $data
    # A tibble: 58 x 2
       characteristic status
                <dbl>  <dbl>
     1              2      1
     2             28      1
     3             67      0
     4            119      1
     5            179      0
     6            236      1
     7            282      1
     8            317      1
     9            348      1
    10            387      1
    # ... with 48 more rows
    
    $distribution
    [1] "weibull"
    
    attr(,"class")
    [1] "ml_estimation"    "model_estimation" "list"            

