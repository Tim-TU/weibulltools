# ml_estimation remains stable

            mu      sigma 
    11.5061567  0.5226762 

---

                  mu      sigma
    mu    0.08298129 0.02651965
    sigma 0.02651965 0.04789639

---

    $coefficients
             eta         beta 
    99325.410455     1.913231 
    
    $confint
                  5 %         95 %
    eta  6.184165e+04 1.595290e+05
    beta 9.608548e-01 3.809578e+00
    
    $loc_sc_params
            mu      sigma 
    11.5061567  0.5226762 
    
    $loc_sc_confint
                 5 %     95 %
    mu    11.0323324 11.97998
    sigma  0.2624963  1.04074
    
    $loc_sc_varcov
                  mu      sigma
    mu    0.08298129 0.02651965
    sigma 0.02651965 0.04789639
    
    $logL
    [1] -50.39596
    
    $aic
    [1] 104.7919
    
    $bic
    [1] 105.3971
    
    $data
    # A tibble: 10 x 2
       characteristic status
                <dbl>  <dbl>
     1          10000      0
     2          20000      1
     3          30000      1
     4          40000      0
     5          50000      0
     6          60000      0
     7          70000      1
     8          80000      0
     9          90000      1
    10         100000      0
    
    $distribution
    [1] "weibull"
    
    attr(,"class")
    [1] "model_estimation" "list"            

---

              mu        sigma        gamma 
    1.150616e+01 5.226873e-01 1.205127e-05 

---

                     mu         sigma         gamma
    mu     1.484711e-01 -7.709891e-02 -8.477477e+03
    sigma -7.709891e-02  2.118501e-01  1.341356e+04
    gamma -8.477477e+03  1.341356e+04  1.097417e+09

---

    $coefficients
             eta         beta        gamma 
    9.932536e+04 1.913190e+00 1.205127e-05 
    
    $confint
                  2.5 %      97.5 %
    eta    4.667406e+04 211370.6524
    beta   3.405656e-01     10.7477
    gamma -6.492829e+04  64928.2928
    
    $loc_sc_params
              mu        sigma        gamma 
    1.150616e+01 5.226873e-01 1.205127e-05 
    
    $loc_sc_confint
                  2.5 %       97.5 %
    mu     1.075094e+01    12.261369
    sigma  9.304320e-02     2.936292
    gamma -6.492829e+04 64928.292766
    
    $loc_sc_varcov
                     mu         sigma         gamma
    mu     1.484711e-01 -7.709891e-02 -8.477477e+03
    sigma -7.709891e-02  2.118501e-01  1.341356e+04
    gamma -8.477477e+03  1.341356e+04  1.097417e+09
    
    $logL
    [1] -50.39596
    
    $aic
    [1] 106.7919
    
    $bic
    [1] 107.6997
    
    $data
    # A tibble: 10 x 2
       characteristic status
                <dbl>  <dbl>
     1          10000      0
     2          20000      1
     3          30000      1
     4          40000      0
     5          50000      0
     6          60000      0
     7          70000      1
     8          80000      0
     9          90000      1
    10         100000      0
    
    $distribution
    [1] "weibull3"
    
    attr(,"class")
    [1] "model_estimation" "list"            

