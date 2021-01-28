# dist_mileage remains stable

           loc         sc 
    10.3113659  0.6445663 

---

     [1]         NA  33612.206  23688.500  31345.446         NA         NA
     [7]  39255.048         NA  23543.042  31150.143  20397.401         NA
    [13] 115630.298         NA         NA  30211.284         NA   8624.672
    [19]         NA  87572.910  14147.400         NA         NA

# mcs_mileage remains stable by defining the seed

    Reliability Data with characteristic 'mileage':
    # A tibble: 23 x 4
       mileage  time status id   
         <dbl> <dbl>  <dbl> <chr>
     1  11837.  1000      0 ID1  
     2  15655   1000      1 ID2  
     3  13629   1000      1 ID3  
     4  18292   1000      1 ID4  
     5  37803.  1000      0 ID5  
     6  39924.  1000      0 ID6  
     7  33555   1000      1 ID7  
     8  18915.  1000      0 ID8  
     9  21737   1000      1 ID9  
    10  29870   1000      1 ID10 
    # ... with 13 more rows

---

    # A tibble: 23 x 1
       sim_mileage
             <dbl>
     1       4321.
     2          0 
     3          0 
     4          0 
     5      13798.
     6      14572.
     7          0 
     8       6904.
     9          0 
    10          0 
    # ... with 13 more rows

