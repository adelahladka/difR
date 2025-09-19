# difMH - examples at help page

    Code
      (r1 <- difMH(verbal, group = 25, focal.name = 1))
    Output
      
      Detection of Differential Item Functioning using Mantel-Haenszel method 
      with continuity correction and without item purification
      
      Results based on asymptotic inference 
       
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Mantel-Haenszel Chi-square statistic: 
       
                  Stat.  P-value   
      S1wantCurse 1.7076 0.1913    
      S1WantScold 2.1486 0.1427    
      S1WantShout 0.9926 0.3191    
      S2WantCurse 1.9302 0.1647    
      S2WantScold 2.9540 0.0857  . 
      S2WantShout 9.6032 0.0019  **
      S3WantCurse 0.0013 0.9711    
      S3WantScold 0.6752 0.4112    
      S3WantShout 0.8185 0.3656    
      S4WantCurse 1.6292 0.2018    
      S4WantScold 0.0152 0.9020    
      S4WantShout 4.1188 0.0424  * 
      S1DoCurse   0.1324 0.7160    
      S1DoScold   2.7501 0.0972  . 
      S1DoShout   0.0683 0.7938    
      S2DoCurse   6.3029 0.0121  * 
      S2DoScold   6.8395 0.0089  **
      S2DoShout   0.2170 0.6414    
      S3DoCurse   5.7817 0.0162  * 
      S3DoScold   3.8880 0.0486  * 
      S3DoShout   0.2989 0.5846    
      S4DoCurse   1.1220 0.2895    
      S4DoScold   1.4491 0.2287    
      S4DoShout   0.8390 0.3597    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                  
       S2WantShout
       S4WantShout
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
      
       
      Effect size (ETS Delta scale): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  alphaMH deltaMH  
      S1wantCurse  1.7005 -1.2476 B
      S1WantScold  1.7702 -1.3420 B
      S1WantShout  1.4481 -0.8701 A
      S2WantCurse  1.9395 -1.5567 C
      S2WantScold  1.9799 -1.6052 C
      S2WantShout  2.8804 -2.4861 C
      S3WantCurse  0.9439  0.1358 A
      S3WantScold  0.7194  0.7741 A
      S3WantShout  1.5281 -0.9965 A
      S4WantCurse  1.6849 -1.2260 B
      S4WantScold  1.0901 -0.2028 A
      S4WantShout  2.3458 -2.0036 C
      S1DoCurse    0.7967  0.5340 A
      S1DoScold    0.4995  1.6313 C
      S1DoShout    1.1765 -0.3821 A
      S2DoCurse    0.3209  2.6709 C
      S2DoScold    0.3746  2.3072 C
      S2DoShout    0.7931  0.5447 A
      S3DoCurse    0.4616  1.8165 C
      S3DoScold    0.4727  1.7606 C
      S3DoShout    0.6373  1.0585 B
      S4DoCurse    0.6444  1.0327 B
      S4DoScold    0.6385  1.0541 B
      S4DoShout    1.6053 -1.1123 B
      
      Effect size codes: 0 'A' 1.0 'B' 1.5 'C' 
       (for absolute values of 'deltaMH') 
       
      Output was not captured! 

---

    Code
      (r2 <- difMH(verbal, group = 25, focal.name = 1, MHstat = "logOR"))
    Output
      
      Detection of Differential Item Functioning using Mantel-Haenszel method 
      with continuity correction and without item purification
      
      Results based on asymptotic inference 
       
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Log odds-ratio statistic: 
       
                  Stat.   P-value   
      S1wantCurse  1.4759  0.1400   
      S1WantScold  1.6751  0.0939 . 
      S1WantShout  1.1404  0.2541   
      S2WantCurse  1.6327  0.1025   
      S2WantScold  1.9103  0.0561 . 
      S2WantShout  3.1373  0.0017 **
      S3WantCurse -0.1889  0.8502   
      S3WantScold -0.9984  0.3181   
      S3WantShout  1.1136  0.2654   
      S4WantCurse  1.4869  0.1370   
      S4WantScold  0.2735  0.7844   
      S4WantShout  2.2368  0.0253 * 
      S1DoCurse   -0.5675  0.5704   
      S1DoScold   -1.8434  0.0653 . 
      S1DoShout    0.4495  0.6531   
      S2DoCurse   -2.6614  0.0078 **
      S2DoScold   -2.6876  0.0072 **
      S2DoShout   -0.6396  0.5224   
      S3DoCurse   -2.4687  0.0136 * 
      S3DoScold   -2.1368  0.0326 * 
      S3DoShout   -0.8366  0.4028   
      S4DoCurse   -1.2436  0.2137   
      S4DoScold   -1.3810  0.1673   
      S4DoShout    1.1203  0.2626   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 1.96 (significance level: 0.05)
      
      Items detected as DIF items: 
                  
       S2WantShout
       S4WantShout
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
      
       
      Effect size (ETS Delta scale): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  alphaMH deltaMH  
      S1wantCurse  1.7005 -1.2476 B
      S1WantScold  1.7702 -1.3420 B
      S1WantShout  1.4481 -0.8701 A
      S2WantCurse  1.9395 -1.5567 C
      S2WantScold  1.9799 -1.6052 C
      S2WantShout  2.8804 -2.4861 C
      S3WantCurse  0.9439  0.1358 A
      S3WantScold  0.7194  0.7741 A
      S3WantShout  1.5281 -0.9965 A
      S4WantCurse  1.6849 -1.2260 B
      S4WantScold  1.0901 -0.2028 A
      S4WantShout  2.3458 -2.0036 C
      S1DoCurse    0.7967  0.5340 A
      S1DoScold    0.4995  1.6313 C
      S1DoShout    1.1765 -0.3821 A
      S2DoCurse    0.3209  2.6709 C
      S2DoScold    0.3746  2.3072 C
      S2DoShout    0.7931  0.5447 A
      S3DoCurse    0.4616  1.8165 C
      S3DoScold    0.4727  1.7606 C
      S3DoShout    0.6373  1.0585 B
      S4DoCurse    0.6444  1.0327 B
      S4DoScold    0.6385  1.0541 B
      S4DoShout    1.6053 -1.1123 B
      
      Effect size codes: 0 'A' 1.0 'B' 1.5 'C' 
       (for absolute values of 'deltaMH') 
       
      Output was not captured! 

---

    Code
      (r3 <- difMH(verbal, group = 25, focal.name = 1, exact = TRUE))
    Output
      
      Detection of Differential Item Functioning using Mantel-Haenszel method 
      without continuity correction and without item purification
      
      Results based on exact inference 
       
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Exact statistic: 
       
                  Stat.    P-value    
      S1wantCurse 174.0000   0.1485   
      S1WantScold 147.0000   0.1373   
      S1WantShout 126.0000   0.2630   
      S2WantCurse 194.0000   0.1527   
      S2WantScold 154.0000   0.0835 . 
      S2WantShout 129.0000   0.0015 **
      S3WantCurse 143.0000   0.8798   
      S3WantScold  84.0000   0.3304   
      S3WantShout  61.0000   0.3021   
      S4WantCurse 169.0000   0.1673   
      S4WantScold 102.0000   0.8680   
      S4WantShout  80.0000   0.0334 * 
      S1DoCurse   168.0000   0.7016   
      S1DoScold   129.0000   0.0708 . 
      S1DoShout    83.0000   0.7313   
      S2DoCurse   148.0000   0.0082 **
      S2DoScold   106.0000   0.0059 **
      S2DoShout    57.0000   0.5756   
      S3DoCurse   100.0000   0.0131 * 
      S3DoScold    51.0000   0.0345 * 
      S3DoShout    20.0000   0.3998   
      S4DoCurse   145.0000   0.2462   
      S4DoScold    96.0000   0.2010   
      S4DoShout    45.0000   0.3341   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Significance level: 0.05
      
      Items detected as DIF items: 
                  
       S2WantShout
       S4WantShout
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
      
       
      Output was not captured! 

---

    Code
      difMH(verbal, group = 25, focal.name = 1, p.adjust.method = "BH")
    Output
      
      Detection of Differential Item Functioning using Mantel-Haenszel method 
      with continuity correction and without item purification
      
      Results based on asymptotic inference 
       
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      Multiple comparisons made with Benjamini-Hochberg adjustement of p-values
      
      Mantel-Haenszel Chi-square statistic: 
       
                  Stat.  P-value Adj. P  
      S1wantCurse 1.7076 0.1913  0.4036  
      S1WantScold 2.1486 0.1427  0.3805  
      S1WantShout 0.9926 0.3191  0.5106  
      S2WantCurse 1.9302 0.1647  0.3954  
      S2WantScold 2.9540 0.0857  0.2917  
      S2WantShout 9.6032 0.0019  0.0466 *
      S3WantCurse 0.0013 0.9711  0.9711  
      S3WantScold 0.6752 0.4112  0.5483  
      S3WantShout 0.8185 0.3656  0.5162  
      S4WantCurse 1.6292 0.2018  0.4036  
      S4WantScold 0.0152 0.9020  0.9412  
      S4WantShout 4.1188 0.0424  0.1945  
      S1DoCurse   0.1324 0.7160  0.8182  
      S1DoScold   2.7501 0.0972  0.2917  
      S1DoShout   0.0683 0.7938  0.8660  
      S2DoCurse   6.3029 0.0121  0.0964 .
      S2DoScold   6.8395 0.0089  0.0964 .
      S2DoShout   0.2170 0.6414  0.7696  
      S3DoCurse   5.7817 0.0162  0.0972 .
      S3DoScold   3.8880 0.0486  0.1945  
      S3DoShout   0.2989 0.5846  0.7384  
      S4DoCurse   1.1220 0.2895  0.4963  
      S4DoScold   1.4491 0.2287  0.4222  
      S4DoShout   0.8390 0.3597  0.5162  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                  
       S2WantShout
      
       
      Effect size (ETS Delta scale): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  alphaMH deltaMH  
      S1wantCurse  1.7005 -1.2476 B
      S1WantScold  1.7702 -1.3420 B
      S1WantShout  1.4481 -0.8701 A
      S2WantCurse  1.9395 -1.5567 C
      S2WantScold  1.9799 -1.6052 C
      S2WantShout  2.8804 -2.4861 C
      S3WantCurse  0.9439  0.1358 A
      S3WantScold  0.7194  0.7741 A
      S3WantShout  1.5281 -0.9965 A
      S4WantCurse  1.6849 -1.2260 B
      S4WantScold  1.0901 -0.2028 A
      S4WantShout  2.3458 -2.0036 C
      S1DoCurse    0.7967  0.5340 A
      S1DoScold    0.4995  1.6313 C
      S1DoShout    1.1765 -0.3821 A
      S2DoCurse    0.3209  2.6709 C
      S2DoScold    0.3746  2.3072 C
      S2DoShout    0.7931  0.5447 A
      S3DoCurse    0.4616  1.8165 C
      S3DoScold    0.4727  1.7606 C
      S3DoShout    0.6373  1.0585 B
      S4DoCurse    0.6444  1.0327 B
      S4DoScold    0.6385  1.0541 B
      S4DoShout    1.6053 -1.1123 B
      
      Effect size codes: 0 'A' 1.0 'B' 1.5 'C' 
       (for absolute values of 'deltaMH') 
       
      Output was not captured! 

---

    Code
      (r4 <- difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE))
    Output
      
      Detection of Differential Item Functioning using Mantel-Haenszel method 
      with continuity correction and with item purification
      
      Results based on asymptotic inference 
       
      Convergence reached after 6 iterations
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Mantel-Haenszel Chi-square statistic: 
       
                  Stat.   P-value    
      S1wantCurse  0.0069  0.9336    
      S1WantScold  0.0376  0.8462    
      S1WantShout  0.0087  0.9256    
      S2WantCurse  0.8881  0.3460    
      S2WantScold  0.1112  0.7388    
      S2WantShout  4.2680  0.0388 *  
      S3WantCurse  0.0987  0.7534    
      S3WantScold  4.3724  0.0365 *  
      S3WantShout  0.3454  0.5567    
      S4WantCurse  0.1406  0.7077    
      S4WantScold  1.6853  0.1942    
      S4WantShout  1.0766  0.2995    
      S1DoCurse    2.0959  0.1477    
      S1DoScold    6.2736  0.0123 *  
      S1DoShout    0.0172  0.8957    
      S2DoCurse    9.6672  0.0019 ** 
      S2DoScold   11.9436  0.0005 ***
      S2DoShout    0.6997  0.4029    
      S3DoCurse    9.4644  0.0021 ** 
      S3DoScold    6.4356  0.0112 *  
      S3DoShout    1.4190  0.2336    
      S4DoCurse    3.9323  0.0474 *  
      S4DoScold    5.7987  0.0160 *  
      S4DoShout    0.3223  0.5702    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                  
       S2WantShout
       S3WantScold
       S1DoScold  
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
       S4DoCurse  
       S4DoScold  
      
       
      Effect size (ETS Delta scale): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  alphaMH deltaMH  
      S1wantCurse  1.1054 -0.2355 A
      S1WantScold  1.1357 -0.2990 A
      S1WantShout  1.0919 -0.2066 A
      S2WantCurse  1.5998 -1.1042 B
      S2WantScold  1.1994 -0.4272 A
      S2WantShout  2.2088 -1.8623 C
      S3WantCurse  0.8584  0.3588 A
      S3WantScold  0.4593  1.8283 C
      S3WantShout  1.3348 -0.6786 A
      S4WantCurse  1.2183 -0.4639 A
      S4WantScold  0.6322  1.0776 B
      S4WantShout  1.5764 -1.0696 B
      S1DoCurse    0.5481  1.4130 B
      S1DoScold    0.3832  2.2540 C
      S1DoShout    0.9822  0.0421 A
      S2DoCurse    0.2658  3.1134 C
      S2DoScold    0.3014  2.8186 C
      S2DoShout    0.6832  0.8954 A
      S3DoCurse    0.3713  2.3284 C
      S3DoScold    0.4079  2.1075 C
      S3DoShout    0.4654  1.7974 C
      S4DoCurse    0.4744  1.7526 C
      S4DoScold    0.4148  2.0681 C
      S4DoShout    1.4040 -0.7974 A
      
      Effect size codes: 0 'A' 1.0 'B' 1.5 'C' 
       (for absolute values of 'deltaMH') 
       
      Output was not captured! 

---

    Code
      difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE, nrIter = 5)
    Output
      
      Detection of Differential Item Functioning using Mantel-Haenszel method 
      with continuity correction and with item purification
      
      Results based on asymptotic inference 
       
      WARNING: no item purification convergence after 5 iterations
      (Note: no loop detected in less than 5 iterations)
      WARNING: following results based on the last iteration of the purification 
       
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Mantel-Haenszel Chi-square statistic: 
       
                  Stat.   P-value    
      S1wantCurse  0.0974  0.7550    
      S1WantScold  0.0989  0.7531    
      S1WantShout  0.2182  0.6404    
      S2WantCurse  1.2035  0.2726    
      S2WantScold  0.2336  0.6289    
      S2WantShout  4.1092  0.0427 *  
      S3WantCurse  0.0344  0.8528    
      S3WantScold  4.3724  0.0365 *  
      S3WantShout  0.4716  0.4923    
      S4WantCurse  0.2721  0.6020    
      S4WantScold  1.2605  0.2616    
      S4WantShout  1.3271  0.2493    
      S1DoCurse    1.3384  0.2473    
      S1DoScold    6.5336  0.0106 *  
      S1DoShout    0.0075  0.9311    
      S2DoCurse    8.1200  0.0044 ** 
      S2DoScold   13.1912  0.0003 ***
      S2DoShout    0.4496  0.5025    
      S3DoCurse    8.4944  0.0036 ** 
      S3DoScold    5.2667  0.0217 *  
      S3DoShout    1.0026  0.3167    
      S4DoCurse    4.4670  0.0346 *  
      S4DoScold    5.9626  0.0146 *  
      S4DoShout    0.0844  0.7715    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                  
       S2WantShout
       S3WantScold
       S1DoScold  
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
       S4DoCurse  
       S4DoScold  
      
       
      Effect size (ETS Delta scale): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  alphaMH deltaMH  
      S1wantCurse  1.2101 -0.4482 A
      S1WantScold  1.1820 -0.3929 A
      S1WantShout  1.2272 -0.4812 A
      S2WantCurse  1.6872 -1.2293 B
      S2WantScold  1.2539 -0.5318 A
      S2WantShout  2.1516 -1.8006 C
      S3WantCurse  0.8955  0.2594 A
      S3WantScold  0.4593  1.8283 C
      S3WantShout  1.3783 -0.7540 A
      S4WantCurse  1.2728 -0.5668 A
      S4WantScold  0.6692  0.9440 A
      S4WantShout  1.6704 -1.2058 B
      S1DoCurse    0.6234  1.1104 B
      S1DoScold    0.3710  2.3300 C
      S1DoShout    1.0975 -0.2186 A
      S2DoCurse    0.3212  2.6688 C
      S2DoScold    0.2733  3.0487 C
      S2DoShout    0.7296  0.7409 A
      S3DoCurse    0.3855  2.2398 C
      S3DoScold    0.4463  1.8956 C
      S3DoShout    0.5168  1.5511 C
      S4DoCurse    0.4471  1.8918 C
      S4DoScold    0.4196  2.0407 C
      S4DoShout    1.2320 -0.4903 A
      
      Effect size codes: 0 'A' 1.0 'B' 1.5 'C' 
       (for absolute values of 'deltaMH') 
       
      Output was not captured! 

---

    Code
      difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE, p.adjust.method = "BH",
        puriadjType = "simple")
    Output
      
      Detection of Differential Item Functioning using Mantel-Haenszel method 
      with continuity correction and with item purification
      
      Results based on asymptotic inference 
       
      Convergence reached after 6 iterations
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      Multiple comparisons made with Benjamini-Hochberg adjustement of p-values
      Multiple comparison applied after item purification 
      
      Mantel-Haenszel Chi-square statistic: 
       
                  Stat.   P-value Adj. P   
      S1wantCurse  0.0069  0.9336  0.9336  
      S1WantScold  0.0376  0.8462  0.9336  
      S1WantShout  0.0087  0.9256  0.9336  
      S2WantCurse  0.8881  0.3460  0.5931  
      S2WantScold  0.1112  0.7388  0.9041  
      S2WantShout  4.2680  0.0388  0.1165  
      S3WantCurse  0.0987  0.7534  0.9041  
      S3WantScold  4.3724  0.0365  0.1165  
      S3WantShout  0.3454  0.5567  0.8050  
      S4WantCurse  0.1406  0.7077  0.9041  
      S4WantScold  1.6853  0.1942  0.4238  
      S4WantShout  1.0766  0.2995  0.5528  
      S1DoCurse    2.0959  0.1477  0.3545  
      S1DoScold    6.2736  0.0123  0.0588 .
      S1DoShout    0.0172  0.8957  0.9336  
      S2DoCurse    9.6672  0.0019  0.0168 *
      S2DoScold   11.9436  0.0005  0.0132 *
      S2DoShout    0.6997  0.4029  0.6446  
      S3DoCurse    9.4644  0.0021  0.0168 *
      S3DoScold    6.4356  0.0112  0.0588 .
      S3DoShout    1.4190  0.2336  0.4671  
      S4DoCurse    3.9323  0.0474  0.1263  
      S4DoScold    5.7987  0.0160  0.0642 .
      S4DoShout    0.3223  0.5702  0.8050  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                
       S2DoCurse
       S2DoScold
       S3DoCurse
      
       
      Effect size (ETS Delta scale): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  alphaMH deltaMH  
      S1wantCurse  1.1054 -0.2355 A
      S1WantScold  1.1357 -0.2990 A
      S1WantShout  1.0919 -0.2066 A
      S2WantCurse  1.5998 -1.1042 B
      S2WantScold  1.1994 -0.4272 A
      S2WantShout  2.2088 -1.8623 C
      S3WantCurse  0.8584  0.3588 A
      S3WantScold  0.4593  1.8283 C
      S3WantShout  1.3348 -0.6786 A
      S4WantCurse  1.2183 -0.4639 A
      S4WantScold  0.6322  1.0776 B
      S4WantShout  1.5764 -1.0696 B
      S1DoCurse    0.5481  1.4130 B
      S1DoScold    0.3832  2.2540 C
      S1DoShout    0.9822  0.0421 A
      S2DoCurse    0.2658  3.1134 C
      S2DoScold    0.3014  2.8186 C
      S2DoShout    0.6832  0.8954 A
      S3DoCurse    0.3713  2.3284 C
      S3DoScold    0.4079  2.1075 C
      S3DoShout    0.4654  1.7974 C
      S4DoCurse    0.4744  1.7526 C
      S4DoScold    0.4148  2.0681 C
      S4DoShout    1.4040 -0.7974 A
      
      Effect size codes: 0 'A' 1.0 'B' 1.5 'C' 
       (for absolute values of 'deltaMH') 
       
      Output was not captured! 

---

    Code
      (r5 <- difMH(verbal, group = "Gender", focal.name = 1, purify = TRUE,
        p.adjust.method = "BH", puriadjType = "combined"))
    Output
      
      Detection of Differential Item Functioning using Mantel-Haenszel method 
      with continuity correction and with item purification
      
      Results based on asymptotic inference 
       
      Convergence reached after 1 iteration
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      Multiple comparisons made with Benjamini-Hochberg adjustement of p-values
      Multiple comparison applied after each iteration of item purification 
      
      Mantel-Haenszel Chi-square statistic: 
       
                  Stat.  P-value Adj. P  
      S1wantCurse 1.7529 0.1855  0.3710  
      S1WantScold 2.0309 0.1541  0.3363  
      S1WantShout 1.2353 0.2664  0.4262  
      S2WantCurse 2.9846 0.0841  0.2882  
      S2WantScold 2.5506 0.1102  0.2975  
      S2WantShout 9.6032 0.0019  0.0466 *
      S3WantCurse 0.0172 0.8958  0.9165  
      S3WantScold 0.2278 0.6331  0.7997  
      S3WantShout 1.4923 0.2219  0.4096  
      S4WantCurse 2.5318 0.1116  0.2975  
      S4WantScold 0.0849 0.7707  0.8809  
      S4WantShout 3.5175 0.0607  0.2882  
      S1DoCurse   0.0110 0.9165  0.9165  
      S1DoScold   2.1092 0.1464  0.3363  
      S1DoShout   0.0554 0.8140  0.8880  
      S2DoCurse   5.0524 0.0246  0.1475  
      S2DoScold   6.5937 0.0102  0.0957 .
      S2DoShout   0.2440 0.6214  0.7997  
      S3DoCurse   6.3161 0.0120  0.0957 .
      S3DoScold   3.1692 0.0750  0.2882  
      S3DoShout   0.1195 0.7296  0.8755  
      S4DoCurse   0.4831 0.4870  0.6876  
      S4DoScold   0.5418 0.4617  0.6876  
      S4DoShout   1.2621 0.2613  0.4262  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                  
       S2WantShout
      
       
      Effect size (ETS Delta scale): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  alphaMH deltaMH  
      S1wantCurse  1.6794 -1.2183 B
      S1WantScold  1.7658 -1.3362 B
      S1WantShout  1.4698 -0.9050 A
      S2WantCurse  2.0937 -1.7365 C
      S2WantScold  1.8301 -1.4203 B
      S2WantShout  2.8804 -2.4861 C
      S3WantCurse  1.0071 -0.0167 A
      S3WantScold  0.8080  0.5011 A
      S3WantShout  1.7136 -1.2656 B
      S4WantCurse  1.8911 -1.4973 B
      S4WantScold  1.1568 -0.3423 A
      S4WantShout  2.1597 -1.8094 C
      S1DoCurse    0.8846  0.2880 A
      S1DoScold    0.5491  1.4089 B
      S1DoShout    1.1583 -0.3454 A
      S2DoCurse    0.3720  2.3240 C
      S2DoScold    0.3824  2.2588 C
      S2DoShout    0.7844  0.5707 A
      S3DoCurse    0.4256  2.0075 C
      S3DoScold    0.5298  1.4929 B
      S3DoShout    0.6958  0.8525 A
      S4DoCurse    0.7330  0.7300 A
      S4DoScold    0.7488  0.6797 A
      S4DoShout    1.7955 -1.3754 B
      
      Effect size codes: 0 'A' 1.0 'B' 1.5 'C' 
       (for absolute values of 'deltaMH') 
       
      Output was not captured! 

---

    Code
      difMH(verbal, group = "Gender", focal.name = 1, alpha = 0.01, correct = FALSE)
    Output
      
      Detection of Differential Item Functioning using Mantel-Haenszel method 
      without continuity correction and without item purification
      
      Results based on asymptotic inference 
       
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Mantel-Haenszel Chi-square statistic: 
       
                  Stat.   P-value   
      S1wantCurse  2.2151  0.1367   
      S1WantScold  2.6579  0.1030   
      S1WantShout  1.3392  0.2472   
      S2WantCurse  2.5359  0.1113   
      S2WantScold  3.5755  0.0586 . 
      S2WantShout 10.6034  0.0011 **
      S3WantCurse  0.0356  0.8504   
      S3WantScold  0.9709  0.3245   
      S3WantShout  1.1630  0.2808   
      S4WantCurse  2.1003  0.1473   
      S4WantScold  0.0838  0.7722   
      S4WantShout  4.8072  0.0283 * 
      S1DoCurse    0.3083  0.5787   
      S1DoScold    3.3869  0.0657 . 
      S1DoShout    0.1880  0.6646   
      S2DoCurse    7.2126  0.0072 **
      S2DoScold    7.7670  0.0053 **
      S2DoShout    0.4256  0.5141   
      S3DoCurse    6.5479  0.0105 * 
      S3DoScold    4.6194  0.0316 * 
      S3DoShout    0.6927  0.4052   
      S4DoCurse    1.5020  0.2204   
      S4DoScold    1.8592  0.1727   
      S4DoShout    1.2302  0.2674   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 6.6349 (significance level: 0.01)
      
      Items detected as DIF items: 
                  
       S2WantShout
       S2DoCurse  
       S2DoScold  
      
       
      Effect size (ETS Delta scale): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  alphaMH deltaMH  
      S1wantCurse  1.7005 -1.2476 B
      S1WantScold  1.7702 -1.3420 B
      S1WantShout  1.4481 -0.8701 A
      S2WantCurse  1.9395 -1.5567 C
      S2WantScold  1.9799 -1.6052 C
      S2WantShout  2.8804 -2.4861 C
      S3WantCurse  0.9439  0.1358 A
      S3WantScold  0.7194  0.7741 A
      S3WantShout  1.5281 -0.9965 A
      S4WantCurse  1.6849 -1.2260 B
      S4WantScold  1.0901 -0.2028 A
      S4WantShout  2.3458 -2.0036 C
      S1DoCurse    0.7967  0.5340 A
      S1DoScold    0.4995  1.6313 C
      S1DoShout    1.1765 -0.3821 A
      S2DoCurse    0.3209  2.6709 C
      S2DoScold    0.3746  2.3072 C
      S2DoShout    0.7931  0.5447 A
      S3DoCurse    0.4616  1.8165 C
      S3DoScold    0.4727  1.7606 C
      S3DoShout    0.6373  1.0585 B
      S4DoCurse    0.6444  1.0327 B
      S4DoScold    0.6385  1.0541 B
      S4DoShout    1.6053 -1.1123 B
      
      Effect size codes: 0 'A' 1.0 'B' 1.5 'C' 
       (for absolute values of 'deltaMH') 
       
      Output was not captured! 

---

    Code
      difMH(verbal, group = "Gender", focal.name = 1, anchor = 1:5)
    Output
      
      Detection of Differential Item Functioning using Mantel-Haenszel method 
      with continuity correction and without item purification
      
      Results based on asymptotic inference 
       
      Matching variable: test score 
       
      Anchor items (provided by the user): 
                  
       S1wantCurse
       S1WantScold
       S1WantShout
       S2WantCurse
       S2WantScold
      
       
      No p-value adjustment for multiple comparisons 
       
      Mantel-Haenszel Chi-square statistic: 
       
                  Stat.   P-value    
      S2WantShout  2.2586  0.1329    
      S3WantCurse  0.4689  0.4935    
      S3WantScold  5.0960  0.0240 *  
      S3WantShout  0.0012  0.9724    
      S4WantCurse  0.0034  0.9532    
      S4WantScold  1.2471  0.2641    
      S4WantShout  0.7411  0.3893    
      S1DoCurse    2.2833  0.1308    
      S1DoScold    8.6892  0.0032 ** 
      S1DoShout    0.0004  0.9850    
      S2DoCurse    9.4633  0.0021 ** 
      S2DoScold   14.1791  0.0002 ***
      S2DoShout    0.6991  0.4031    
      S3DoCurse    9.8728  0.0017 ** 
      S3DoScold    6.7612  0.0093 ** 
      S3DoShout    0.1518  0.6968    
      S4DoCurse    3.5378  0.0600 .  
      S4DoScold    2.9196  0.0875 .  
      S4DoShout    0.0013  0.9717    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                  
       S3WantScold
       S1DoScold  
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
      
       
      Effect size (ETS Delta scale): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  alphaMH deltaMH  
      S2WantShout  1.8374 -1.4296 B
      S3WantCurse  0.7500  0.6760 A
      S3WantScold  0.4018  2.1428 C
      S3WantShout  0.9294  0.1719 A
      S4WantCurse  0.9580  0.1009 A
      S4WantScold  0.6290  1.0896 B
      S4WantShout  1.5377 -1.0112 B
      S1DoCurse    0.5221  1.5272 C
      S1DoScold    0.3038  2.7995 C
      S1DoShout    1.0834 -0.1881 A
      S2DoCurse    0.2945  2.8724 C
      S2DoScold    0.2412  3.3422 C
      S2DoShout    0.6054  1.1792 B
      S3DoCurse    0.3332  2.5824 C
      S3DoScold    0.3476  2.4836 C
      S3DoShout    0.6324  1.0770 B
      S4DoCurse    0.4985  1.6361 C
      S4DoScold    0.5303  1.4906 B
      S4DoShout    1.1428 -0.3137 A
      
      Effect size codes: 0 'A' 1.0 'B' 1.5 'C' 
       (for absolute values of 'deltaMH') 
       
      Output was not captured! 

---

    Code
      difMH(verbal, group = "Gender", focal.name = 1, anchor = 1:5, purify = TRUE)
    Output
      
      Detection of Differential Item Functioning using Mantel-Haenszel method 
      with continuity correction and without item purification
      
      Results based on asymptotic inference 
       
      Matching variable: test score 
       
      Anchor items (provided by the user): 
                  
       S1wantCurse
       S1WantScold
       S1WantShout
       S2WantCurse
       S2WantScold
      
       
      No p-value adjustment for multiple comparisons 
       
      Mantel-Haenszel Chi-square statistic: 
       
                  Stat.   P-value    
      S2WantShout  2.2586  0.1329    
      S3WantCurse  0.4689  0.4935    
      S3WantScold  5.0960  0.0240 *  
      S3WantShout  0.0012  0.9724    
      S4WantCurse  0.0034  0.9532    
      S4WantScold  1.2471  0.2641    
      S4WantShout  0.7411  0.3893    
      S1DoCurse    2.2833  0.1308    
      S1DoScold    8.6892  0.0032 ** 
      S1DoShout    0.0004  0.9850    
      S2DoCurse    9.4633  0.0021 ** 
      S2DoScold   14.1791  0.0002 ***
      S2DoShout    0.6991  0.4031    
      S3DoCurse    9.8728  0.0017 ** 
      S3DoScold    6.7612  0.0093 ** 
      S3DoShout    0.1518  0.6968    
      S4DoCurse    3.5378  0.0600 .  
      S4DoScold    2.9196  0.0875 .  
      S4DoShout    0.0013  0.9717    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                  
       S3WantScold
       S1DoScold  
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
      
       
      Effect size (ETS Delta scale): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  alphaMH deltaMH  
      S2WantShout  1.8374 -1.4296 B
      S3WantCurse  0.7500  0.6760 A
      S3WantScold  0.4018  2.1428 C
      S3WantShout  0.9294  0.1719 A
      S4WantCurse  0.9580  0.1009 A
      S4WantScold  0.6290  1.0896 B
      S4WantShout  1.5377 -1.0112 B
      S1DoCurse    0.5221  1.5272 C
      S1DoScold    0.3038  2.7995 C
      S1DoShout    1.0834 -0.1881 A
      S2DoCurse    0.2945  2.8724 C
      S2DoScold    0.2412  3.3422 C
      S2DoShout    0.6054  1.1792 B
      S3DoCurse    0.3332  2.5824 C
      S3DoScold    0.3476  2.4836 C
      S3DoShout    0.6324  1.0770 B
      S4DoCurse    0.4985  1.6361 C
      S4DoScold    0.5303  1.4906 B
      S4DoShout    1.1428 -0.3137 A
      
      Effect size codes: 0 'A' 1.0 'B' 1.5 'C' 
       (for absolute values of 'deltaMH') 
       
      Output was not captured! 

