# difLogistic - examples at help page

    Code
      (r1 <- difSIBTEST(verbal, group = 25, focal.name = 1))
    Output
      
      Detection of Differential Item Functioning using SIBTEST method 
      without item purification
      
      Investigation of uniform DIF using SIBTEST (Shealy and Stout, 1993) 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
                  Beta    SE      X2 Stat. P-value  
      S1wantCurse  0.1078  0.1154  0.8736   0.3500  
      S1WantScold  0.0017  0.1229  0.0002   0.9892  
      S1WantShout  0.1426  0.1308  1.1891   0.2755  
      S2WantCurse  0.0871  0.1251  0.4842   0.4865  
      S2WantScold  0.0891  0.1408  0.4008   0.5267  
      S2WantShout  0.1118  0.1147  0.9501   0.3297  
      S3WantCurse -0.0249  0.1126  0.0488   0.8251  
      S3WantScold -0.1085  0.1098  0.9755   0.3233  
      S3WantShout -0.0813  0.1194  0.4633   0.4961  
      S4WantCurse  0.0899  0.1213  0.5494   0.4586  
      S4WantScold -0.0855  0.1154  0.5490   0.4587  
      S4WantShout  0.0167  0.1249  0.0179   0.8936  
      S1DoCurse    0.0841  0.1323  0.4041   0.5250  
      S1DoScold   -0.1562  0.0968  2.6043   0.1066  
      S1DoShout    0.1436  0.1061  1.8333   0.1757  
      S2DoCurse    0.0743  0.1865  0.1588   0.6903  
      S2DoScold   -0.0289  0.1222  0.0558   0.8132  
      S2DoShout    0.1975  0.1313  2.2626   0.1325  
      S3DoCurse   -0.2674  0.1137  5.5273   0.0187 *
      S3DoScold   -0.2081  0.1051  3.9240   0.0476 *
      S3DoShout   -0.1704  0.1322  1.6623   0.1973  
      S4DoCurse   -0.0737  0.1568  0.2209   0.6384  
      S4DoScold   -0.2290  0.1321  3.0068   0.0829 .
      S4DoShout    0.0396  0.1088  0.1325   0.7159  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                
       S3DoCurse
       S3DoScold
      
       
      Output was not captured! 

---

    Code
      (r2 <- difSIBTEST(verbal, group = 25, focal.name = 1, type = "nudif"))
    Output
      
      Detection of Differential Item Functioning using SIBTEST method 
      without item purification
      
      Investigation of nonuniform DIF using Crossing-SIBTEST (Chalmers, 2018) 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
                  Beta    SE      X2 Stat. P-value  
      S1wantCurse -0.1078      NA  0.8736   0.3500  
      S1WantScold  0.2506      NA  4.8925   0.0866 .
      S1WantShout  0.1426      NA  1.1891   0.2755  
      S2WantCurse  0.0871      NA  0.4842   0.4865  
      S2WantScold -0.0891      NA  0.4008   0.5267  
      S2WantShout -0.1118      NA  0.9501   0.3297  
      S3WantCurse -0.0249      NA  0.0488   0.8251  
      S3WantScold -0.1085      NA  0.9755   0.3233  
      S3WantShout  0.0813      NA  0.4633   0.4961  
      S4WantCurse  0.0899      NA  0.5494   0.4586  
      S4WantScold -0.0855      NA  0.5490   0.4587  
      S4WantShout  0.0767      NA  0.6962   0.7060  
      S1DoCurse    0.0841      NA  0.4041   0.5250  
      S1DoScold   -0.1562      NA  2.6043   0.1066  
      S1DoShout   -0.1436      NA  1.8333   0.1757  
      S2DoCurse    0.0743      NA  0.1588   0.6903  
      S2DoScold    0.0755      NA  0.4123   0.8137  
      S2DoShout   -0.1975      NA  2.2626   0.1325  
      S3DoCurse    0.2674      NA  5.5273   0.0187 *
      S3DoScold    0.2081      NA  3.9240   0.0476 *
      S3DoShout    0.1704      NA  1.6623   0.1973  
      S4DoCurse    0.1774      NA  2.3644   0.3066  
      S4DoScold    0.2290      NA  3.0068   0.0829 .
      S4DoShout   -0.0396      NA  0.1325   0.7159  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                
       S3DoCurse
       S3DoScold
      
       
      Output was not captured! 

---

    Code
      difSIBTEST(verbal, group = 25, focal.name = 1, p.adjust.method = "BH")
    Output
      
      Detection of Differential Item Functioning using SIBTEST method 
      without item purification
      
      Investigation of uniform DIF using SIBTEST (Shealy and Stout, 1993) 
       
      No set of anchor items was provided 
       
      Multiple comparisons made with Benjamini-Hochberg adjustement of p-values 
       
                  Beta    SE      X2 Stat. P-value Adj. P  
      S1wantCurse  0.1078  0.1154  0.8736   0.3500  0.7436 
      S1WantScold  0.0017  0.1229  0.0002   0.9892  0.9892 
      S1WantShout  0.1426  0.1308  1.1891   0.2755  0.7436 
      S2WantCurse  0.0871  0.1251  0.4842   0.4865  0.7436 
      S2WantScold  0.0891  0.1408  0.4008   0.5267  0.7436 
      S2WantShout  0.1118  0.1147  0.9501   0.3297  0.7436 
      S3WantCurse -0.0249  0.1126  0.0488   0.8251  0.9001 
      S3WantScold -0.1085  0.1098  0.9755   0.3233  0.7436 
      S3WantShout -0.0813  0.1194  0.4633   0.4961  0.7436 
      S4WantCurse  0.0899  0.1213  0.5494   0.4586  0.7436 
      S4WantScold -0.0855  0.1154  0.5490   0.4587  0.7436 
      S4WantShout  0.0167  0.1249  0.0179   0.8936  0.9324 
      S1DoCurse    0.0841  0.1323  0.4041   0.5250  0.7436 
      S1DoScold   -0.1562  0.0968  2.6043   0.1066  0.6361 
      S1DoShout    0.1436  0.1061  1.8333   0.1757  0.6764 
      S2DoCurse    0.0743  0.1865  0.1588   0.6903  0.8591 
      S2DoScold   -0.0289  0.1222  0.0558   0.8132  0.9001 
      S2DoShout    0.1975  0.1313  2.2626   0.1325  0.6361 
      S3DoCurse   -0.2674  0.1137  5.5273   0.0187  0.4493 
      S3DoScold   -0.2081  0.1051  3.9240   0.0476  0.5712 
      S3DoShout   -0.1704  0.1322  1.6623   0.1973  0.6764 
      S4DoCurse   -0.0737  0.1568  0.2209   0.6384  0.8511 
      S4DoScold   -0.2290  0.1321  3.0068   0.0829  0.6361 
      S4DoShout    0.0396  0.1088  0.1325   0.7159  0.8591 
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: No DIF item detected 
       
      Output was not captured! 

---

    Code
      (r3 <- difSIBTEST(verbal, group = 25, focal.name = 1, purify = TRUE))
    Output
      
      Detection of Differential Item Functioning using SIBTEST method 
      with item purification
      
      Convergence reached after 6 iterations
      
      Investigation of uniform DIF using SIBTEST (Shealy and Stout, 1993) 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
                  Beta    SE      X2 Stat. P-value  
      S1wantCurse  0.0333  0.0935  0.1268   0.7218  
      S1WantScold -0.0705  0.1195  0.3480   0.5553  
      S1WantShout  0.1666  0.1180  1.9928   0.1581  
      S2WantCurse  0.0366  0.1492  0.0601   0.8063  
      S2WantScold -0.0069  0.1404  0.0024   0.9609  
      S2WantShout  0.1073  0.1311  0.6697   0.4131  
      S3WantCurse  0.0832  0.1328  0.3926   0.5309  
      S3WantScold -0.0658  0.1351  0.2371   0.6263  
      S3WantShout  0.1184  0.1271  0.8682   0.3515  
      S4WantCurse  0.0671  0.0999  0.4513   0.5017  
      S4WantScold -0.1998  0.1503  1.7668   0.1838  
      S4WantShout  0.0970  0.1038  0.8723   0.3503  
      S1DoCurse    0.1609  0.1577  1.0414   0.3075  
      S1DoScold   -0.0996  0.0899  1.2256   0.2683  
      S1DoShout    0.1004  0.1094  0.8430   0.3585  
      S2DoCurse   -0.0857  0.1670  0.2636   0.6077  
      S2DoScold    0.0729  0.1370  0.2829   0.5948  
      S2DoShout    0.2203  0.1437  2.3525   0.1251  
      S3DoCurse   -0.1938  0.1115  3.0220   0.0821 .
      S3DoScold   -0.2081  0.1051  3.9240   0.0476 *
      S3DoShout   -0.0416  0.1207  0.1186   0.7305  
      S4DoCurse   -0.0646  0.1440  0.2010   0.6539  
      S4DoScold   -0.1665  0.1233  1.8235   0.1769  
      S4DoShout   -0.0008  0.1351  0.0000   0.9953  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                
       S3DoScold
      
       
      Output was not captured! 

---

    Code
      difSIBTEST(verbal, group = 25, focal.name = 1, purify = TRUE, nrIter = 5)
    Output
      
      Detection of Differential Item Functioning using SIBTEST method 
      with item purification
      
      WARNING: no item purification convergence after 5 iterations
      (Note: no loop detected in less than 5 iterations)
      WARNING: following results based on the last iteration of the purification 
       
      Investigation of uniform DIF using SIBTEST (Shealy and Stout, 1993) 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
                  Beta    SE      X2 Stat. P-value  
      S1wantCurse  0.1302  0.1040  1.5669   0.2107  
      S1WantScold -0.0009  0.1153  0.0001   0.9936  
      S1WantShout  0.0336  0.1310  0.0660   0.7973  
      S2WantCurse  0.1951  0.1496  1.7020   0.1920  
      S2WantScold  0.0891  0.1408  0.4008   0.5267  
      S2WantShout  0.1122  0.1129  0.9891   0.3200  
      S3WantCurse  0.0388  0.1379  0.0791   0.7785  
      S3WantScold -0.0345  0.1307  0.0698   0.7916  
      S3WantShout  0.0065  0.1154  0.0032   0.9552  
      S4WantCurse  0.0964  0.1097  0.7731   0.3793  
      S4WantScold -0.0355  0.1027  0.1196   0.7294  
      S4WantShout  0.0360  0.0887  0.1650   0.6846  
      S1DoCurse    0.0000  0.0000     NaN      NaN ?
      S1DoScold   -0.0139  0.1517  0.0083   0.9272  
      S1DoShout    0.1194  0.1630  0.5360   0.4641  
      S2DoCurse    0.1630  0.1745  0.8719   0.3504  
      S2DoScold   -0.1021  0.1180  0.7478   0.3872  
      S2DoShout    0.2081  0.1184  3.0872   0.0789 .
      S3DoCurse   -0.1918  0.1090  3.0949   0.0785 .
      S3DoScold   -0.2556  0.1179  4.7038   0.0301 *
      S3DoShout   -0.0904  0.1836  0.2424   0.6225  
      S4DoCurse   -0.0882  0.1469  0.3611   0.5479  
      S4DoScold   -0.2176  0.1112  3.8311   0.0503 .
      S4DoShout   -0.1053  0.1843  0.3264   0.5678  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                
       S3DoScold
      
       
      Output was not captured! 

---

    Code
      difSIBTEST(verbal, group = "Gender", focal.name = 1, anchor = 1:5)
    Output
      
      Detection of Differential Item Functioning using SIBTEST method 
      without item purification
      
      Investigation of uniform DIF using SIBTEST (Shealy and Stout, 1993) 
       
      Anchor items (provided by the user): 
                  
       S1wantCurse
       S1WantScold
       S1WantShout
       S2WantCurse
       S2WantScold
      
       
      No p-value adjustment for multiple comparisons 
       
                  Beta    SE      X2 Stat. P-value    
      S2WantShout  0.0660  0.0811  0.6620   0.4158    
      S3WantCurse -0.0782  0.0854  0.8387   0.3598    
      S3WantScold -0.1445  0.0843  2.9366   0.0866 .  
      S3WantShout -0.0243  0.0657  0.1368   0.7115    
      S4WantCurse -0.0134  0.0728  0.0339   0.8539    
      S4WantScold -0.0759  0.0809  0.8795   0.3483    
      S4WantShout  0.0546  0.0712  0.5866   0.4437    
      S1DoCurse   -0.1448  0.0728  3.9593   0.0466 *  
      S1DoScold   -0.2090  0.0791  6.9859   0.0082 ** 
      S1DoShout    0.0536  0.0648  0.6847   0.4080    
      S2DoCurse   -0.2516  0.0726 11.9924   0.0005 ***
      S2DoScold   -0.2610  0.0801 10.6227   0.0011 ** 
      S2DoShout   -0.0833  0.0770  1.1694   0.2795    
      S3DoCurse   -0.1752  0.0825  4.5070   0.0338 *  
      S3DoScold   -0.1704  0.0825  4.2595   0.0390 *  
      S3DoShout   -0.1224  0.0867  1.9927   0.1581    
      S4DoCurse   -0.0985  0.0769  1.6423   0.2000    
      S4DoScold   -0.1012  0.0809  1.5678   0.2105    
      S4DoShout    0.0069  0.0807  0.0074   0.9316    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                
       S1DoCurse
       S1DoScold
       S2DoCurse
       S2DoScold
       S3DoCurse
       S3DoScold
      
       
      Output was not captured! 

---

    Code
      difSIBTEST(verbal, group = "Gender", focal.name = 1, anchor = 1:5, purify = TRUE)
    Output
      
      Detection of Differential Item Functioning using SIBTEST method 
      without item purification
      
      Investigation of uniform DIF using SIBTEST (Shealy and Stout, 1993) 
       
      Anchor items (provided by the user): 
                  
       S1wantCurse
       S1WantScold
       S1WantShout
       S2WantCurse
       S2WantScold
      
       
      No p-value adjustment for multiple comparisons 
       
                  Beta    SE      X2 Stat. P-value    
      S2WantShout  0.0660  0.0811  0.6620   0.4158    
      S3WantCurse -0.0782  0.0854  0.8387   0.3598    
      S3WantScold -0.1445  0.0843  2.9366   0.0866 .  
      S3WantShout -0.0243  0.0657  0.1368   0.7115    
      S4WantCurse -0.0134  0.0728  0.0339   0.8539    
      S4WantScold -0.0759  0.0809  0.8795   0.3483    
      S4WantShout  0.0546  0.0712  0.5866   0.4437    
      S1DoCurse   -0.1448  0.0728  3.9593   0.0466 *  
      S1DoScold   -0.2090  0.0791  6.9859   0.0082 ** 
      S1DoShout    0.0536  0.0648  0.6847   0.4080    
      S2DoCurse   -0.2516  0.0726 11.9924   0.0005 ***
      S2DoScold   -0.2610  0.0801 10.6227   0.0011 ** 
      S2DoShout   -0.0833  0.0770  1.1694   0.2795    
      S3DoCurse   -0.1752  0.0825  4.5070   0.0338 *  
      S3DoScold   -0.1704  0.0825  4.2595   0.0390 *  
      S3DoShout   -0.1224  0.0867  1.9927   0.1581    
      S4DoCurse   -0.0985  0.0769  1.6423   0.2000    
      S4DoScold   -0.1012  0.0809  1.5678   0.2105    
      S4DoShout    0.0069  0.0807  0.0074   0.9316    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: 
                
       S1DoCurse
       S1DoScold
       S2DoCurse
       S2DoScold
       S3DoCurse
       S3DoScold
      
       
      Output was not captured! 

