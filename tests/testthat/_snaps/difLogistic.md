# difLogistic - examples at help page

    Code
      (r1 <- difLogistic(verbal, group = 25, focal.name = 1))
    Output
      
      Detection of both types of Differential Item Functioning
      using Logistic regression method, without item purification
      and with LRT DIF statistic
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Logistic regression DIF statistic: 
       
                  Stat.   P-value   
      S1wantCurse  2.0014  0.3676   
      S1WantScold  3.3541  0.1869   
      S1WantShout  2.4742  0.2902   
      S2WantCurse  4.7296  0.0940 . 
      S2WantScold  4.1404  0.1262   
      S2WantShout 11.4111  0.0033 **
      S3WantCurse  1.6061  0.4480   
      S3WantScold  1.6331  0.4419   
      S3WantShout  2.6989  0.2594   
      S4WantCurse  2.4547  0.2931   
      S4WantScold  2.0997  0.3500   
      S4WantShout  3.6877  0.1582   
      S1DoCurse    1.2196  0.5435   
      S1DoScold    4.7304  0.0939 . 
      S1DoShout    1.0456  0.5929   
      S2DoCurse    7.6935  0.0213 * 
      S2DoScold   10.2622  0.0059 **
      S2DoShout    1.7016  0.4271   
      S3DoCurse    7.2379  0.0268 * 
      S3DoScold    5.8680  0.0532 . 
      S3DoShout    1.2763  0.5283   
      S4DoCurse    2.9521  0.2285   
      S4DoScold    2.6956  0.2598   
      S4DoShout    1.3524  0.5085   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
                  
       S2WantShout
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
      
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0067 A  A 
      S1WantScold 0.0101 A  A 
      S1WantShout 0.0074 A  A 
      S2WantCurse 0.0177 A  A 
      S2WantScold 0.0125 A  A 
      S2WantShout 0.0343 A  A 
      S3WantCurse 0.0056 A  A 
      S3WantScold 0.0050 A  A 
      S3WantShout 0.0106 A  A 
      S4WantCurse 0.0085 A  A 
      S4WantScold 0.0060 A  A 
      S4WantShout 0.0133 A  A 
      S1DoCurse   0.0039 A  A 
      S1DoScold   0.0122 A  A 
      S1DoShout   0.0033 A  A 
      S2DoCurse   0.0243 A  A 
      S2DoScold   0.0277 A  A 
      S2DoShout   0.0058 A  A 
      S3DoCurse   0.0232 A  A 
      S3DoScold   0.0211 A  A 
      S3DoShout   0.0078 A  A 
      S4DoCurse   0.0092 A  A 
      S4DoScold   0.0083 A  A 
      S4DoShout   0.0057 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      (r2 <- difLogistic(verbal, group = 25, focal.name = 1, all.cov = TRUE))
    Output
      
      Detection of both types of Differential Item Functioning
      using Logistic regression method, without item purification
      and with LRT DIF statistic
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Logistic regression DIF statistic: 
       
                  Stat.   P-value   
      S1wantCurse  2.0014  0.3676   
      S1WantScold  3.3541  0.1869   
      S1WantShout  2.4742  0.2902   
      S2WantCurse  4.7296  0.0940 . 
      S2WantScold  4.1404  0.1262   
      S2WantShout 11.4111  0.0033 **
      S3WantCurse  1.6061  0.4480   
      S3WantScold  1.6331  0.4419   
      S3WantShout  2.6989  0.2594   
      S4WantCurse  2.4547  0.2931   
      S4WantScold  2.0997  0.3500   
      S4WantShout  3.6877  0.1582   
      S1DoCurse    1.2196  0.5435   
      S1DoScold    4.7304  0.0939 . 
      S1DoShout    1.0456  0.5929   
      S2DoCurse    7.6935  0.0213 * 
      S2DoScold   10.2622  0.0059 **
      S2DoShout    1.7016  0.4271   
      S3DoCurse    7.2379  0.0268 * 
      S3DoScold    5.8680  0.0532 . 
      S3DoShout    1.2763  0.5283   
      S4DoCurse    2.9521  0.2285   
      S4DoScold    2.6956  0.2598   
      S4DoShout    1.3524  0.5085   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
                  
       S2WantShout
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
      
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0067 A  A 
      S1WantScold 0.0101 A  A 
      S1WantShout 0.0074 A  A 
      S2WantCurse 0.0177 A  A 
      S2WantScold 0.0125 A  A 
      S2WantShout 0.0343 A  A 
      S3WantCurse 0.0056 A  A 
      S3WantScold 0.0050 A  A 
      S3WantShout 0.0106 A  A 
      S4WantCurse 0.0085 A  A 
      S4WantScold 0.0060 A  A 
      S4WantShout 0.0133 A  A 
      S1DoCurse   0.0039 A  A 
      S1DoScold   0.0122 A  A 
      S1DoShout   0.0033 A  A 
      S2DoCurse   0.0243 A  A 
      S2DoScold   0.0277 A  A 
      S2DoShout   0.0058 A  A 
      S3DoCurse   0.0232 A  A 
      S3DoScold   0.0211 A  A 
      S3DoShout   0.0078 A  A 
      S4DoCurse   0.0092 A  A 
      S4DoScold   0.0083 A  A 
      S4DoShout   0.0057 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      difLogistic(verbal, group = 25, focal.name = 1, criterion = "Wald")
    Output
      
      Detection of both types of Differential Item Functioning
      using Logistic regression method, without item purification
      and with Wald DIF statistic
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Logistic regression DIF statistic: 
       
                  Stat.   P-value   
      S1wantCurse  2.0207  0.3641   
      S1WantScold  3.0060  0.2225   
      S1WantShout  2.5317  0.2820   
      S2WantCurse  4.6032  0.1001   
      S2WantScold  3.7357  0.1545   
      S2WantShout 11.0242  0.0040 **
      S3WantCurse  1.6850  0.4306   
      S3WantScold  1.6466  0.4390   
      S3WantShout  2.6446  0.2665   
      S4WantCurse  2.3689  0.3059   
      S4WantScold  2.2328  0.3275   
      S4WantShout  3.4621  0.1771   
      S1DoCurse    1.0610  0.5883   
      S1DoScold    4.8595  0.0881 . 
      S1DoShout    0.9723  0.6150   
      S2DoCurse    6.9308  0.0313 * 
      S2DoScold   10.4798  0.0053 **
      S2DoShout    1.8301  0.4005   
      S3DoCurse    7.1998  0.0273 * 
      S3DoScold    6.2066  0.0449 * 
      S3DoShout    1.2589  0.5329   
      S4DoCurse    2.5362  0.2814   
      S4DoScold    2.5704  0.2766   
      S4DoShout    1.2147  0.5448   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
                  
       S2WantShout
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
      
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0067 A  A 
      S1WantScold 0.0101 A  A 
      S1WantShout 0.0074 A  A 
      S2WantCurse 0.0177 A  A 
      S2WantScold 0.0125 A  A 
      S2WantShout 0.0343 A  A 
      S3WantCurse 0.0056 A  A 
      S3WantScold 0.0050 A  A 
      S3WantShout 0.0106 A  A 
      S4WantCurse 0.0085 A  A 
      S4WantScold 0.0060 A  A 
      S4WantShout 0.0133 A  A 
      S1DoCurse   0.0039 A  A 
      S1DoScold   0.0122 A  A 
      S1DoShout   0.0033 A  A 
      S2DoCurse   0.0243 A  A 
      S2DoScold   0.0277 A  A 
      S2DoShout   0.0058 A  A 
      S3DoCurse   0.0232 A  A 
      S3DoScold   0.0211 A  A 
      S3DoShout   0.0078 A  A 
      S4DoCurse   0.0092 A  A 
      S4DoScold   0.0083 A  A 
      S4DoShout   0.0057 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      difLogistic(verbal, group = 25, focal.name = 1, type = "nudif")
    Output
      
      Detection of nonuniform Differential Item Functioning
      using Logistic regression method, without item purification
      and with LRT DIF statistic
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Logistic regression DIF statistic: 
       
                  Stat.  P-value 
      S1wantCurse 0.0016 0.9686  
      S1WantScold 1.4476 0.2289  
      S1WantShout 0.3136 0.5755  
      S2WantCurse 0.4658 0.4949  
      S2WantScold 1.2367 0.2661  
      S2WantShout 0.1080 0.7425  
      S3WantCurse 1.5106 0.2190  
      S3WantScold 0.0071 0.9330  
      S3WantShout 0.5592 0.4546  
      S4WantCurse 0.4983 0.4802  
      S4WantScold 2.0955 0.1477  
      S4WantShout 0.0386 0.8442  
      S1DoCurse   0.7962 0.3722  
      S1DoScold   0.6335 0.4261  
      S1DoShout   0.3293 0.5661  
      S2DoCurse   0.0563 0.8124  
      S2DoScold   1.1221 0.2895  
      S2DoShout   1.6088 0.2047  
      S3DoCurse   0.0945 0.7585  
      S3DoScold   1.2184 0.2697  
      S3DoShout   0.7505 0.3863  
      S4DoCurse   1.1182 0.2903  
      S4DoScold   0.3039 0.5815  
      S4DoShout   0.2947 0.5872  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as DIF items: No DIF item detected 
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0000 A  A 
      S1WantScold 0.0043 A  A 
      S1WantShout 0.0009 A  A 
      S2WantCurse 0.0017 A  A 
      S2WantScold 0.0037 A  A 
      S2WantShout 0.0003 A  A 
      S3WantCurse 0.0053 A  A 
      S3WantScold 0.0000 A  A 
      S3WantShout 0.0022 A  A 
      S4WantCurse 0.0017 A  A 
      S4WantScold 0.0060 A  A 
      S4WantShout 0.0001 A  A 
      S1DoCurse   0.0026 A  A 
      S1DoScold   0.0016 A  A 
      S1DoShout   0.0010 A  A 
      S2DoCurse   0.0002 A  A 
      S2DoScold   0.0030 A  A 
      S2DoShout   0.0055 A  A 
      S3DoCurse   0.0003 A  A 
      S3DoScold   0.0043 A  A 
      S3DoShout   0.0046 A  A 
      S4DoCurse   0.0035 A  A 
      S4DoScold   0.0009 A  A 
      S4DoShout   0.0012 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      difLogistic(verbal, group = 25, focal.name = 1, type = "udif")
    Output
      
      Detection of uniform Differential Item Functioning
      using Logistic regression method, without item purification
      and with LRT DIF statistic
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Logistic regression DIF statistic: 
       
                  Stat.   P-value    
      S1wantCurse  1.9998  0.1573    
      S1WantScold  1.9065  0.1673    
      S1WantShout  2.1606  0.1416    
      S2WantCurse  4.2639  0.0389 *  
      S2WantScold  2.9036  0.0884 .  
      S2WantShout 11.3031  0.0008 ***
      S3WantCurse  0.0955  0.7573    
      S3WantScold  1.6260  0.2023    
      S3WantShout  2.1397  0.1435    
      S4WantCurse  1.9563  0.1619    
      S4WantScold  0.0042  0.9485    
      S4WantShout  3.6491  0.0561 .  
      S1DoCurse    0.4234  0.5153    
      S1DoScold    4.0968  0.0430 *  
      S1DoShout    0.7163  0.3974    
      S2DoCurse    7.6372  0.0057 ** 
      S2DoScold    9.1401  0.0025 ** 
      S2DoShout    0.0928  0.7607    
      S3DoCurse    7.1433  0.0075 ** 
      S3DoScold    4.6496  0.0311 *  
      S3DoShout    0.5258  0.4684    
      S4DoCurse    1.8339  0.1757    
      S4DoScold    2.3918  0.1220    
      S4DoShout    1.0577  0.3037    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 3.8415 (significance level: 0.05)
      
      Items detected as uniform DIF items:
                  
       S2WantCurse
       S2WantShout
       S1DoScold  
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
      
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0067 A  A 
      S1WantScold 0.0058 A  A 
      S1WantShout 0.0065 A  A 
      S2WantCurse 0.0159 A  A 
      S2WantScold 0.0088 A  A 
      S2WantShout 0.0340 A  A 
      S3WantCurse 0.0003 A  A 
      S3WantScold 0.0049 A  A 
      S3WantShout 0.0084 A  A 
      S4WantCurse 0.0068 A  A 
      S4WantScold 0.0000 A  A 
      S4WantShout 0.0131 A  A 
      S1DoCurse   0.0014 A  A 
      S1DoScold   0.0105 A  A 
      S1DoShout   0.0023 A  A 
      S2DoCurse   0.0241 A  A 
      S2DoScold   0.0247 A  A 
      S2DoShout   0.0003 A  A 
      S3DoCurse   0.0229 A  A 
      S3DoScold   0.0167 A  A 
      S3DoShout   0.0032 A  A 
      S4DoCurse   0.0057 A  A 
      S4DoScold   0.0073 A  A 
      S4DoShout   0.0044 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      difLogistic(verbal, group = 25, focal.name = 1, p.adjust.method = "BH")
    Output
      
      Detection of both types of Differential Item Functioning
      using Logistic regression method, without item purification
      and with LRT DIF statistic
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      Multiple comparisons made with Benjamini-Hochberg adjustement of p-values
      
      Logistic regression DIF statistic: 
       
                  Stat.   P-value Adj. P   
      S1wantCurse  2.0014  0.3676  0.5190  
      S1WantScold  3.3541  0.1869  0.4486  
      S1WantShout  2.4742  0.2902  0.4689  
      S2WantCurse  4.7296  0.0940  0.3222  
      S2WantScold  4.1404  0.1262  0.3785  
      S2WantShout 11.4111  0.0033  0.0709 .
      S3WantCurse  1.6061  0.4480  0.5376  
      S3WantScold  1.6331  0.4419  0.5376  
      S3WantShout  2.6989  0.2594  0.4689  
      S4WantCurse  2.4547  0.2931  0.4689  
      S4WantScold  2.0997  0.3500  0.5190  
      S4WantShout  3.6877  0.1582  0.4219  
      S1DoCurse    1.2196  0.5435  0.5671  
      S1DoScold    4.7304  0.0939  0.3222  
      S1DoShout    1.0456  0.5929  0.5929  
      S2DoCurse    7.6935  0.0213  0.1609  
      S2DoScold   10.2622  0.0059  0.0709 .
      S2DoShout    1.7016  0.4271  0.5376  
      S3DoCurse    7.2379  0.0268  0.1609  
      S3DoScold    5.8680  0.0532  0.2553  
      S3DoShout    1.2763  0.5283  0.5671  
      S4DoCurse    2.9521  0.2285  0.4689  
      S4DoScold    2.6956  0.2598  0.4689  
      S4DoShout    1.3524  0.5085  0.5671  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items: No DIF item detected 
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0067 A  A 
      S1WantScold 0.0101 A  A 
      S1WantShout 0.0074 A  A 
      S2WantCurse 0.0177 A  A 
      S2WantScold 0.0125 A  A 
      S2WantShout 0.0343 A  A 
      S3WantCurse 0.0056 A  A 
      S3WantScold 0.0050 A  A 
      S3WantShout 0.0106 A  A 
      S4WantCurse 0.0085 A  A 
      S4WantScold 0.0060 A  A 
      S4WantShout 0.0133 A  A 
      S1DoCurse   0.0039 A  A 
      S1DoScold   0.0122 A  A 
      S1DoShout   0.0033 A  A 
      S2DoCurse   0.0243 A  A 
      S2DoScold   0.0277 A  A 
      S2DoShout   0.0058 A  A 
      S3DoCurse   0.0232 A  A 
      S3DoScold   0.0211 A  A 
      S3DoShout   0.0078 A  A 
      S4DoCurse   0.0092 A  A 
      S4DoScold   0.0083 A  A 
      S4DoShout   0.0057 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      (r3 <- difLogistic(verbal, group = "Gender", focal.name = 1, purify = TRUE))
    Output
      
      Detection of both types of Differential Item Functioning
      using Logistic regression method, with item purification
      and with LRT DIF statistic
      
      Convergence reached after 2 iterations
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Logistic regression DIF statistic: 
       
                  Stat.   P-value   
      S1wantCurse  0.8695  0.6474   
      S1WantScold  0.9981  0.6071   
      S1WantShout  1.1262  0.5694   
      S2WantCurse  2.6707  0.2631   
      S2WantScold  1.5035  0.4715   
      S2WantShout  7.4079  0.0246 * 
      S3WantCurse  1.3429  0.5110   
      S3WantScold  3.4083  0.1819   
      S3WantShout  2.0841  0.3527   
      S4WantCurse  1.4013  0.4963   
      S4WantScold  4.9929  0.0824 . 
      S4WantShout  2.5516  0.2792   
      S1DoCurse    2.1864  0.3351   
      S1DoScold    7.1927  0.0274 * 
      S1DoShout    0.2852  0.8671   
      S2DoCurse   10.1885  0.0061 **
      S2DoScold   12.6729  0.0018 **
      S2DoShout    3.2351  0.1984   
      S3DoCurse    9.1152  0.0105 * 
      S3DoScold    7.4605  0.0240 * 
      S3DoShout    1.4878  0.4753   
      S4DoCurse    4.5572  0.1024   
      S4DoScold    4.5671  0.1019   
      S4DoShout    0.5883  0.7452   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
                  
       S2WantShout
       S1DoScold  
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
      
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0028 A  A 
      S1WantScold 0.0029 A  A 
      S1WantShout 0.0033 A  A 
      S2WantCurse 0.0096 A  A 
      S2WantScold 0.0044 A  A 
      S2WantShout 0.0211 A  A 
      S3WantCurse 0.0044 A  A 
      S3WantScold 0.0105 A  A 
      S3WantShout 0.0081 A  A 
      S4WantCurse 0.0045 A  A 
      S4WantScold 0.0137 A  A 
      S4WantShout 0.0088 A  A 
      S1DoCurse   0.0074 A  A 
      S1DoScold   0.0188 A  A 
      S1DoShout   0.0009 A  A 
      S2DoCurse   0.0327 A  A 
      S2DoScold   0.0352 A  B 
      S2DoShout   0.0112 A  A 
      S3DoCurse   0.0294 A  A 
      S3DoScold   0.0275 A  A 
      S3DoShout   0.0090 A  A 
      S4DoCurse   0.0145 A  A 
      S4DoScold   0.0142 A  A 
      S4DoShout   0.0024 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      difLogistic(verbal, group = "Gender", focal.name = 1, purify = TRUE, nrIter = 5)
    Output
      
      Detection of both types of Differential Item Functioning
      using Logistic regression method, with item purification
      and with LRT DIF statistic
      
      Convergence reached after 2 iterations
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Logistic regression DIF statistic: 
       
                  Stat.   P-value   
      S1wantCurse  0.8695  0.6474   
      S1WantScold  0.9981  0.6071   
      S1WantShout  1.1262  0.5694   
      S2WantCurse  2.6707  0.2631   
      S2WantScold  1.5035  0.4715   
      S2WantShout  7.4079  0.0246 * 
      S3WantCurse  1.3429  0.5110   
      S3WantScold  3.4083  0.1819   
      S3WantShout  2.0841  0.3527   
      S4WantCurse  1.4013  0.4963   
      S4WantScold  4.9929  0.0824 . 
      S4WantShout  2.5516  0.2792   
      S1DoCurse    2.1864  0.3351   
      S1DoScold    7.1927  0.0274 * 
      S1DoShout    0.2852  0.8671   
      S2DoCurse   10.1885  0.0061 **
      S2DoScold   12.6729  0.0018 **
      S2DoShout    3.2351  0.1984   
      S3DoCurse    9.1152  0.0105 * 
      S3DoScold    7.4605  0.0240 * 
      S3DoShout    1.4878  0.4753   
      S4DoCurse    4.5572  0.1024   
      S4DoScold    4.5671  0.1019   
      S4DoShout    0.5883  0.7452   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
                  
       S2WantShout
       S1DoScold  
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
      
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0028 A  A 
      S1WantScold 0.0029 A  A 
      S1WantShout 0.0033 A  A 
      S2WantCurse 0.0096 A  A 
      S2WantScold 0.0044 A  A 
      S2WantShout 0.0211 A  A 
      S3WantCurse 0.0044 A  A 
      S3WantScold 0.0105 A  A 
      S3WantShout 0.0081 A  A 
      S4WantCurse 0.0045 A  A 
      S4WantScold 0.0137 A  A 
      S4WantShout 0.0088 A  A 
      S1DoCurse   0.0074 A  A 
      S1DoScold   0.0188 A  A 
      S1DoShout   0.0009 A  A 
      S2DoCurse   0.0327 A  A 
      S2DoScold   0.0352 A  B 
      S2DoShout   0.0112 A  A 
      S3DoCurse   0.0294 A  A 
      S3DoScold   0.0275 A  A 
      S3DoShout   0.0090 A  A 
      S4DoCurse   0.0145 A  A 
      S4DoScold   0.0142 A  A 
      S4DoShout   0.0024 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      difLogistic(verbal, group = "Gender", focal.name = 1, purify = TRUE,
        p.adjust.method = "BH", puriadjType = "simple")
    Output
      
      Detection of both types of Differential Item Functioning
      using Logistic regression method, with item purification
      and with LRT DIF statistic
      
      Convergence reached after 2 iterations
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      Multiple comparisons made with Benjamini-Hochberg adjustement of p-values
      Multiple comparison applied after item purification 
      
      Logistic regression DIF statistic: 
       
                  Stat.   P-value Adj. P   
      S1wantCurse  0.8695  0.6474  0.7063  
      S1WantScold  0.9981  0.6071  0.6938  
      S1WantShout  1.1262  0.5694  0.6833  
      S2WantCurse  2.6707  0.2631  0.5155  
      S2WantScold  1.5035  0.4715  0.6454  
      S2WantShout  7.4079  0.0246  0.1097  
      S3WantCurse  1.3429  0.5110  0.6454  
      S3WantScold  3.4083  0.1819  0.4328  
      S3WantShout  2.0841  0.3527  0.5644  
      S4WantCurse  1.4013  0.4963  0.6454  
      S4WantScold  4.9929  0.0824  0.2731  
      S4WantShout  2.5516  0.2792  0.5155  
      S1DoCurse    2.1864  0.3351  0.5644  
      S1DoScold    7.1927  0.0274  0.1097  
      S1DoShout    0.2852  0.8671  0.8671  
      S2DoCurse   10.1885  0.0061  0.0736 .
      S2DoScold   12.6729  0.0018  0.0425 *
      S2DoShout    3.2351  0.1984  0.4328  
      S3DoCurse    9.1152  0.0105  0.0839 .
      S3DoScold    7.4605  0.0240  0.1097  
      S3DoShout    1.4878  0.4753  0.6454  
      S4DoCurse    4.5572  0.1024  0.2731  
      S4DoScold    4.5671  0.1019  0.2731  
      S4DoShout    0.5883  0.7452  0.7776  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
                
       S2DoScold
      
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0028 A  A 
      S1WantScold 0.0029 A  A 
      S1WantShout 0.0033 A  A 
      S2WantCurse 0.0096 A  A 
      S2WantScold 0.0044 A  A 
      S2WantShout 0.0211 A  A 
      S3WantCurse 0.0044 A  A 
      S3WantScold 0.0105 A  A 
      S3WantShout 0.0081 A  A 
      S4WantCurse 0.0045 A  A 
      S4WantScold 0.0137 A  A 
      S4WantShout 0.0088 A  A 
      S1DoCurse   0.0074 A  A 
      S1DoScold   0.0188 A  A 
      S1DoShout   0.0009 A  A 
      S2DoCurse   0.0327 A  A 
      S2DoScold   0.0352 A  B 
      S2DoShout   0.0112 A  A 
      S3DoCurse   0.0294 A  A 
      S3DoScold   0.0275 A  A 
      S3DoShout   0.0090 A  A 
      S4DoCurse   0.0145 A  A 
      S4DoScold   0.0142 A  A 
      S4DoShout   0.0024 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      (r4 <- difLogistic(verbal, group = "Gender", focal.name = 1, purify = TRUE,
        p.adjust.method = "BH", puriadjType = "combined"))
    Output
      
      Detection of both types of Differential Item Functioning
      using Logistic regression method, with item purification
      and with LRT DIF statistic
      
      Convergence reached after 0 iteration
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      Multiple comparisons made with Benjamini-Hochberg adjustement of p-values
      Multiple comparison applied after each iteration of item purification 
      
      Logistic regression DIF statistic: 
       
                  Stat.   P-value Adj. P   
      S1wantCurse  2.0014  0.3676  0.5190  
      S1WantScold  3.3541  0.1869  0.4486  
      S1WantShout  2.4742  0.2902  0.4689  
      S2WantCurse  4.7296  0.0940  0.3222  
      S2WantScold  4.1404  0.1262  0.3785  
      S2WantShout 11.4111  0.0033  0.0709 .
      S3WantCurse  1.6061  0.4480  0.5376  
      S3WantScold  1.6331  0.4419  0.5376  
      S3WantShout  2.6989  0.2594  0.4689  
      S4WantCurse  2.4547  0.2931  0.4689  
      S4WantScold  2.0997  0.3500  0.5190  
      S4WantShout  3.6877  0.1582  0.4219  
      S1DoCurse    1.2196  0.5435  0.5671  
      S1DoScold    4.7304  0.0939  0.3222  
      S1DoShout    1.0456  0.5929  0.5929  
      S2DoCurse    7.6935  0.0213  0.1609  
      S2DoScold   10.2622  0.0059  0.0709 .
      S2DoShout    1.7016  0.4271  0.5376  
      S3DoCurse    7.2379  0.0268  0.1609  
      S3DoScold    5.8680  0.0532  0.2553  
      S3DoShout    1.2763  0.5283  0.5671  
      S4DoCurse    2.9521  0.2285  0.4689  
      S4DoScold    2.6956  0.2598  0.4689  
      S4DoShout    1.3524  0.5085  0.5671  
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items: No DIF item detected 
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0067 A  A 
      S1WantScold 0.0101 A  A 
      S1WantShout 0.0074 A  A 
      S2WantCurse 0.0177 A  A 
      S2WantScold 0.0125 A  A 
      S2WantShout 0.0343 A  A 
      S3WantCurse 0.0056 A  A 
      S3WantScold 0.0050 A  A 
      S3WantShout 0.0106 A  A 
      S4WantCurse 0.0085 A  A 
      S4WantScold 0.0060 A  A 
      S4WantShout 0.0133 A  A 
      S1DoCurse   0.0039 A  A 
      S1DoScold   0.0122 A  A 
      S1DoShout   0.0033 A  A 
      S2DoCurse   0.0243 A  A 
      S2DoScold   0.0277 A  A 
      S2DoShout   0.0058 A  A 
      S3DoCurse   0.0232 A  A 
      S3DoScold   0.0211 A  A 
      S3DoShout   0.0078 A  A 
      S4DoCurse   0.0092 A  A 
      S4DoScold   0.0083 A  A 
      S4DoShout   0.0057 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      difLogistic(verbal, group = 25, focal.name = 1, anchor = 1:5)
    Output
      
      Detection of both types of Differential Item Functioning
      using Logistic regression method, without item purification
      and with LRT DIF statistic
      
      Matching variable: test score 
       
      Anchor items (provided by the user): 
                  
       S1wantCurse
       S1WantScold
       S1WantShout
       S2WantCurse
       S2WantScold
      
       
      No p-value adjustment for multiple comparisons 
       
      Logistic regression DIF statistic: 
       
                  Stat.   P-value    
      S2WantShout  3.1679  0.2052    
      S3WantCurse  1.6638  0.4352    
      S3WantScold  5.6656  0.0588 .  
      S3WantShout  1.9840  0.3708    
      S4WantCurse  0.0168  0.9916    
      S4WantScold  3.1868  0.2032    
      S4WantShout  1.6739  0.4330    
      S1DoCurse    3.9563  0.1383    
      S1DoScold   11.4683  0.0032 ** 
      S1DoShout    0.1682  0.9193    
      S2DoCurse   14.2314  0.0008 ***
      S2DoScold   21.0459  0.0000 ***
      S2DoShout    7.6834  0.0215 *  
      S3DoCurse   13.5954  0.0011 ** 
      S3DoScold    7.7476  0.0208 *  
      S3DoShout    1.2114  0.5457    
      S4DoCurse    5.6697  0.0587 .  
      S4DoScold    6.1237  0.0468 *  
      S4DoShout    0.3432  0.8423    
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
                
       S1DoScold
       S2DoCurse
       S2DoScold
       S2DoShout
       S3DoCurse
       S3DoScold
       S4DoScold
      
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S2WantShout 0.0084 A  A 
      S3WantCurse 0.0047 A  A 
      S3WantScold 0.0153 A  A 
      S3WantShout 0.0076 A  A 
      S4WantCurse 0.0001 A  A 
      S4WantScold 0.0084 A  A 
      S4WantShout 0.0058 A  A 
      S1DoCurse   0.0130 A  A 
      S1DoScold   0.0300 A  A 
      S1DoShout   0.0005 A  A 
      S2DoCurse   0.0450 A  B 
      S2DoScold   0.0535 A  B 
      S2DoShout   0.0238 A  A 
      S3DoCurse   0.0416 A  B 
      S3DoScold   0.0281 A  A 
      S3DoShout   0.0074 A  A 
      S4DoCurse   0.0196 A  A 
      S4DoScold   0.0194 A  A 
      S4DoShout   0.0015 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      difLogistic(verbal, group = 25, focal.name = 1, match = anger)
    Output
      
      Detection of both types of Differential Item Functioning
      using Logistic regression method, without item purification
      and with LRT DIF statistic
      
      Matching variable: specified matching variable 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Logistic regression DIF statistic: 
       
                  Stat.   P-value   
      S1wantCurse  0.5349  0.7653   
      S1WantScold  0.6875  0.7091   
      S1WantShout  0.3500  0.8395   
      S2WantCurse  0.8936  0.6397   
      S2WantScold  0.5492  0.7599   
      S2WantShout  4.0264  0.1336   
      S3WantCurse  1.7564  0.4155   
      S3WantScold  6.8101  0.0332 * 
      S3WantShout  0.7503  0.6872   
      S4WantCurse  0.3255  0.8498   
      S4WantScold  1.0850  0.5813   
      S4WantShout  1.7647  0.4138   
      S1DoCurse    4.4281  0.1093   
      S1DoScold    7.0720  0.0291 * 
      S1DoShout    0.0705  0.9654   
      S2DoCurse   12.0395  0.0024 **
      S2DoScold   13.6888  0.0011 **
      S2DoShout    1.5842  0.4529   
      S3DoCurse   11.8862  0.0026 **
      S3DoScold    6.3391  0.0420 * 
      S3DoShout    2.2528  0.3242   
      S4DoCurse    4.7702  0.0921 . 
      S4DoScold    5.2016  0.0742 . 
      S4DoShout    0.4464  0.8000   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
                  
       S3WantScold
       S1DoScold  
       S2DoCurse  
       S2DoScold  
       S3DoCurse  
       S3DoScold  
      
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0024 A  A 
      S1WantScold 0.0029 A  A 
      S1WantShout 0.0015 A  A 
      S2WantCurse 0.0044 A  A 
      S2WantScold 0.0023 A  A 
      S2WantShout 0.0165 A  A 
      S3WantCurse 0.0074 A  A 
      S3WantScold 0.0290 A  A 
      S3WantShout 0.0035 A  A 
      S4WantCurse 0.0014 A  A 
      S4WantScold 0.0046 A  A 
      S4WantShout 0.0077 A  A 
      S1DoCurse   0.0197 A  A 
      S1DoScold   0.0294 A  A 
      S1DoShout   0.0003 A  A 
      S2DoCurse   0.0498 A  B 
      S2DoScold   0.0550 A  B 
      S2DoShout   0.0071 A  A 
      S3DoCurse   0.0489 A  B 
      S3DoScold   0.0295 A  A 
      S3DoShout   0.0155 A  A 
      S4DoCurse   0.0202 A  A 
      S4DoScold   0.0219 A  A 
      S4DoShout   0.0023 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

---

    Code
      difLogistic(verbal[, 1:24], group = anger, member.type = "cont")
    Output
      
      Detection of both types of Differential Item Functioning
      using Logistic regression method, without item purification
      and with LRT DIF statistic
      
      Matching variable: test score 
       
      No set of anchor items was provided 
       
      No p-value adjustment for multiple comparisons 
       
      Logistic regression DIF statistic: 
       
                  Stat.  P-value  
      S1wantCurse 4.0005 0.1353   
      S1WantScold 4.9237 0.0853  .
      S1WantShout 0.6449 0.7244   
      S2WantCurse 1.0667 0.5866   
      S2WantScold 0.0644 0.9683   
      S2WantShout 5.2977 0.0707  .
      S3WantCurse 0.0232 0.9884   
      S3WantScold 2.9149 0.2328   
      S3WantShout 1.2282 0.5411   
      S4WantCurse 2.7120 0.2577   
      S4WantScold 1.7695 0.4128   
      S4WantShout 3.0070 0.2223   
      S1DoCurse   0.4631 0.7933   
      S1DoScold   0.0468 0.9769   
      S1DoShout   2.3138 0.3145   
      S2DoCurse   5.8759 0.0530  .
      S2DoScold   2.2272 0.3284   
      S2DoShout   9.0922 0.0106  *
      S3DoCurse   0.0264 0.9869   
      S3DoScold   0.2519 0.8817   
      S3DoShout   0.6742 0.7139   
      S4DoCurse   2.8782 0.2371   
      S4DoScold   1.2859 0.5257   
      S4DoShout   0.8240 0.6623   
      
      Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1  
      
      Detection threshold: 5.9915 (significance level: 0.05)
      
      Items detected as DIF items:
                
       S2DoShout
      
       
      Effect size (Nagelkerke's R^2): 
       
      Effect size code: 
       'A': negligible effect 
       'B': moderate effect 
       'C': large effect 
       
                  R^2    ZT JG
      S1wantCurse 0.0134 A  A 
      S1WantScold 0.0148 A  A 
      S1WantShout 0.0019 A  A 
      S2WantCurse 0.0040 A  A 
      S2WantScold 0.0002 A  A 
      S2WantShout 0.0161 A  A 
      S3WantCurse 0.0001 A  A 
      S3WantScold 0.0089 A  A 
      S3WantShout 0.0048 A  A 
      S4WantCurse 0.0094 A  A 
      S4WantScold 0.0051 A  A 
      S4WantShout 0.0108 A  A 
      S1DoCurse   0.0015 A  A 
      S1DoScold   0.0001 A  A 
      S1DoShout   0.0073 A  A 
      S2DoCurse   0.0186 A  A 
      S2DoScold   0.0061 A  A 
      S2DoShout   0.0305 A  A 
      S3DoCurse   0.0001 A  A 
      S3DoScold   0.0009 A  A 
      S3DoShout   0.0041 A  A 
      S4DoCurse   0.0090 A  A 
      S4DoScold   0.0040 A  A 
      S4DoShout   0.0035 A  A 
      
      Effect size codes: 
       Zumbo & Thomas (ZT): 0 'A' 0.13 'B' 0.26 'C' 1 
       Jodoin & Gierl (JG): 0 'A' 0.035 'B' 0.07 'C' 1 
      
       Output was not captured! 

