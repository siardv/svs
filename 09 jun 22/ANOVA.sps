﻿* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
ONEWAY r_pre_wst_sum r_pre_tak_sum r_post_wst_sum r_post_tak_sum BY groep_1
  /POLYNOMIAL=1
  /ES=OVERALL
  /STATISTICS DESCRIPTIVES EFFECTS HOMOGENEITY BROWNFORSYTHE WELCH 
  /PLOT MEANS
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95)
  /POSTHOC=TUKEY DUNCAN ALPHA(0.05).