## /Outputs

This directory contains statistical outputs from the MANOVA, ANOVA, post-hoc, and coefficient of variation calculations provided in the manuscript. Specific details:

`gh_cv.csv` is the coefficient of variation output for each trait created in the `3_post_hoc.R` script.

`gh_stats.txt` contains the MANOVA and ANOVA outputs for each trait created in the `2_two_way_manova.R` script.

`gh_stats_tukey.txt` contains the post-hoc tests with lettering reports for each trait created in the `3_post_hoc.R` script.

Descriptive stats provided in manuscript Table S1 was gathered using the `describeBy()` function in the `psych` package in the `3_post_hoc.R` script.