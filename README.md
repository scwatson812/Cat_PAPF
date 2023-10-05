# Cat_PAPF
Contains code to implement the hypothesis testing procedure described in ' Hypothesis Test for Detecting Spatial Patterns in Categorical Areal Data'

'Test Statistic.R' computes the test statistic from observed data.
Data must be saved in a shapefile in a layer called 'selected'.
'Null Distribution.R' estimates the null distribution of the test statistic with Monte Carlo simulations.
'Compute Lower Tail Critical Values.R' uses the output from 'Null Distribution.R' to compute lower tail critical values to test for dispersion.
'Computer Upper Tail Critical Values.R' use teh outpu from 'Null Distribution.R' to compute upper tail critical values to test for clustering.
"ExampleData.xxx' contains example data to run the code.
