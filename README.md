# forestTrends
<h3>Package for using non-parametric bootstrapping to assess temporal trends for mixed models</h3>
<br>
Set of functions that uses case bootstrapping methods to assess temporal trends in permanent plots with repeated visits. Package was designed to work with forestNETN/forestMIDN or NPSForVeg datasets, but should work with most datasets that have plot-level visits as rows and variables as columns. At the very least, a compatible dataset will have an ID column that uniquely identifies the plot names, a column for time (eg year or cycle), and a response variable. 
