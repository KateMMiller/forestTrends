# forestTrends
<h3>WIP package for using case bootstrap to assess temporal trends</h3>
<br>
Work in progress set of functions that uses case bootstrapping methods to assess temporal trends in permanent plots with repeated visits. Package was designed to work with forestNETN/forestMIDN or NPSForVeg datasets, but should work with most datasets that have plot-level visits as rows and variables as columns. At the very least, a compatible dataset will have an ID column that uniquely identifies the plot names, a column for time (eg year or cycle), and a response variable. 
