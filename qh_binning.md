

## Help
##### (Click ? for hints)

* Binning can only be done once the sampling has been completed in the Sample
tab. Refreshing the sample will require binning to be done again.

* Choose a variable and click 'Bin' to auto-bin it. Numeric variables are
binned using conditional inference trees or upto 20 quantiles (user choice). All 
unique values of a categorical variable and placed into their own bins.

* Missing values will always be placed in their own bin and cannot be combined
with any other bin.

* Combine and split bins to modify the binning. For numeric variables, only
adjacent bins can be combined. Splitting a bin for a categorical variable will
split all distinct values in the existing bin.

* Missing or infinite WoEs with at least 1 record in the bin will result in an
error in the Model tab.


