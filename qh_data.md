

## Help
##### (Click ? for an introduction to scorecards and other hints)

* Upload the model data by clicking 'Browse'.

* Download simulated data from the link provided if you do not have any credit
scoring data handy.

* Variable names are converted to synctactically valid names using the *make.names* function and '.' will be replaced by '_'.

* The variable types are auto-detected from the data. Use the dropdown against
each variable to modify the type.

* Choose a variable as the good/bad flag. If multiple variables are chosen, only the first one will be considered. This variable must have the values 0 and 1
only and the value 1 must identify good accounts.

* Choose the variables for stratification while creating train and test samples. The good/bad flag will always be used as a stratification variable. Choosing a
numeric or categorical variable with too many distinct values may cause errors
in the Sample tab.

* Choose the variables which you want to bin and consider as predictor variables during model building. The good/bad flag will not be binned. Only categorical,
numeric or integer variables can be binned.



