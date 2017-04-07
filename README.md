# TaxCI
TaxCI is a R-package for checking the consistency of huge DNA-barcode datasets.
It provides functions for semi-automated data pre-processing prior to submission
to a public database.

## Installation
Prerequisites:
* TaxCI works cross-platform; Depending on the dataset >4GB RAM are recommended
* an up to date R-installation (64-bit recommended)
* The following R packages must be installed as dependencies:
	* ape
	* spider
	* doSNOW
	Use `install.packages("ape")`, `install.packages("spider")`, and `install.packages("doSNOW")` in the R console to install them from the CRAN repository.

The easiest way to install the TaxCI package from GitHub is using the devtools-package:
```R
install.packages("devtools")
install_github("eberlejonas/TaxCI")
```
	

## Quick start
First, load the TaxCI-package:
```R
library(TaxCI)
```
To see the help file, type:
```R
help("TaxCI")
```
At the end of the help page you will find a complete example using the accompanying Carabidae data set.
Use `help(function)` to see detailed information on how to use a specific function (e.g., `help(tci)`).

A more detailed tutorial R-script is available as supporting information to the article below (Rulik et al. under review).

## Reference
Björn Rulik, Jonas Eberle, Laura von der Mark, Jana Thormann, Manfred Jung, Frank Köhler, Wolfgang Apfel, Andreas Weigel, Andreas Kopetz, Jonas Köhler, Frank Fritzlar, Matthias Hartmann, Karl Hadulla, Joachim Schmidt, Thomas Hörren, Detlef Krebs, Florian Theves, Ute Eulitz, André Skale, Dirk Rohwedder, Andreas Kleeberg, Jonas Astrin, Matthias Geiger, Wolfgang Wägele, Peter Grobe, Dirk Ahrens (under review) *Using taxonomic consistency with semi-automated data pre-processing for high quality DNA barcodes*.
