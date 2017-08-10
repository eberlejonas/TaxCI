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

Use `install.packages("ape")`, `install.packages("spider")`, and `install.packages("doSNOW")` in the R console to install them from the CRAN repository. Due to the rapid development of R, some packages might not always be available from CRAN. In this case you can try to install them from R-Forge (`install.packages("spider", repos="http://R-Forge.R-project.org")`) or from GitHub (instructions equivalent to TaxCI-installation below). In this case you also need to manually install the spider-dependency pegas beforehand: `install.packages("pegas")`.

An easy way to install the TaxCI package from GitHub is using the devtools-package:
```R
install.packages("devtools")
library(devtools)
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
At the end of the help page you will find an example of a workflow for a complete TaxCI analysis using the accompanying Carabidae (ground beetles) data set.

Use `help(function)` to see detailed information on how to use a specific function (e.g., `help(tci)`).

A more detailed tutorial R-script is available as supporting information to the article below (Rulik et al. in press).

## Reference
B Rulik, J Eberle, L von der Mark, J Thormann, M Jung, F Köhler, W Apfel, A Weigel, A Kopetz, J Köhler, F Fritzlar, M Hartmann, K Hadulla, J Schmidt, T Hörren, D Krebs, F Theves, U Eulitz, A Skale, D Rohwedder, A Kleeberg, J Astrin, M Geiger, W Wägele, P Grobe, D Ahrens (2017) Using taxonomic consistency with semi-automated data pre-processing for high quality DNA barcodes. *Methods in Ecology and Evolution*. doi: 10.1111/2041-210X.12824
