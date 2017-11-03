
<!-- README.md is generated from README.Rmd. Please edit that file -->
Habitat modeling
================

Data is held in a private google drive folder. If you have access to the folder, open analysis/data/raw\_data/load\_new\_data.R.

1.  Make sure that you have googledrive loaded
2.  Authenticate the connection (this will open up a browser window. Sign in to your relevant google account, copy and paste the code into the console when it asks to "Enter authorization code:")

    ``` r
    options(httr_oob_default=TRUE)
    googledrive::drive_auth()
    ```

3.  Make sure the "HabMod" package is loaded with ctrl+shift+l, which brings a few helper functions into the current environment.
4.  Download spring.data.RData and fall.data.Rdata from appropriate GoogleDrive locations
5.  Now you are all set to run the Random Forest (analysis/procedure/habitatmodel.R) and Extreme Gradient Boosting (analysis/procedure/xgboost\_habitat.R) analyses.
    -   Note, for the Random Forest analysis, you will need to have BlueBRIDGE access to the [https://i-marine.d4science.org/group/stockassessment](BlueBRIDGE%20Stock%20Assessment%20VRE) to run the analysis on the BlueBRIDGE e-infrastructure. Once you have access, you will be asked to provide your login and token when you run the analysis.

### Citation

Please cite this compendium as:

> Authors, (2017). *Title of compendium*. Accessed 03 Nov 2017. Online at <https://doi.org/xxx/xxx>

### Installation

You can install habitatmodel from github with:

``` r
# install.packages("devtools")
devtools::install_github("slarge/HabMod")
```

### Licenses

**Text and figures:** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code:** See the [DESCRIPTION](DESCRIPTION) file

**Data:** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/) attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please see our [contributor guidelines](CONTRIBUTING.md). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
