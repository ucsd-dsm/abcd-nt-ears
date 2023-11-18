# ABCD Study (Novel Technology) - EARS

This repository provides R code examples of how to process and analyze EARS data from the Adolescent Brain Cognitive Development (ABCD) Study. [add details about study, EARS, and/or paper?...]

## Data 

To run these examples, you will need access to the ABCD data resource [add details about DUC, how to download the EARS tabulated and raw data, etc. ...]

Once you obtained access and downloaded the EARS, data

- copy the `nt_y_ears.csv` table from the tabulated data resource into the `data/` directory
- copy the `abcd_earsraw01/` directory containing the EARS raw data into the `data/` directory

After that, the `data/` directory should look like this

```
data/
 ├── README.md
 ├── abcd_earsraw01
 │   ├── ABCD_usage_log_NDAR_INVXXXXXXXX.zip
 │   ├── ...
 │   ├── earrawdata_NDAR_INVXXXXXXXX.zip
 │   ├── ...
 └── nt_y_ears.csv
```

## R environment

This repository is organized as an RStudio project and the needed packages and their versions are tracked using the [`{renv}` package](https://rstudio.github.io/renv/articles/renv.html). The easiest way to recreate the needed R environment to use the code is to follow the following steps:

- (install [R](https://cran.rstudio.com/) and [RStudio](https://posit.co/download/rstudio-desktop/))
- clone this repository to your local computer
- navigate into the top level directory
- double click the `abcd-nt-ears.Rproj` file (this will open RStudio with the working directory correctly set)
- execute `renv::restore()` from the command prompt

The R version used to create and test this code is 4.2.3.

## Code examples

The example code for how to process and analyze EARS data can be found in the following two R scripts in the top level directory:

- `ears-and-ios-categ.R`: Code for combining Android (Google Play) and iOS (App Store) categories.
- `ears-hourly-binning.R`: Code for extracting EARS raw data, binning it into hours, and plotting it.
