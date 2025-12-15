# Eurostat statistics browser

## 1. Clone the Repository or Download from GitHub Web GUI

### Clone the Repository
To clone the repository to your local machine, use the following command in your terminal:

```bash
git clone https://github.com/pitkant/stats_shiny.git
```

The repository will be cloned to the directory which you have open on your terminal. Change the directory if needed.

This assumes you have Git installed on your computer. If you do not have Git installed yet, please download the latest release from the [Git website](https://git-scm.com) and install it.

### Download from GitHub Web GUI

If you prefer not to use Git or want to download the contents directly, you can do so by visiting the GitHub repository's web page. Click the green "Code" button on the upper right corner of the file view and select "Download ZIP" from the dropdown menu.

## 2. Install RStudio

To run Shiny apps in R you need to have R installed on your computer. Since this is an R Shiny application this guide presumes that you have R installed, but if you don't you can download it from [The R Project website](https://www.r-project.org).

To run the Shiny app in this repository, you need to have RStudio installed on your computer. Follow the instructions below based on your operating system:

### Windows

1. Download RStudio from the [official RStudio download page](https://posit.co/download/rstudio-desktop/).
2. Run the installer and follow the on-screen instructions.

### macOS

1. Download RStudio from the [official RStudio download page](https://posit.co/download/rstudio-desktop/).
2. Open the downloaded .dmg file and drag the RStudio icon to your Applications folder.

### Linux

1. Follow the instructions for your specific Linux distribution on the [official RStudio download page](https://posit.co/download/rstudio-desktop/).

## 3. Run the Shiny App in RStudio

1. Open RStudio on your computer.
2. Navigate to the directory where you cloned or downloaded the repository.
3. Open the app.R file in RStudio.
4. In the RStudio console, run the following commands to install necessary packages (if not already installed):

```
install.packages(c("shiny", "bslib", "bsicons", "DT", "dataset", "rdflib", "csvwr", "jsonlite", "jsonld"))
```

Development version of Eurostat:

```
remotes::install_github("ropengov/eurostat", ref = "v4.1")
```

Please note that rdflib package may require installing additional system dependencies on your computer, depending on whether you are running macOS or some Linux distribution. If you receive an error message while trying to install rdflib or its dependency redland, please read the installation instructions there carefully and follow them.

Alternatively, the specific packages can be retrieved and installed by reading the renv lockfile and restoring the status of the project as it was on the computer used to develop this Shiny application:

```
renv::restore()
```

### Run from R console

Once the packages are installed run the following command in the console:

```
shiny::runApp()
```

This will launch the Shiny app in your default web browser.

Please make sure that your working directory is the same as the directory where `app.R` file is. You can check this with `getwd()` and change the directory with `setwd()`.

### Run from RStudio GUI

With app.R file open in your workspace, click the "Run App" button next to the green arrow on the upper right corner of the code editor.

## Acknowledgements

We wish to acknowledge the importance of various open source contributors to realising this project. This Shiny application depends directly on `R: A Language and Environment for Statistical Computing` and various R Extensions, most notably `shiny`, `bslib`, `bsicons`, `eurostat`, `DT`, `dataset`, `rdflib`, `csvwr`, `jsonlite`, `jsonld`. Additionally, these direct dependencies depend on other R packages and external libraries. We are grateful to all open-source contributors.

### Direct dependencies (references in alphabetical order)

Antal D (2024). _dataset: Create Data Frames that are Easier to Exchange and
  Reuse_. R package version 0.3.0,
  <https://CRAN.R-project.org/package=dataset>.
  
Carl Boettiger. (2018). rdflib: A high level wrapper around the redland
  package for common rdf applications (Version 0.1.0). Zenodo.
  <https://doi.org/10.5281/zenodo.1098478>

Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J,
  McPherson J, Dipert A, Borges B (2023). _shiny: Web Application Framework for
  R_. R package version 1.8.0, <https://CRAN.R-project.org/package=shiny>.
  
Gower R (2022). _csvwr: Read and Write CSV on the Web (CSVW) Tables and
  Metadata_. R package version 0.1.7,
  <https://CRAN.R-project.org/package=csvwr>.
  
Lahti L., Huovari J., Kainu M., and Biecek P. (2017). Retrieval and analysis
  of Eurostat open data with the eurostat package. The R Journal 9(1), pp.
  385-392. doi: 10.32614/RJ-2017-019
  
Ooms J (2020). _jsonld: JSON for Linking Data_. R package version 2.2,
  <https://CRAN.R-project.org/package=jsonld>.
  
Ooms J (2014). “The jsonlite Package: A Practical and Consistent Mapping
  Between JSON Data and R Objects.” _arXiv:1403.2805 [stat.CO]_.
  <https://arxiv.org/abs/1403.2805>.
  
Sievert C (2023). _bsicons: Easily Work with 'Bootstrap' Icons_. R package
  version 0.1.2, <https://CRAN.R-project.org/package=bsicons>.
  
Sievert C, Cheng J, Aden-Buie G (2023). _bslib: Custom 'Bootstrap' 'Sass'
  Themes for 'shiny' and 'rmarkdown'_. R package version 0.6.1,
  <https://CRAN.R-project.org/package=bslib>.
  
Xie Y, Cheng J, Tan X (2024). _DT: A Wrapper of the JavaScript Library
  'DataTables'_. R package version 0.32,
  <https://CRAN.R-project.org/package=DT>.

## Disclaimer

This project is not in any way affiliated with or endorsed by the statistical authority of the European Union, Eurostat.

By importing or otherwise utilizing different software packages we do not imply or claim responsibility on the part of the contributors of these software packages for any code or other materials that are included with this Shiny app.

## Copyright notice and free re-use of data

See Eurostat website for most recent version of this copyright notice: https://ec.europa.eu/eurostat/about-us/policies/copyright

### © European Union, 1995 - today

Eurostat has a policy of encouraging free re-use of its data, both for non-commercial and commercial purposes. All statistical data, metadata, content of web pages or other dissemination tools, official publications and other documents published on its website, with the exceptions listed below, can be reused without any payment or written licence provided that:

- the source is indicated as Eurostat;
- when re-use involves modifications to the data or text, this must be stated clearly to the end user of the information.

#### Exceptions

1. The permission granted above does not extend to any material whose copyright is identified as belonging to a third-party, such as photos or illustrations from copyright holders other than the European Union. In these circumstances, authorisation must be obtained from the relevant copyright holder(s).
2. Logos and trademarks are excluded from the above mentioned general permission, except if they are redistributed as an integral part of a Eurostat publication and if the publication is redistributed unchanged.
3. When reuse involves translations of publications or modifications to the data or text, this must be stated clearly to the end user of the information. A disclaimer regarding the non-responsibility of Eurostat shall be included.
4. The following Eurostat data and documents may not be reused for commercial purposes (but non-commercial reuse is possible without restriction):
    a. Data identified as belonging to sources other than Eurostat; all data published on Eurostat's website can be regarded as belonging to Eurostat for the purpose of their reuse, with the exceptions stated below, or if it is explicitly stated otherwise.
    b. Publications or documents where the copyright belongs partly or wholly to other organisations, for example concerning co-publications between Eurostat and other publishers.
    c. Data on countries other than
        - Member States of the European Union (EU), and
        - Member States of the European Free Trade Association (EFTA), and
        - official EU acceding and candidate countries.
        - Examples are data on the United States of America, Japan or China. Often, such data are included in Eurostat data tables. In such cases, a re-user would need to eliminate such data from the tables before reusing them commercially.
     d. Trade data originating from Liechtenstein and Switzerland (as declaring countries), from 1995 onwards, and concerning the following commodity classifications: HS, SITC, BEC, NSTR and national commodity classifications. Thus it is, for example, not allowed to sell export/import data declared by Switzerland (concerning the above named commodity classifications). However, it is allowed to sell Swiss export/import data declared by an EU Member State (but see below a similar exception for Austria).
     e. Trade data originating from Austria (as a declaring country) for a level of detail of the Combined Nomenclature of 8 digits; again, it is not allowed to sell export/import declared by Austria (concerning the above named commodity classifications), but it is allowed to sell Austrian export/import data declared by another EU Member State.
  
