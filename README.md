# Eurostat statistics browser

## 1. Clone the Repository or Download from GitHub Web GUI

### Clone the Repository
To clone the repository to your local machine, use the following command in your terminal:

```bash
git clone https://github.com/pitkant/stats_shiny.git
```

The repository will be cloned to the directory which you have open on your terminal. Change the directory if needed.

### Download from GitHub Web GUI

If you prefer not to use Git or want to download the contents directly, you can do so by visiting the GitHub repository's web page. Click the green "Code" button on the upper right corner of the file view and select "Download ZIP" from the dropdown menu.

## 2. Install RStudio

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
4. In the RStudio console, run the following command to install necessary packages (if not already installed):

```
install.packages(c("shiny", "bslib", "bsicons", "eurostat", "DT", "dataset", "rdflib", "csvwr", "jsonlite", "jsonld"))
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
