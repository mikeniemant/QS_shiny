# Visuale QS results

## Description
R Shiny app to plot output from the QS system.

Interactive Shiny interface to plot Ct values measured by the QS machine. User can import one or multiple files (xlsx format) and check multiple parameters (Ct / Cq Conf / Positive Control / Negative Control / MTP	/ Tm1). Final results from multiple files can be extracted from the program.

## Installation

Install the latest R version, the necessary libraries and fork repo from GitHub.

1. Install R: https://cran.r-project.org

2. Start R console and install the necessary libraries by running the following commands

`install.packages("tidyverse")`
`install.packages("shiny")`
`install.packages("shinydashboard")`
`install.packages("shinyFiles")`
`install.packages("plotly")`
`install.packages("DT")`
`install.packages("xlsx")`

3. Fork repository form GitHub

## Usage
Start terminal and set directory to forked directory. 

### MacOS

- Launch the app in terminal with the following command: `R -e "shiny::runApp('DIR_APP', port = 8888)"`
- Open the following link in your browser: http://127.0.0.1:8888

### Windows

Multiple options to start the Shiny app in Windows. Before we start, look for the directory of the Shiny app. Should look something like this `C:/Shiny/QS_shiny-master/app.R`. 

- Start the R console in either:
  - `C:\"Program Files"\R\R-[VERSION]\bin\Rscript` OR
  - RStudio
- Type the following command in the console `shiny::runApp([insert the Shiny app dir here], port = 8888)`

However, we can automate the above tasks by creating a CMD file. For this, we need the following directories

- Shiny App directory. should look something like this `C:/Shiny/QS_shiny-master/app.R`
- Rscript directory, should look something like this: `C:\"Program Files"\R\R-3.6.1\bin\Rscript`

No we have to paste these two directories together:
  
  - C:\"Program Files"\R\R-3.6.1\bin\Rscript -e "shiny::runApp('C:/Shiny/QS_shiny-master/app.R', port = 1111, launch.browser =  T)"

## To do:
- U.S. dates are converted to CEST time zone, instead of time conversion
  - Example: 2020-06-03 15:31:21 PM PDT --> 03-06-2020 15:31
  - Example: 2020-06-03 15:31:21 PM PST --> 03-06-2020 15:31
- Change number of decimals in output xlsx file
  - Example: 30.12 --> 30.120
- Import files from stored in multiple directories
  - Add a radio button: "add files to import"
- The app is only able to work with the study-specific targets. Other target combinations cannot be exported as xlsx.
- Implement dark-mode: https://shiny.rstudio.com/app-stories/weather-lookup-bslib.html

## Known bugs
- Looking at trending from data that originates from the US and EU  may not be valid as the time-zones are not taken into account.

### Windows
When using R for the first time, the LC_CTYPE global parameter may either not be defined or is set on another language. To resolve this problem we have to change this global parameter to "English_United States.1250"`. In R, we can do this with the following commands:

`user_renviron = path.expand(file.path("~", ".Renviron"))`
`file.edit(user_renviron)`

Paste the following lines in the script that just opened:
  
`LC_COLLATE  = "English_United States.1250"`
`LC_CTYPE    = "English_United States.1250"`
`LC_MONETARY = "English_United States.1250"`
`LC_NUMERIC  = "English_United States.1250"`
`LC_TIME     = "English_United States.1250"`

Save file and restart R / RStudio

The next time we start R, R knows what language to use for understanding sequences of bytes of text data characters.

## License
QS_shiny app is under the APACHE-II license (2021).
