# Visuale QS results

## Description
R Shiny app to plot output from the QS system.

Interactive Shiny interface to plot Ct values measured by the QS machine. User can import one or multiple files (xlsx format) and check multiple parameters (Ct / Cq Conf / Positive Control / Negative Control / MTP	/ Tm1). Final results from multiple files can be extracted from the program.

## Installation

Install the latest R version, the necessary libraries and fork repo from GitHub.

## Usage
Start terminal and set directory to forked directory. Launch the app with the following command:

`R -e "shiny::runApp('DIR_APP', port = 8888)"`

Open the following link in your browser: http://127.0.0.1:8888

## To do:
- U.S. dates are converted to CEST time zone, instead of time conversion
  - Example: 2020-06-03 15:31:21 PM PDT --> 03-06-2020 15:31
- Change number of decimals in output xlsx file
  - Example: 30.12 --> 30.120
- Import files from stored in multiple directories
  - Add a radio button: "add files to import"
- The app is only able to work with the study-specific targets. Other target combinations cannot be exported as xlsx.
- Implement dark-mode: https://shiny.rstudio.com/app-stories/weather-lookup-bslib.html

## License
QS_shiny app is under the APACHE-II license.
