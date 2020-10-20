# Visuale QS results

## Description
R Shiny app to plot output from the QS system.

Interactive Shiny interface to plot Ct values measured by the QS machine. User can import one or multiple files (xlsx format) and check multiple parameters (Ct / Cq Conf / Positive Control / Negative Control / MTP	/ Tm1). Final results from multiple files can be extracted from the program.

## Installation

Install the latest R version, the necessary libraries and fork repo from GitHub.

## Usage
Start terminal and set directory to forked directory. Launch the with the following command

`R -e "shiny::runApp('DIR_APP', port = 8888)"`

Go the your browser: http://127.0.0.1:8888

## Todo list:
- Add functionality that prompts errors instead of exititing the program
- Include help text in startup screen
- Take information from 

## License
QS_shiny app is under the APACHE-II license
