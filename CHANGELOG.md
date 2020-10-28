# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
## [0.7.9] - 2020-10-28
### Changed
Changed date preprocessing steps to evaluate both American as well as European date formats.

## [0.7.8] - 2020-10-23
### Added
Added functionality to assess whether the "instrument ID" parameter is equal to "Not Started".

### Removed
Removed code that allowed for the removal of positive and negative controles before compiling the xlsx output.

## [0.7.7] - 2020-10-22
### Added
Added functionality to assess whether the "run end experiment date" parameter is equal to "Not Started". Set exp data to "Date error".

### Changed
Changed text of README.md

## [0.7.6] - 2020-10-20
### Added
Added line of code to change the names of the postive and negative controls in the old QS template to the correct ones

## [0.7.5] - 2020-10-20
### Added
Some files were not preprocessed because the experiment name in the file did not correspond to the file name. This issue was resolved by extracting the experiment name from the file name, removing the four last characters (".txt")

### Changed
Changed the column order of the files data frame
American dates were not correctly processed. Changed the preprocess date functionality

### Removed
Removed the analytical validation code
Removed all Excel file input functionality
Removed the test that checked whether the 'Results sheet' was available in the xlsx input
Removed the buttons functionality of the output excel table

## [0.7.4] - 2020-10-20
### Changed
Fixed the trend tab
Set the gene as input to draw line

### Removed
Removed two tabs
- *Plot sample x lot*
- *Plot probability x sample*

## [0.7.3] - 2020-10-10
### Added
Added functionality to import txt files

## [0.7.2] - 2020-06-09

### Changed

Edited Sample ID order to keep order from the input
Edited US time formatting
Removed the table row number from the output

## [0.7.1] - 2020-04-02

### Added
Added changelog file

### Changed
Sort date column

New table is compiled each time new data is imported

## Older versions

* 0.7.0:
  * Edit date column from output
  * Process new QS_prob file
* 0.6.0: 
  * Create GitHub repository (QS_shiny)
  * Change "cT" --> "Ct"
  * Adapt colour scheme: SL colors are used when number of targets == 10, if not, then standard colors
  * Add and edit generic file names
  * Move functionality to individual scripts
  * Add licensing
  * Integrate two additional tabs
    * Sample x lot x ct
    * Sample x probability
* 0.5.0: QS analysis with top bar menu + interactive UI
  * Resolved remaining questions
* 0.4.1: QS analysis with top bar menu + interactive UI
  * Reduced the number of packages imported
* 0.4.0: 
  * Top bar menu + interactive UI, no QS input
* 0.1.0 --> 0.4.0: test
