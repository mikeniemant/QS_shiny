# Visuale QS results

## Premise
Build R Shiny app to plot output from the QS system.

Interactive Shiny interface to plot Ct values measured by the QS machine. User can import one or multiple files (xlsx format) and check multiple parameters (Ct / Cq Conf / Positive Control / Negative Control / MTP	/ Tm1). Final results from multiple files can be extracted from the program.

-------------------------------------------------------------------------------

## Version

* 0.1.0 --> 0.4.0: test
* 0.4.0: 
  * Top bar menu + interactive UI, no QS input
* 0.4.1: QS analysis with top bar menu + interactive UI
  * Reduced the number of packages imported
* 0.5.0: QS analysis with top bar menu + interactive UI
  * Resolved remaining questions
* 0.6.0: 
  * Create GitHub repository (QS_shiny)
  * Change "cT" --> "Ct"
  * Adapt colour scheme: Skyline colors are used when number of targets == 10, if not, then standard colors
  * Add and edit generic file names
  * Move functionality to individual scripts
  * Add licensing
  * Integrate two additional tabs
    * Sample x lot x ct
    * Sample x probability
* 0.7.0:
  * Edit date column from output
  * Process new QS_prob file
