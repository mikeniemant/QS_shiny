# Skyline Dx project - dashboard for visualising QuantStudio results

## Premise
Build R Shiny app to plot output from the QuantStudio system.

Interactive Shiny interface to plot Ct values measured by the QuantStudio (QS) machine. User can import one or multiple files (xlsx format) and check multiple parameters (Ct / Cq Conf / Positive Control / Negative Control / MTP	/ Tm1). Final results from multiple files can be extracted from the program.

Remaining questions:
* [ ] Rename the column names of the preprocessed data object
* [ ] Check code one last time
* [ ] Any more validation tests?

End phase

* [ ] Documentation --> in progress
* [ ] Compare probabilities for testing @LB
* [ ] Add the validation steps
  * r.df <- r.df %>% mutate_if(is.numeric, function(x) round(x+100*.Machine$double.eps, 3))
  * Possible to include GitHub continuous integration (CI) server functionality
  * However, only two operations need to be tested
    * File valid to be imported
    * Information valid for pre-processing
    * Number of unique targets -->

-------------------------------------------------------------------------------

##Version

* 0.1.0: test
* 0.2.0: test
* 0.3.0: test
* 0.3.1: test
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
    * Sample x lot
    * Sample x probability
