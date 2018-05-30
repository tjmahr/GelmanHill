# Gelman and Hill (2006)

Examples and data for the classic textbook [Gelman and Hill (2006)][book-link].

## Notebooks

A notebook file contains the code, output and formatted comments for an R
script. As I work through the examples, I save notebooks.

- Chapter 7
  - [07-01 Simulating Probability Models](examples/Ch07/07-01_SimulationOfProbabilityModels.md)
  - [07-02 Summarizing Linear Regression Using Simulation](examples/Ch07/07-02_SummarizingLinearRegressionUsingSimulation.md)
  - [07-03 Simulation for Nonlinear Predictions](examples/Ch07/07-03_SimulationForNonLinearPredictions.md)


## BUGS vs. Stan

I created this repository so that I could work through the book's examples 
as an RStudio project. Things have become slightly more complicated:
The BUGS code in the book is effectively unsupported (in favor of Stan).

I originally downloaded the materials from [Gelman's page][arm-page] and 
stored them in this repository. These files used BUGS for model fitting, 
so they represent the canonical computing materials for the first 
edition of the book (2006). 

Gelman's [instructions page for BUGS][bugsR] is now titled "Use Stan 
instead", so I'll be using [Stan](http://mc-stan.org/)/RStan instead. 
Fortunately, Stan materials for the book were 
[available][examples-commit], and I've replaced the BUGS examples with 
Stan examples. These Stan examples are from circa 2015.

I'm taking the liberty to modify examples and rename files as I work 
through them. I've tagged the commits with unmodified versions of the 
[original BUGS examples][pure-bugs] and [original Stan 
examples][pure-stan] so that those are readily available. 


[book-link]: http://amzn.to/1Mjudi0
[arm-page]: http://www.stat.columbia.edu/~gelman/arm/software/
[bugsR]: http://www.stat.columbia.edu/~gelman/bugsR/
[examples-commit]: https://github.com/stan-dev/example-models/tree/57f9cbcb0d6355e663679f1088adb21261da73bf
[pure-bugs]: https://github.com/tjmahr/GelmanHill/releases/tag/v0.0.1
[pure-stan]:https://github.com/tjmahr/GelmanHill/releases/tag/v.0.1.0
