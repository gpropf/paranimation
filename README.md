# Paranimation: Parametric Animations


## Description:

This package defines a set of data types and functions designed to make
it easier to create parameter driven animations. A good example of a
parameter driven animation would one of those Mandelbrot set zoom
movies that you can find all over the place now. Basically, what's
happening is that the parameters defining the window within the set
are changing slowly with the set being re-rendered each time.

I wanted to abstract the process of generating the images for this
type of movie out from the process of defining keyvalues and
interpolating between them. Essentially I wanted to be able to define
arbitray keyvalues for an arbitrary number of parameters of varied
types that would ultimately depend on a single driving parameter
(essentially time or frame number if you like). There would also an
automated process for interpolating along the chain of keyvalues with
the possibility of defining varied interpolator functions as well
though initially I would provide only a linear interpolator function.

We provide a command line tool (currently called 'paranimate-exe')
that offers a fairly sophisticated set of command line features
powered by the optparse-applicative command-line parser
library. Running the program without options will show a help message
that details what the various options do.

## Modules

The package is designed to be modular so that one can create new
animations relatively easily by just filling in the algorithm that
generates a single frame. Providing the changing parameters that may
be needed to drive it is fully automated by the overall framework.


