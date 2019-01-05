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
generates a single frame and providing a list of keyvalues for each
variable used in the animation. The convention is to define a function
in your module called makeFrame with the following type:

```   
makeFrame :: Data.Map.Map [Char] [(Double, BV Double)]
  -> StdGen -> Double
  -> Codec.Picture.Image Codec.Picture.PixelRGBA8

makeFrame paramHash g t = ...
```
This function takes the following as arguments:

paramHash: The special map that defines the keyvalues of our variables.

g: A Haskell random number generator of type `StdGen`. This is provided as a convenience since many animation algoritms require a random element.

t: A Double value that gives us the time. All values in our animation
are ultimately a function of this index value.

It produces an abstract representation of a 4 channel color image of the RGBA type. This is a type defined in the Rasterific package as `Codec.Picture.Image Codec.Picture.PixelRGBA8`. This can be encoded as an actual image file by the `Codec.Picture.writePng` function, also from the Rasterific package.


Finally, there is also a special global map (called "paramHash" by our
convention) that defines the keyvalues for our animation:

```
paramHash :: Data.Map.Map [Char] [(Double, BV Double)]
```

Please note that this object has the same type as the first argument
in the `makeFrame` function. Providing the *specific* changing
parameters that may be needed to drive the animation is fully
automated by the overall framework. This is accomplished by a special
type called "BV" and associated functions that allows the same set of
functions to be used to interpolate values of various
types. Currently, values of type Double, Int, and Complex from the
Data.Complex package are supported.

## Adding a New Module

After creating your `makeFrame` function and `paramHash` map in your
new module you will want to make it part of the larger
program. Initially I had planned on creating a dynamic module loading
architecture similar to that used in countless C and C++
applications. I discovered that this is rather complicated and error
prone in Haskell and decided to put it on the shelf for the moment. As a result, new modules are simply added to the project by adding a case branch in the `runModule` function in `Main.hs`. Here's the code that hooks in the "SierpinskyDust" module for example.

```
case m of
  "sd" ->
        let mw = ModuleWorkers SD.paramHash SD.makeFrame
        in
          sequenceFrames mw g rng "sd"
```

## Summary

So to sum up, there are 3 steps to creating your own animation module.

1. Create a `makeFrame` function to draw a single frame using the
   parameters interpolated from the `paramHash` map.

2. Define your `paramHash` keyvalues.

3. Write your module into the `runModule` function in `Main.hs`.

## Todo

* `paramHash` should clearly be parsed in from a text file rather than
  compiled in as at present. I used the compiled in approach because I
  wanted to support several types without writing several parsers. My
  plan is to ultimately use the Parsec library to parse this data.

* Tests - we don't have any...


