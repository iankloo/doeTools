# doeTools

Tools for teaching design of experiments (DOE).

So far, these functions focus on fully replicated designs with a single replicate.  More to come on that...

## Install

This package is in development.  To install:

```
library(devtools)
install_github('iankloo/doeTools')
```

The only dependencies are `data.table` and `stats`.

Here's a quick rundown of the functions:

## Making designs

The first tool for making designs is `makeTreatments()`.  This function takes a vector of factors and creates treatments in the standard order:

```
makeTreatments(factors = c('A','B','C','D'))
```

You can also generate an entire design matrix with `designMatrix()`:

```
designMatrix(factors = c('A','B','C','D'))
```

If you want to only include main effects:

```
designMatrix(factors = c('A','B','C','D'), mainOnly = TRUE)
```

## Analyzing designs

This is a work in-progress...but you can do a couple of things:

First, you can estimate the effects of your design using `estEffects()`

```
#create a design matrix
z <- designMatrix(c('A','B','C','D'), standardOrder = TRUE)

#add observations
z <- cbind(z, obs = c(550,669,604,650,633,642,601,635,1037,749,1052,868,1075,860, 1063, 729))

#estimate effects
estEffects(dm = z, obsCol = 'obs')
```

This is useful for determining which effects are worth looking into.

Next, you can run ANOVA using either centerpoints or some of your interactions to estimate error with `analyzeDesign()`:

```
##Analysis using high-order interactions to estimate error:
#create a design matrix with observations
z <- designMatrix(c('A','B','C','D'), standardOrder = TRUE)
z <- cbind(z, obs = c(550,669,604,650,633,642,601,635,1037,749,1052,868,1075,860, 1063, 729))

#analyze the design using 3 and 4-factor interactions to estimate error
analyzeDesign(z, errorEst = c('ABC','ABD','ACD','BCD','ABCD'))

##Analysis using centerpoints to estimate error:
#create a design matrix with observations
z <- designMatrix(c('A','B'), standardOrder = TRUE)
z <- cbind(z, obs = c(39.3,40.9,40,41.5))

#analyze the design using centerpoints to estimate error
analyzeDesign(z, errorEst = c('ABC','ABD','ACD','BCD','ABCD'))
```

## New developments

Too much to write out...lots of stuff would be helpful to include in this package.





