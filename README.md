# `folderfun`: Manage paths with R folder functions 
[![Build Status](https://travis-ci.org/databio/folderfun.svg?branch=master)](https://travis-ci.org/databio/folderfun)

If you find yourself working on multiple different projects in R, you'll want to sort your input data and output results into folders containing raw data, processed data, plot results, intermediate table outputs, etc. This package makes it easier to do that by providing a quick and easy way to create and use functions for any directories, so you don't have to deal with annoying hard-coded absolute paths.

Go from this:

```{r}
d = read.table("/long/and/annoying/path/to/hard/coded/file/data.txt")
```

to this:

```{r}
d = read.table(ffIn("data.txt"))
```

## Installing

```{r}
devtools::install_github("databio/folderfun")
```

## Quick start

See the vignettes for more information: http://code.databio.org/folderfun
