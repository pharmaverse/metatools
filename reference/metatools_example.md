# Get path to pkg example

pkg comes bundled with a number of sample files in its `inst/extdata`
directory. This function make them easy to access

## Usage

``` r
metatools_example(file = NULL)
```

## Arguments

- file:

  Name of file. If `NULL`, the example files will be listed.

## Examples

``` r
metatools_example()
#> [1] "adsl.xpt" "dm.xpt"  
metatools_example("dm.xpt")
#> [1] "/home/runner/work/_temp/Library/metatools/extdata/dm.xpt"
```
