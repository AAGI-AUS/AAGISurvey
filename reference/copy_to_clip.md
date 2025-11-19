# Copy an R Object's Text to the OS's Clipboard

Copy an R Object's Text to the OS's Clipboard

## Usage

``` r
copy_to_clip(x)
```

## Arguments

- x:

  An object to be written to the system clipboard.

## Value

A URL copied (invisibly) to the OS's clipboard.

## Examples

``` r
copy_to_clip(x = "https://github.com/AAGI-AUS")
#> Error in copy_to_clip(x = "https://github.com/AAGI-AUS"): could not find function "copy_to_clip"
```
