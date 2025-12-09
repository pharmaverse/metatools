# Create Subgroups

Create Subgroups

## Usage

``` r
create_subgrps(ref_vec, grp_defs, grp_labs = NULL)
```

## Arguments

- ref_vec:

  Vector of numeric values

- grp_defs:

  Vector of strings with groupings defined. Format must be either: \<00,
  \>=00, 00-00, or 00-\<00

- grp_labs:

  Vector of strings with labels defined. The labels correspond to the
  associated `grp_defs`. i.e., "12-17" may translate to "12-17 years".
  If no `grp_labs` specified then `grp_defs` will be used.

## Value

Character vector of the values in the subgroups

## Examples

``` r
create_subgrps(c(1:10), c("<2", "2-5", ">5"))
#>  [1] "<2"  "2-5" "2-5" "2-5" "2-5" ">5"  ">5"  ">5"  ">5"  ">5" 
create_subgrps(c(1:10), c("<=2", ">2-5", ">5"))
#>  [1] "<=2"  "<=2"  ">2-5" ">2-5" ">2-5" ">5"   ">5"   ">5"   ">5"   ">5"  
create_subgrps(c(1:10), c("<2", "2-<5", ">=5"))
#>  [1] "<2"   "2-<5" "2-<5" "2-<5" ">=5"  ">=5"  ">=5"  ">=5"  ">=5"  ">=5" 
create_subgrps(c(1:10), c("<2", "2-<5", ">=5"), c("<2 years", "2-5 years", ">=5 years"))
#>  [1] "<2 years"  "2-5 years" "2-5 years" "2-5 years" ">=5 years" ">=5 years"
#>  [7] ">=5 years" ">=5 years" ">=5 years" ">=5 years"
```
