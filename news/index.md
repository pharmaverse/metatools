# Changelog

## metatools 0.2.0

CRAN release: 2025-07-16

- Functions now require a subsetted metacore object to be used i.e.,
  created via
  [`metacore::select_dataset()`](https://atorus-research.github.io/metacore/reference/select_dataset.html).
- [`create_cat_var()`](https://pharmaverse.github.io/metatools/reference/create_cat_var.md)
  now allows the new variable to be create using the `decode` part of
  the codelist as a
  label.([\#82](https://github.com/pharmaverse/metatools/issues/82))
- [`create_var_from_codelist()`](https://pharmaverse.github.io/metatools/reference/create_var_from_codelist.md)
  now allows the user to specify a codelist to use to create the new
  variables.
  ([\#83](https://github.com/pharmaverse/metatools/issues/83))
- [`check_variables()`](https://pharmaverse.github.io/metatools/reference/check_variables.md)
  now provides a `strict` option that issues warnings rather than errors
  when `FALSE`.
  ([\#86](https://github.com/pharmaverse/metatools/issues/86))
- [`combine_supp()`](https://pharmaverse.github.io/metatools/reference/combine_supp.md)
  now adds labels from the “QLABEL”
  ([\#71](https://github.com/pharmaverse/metatools/issues/71))
- [`combine_supp()`](https://pharmaverse.github.io/metatools/reference/combine_supp.md)
  no longer creates an “IDVARVAL” column
  ([\#78](https://github.com/pharmaverse/metatools/issues/78))
- Improved error reporting for `create_cat_var`,
  `create_var_from_codelist`, and `check_variables`.
- The `dataset_name` argument has been deprecated in various functions.
- The `make_lone_dataset` function has been deprecated.

## metatools 0.1.6

CRAN release: 2024-07-23

- Breaking change:
  [`combine_supp()`](https://pharmaverse.github.io/metatools/reference/combine_supp.md)
  requires that the QNAM columns are not in the source dataset
  ([\#64](https://github.com/pharmaverse/metatools/issues/64))
- Breaking change:
  [`build_from_derived()`](https://pharmaverse.github.io/metatools/reference/build_from_derived.md)
  no longer requires all datasets with predecessors, but can run on a
  single dataset or multiple. It will also now accept unnamed datasets.
- [`combine_supp()`](https://pharmaverse.github.io/metatools/reference/combine_supp.md)
  now allows multiple `QNAM` values to go to the same `IDVAR`
  ([\#63](https://github.com/pharmaverse/metatools/issues/63))
- Allow supp data to be zero-row with
  [`combine_supp()`](https://pharmaverse.github.io/metatools/reference/combine_supp.md)
  ([\#45](https://github.com/pharmaverse/metatools/issues/45))
- Enhance
  [`check_ct_data()`](https://pharmaverse.github.io/metatools/reference/check_ct_data.md)
  so that `na_acceptable` can now accept a vector of variables. Also add
  new argument `omit_vars` to control if any variables should be skipped
  when doing controlled terminology checks
  ([\#57](https://github.com/pharmaverse/metatools/issues/57))
- Add
  [`check_unique_keys()`](https://pharmaverse.github.io/metatools/reference/check_unique_keys.md)
  to check the uniqueness of records in the dataset by variable keys
  ([\#52](https://github.com/pharmaverse/metatools/issues/52))
- Correct the description of
  [`build_from_derived()`](https://pharmaverse.github.io/metatools/reference/build_from_derived.md)
  predecessor_only parameter

## metatools 0.1.4

CRAN release: 2023-02-13

- correct bug with
  [`combine_supp()`](https://pharmaverse.github.io/metatools/reference/combine_supp.md)
  when the data and the supp have white space. Now it will be trimmed
  before attempting to merge  
- Updates made to work with the newest version of dplyr

## metatools 0.1.3

CRAN release: 2022-10-06

- correct bug with
  [`build_from_derived()`](https://pharmaverse.github.io/metatools/reference/build_from_derived.md)
  that prevented multiple from being combined
- removed library calls from tests
- remove `floating_pt_correction` from
  [`combine_supp()`](https://pharmaverse.github.io/metatools/reference/combine_supp.md)
  as it is best to never change the type of the main dataset
- Add a message to `drop_unspec_vars` to explain which variables are
  dropped
- Correct bug in `order_cols`, so it will still work when not all
  columns are present

## metatools 0.1.2

- correct bug with
  [`combine_supp()`](https://pharmaverse.github.io/metatools/reference/combine_supp.md)
  when the data and the supp have different classes for the IDVARVAL
- add error to
  [`combine_supp()`](https://pharmaverse.github.io/metatools/reference/combine_supp.md)
  to report when not all the rows in the supp have merged
- add `floating_pt_correction` argument to
  [`combine_supp()`](https://pharmaverse.github.io/metatools/reference/combine_supp.md)
  used for when there are floating point errors with `IDVARVAL`

## metatools 0.1.1

CRAN release: 2022-04-20

- Based on tester feedback, remove any row in supplemental qualifiers
  that are empty and rearranged the columns
