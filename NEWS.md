# metatools 0.1.5.9000
* Breaking change: `combine_supp()` requires that the QNAM columns are not in the source dataset (#64)
* `combine_supp()` now allows multiple `QNAM` values to go to the same `IDVAR` (#63)
* Allow supp data to be zero-row with `combine_supp()` (#45)
* Enhance `check_ct_data()` so that `na_acceptable` can now accept a vector of variables. Also add new argument `omit_vars` to control if any variables should be skipped when doing controlled terminology checks (#57).

# metatools 0.1.4
* correct bug with `combine_supp()` when the data and the supp have white space. Now it will be trimmed before attempting to merge  
* Updates made to work with the newest version of dplyr 

# metatools 0.1.3
* correct bug with `build_from_derived()` that prevented multiple from being combined 
* removed library calls from tests 
* remove `floating_pt_correction` from `combine_supp()` as it is best to never change the type of the main dataset 
* Add a message to `drop_unspec_vars` to explain which variables are dropped 
* Correct bug in `order_cols`, so it will still work when not all columns are present 

# metatools 0.1.2
* correct bug with `combine_supp()` when the data and the supp have different classes for the IDVARVAL
* add error to `combine_supp()` to report when not all the rows in the supp have merged
* add `floating_pt_correction` argument to `combine_supp()` used for when there are floating point errors with `IDVARVAL`


# metatools 0.1.1

* Based on tester feedback, remove any row in supplemental qualifiers that are empty and rearranged the columns

