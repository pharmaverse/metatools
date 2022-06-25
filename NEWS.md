# metatools 0.1.3
* correct bug with `build_from_derived()` that prevented multiple from being combined 
* removed library calls from tests 
* remove `floating_pt_correction` from `combine_supp()` as it is best to never change the type of the main dataset 

# metatools 0.1.2
* correct bug with `combine_supp()` when the data and the supp have different classes for the IDVARVAL
* add error to `combine_supp()` to report when not all the rows in the supp have merged
* add `floating_pt_correction` argument to `combine_supp()` used for when there are floating point errors with `IDVARVAL`


# metatools 0.1.1

* Based on tester feedback, remove any row in supplemental qualifiers that are empty and rearranged the columns

