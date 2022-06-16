# metatools 0.1.2
* correct bug with `combine_supp()` when the data and the supp have different classes for the IDVARVAL
* add warning to `combine_supp()` so it will report when not all the rows in the supp have merged
* add `floating_pt_correction` argument to `combine_supp()` used for when there are floating point errors with `IDVARVAL`


# metatools 0.1.1

* Based on tester feedback, remove any row in supplemental qualifiers that are empty and rearranged the columns

