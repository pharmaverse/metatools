# metatools 0.1.2
* correct bug with `combine_supp()` when the data and the supp have different classes for the IDVARVAL
* Raise an error if not all supplemental rows are merged into the data with `combine_supp()` (#31)
* Protect against floating point issues when merging with `combine_supp()` (#33)

# metatools 0.1.1

* Based on tester feedback, remove any row in supplemental qualifiers that are empty and rearranged the columns

