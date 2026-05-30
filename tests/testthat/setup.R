# Define specifications to be used in tests
p21_spec <- metacore::spec_to_metacore(metacore::metacore_example("p21_mock.xlsx"), verbose = "silent")
suppae_spec <- metacore::select_dataset(p21_spec, "SUPPAE", verbose = "silent")

vlm_spec <- metacore::spec_to_metacore("vlm_test_spec.xlsx", where_sep_sheet = FALSE, verbose = "silent")
adex_spec <- metacore::select_dataset(vlm_spec, "ADEX", verbose = "silent")

adex <- haven::read_xpt("../../inst/extdata/adex.xpt")


# # TODO: delete later
# vlm_spec <- metacore::spec_to_metacore("tests/testthat/vlm_test_spec.xlsx", where_sep_sheet = FALSE, verbose = "silent")
# adex_spec <- metacore::select_dataset(vlm_spec, "ADEX", verbose = "silent")
#
# adex <- haven::read_xpt("inst/extdata/adex.xpt")
