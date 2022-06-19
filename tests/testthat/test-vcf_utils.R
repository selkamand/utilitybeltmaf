test_that("vcf_read works", {
  path_to_vcf = system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.annotated.vcf")
  expect_error(vcf_read(path_to_vcf), regexp = NA)
})

## Test removed to ensure tests are appropriate irrespective of packages used for implementation
# test_that("vcf_read returns appropriate class", {
#   path_to_vcf = system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.annotated.vcf")
#   expect_s4_class(vcf_read(path_to_vcf), "CollapsedVCF")
# })

test_that("check_vcf_is_multisample works for single sample VCFs", {
  path_to_single_sample_vcf = system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.annotated.vcf")
  vcf_single_sample = vcf_read(path_to_single_sample_vcf)
  expect_false(vcf_is_multisample(vcf_single_sample))
})

test_that("check_vcf_is_multisample works for multi-sample VCFs", {
  path_to_multisample_vcf = system.file(package="utilitybeltmaf","inst/testfiles/1000genomes.example.multisample.vcf")
  vcf_multi_sample = vcf_read(path_to_multisample_vcf)
  expect_true(vcf_is_multisample(vcf_multi_sample))
})


# Vep Annotation ----------------------------------------------------------
test_that("vcf_is_vep_annotated works for annotated VCFs", {
  path_to_annotated_vcf = system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.annotated.vcf")
  vcf = vcf_read(path_to_annotated_vcf)
  expect_true(vcf_is_vep_annotated(vcf))
})

test_that("vcf_is_vep_annotated works for non-annotated VCFs", {
  path_to_vcf = system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.vcf")
  vcf = vcf_read(path_to_vcf)
  expect_false(vcf_is_vep_annotated(vcf))
})

test_that("vcf_assert_vep_annotated throws no error on annotated vcfs", {
  path_to_vcf = system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.annotated.vcf")
  vcf = vcf_read(path_to_vcf)
  expect_error(vcf_assert_vep_annotated(vcf, verbose = FALSE), NA)
})

test_that("vcf_assert_vep_annotated throws error on non-annotated vcfs", {
  path_to_vcf = system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.vcf")
  vcf = vcf_read(path_to_vcf)
  expect_error(vcf_assert_vep_annotated(vcf, verbose = FALSE))
})



# VCF describe ------------------------------------------------------------


test_that("vcf_describe runs successfully", {
  path_to_vcf = system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.annotated.vcf")
  vcf = vcf_read(path_to_vcf)
  expect_error(vcf_describe(vcf), NA)
})


# Guessing Sample Name ----------------------------------------------------

test_that("vcf_guess_sample_name_from_filepath works", {
  sample_name_to_path_mappings <- c(
    "mysample" = "/root/mysample.singlesample.annotated.vcf",
    "mysample" = "mysample.singlesample.annotated.vcf",
    "my_sample" = "my_sample.vep.vcf",
    "my-sample" = "my-sample.vep.vcf"
  )

  expect_equal(
    object = vcf_guess_sample_name_from_filepath(filepath = sample_name_to_path_mappings),
    expected = names(sample_name_to_path_mappings)
  )
})


# Biallelic VS multiallelic -----------------------------------------------
test_that("vcf_is_biallelic works with biallelic vcfs", {
  path_to_vcf = system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.vcf")
  vcf = vcf_read(path_to_vcf)
  expect_true(vcf_is_biallelic(vcf))
})

test_that("vcf_is_biallelic works with non-biallelic vcfs (more than 2 alt alleles for one mutation)", {
  path_to_vcf = system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.multialt.vcf")
  vcf = vcf_read(path_to_vcf)
  expect_false(vcf_is_biallelic(vcf))
})

