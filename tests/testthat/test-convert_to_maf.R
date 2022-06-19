test_that("vcf2maf conversion works ", {

  # Define expected names
  expected_names <- c(
    "Hugo_Symbol",
    "Chromosome",
    "Start_Position",
    "End_Position",
    "Reference_Allele",
    "Tumor_Seq_Allele2",
    "Variant_Classification",
    "Variant_Type",
    "Tumor_Sample_Barcode"
  )

  # Define path to vcf
  path_to_vcf = system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.annotated.vcf")

  # Ensure conversion code runs without errors
  expect_error(vcf_convert_to_maf(path_to_vcf = path_to_vcf, verbose = FALSE),NA)

  # Rerun conversion code but save result
  maf_df = vcf_convert_to_maf(path_to_vcf = path_to_vcf, verbose = FALSE)

  # Ensure data.frame was returned
  expect_s3_class(maf_df, "data.frame")

  # Ensure data.frame has all the names we expect in a MAF
  expect_true(expected_names %in% names(maf_df))
})
