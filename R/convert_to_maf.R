vcf_guess_sample_name_from_filepath <- function(filepath){
  sample_name = sub(pattern = "\\..*$", replacement = "", x = basename(filepath))
  return(sample_name)
}

#testfile=system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.annotated.vcf")
vcf_read <- function(path_to_vcf){
  VariantAnnotation::readVcf(path_to_vcf)
}

vcf_assert_collapsedvcf <- function(){
  assertthat::assert_that(class(collapsed_vcf) == "CollapsedVCF", msg = "Supplied VCF ")
}

vcf_assert_vep_annotated <- function(collapsed_vcf, verbose = TRUE){
  vcf_assert_collapsedvcf()
  assertthat::assert_that(vcf_is_vep_annotated(collapsed_vcf), msg = "\u2718 Could not find VEP annotation (CSQ) in info field. Please ensure VCF is VEP annotated ")
  if(verbose) message("\u2714 VCF is VEP annotated ")
}

vcf_is_vep_annotated <-function(collapsed_vcf){
  csq_info_field_present = "CSQ" %in% names(collapsed_vcf@info)
  return(csq_info_field_present)
}

vcf_is_multisample <- function(collapsed_vcf){
  return(vcf_count_samples(collapsed_vcf) > 0)
}

vcf_count_samples <- function(collapsed_vcf){
  samplenames = VariantAnnotation::samples(VariantAnnotation::header(collapsed_vcf))
  return(length(samplenames))
}

vcf_is_multiallelic <- function(collapsed_vcf){
  length(VariantAnnotation::alt(collapsed_vcf)) < length(unlist(VariantAnnotation::alt(collapsed_vcf)))
}

vcf_describe <- function(collapsed_vcf){
  data.frame(
    multisample = vcf_is_multisample(collapsed_vcf),
    #samples = vcf_count_samples(collapsed_vcf),
    vep_annotated = vcf_is_vep_annotated(collapsed_vcf),
    multiallelic = vcf_is_multiallelic(collapsed_vcf)
  )
}

single_sample_vcf_to_maf <- function(collapsed_vcf, sample_id){
  #browser()
  maf_df <- data.frame(
    Tumor_Sample_Barcode = sample_id,
    #Hugo_Symbol = vcf_get_hugo_symbols(collapsed_vcf),
    Chromosome = as.character(collapsed_vcf@rowRanges@seqnames),
    Start_Position = collapsed_vcf@rowRanges@ranges@start,
    End_Position = as.data.frame(collapsed_vcf@rowRanges@ranges)[["end"]],
    Reference_Allele = sapply(VariantAnnotation::ref(collapsed_vcf), FUN = function(.x) paste0(.x, collapse = ",")),
    Tumor_Seq_Allele2 = sapply(VariantAnnotation::alt(collapsed_vcf), FUN = function(.x) paste0(.x, collapse = ","))
    #Variant_Classification = vcf_get_variant_classification(),
    #Variant_Type = vcf_get_variant_type(),
    #AAchange = vcf_get_amino_acid_change(),
    #VAF = vcf_get_variant_allele_frequency()
  )
  return(maf_df)
}

#' VCF2MAF
#'
#' Convert a vcf (or vcfs) to a MAF file
#'
#' @param path_to_vcf
#'
#' @return MAF dataframe
#' @export
#'
vcf_convert_to_maf <- function(path_to_vcf, sampleid = NULL, verbose = TRUE){
  #browser()
  collapsed_vcf = vcf_read(path_to_vcf)
  #vcf_df = vcf_read(path_to_vcf)
  vcf_is_multisample = vcf_is_multisample(collapsed_vcf)

  if(!vcf_is_multisample){
    if(is.null(sampleid)){
      sampleid = vcf_guess_sample_name_from_filepath(path_to_vcf)
      if(verbose) message("Guessing Samplename from filepath: ", sampleid)
    }
    maf = single_sample_vcf_to_maf(collapsed_vcf, sampleid)
    return(maf)
  }
}
#vcf_path = system.file("testfiles/mysample.singlesample.vcf", package = "utilitybeltmaf")
# vcf_convert_to_maf(system.file("testfiles/mysample.singlesample.vcf", package = "utilitybeltmaf"))

