vcf_guess_sample_name_from_filepath <- function(filepath){
  sample_name = sub(pattern = "\\..*$", replacement = "", x = basename(filepath))
  return(sample_name)
}

#testfile=system.file(package="utilitybeltmaf","inst/testfiles/mysample.singlesample.annotated.vcf")
vcf_read <- function(path_to_vcf){
  VariantAnnotation::readVcf(path_to_vcf)
}

vcf_assert_collapsedvcf <- function(collapsed_vcf){
  assertthat::assert_that(class(collapsed_vcf) == "CollapsedVCF", msg = "Supplied VCF ")
}

vcf_assert_vep_annotated <- function(collapsed_vcf, verbose = TRUE){
  vcf_assert_collapsedvcf(collapsed_vcf)
  assertthat::assert_that(vcf_is_vep_annotated(collapsed_vcf), msg = "\u2718 Could not find VEP annotation (CSQ) in info field. Please ensure VCF is VEP annotated ")
  if(verbose) message("\u2714 VCF is VEP annotated ")
}

vcf_assert_biallelic <- function(collapsed_vcf){
  assertthat::assert_that(vcf_is_biallelic(collapsed_vcf), msg = "\u2718 VCF is not biallelic. It has multiple alternative alleles for at least one mutation. Please normalize VCF then retry")
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

#' Is VCF Biallelic?
#'
#' A biallelic VCF has exactly 2 alleles represented per line in the VCF. (1) Ref allele. (2) Alternative allele.
#'
#'
#' Not all VCFs are bi-allelic. Some have multiple alt alleles at one position. For example, sometimes if the VCF is multi-sample 2 alt alleles might be given to two possible alleles present in different samples.
#' In these multi-allelic VCFS, the genotype field then usually describes what allele is present in each sample.
#' In virtually all cases, multi-allelic VCFs can be simplified to biallelic VCFs through 'normalisation' (e.g. using bcftools norm) without losing information.
#'
#'
#' @param collapsed_vcf result of [vcf_read()]
#'
#' @return TRUE if each entry in VCF has no more than 1 Alt allele. otherwise returns FALSE (boolean)
#' @export
#'
vcf_is_biallelic <- function(collapsed_vcf){
  length(VariantAnnotation::alt(collapsed_vcf)) == length(unlist(VariantAnnotation::alt(collapsed_vcf)))
}

vcf_count_variants <- function(collapsed_vcf){
  length(VariantAnnotation::ref(collapsed_vcf))
}

vcf_describe <- function(collapsed_vcf){
  data.frame(
    multisample = vcf_is_multisample(collapsed_vcf),
    #samples = vcf_count_samples(collapsed_vcf),
    vep_annotated = vcf_is_vep_annotated(collapsed_vcf),
    biallelic = vcf_is_biallelic(collapsed_vcf),
    n_variants = vcf_count_variants(collapsed_vcf)
  )
}

single_sample_vcf_to_maf <- function(collapsed_vcf, sample_id){
  #browser()
  maf_df <- data.frame(
    Tumor_Sample_Barcode = sample_id,
    Hugo_Symbol = vcf_parse_vep_annotation(collapsed_vcf = collapsed_vcf,field_to_extract =  "SYMBOL"),
    Chromosome = as.character(collapsed_vcf@rowRanges@seqnames),
    Start_Position = collapsed_vcf@rowRanges@ranges@start,
    End_Position = as.data.frame(collapsed_vcf@rowRanges@ranges)[["end"]],
    Reference_Allele = sapply(VariantAnnotation::ref(collapsed_vcf), FUN = function(.x) paste0(.x, collapse = ",")),
    Tumor_Seq_Allele2 = sapply(VariantAnnotation::alt(collapsed_vcf), FUN = function(.x) paste0(.x, collapse = ","))
    #AAchange = vcf_parse_vep_annotation(collapsed_vcf, "Amino_acids")
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

