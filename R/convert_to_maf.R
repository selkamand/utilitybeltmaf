#' VCF2MAF
#'
#' Convert a vcf (or vcfs) to a MAF file
#'
#' @param path_to_vcf
#'
#' @return MAF dataframe
#' @export
#'
#' @examples
#'
convert_vcf_to_maf <- function(path_to_vcf, sampleid = NULL, verbose = TRUE){
  vcf_df = vcf_read(path_to_vcf)
  vcf_is_multisample = check_vcf_is_multisample(vcf_df)

  if(!vcf_is_multisample){
    if(is.null(sampleid)){
      sampleid = vcf_guess_sample_name_from_filepath(path_to_vcf)
      if(verbose) message("Guessing Samplename from filepath: ", sampleid)
    }
    maf = single_sample_vcf_to_maf(vcf_df, sampleid)
    return(maf)
  }
}

check_if_vep_annotated <- function(){

}

vcf_get_hugo_symbols <- function(){
  return("")
}
vcf_get_variant_classification <- function(){
  return("")
}

#Type of mutation. TNP (tri-nucleotide polymorphism) is analogous to DNP (di-nucleotide polymorphism) but for three consecutive nucleotides. ONP (oligo-nucleotide polymorphism) is analogous to TNP but for consecutive runs of four or more (SNP, DNP, TNP, ONP, INS, DEL, or Consolidated)
vcf_get_variant_type <- function(){
  return("")
}

vcf_get_amino_acid_change <- function(){
  return("")
}

vcf_get_variant_allele_frequency <- function(){
  return("")
}

single_sample_vcf_to_maf <- function(vcf_df, sample_id){
  maf_df <- data.frame(
    Tumor_Sample_Barcode = sample_id,
    Hugo_Symbol = vcf_get_hugo_symbols(),
    Chromosome = vcf_df[['#CHROM']],
    Start_Position = vcf_df[['POS']],
    End_Position = vcf_df[['POS']],
    Reference_Allele = vcf_df[['REF']],
    Tumor_Seq_Allele2 = vcf_df[['ALT']],
    Variant_Classification = vcf_get_variant_classification(),
    Variant_Type = vcf_get_variant_type(),
    AAchange = vcf_get_amino_acid_change(),
    VAF = vcf_get_variant_allele_frequency()
  )
  return(maf_df)
}

vcf_guess_sample_name_from_filepath <- function(filepath){
  sample_name = sub(pattern = "\\..*$", replacement = "", x = basename(filepath))
  return(sample_name)
}

vcf_read <- function(path_to_file, sampleid = NULL, verbose = TRUE){
  utilitybeltassertions::assert_files_exist(path_to_file)

  vcf_df = data.table::fread(path_to_file)
  vcf_assert_valid(vcf_df, multi_sample_vcfs_allowed = TRUE)
  return(vcf_df)
}

vcf_assert_valid <- function(vcf_df, multi_sample_vcfs_allowed = TRUE){
  valid_vcf_fields = c('#CHROM', "POS", "REF", "ALT", "QUAL","FILTER", "INFO", "FORMAT")
  utilitybeltassertions::assert_names_include(vcf_df, valid_vcf_fields)

  if(!multi_sample_vcfs_allowed){
    assertthat::assert_that(ncol(vcf_df) == length(valid_vcf_fields), msg = utilitybeltassertions::fmterror("More VCF columns than expected. Probably input a multi-sample VCF, which  this tool does not support"))
  }
}

check_vcf_is_multisample <- function(vcf_df){
  valid_vcf_fields = c('#CHROM', "POS", "REF", "ALT", "QUAL","FILTER", "INFO", "FORMAT")
  ncol(vcf_df) > length(valid_vcf_fields)
  vcf_is_multisample = all((length(valid_vcf_fields)):ncol(vcf_df)==which(! names(vcf_df) %in% valid_vcf_fields))
  return(vcf_is_multisample)
}

vcf_multisample_get_sample_ids <- function(vcf_df){

}



# vcf_path = system.file("testfiles/mysample.singlesample.vcf", package = "utilitybeltmaf")
# convert_vcf_to_maf(system.file("testfiles/mysample.singlesample.vcf", package = "utilitybeltmaf"))
