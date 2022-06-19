

# single_sample_vcf_to_maf2 <- function(vcf_df, sample_id){
#   maf_df <- data.frame(
#     Tumor_Sample_Barcode = sample_id,
#     Hugo_Symbol = vcf_get_hugo_symbols(),
#     Chromosome = vcf_df[['#CHROM']],
#     Start_Position = vcf_df[['POS']],
#     End_Position = vcf_df[['POS']],
#     Reference_Allele = vcf_df[['REF']],
#     Tumor_Seq_Allele2 = vcf_df[['ALT']],
#     Variant_Classification = vcf_get_variant_classification(),
#     Variant_Type = vcf_get_variant_type(),
#     AAchange = vcf_get_amino_acid_change(),
#     VAF = vcf_get_variant_allele_frequency()
#   )
#   return(maf_df)
# }
#
# vcf_guess_sample_name_from_filepath <- function(filepath){
#   sample_name = sub(pattern = "\\..*$", replacement = "", x = basename(filepath))
#   return(sample_name)
# }
#
#
#
#
#
# vcf_read <- function(path_to_vcf){
#   VariantAnnotation::readVcf(path_to_vcf)
# }
#
# vcf_assert_collapsedvcf <- function(collapsed_vcf){
#   assertthat::assert_that(class(collapsed_vcf) == "CollapsedVCF", msg = paste0("Expected vcf to be a 'CollapsedVCF' object. Instead got ", class(collapsed_vcf)))
# }
#
# vcf_assert_vep_annotated <- function(collapsed_vcf, verbose = TRUE){
#   vcf_assert_collapsedvcf(collapsed_vcf)
#   assertthat::assert_that("CSQ" %in% names(collapsed_vcf@info), msg = "\u2718 Please ensure VCF is VEP annotated. Could not find VEP annotation (CSQ) in info field.")
#   if(verbose) message("\u2714 VCF is VEP annotated ")
# }
#
# check_vcf_is_multisample <- function(collapsed_vcf){
#   samplenames = VariantAnnotation::samples(VariantAnnotation::header(collapsed_vcf))
#   if(length(samplenames) > 0)
#     return(TRUE)
#   else
#     return(FALSE)
# }

# single_sample_vcf_to_maf <- function(collapsed_vcf, sample_id){
#   #browser()
#   maf_df <- data.frame(
#     Tumor_Sample_Barcode = sample_id,
#     Hugo_Symbol = parse_vep_annotation(collapsed_vcf, "SYMBOL"),
#     Chromosome = as.character(collapsed_vcf@rowRanges@seqnames),
#     Start_Position = collapsed_vcf@rowRanges@ranges@start,
#     End_Position = as.data.frame(collapsed_vcf@rowRanges@ranges)[["end"]],
#     Reference_Allele = sapply(VariantAnnotation::ref(collapsed_vcf), FUN = function(.x) paste0(.x, collapse = ",")),
#     Tumor_Seq_Allele2 = sapply(VariantAnnotation::alt(collapsed_vcf), FUN = function(.x) paste0(.x, collapse = ",")),
#     Variant_Classification = parse_vep_annotation(collapsed_vcf, "Consequence"),
#     #Variant_Type = vcf_get_variant_type(), #Variant_TypeTNP (tri-nucleotide polymorphism) is analogous to DNP (di-nucleotide polymorphism) but for three consecutive nucleotides. ONP (oligo-nucleotide polymorphism) is analogous to TNP but for consecutive runs of four or more (SNP, DNP, TNP, ONP, INS, DEL, or Consolidated)
#     AAchange = parse_vep_annotation(collapsed_vcf, "Amino_acids")
#     #VAF = vcf_get_variant_allele_frequency()
#   )
#   return(maf_df)
#}


# parse_vep_annotation <- function(collapsed_vcf, field_to_extract){
#   #browser()
#   vcf_assert_collapsedvcf(collapsed_vcf)
#   vcf_assert_vep_annotated(collapsed_vcf, verbose = FALSE)
#   vcf_assert_biallelic(collapsed_vcf)
#
#   vep_csq_fields = get_csq_fields(collapsed_vcf)
#   index_of_field_to_extract = which(vep_csq_fields == field_to_extract)
#
#   assertthat::assert_that(
#     length(index_of_field_to_extract) != 0,
#     msg = paste0("Could not find field: [", field_to_extract, "] in VCF vep annotation. Accessible VEP annotations include: [", paste0(vep_csq_fields, collapse = ", ") ,"]")
#   )
#
#   veplist <- VariantAnnotation::info(collapsed_vcf)[["CSQ"]]
#   vep_df <- purrr::map_dfr(.x =  as.list(veplist), .f = ~ stringr::str_split(string = .x, pattern = "\\|", simplify = TRUE) |> as.data.frame() |> magrittr::set_names(vep_csq_fields), .id = "VCF_line_ID")
#
#   browser()
#
#   # rank
#   #do.call(rbind, veplistsplit[[5]]) |> as.data.frame()
#
#
#   # veplist <- strsplit(vepvec, split = "\\|")
#   #
#   # annotation_values = vapply(X = veplist, FUN = function(x) { x[[index_of_field_to_extract]]}, FUN.VALUE = character(1))
#
#   return(annotation_values)
# }
#
# get_worst_vep_consequence <- function(vep_entry, vep_csq_fields){
#   browser()
#   vep_entries_df = as.data.frame(do.call(rbind, vep_entry))
#   names(vep_entries_df) <- vep_csq_fields
# }
#
# get_csq_fields <-function(collapsed_vcf){
#   vcf_assert_collapsedvcf(collapsed_vcf)
#
#   header_info = VariantAnnotation::info(VariantAnnotation::header(collapsed_vcf))
#   info_names = row.names(header_info)
#   assertthat::assert_that("CSQ" %in% info_names)
#
#   description = header_info["CSQ",3]
#   description_cleaned = sub(pattern = "^.*?: ", replacement = "", x =  description)
#   description_vector = unlist(strsplit(description_cleaned, split = "\\|"))
#
#   return(description_vector)
#   #browser()
# }


#
# vcf_is_biallelic <- function(collapsed_vcf){
#   vcf_assert_collapsedvcf(collapsed_vcf)
#   vcf_entry_is_biallelic = vapply(FUN.VALUE = numeric(1), FUN = length, X=as.list(VariantAnnotation::alt(collapsed_vcf))) == 1
#   return(all(vcf_entry_is_biallelic))
# }
#
# vcf_assert_biallelic <- function(collapsed_vcf){
#   assertthat::assert_that(vcf_is_biallelic(collapsed_vcf), msg = "VCF object was multiallelic. Please normalise VCF (split multiallelic entriest over multiple lines) then retry")
# }


#' #' VCF2MAF
#' #'
#' #' Convert a vcf (or vcfs) to a MAF file
#' #'
#' #' @param path_to_vcf
#' #'
#' #' @return MAF dataframe
#' #' @export
#' #'
#' vcf_to_maf <- function(path_to_vcf, sampleid = NULL, verbose = TRUE){
#'   #browser()
#'   collapsed_vcf = vcf_read(path_to_vcf)
#'   #vcf_df = vcf_read(path_to_vcf)
#'   vcf_is_multisample = check_vcf_is_multisample(collapsed_vcf)
#'
#'   if(!vcf_is_multisample){
#'     if(is.null(sampleid)){
#'       sampleid = vcf_guess_sample_name_from_filepath(path_to_vcf)
#'       if(verbose) message("Guessing Samplename from filepath: ", sampleid)
#'     }
#'     maf = single_sample_vcf_to_maf(collapsed_vcf, sampleid)
#'     return(maf)
#'   }
#' }
#vcf_path = system.file("testfiles/mysample.singlesample.vcf", package = "utilitybeltmaf")
# devtools::load_all(); convert_vcf_to_maf(system.file("testfiles/mysample.singlesample.vcf", package = "utilitybeltmaf"))



