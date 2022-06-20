biotype_rankings <- function(){
  dplyr::tibble(
    utils::read.csv(
      system.file(package = "utilitybeltmaf","required_assets/biotype_ranking"),
      sep = "\t",
      header = TRUE
    )
  )
}

effect_rankings <- function(){
  dplyr::tibble(
    utils::read.csv(
      system.file(package = "utilitybeltmaf","required_assets/effect_ranking"),
      sep = "\t",
      header = TRUE
    )
  )
}


#' Parse Vep Annotation
#'
#' Takes a vep annotated VCF and pulls out a user_specified field.
#' For example, could pull out 'Consequence', 'Impact' or 'SYMBOL'.
#' Doing this is more tricky than you might think. Each variant might have a different effect on different but overlapping transcripts / genes /etc
#' So which transcripts effect do we want to pull out?
#' Well this code first sorts effects by Biotype according to '[biotype_rankings()]',
#' then by effect everity according to '[effect_rankings()]',
#' then by transcript length (inferred fom cDNA_position).
#' We then take the highest priority effect with a gene symbol.
#' This gene is the MAF symbol that we'll use.
#' Now that we know the gene we care about - we choose which consequence we care about for this specific gene.
#' We default to whatever is the VEP-preferred: 'CANONICAL' transcript (requires --canonical flag to VEP to be present). If no canonical we'll report the most severe consequence.
#'
#' @param collapsed_vcf result of [vcf_read()]
#' @param field_to_extract vep field to extract
#'
#' @return value in field_to_extract for each vcf character
#' @export
#'
#' @examples
vcf_parse_vep_annotation <- function(collapsed_vcf, field_to_extract){
  #browser()
  vcf_assert_collapsedvcf(collapsed_vcf)
  vcf_assert_vep_annotated(collapsed_vcf, verbose = FALSE)
  vcf_assert_biallelic(collapsed_vcf)

  vep_csq_fields = get_csq_fields(collapsed_vcf)
  index_of_field_to_extract = which(vep_csq_fields == field_to_extract)

  assertthat::assert_that(
    length(index_of_field_to_extract) != 0,
    msg = paste0("Could not find field: [", field_to_extract, "] in VCF vep annotation. Accessible VEP annotations include: [", paste0(vep_csq_fields, collapse = ", ") ,"]")
  )

  veplist <- VariantAnnotation::info(collapsed_vcf)[["CSQ"]]

  # Create a dataframe for a VCF. 1 row for each VEP predicted consequence.
  # VCF_line_id column describes which line from the column it came from
  vep_df <- purrr::map_dfr(
    .x =  as.list(veplist),
    .f = ~ stringr::str_split(string = .x, pattern = "\\|", simplify = TRUE) |>
      as.data.frame() |>
      magrittr::set_names(vep_csq_fields),
    .id = "VCF_line_id"
  )

  # add code to rank vep effects by biotype then  and effect then transcript id....see description roxygen comment for more instructions on

  biotype_ranking = biotype_rankings()
  effect_rankings = effect_rankings()

  # annotate vep_df with biotype, effect, and transcript length rankings
  vep_df |>
    mutate(
      biotype_priority = biotype_rankings[["priority"]][match(BIOTYPE, biotype_rankings[["biotype"]])],
      effect_priority = effect_rankings[["priority"]][match(Consequence, effect_rankings[["effect"]])],
      transcript_length = vep_df[["cDNA_position"]] # How does vcf2maf infer transcript length from this? - higher values mean cdna start is further up ... could suggest logner OR that the transcript
      )


  browser()

  # rank
  #do.call(rbind, veplistsplit[[5]]) |> as.data.frame()


  # veplist <- strsplit(vepvec, split = "\\|")
  #
  # annotation_values = vapply(X = veplist, FUN = function(x) { x[[index_of_field_to_extract]]}, FUN.VALUE = character(1))

  return(annotation_values)
}

get_worst_vep_consequence <- function(vep_entry, vep_csq_fields){
  browser()
  vep_entries_df = as.data.frame(do.call(rbind, vep_entry))
  names(vep_entries_df) <- vep_csq_fields
}

get_csq_fields <-function(collapsed_vcf){
  vcf_assert_collapsedvcf(collapsed_vcf)

  header_info = VariantAnnotation::info(VariantAnnotation::header(collapsed_vcf))
  info_names = row.names(header_info)
  assertthat::assert_that("CSQ" %in% info_names)

  description = header_info["CSQ",3]
  description_cleaned = sub(pattern = "^.*?: ", replacement = "", x =  description)
  description_vector = unlist(strsplit(description_cleaned, split = "\\|"))

  return(description_vector)
  #browser()
}
