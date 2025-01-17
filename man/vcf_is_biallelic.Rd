% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_to_maf.R
\name{vcf_is_biallelic}
\alias{vcf_is_biallelic}
\title{Is VCF Biallelic?}
\usage{
vcf_is_biallelic(collapsed_vcf)
}
\arguments{
\item{collapsed_vcf}{result of \code{\link[=vcf_read]{vcf_read()}}}
}
\value{
TRUE if each entry in VCF has no more than 1 Alt allele. otherwise returns FALSE (boolean)
}
\description{
A biallelic VCF has exactly 2 alleles represented per line in the VCF. (1) Ref allele. (2) Alternative allele.
}
\details{
Not all VCFs are bi-allelic. Some have multiple alt alleles at one position. For example, sometimes if the VCF is multi-sample 2 alt alleles might be given to two possible alleles present in different samples.
In these multi-allelic VCFS, the genotype field then usually describes what allele is present in each sample.
In virtually all cases, multi-allelic VCFs can be simplified to biallelic VCFs through 'normalisation' (e.g. using bcftools norm) without losing information.
}
