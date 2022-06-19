biotype_rankings <- function(){
  read.csv(
    system.file(package = "utilitybeltmaf","required_assets/biotype_ranking"),
    sep = "\t",
    header = TRUE
  )
}

effect_rankings <- function(){
  read.csv(
    system.file(package = "utilitybeltmaf","required_assets/effect_ranking"),
    sep = "\t",
    header = TRUE
  )
}


