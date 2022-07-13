## source file
CLINVAR_SUMMARY_FILEPATH = "/home/ionadmin/ngs_variant_annotation/variantAnnotation/clinvar/variant_summary.txt.gz"

################################################# EXON_GR ###############################################
EXON_GR <- readr::read_tsv("/home/ionadmin/ngs_variant_annotation/variantAnnotation/IR_references/Hg19_refseq_v95_canon_exonInfo.tsv") %>% as(., "GRanges")

######################################### Tumor Suppressor Gene list #####################################
TSG_LENGTHS <- readRDS("/home/ionadmin/ngs_variant_annotation/variantAnnotation/TumorSuppressorGenes/OncoKB_TSG_maxLength.RDS")

#################################################  NCBI Clinvar Variant summary table Download #################################################

## Works if most recent clinvar is in following ftp directory:
# https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/
while(!clinvarCheck()){
  download.file("https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz",
                destfile = CLINVAR_SUMMARY_FILEPATH)
}
print("Clinvar: check")

################################################# Read in Clinvar table

CLINVAR <- data.table::fread(CLINVAR_SUMMARY_FILEPATH) %>%
  dplyr::filter(Assembly == "GRCh37") %>%
  janitor::clean_names()

print("CLINVAR: loaded")


##### COSMIC VARIANT SQLITE database

COSMIC_SQL <- '/home/ionadmin/ngs_variant_annotation/variantAnnotation/cosmic/cut_CosmicVariant.sdb'
SQLITE <- DBI::dbDriver("SQLite")
CONN <- dbConnect(SQLITE, COSMIC_SQL,
                  encoding = "ISO-8859-1")
CON_TBL <- dplyr::tbl(CONN, "cosmic_var")

