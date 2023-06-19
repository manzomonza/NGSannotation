# COUNT Variants per panel -- only SNVs for now.
library(NGSannotation)
sample_snv = readr::read_tsv('/mnt/NGS_Diagnostik/SampleCentricVariantCollection/Sample_centric_SNV.tsv')
#sample_snv = readr::read_tsv('/Users/manzo/USB/USB_Diagnostics/NGS_variant_annotation/NGSannotation_dirs/Sample_centric_SNV.tsv')
sample_snv = dplyr::filter(sample_snv, workflowName != 'workflowName')
sample_snv = dplyr::select(sample_snv, gene, coding, amino_acid_change, transcript, workflowName)
sample_snv$one_AA = unname(sapply(sample_snv$amino_acid_change, function(x) amino_acid_conversion_three_to_one(x)))
sample_snv = dplyr::group_by(sample_snv, gene, coding, one_AA, workflowName)
sample_snv$workflowName = ifelse(grepl("Oncomine Precision", sample_snv$workflowName), "Precision",
                                 ifelse(grepl("OPA DNA", sample_snv$workflowName), "Precision", sample_snv$workflowName))
snv_count = dplyr::count(sample_snv, sort = TRUE)


fil_tbl = dplyr::filter(snv_count, gene == "KRAS" & coding == 'c.35G>T')
fil_tbl$total = sum(fil_tbl$n)
fil_tbl$panel_list = paste0(paste0(fil_tbl$workflowName, " (", fil_tbl$n, ")"), collapse = "; ")
fil_tbl = dplyr::ungroup(fil_tbl)
fil_tbl = dplyr::select(fil_tbl, -workflowName, -n)
fil_tbl = dplyr::distinct(fil_tbl)
