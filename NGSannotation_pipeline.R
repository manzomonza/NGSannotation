#!/usr/bin/R

#NGSannotation pipeline

suppressPackageStartupMessages({
  library(janitor)
  library(tidyverse)
  library(RCurl)
  library(R.utils)
  library(data.table)
  library(tools)
  library(curl)
  library(GenomicRanges)
  library(Biostrings)
  library(writexl)
  library(NGSannotation)
})
print("Packages: loaded")


################################################# OPTPARSE #################################################
library(optparse)

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL,
              help="dataset file name", metavar="character"))

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser)

print(opt$file)

## Source config
source("/home/ionadmin/ngs_variant_annotation/variantAnnotation/scripts/ngs_annot_env/NGSannotation_config.R")

################################################# IR MUTATION REPORT ANNOTATION #################################################
################################################# PIPELINE SELECTION #################################################


generateWatchdogTables <- function(filepath){
  if(nrow(readIn(filepath) > 0)){
    if(grepl("Snvindel.tsv", filepath, ignore.case = FALSE)){
      precisionPipeline_TableOutput(filepath)
    }else{
      stdPipeline_TableOutput(filepath)
      std_metadataCollector(filepath)
    }
  }else{}
}

if(grepl("Fusion.tsv", opt$file, ignore.case = FALSE)){
	print("Generating Fusion.tsv output")
	FusionOutput(opt$file)
}else{
	print("Generating watchdog folder and output tables")
	generateWatchdogTables(opt$file)
	print("Generating Metainformation table (Info.csv)")
	std_metadataCollector(opt$file)
	print("Generating Pathowin annotation")
	annotateWatchdogTables(opt$file)
	print("Generating Combined output file")
	saveReport_xlsx(opt$file)
}



