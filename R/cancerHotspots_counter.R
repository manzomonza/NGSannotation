# CancerHotspots

ch2 = readxl::read_xls("/Users/manzo/Downloads/hotspots_v2.xls")


ch2 <- ch2 %>% dplyr::select(Hugo_Symbol, Amino_Acid_Position, n_MSK, n_Retro, inOncokb, inNBT ) |>
  dplyr::distinct() |>
  dplyr::arrange(Hugo_Symbol) |>
  dplyr::mutate(n_total = as.numeric(n_MSK) + as.numeric(n_Retro))


testdir = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1yuFiN1dlcUgo1_ELdNVXegTfB61oDv8G/Patientendaten/2022/W1851_W1900'
testfiles <- list.files(path = testdir, "prep_snv.txt", recursive = TRUE, full.names = TRUE)

testfiles <- lapply(testfiles, data.table::fread)
testfiles <- lapply(testfiles, function(x) x %>% dplyr::mutate(across(.cols = everything(), .fns = as.character)))
testfiles <- dplyr::bind_rows(testfiles)

sapply(testfiles$one_AA, extract_snv_position)




