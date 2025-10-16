# Select individuals (random sample in ukbb)
set.seed(2)
ukb <- ukb[sample(1:nrow(ukb)),]
keeps <- subset(counts, touse==1)

ukb_matched_ids <- c()
for (i in 1:nrow(keeps)){
  keep <- keeps[i,]
  ukb_to_keep <- keep$count_pro * ukb_factor
  ukb_matched_ids <- c(ukb_matched_ids,
     ukb[ukb$sex_MF == keep$sex &
           !is.na(ukb$age_cut) &
           ukb$age_cut == keep$age & 
           ukb$bmi_cut == keep$bmi &
           !is.na(ukb$bmi_cut), 'Participant_ID'][1:ukb_to_keep]
  )
}
write.table(ukb_matched_ids, 'match_ids_ukb.txt', row.names = F, col.names = F)
