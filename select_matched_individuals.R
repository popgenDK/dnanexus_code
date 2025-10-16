# Load UKB samples
ukb_fieldnames <- data.table::fread('~/foi.fieldnames', da=F)
rownames(ukb_fieldnames) <- ukb_fieldnames$entityfield
ukb <- data.table::fread('~/foi.tsv', da=F)
colnames(ukb) <- ukb_fieldnames[colnames(ukb), 'title']
ukb <- subset(ukb, Genetic_ethnic_grouping == 1)
ukb[!ukb$Diabetes_diagnosed_by_doctor %in% 0:1, 'Diabetes_diagnosed_by_doctor'] <- NA

# BMI and age cuts.
steps <- 5
agerange <- c(15, 90)
bmirange <- c(10, 70)
ukb$age_cut <- cut(round(ukb$Age_at_recruitment), seq(agerange[1], agerange[2], steps))
ukb$bmi_cut <- cut(round(ukb$Body_mass_index_BMI), seq(bmirange[1], bmirange[2], steps))
ukb$sex_MF <- ifelse(ukb$Sex==1, 'M', 'F')

# Select individuals (random sample in ukbb)
set.seed(2)
ukb <- ukb[sample(1:nrow(ukb)),]

# Load matched
counts <- data.table::fread('~/counts_touse.tsv', da=F)
keeps <- subset(counts, touse==1)

ukb_matched_ids <- c()
for (i in 1:nrow(keeps)){
  keep <- keeps[i,]
  ukb_matched_ids <- c(ukb_matched_ids,
    ukb[ukb$sex_MF == keep$sex &
        ukb$age_cut == keep$age & 
        !is.na(ukb$age_cut) &
        ukb$bmi_cut == keep$bmi &
        !is.na(ukb$bmi_cut) & 
        ukb$Diabetes_diagnosed_by_doctor == keep$t2d &
        !is.na(ukb$Diabetes_diagnosed_by_doctor), 'Participant_ID'][1:keep$ukb_matched_n])
}
matched <- subset(ukb, Participant_ID %in% ukb_matched_ids)
write.table(ukb_matched_ids, '~/match_ids_ukb.txt', row.names = F, col.names = F)
