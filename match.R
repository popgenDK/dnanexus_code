ukb_fieldnames <- data.table::fread('foi.fieldnames', da=F)
rownames(ukb_fieldnames) <- ukb_fieldnames$entityfield
ukb <- data.table::fread('foi.tsv', da=F)
colnames(ukb) <- ukb_fieldnames[colnames(ukb), 'title']
ukb <- subset(ukb, Genetic_ethnic_grouping == 1)

# BMI and age cuts.
steps <- 5
agerange <- c(15, 90)
bmirange <- c(10, 70)
ukb$age_cut <- cut(round(ukb$Age_at_recruitment), seq(agerange[1], agerange[2], steps))
ukb$bmi_cut <- cut(round(ukb$Body_mass_index_BMI), seq(bmirange[1], bmirange[2], steps))
ukb$sex_MF <- ifelse(ukb$Sex==1, 'M', 'F')
ukb_counts <- as.data.frame(table(ukb$age_cut, ukb$bmi_cut, ukb$sex_MF))
colnames(ukb_counts) <- c('age', 'bmi', 'sex', 'count_ukb')

# Read the prophecy counts
pro_counts <- data.table::fread('bmiage_cuts.step5.long.tsv', da=F)
stopifnot(sum(pro_counts$age != ukb_counts$age |
                pro_counts$bmi != ukb_counts$bmi | 
                pro_counts$sex != ukb_counts$sex) == 0)

counts <- pro_counts
counts$count_ukb <- ukb_counts$count_ukb
counts$ukb_div_pro <- counts$count_ukb / counts$count_pro

# Plot
png('plots/prophecyindvsused.png',
    width = 3.5, height = 3.5, res=300, units = 'in')
par(mar=c(2.5,3,1,0.1), mgp=c(1.6,0.6,0), font.main=1, cex.main=1)
pro_indvsused <- lapply(1:100, function(x){sum(counts[counts$count_pro>1 & counts$ukb_div_pro>x, 'count_pro'])})
plot(1:100, pro_indvsused, bty='L', las=1,
     xlab='Number of UKB indvs per Prophecy',
     ylab='', type='o', pch=19, cex=0.5, ylim=c(530,750),
     main='How many UKB per prophecy indvs')
mtext('Number of Prophecy indvs used', 2, line=2)
pro_n <- sum(pro_counts$count_pro)
pcts <- pro_n * c(0.5, 0.55, 0.6)
abline(h=pcts, lty=2, col='gray')
text(100, pcts, paste0(c(0.5,0.55,0.6)*100, '%'), adj=c(1,0))
pro_indvsused[[10]] / pro_n
points(10, 722, col='red', pch=19, cex=0.5)
text(12, 722, 'Choice: 10, 722 (~60.1%)', adj=0, col='red', cex=0.8)
dev.off()

# Choose 10
which(unlist(pro_indvsused)>721) # 10
ukb_factor <- 10
counts$touse <- as.numeric(counts$count_pro > 1 &
                             counts$ukb_div_pro > ukb_factor &
                             !is.na(counts$ukb_div_pro))
sum(counts[counts$touse==1, 'count_pro'])
counts$bmilow <- as.numeric(gsub('\\(([^,]*),[^\\]*]','\\1',counts$bmi))
counts$bmihig <- as.numeric(gsub('\\([^,]*,([^\\]*)]','\\1',counts$bmi))
counts$agelow <- as.numeric(gsub('\\(([^,]*),[^\\]*]','\\1',counts$age))
counts$agehig <- as.numeric(gsub('\\([^,]*,([^\\]*)]','\\1',counts$age))

counts$ukb_div_pro[is.infinite(counts$ukb_div_pro) |
                     is.nan(counts$ukb_div_pro) |
                     counts$ukb_div_pro==0 |
                     counts$count_pro<2] <- NA
write.table(counts, 'counts_touse.tsv',sep='\t', row.names = F, quote = F)

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


