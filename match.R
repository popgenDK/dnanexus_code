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

ukb_counts <- as.data.frame(table(ukb$age_cut, ukb$bmi_cut, ukb$sex_MF, ukb$Diabetes_diagnosed_by_doctor))
colnames(ukb_counts) <- c('age', 'bmi', 'sex', 't2d','count_ukb')


# Read the prophecy counts
pro_counts <- data.table::fread('~/pro.bmiagediab_cuts.step5.long.tsv', da=F)
stopifnot(sum(pro_counts$age != ukb_counts$age |
                pro_counts$bmi != ukb_counts$bmi | 
                pro_counts$sex != ukb_counts$sex) == 0)
counts <- pro_counts
counts$count_ukb <- ukb_counts$count_ukb
counts$ukb_div_pro <- counts$count_ukb / counts$count_pro
counts[is.nan(counts$ukb_div_pro) | is.infinite(counts$ukb_div_pro), 'ukb_div_pro'] <- NA

# Plot
png('~/prophecyindvsused.png',
    width = 3.5, height = 3.5, res=300, units = 'in')
par(mar=c(2.5,3,1,0.1), mgp=c(1.6,0.6,0), font.main=1, cex.main=1)
pro_indvsused <- lapply(1:100, function(x){sum(counts[counts$count_pro>1 & counts$ukb_div_pro>=x, 'count_pro'])})
plot(1:100, pro_indvsused, bty='L', las=1,
     xlab='Number of UKB indvs per Prophecy', xlim=c(0,40),
     ylab='', type='o', pch=19, cex=0.5, ylim=c(530,750),
     main='How many UKB per prophecy indvs')
mtext('Number of Prophecy indvs used', 2, line=2)

pro_n <- sum(pro_counts$count_pro)
pcts <- pro_n * c(0.5, 0.55, 0.6)
abline(h=pcts, lty=2, col='gray')
text(40, pcts, paste0(c(0.5,0.55,0.6)*100, '%'), adj=c(1,0))

# Manually set the choice:
choice <- 9
choice_prop <- pro_indvsused[[choice]] / pro_n
points(choice, pro_indvsused[choice], col='red', pch=19, cex=0.5)
text(choice+2, pro_indvsused[choice], paste0(
  'Choice: ', choice,
  ', ', pro_indvsused[choice],
  ' (~', round(choice_prop*100,1), '%)'), adj=0, col='red', cex=0.8)
dev.off()

# Add to use variable to counts.
counts$touse <- as.numeric(counts$count_pro > 1 &
  counts$ukb_div_pro >= choice &
  !is.na(counts$ukb_div_pro))
counts$bmilow <- as.numeric(gsub('\\(([^,]*),[^\\]*]','\\1',counts$bmi))
counts$bmihig <- as.numeric(gsub('\\([^,]*,([^\\]*)]','\\1',counts$bmi))
counts$agelow <- as.numeric(gsub('\\(([^,]*),[^\\]*]','\\1',counts$age))
counts$agehig <- as.numeric(gsub('\\([^,]*,([^\\]*)]','\\1',counts$age))

write.table(counts, '~/counts_touse.tsv',sep='\t', row.names = F, quote = F)
write.table(choice, '~/ukb_factor_choice.txt', row.names = F, quote = F, col.names = F)

