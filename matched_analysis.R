# Load UKB samples
ukb_fieldnames <- data.table::fread('~/foi.fieldnames', da=F)
rownames(ukb_fieldnames) <- ukb_fieldnames$entityfield
ukb <- data.table::fread('~/foi.tsv', da=F)
colnames(ukb) <- ukb_fieldnames[colnames(ukb), 'title']
ukb[!ukb$Diabetes_diagnosed_by_doctor %in% 0:1, 'Diabetes_diagnosed_by_doctor'] <- NA

ukb_keep <- data.table::fread('~/matched_ids_ukb.txt', da=F, header = F)[,1]
ukb <- subset(ukb, Participant_ID %in% ukb_keep)
ukb$waist_hip <- ukb$Waist_circumference/ukb$Hip_circumference
ukb$t2d <- ukb$Diabetes_diagnosed_by_doctor

# Waist cuts
waistcuts <- c(0, seq(80, 120, by=10), 1000)
waist_names <- c('<80', '80-90', '90-100', '100-110', '110-120', '>120')
bmicuts <- c(0, seq(25,40, by=5), 100)
bmi_names <- c('<25', '25-30', '30-35', '35-40', '>40')
waisthipcuts <- c(0, seq(0.84,1.08, by=0.06), 10)
waisthip_names <- c('<0.84', '0.84-0.90', '0.90-0.96', '0.96-1.02', '1.02-1.08', '>1.08')
cutphenos <- list(
  'waist'=list('name'='waist', 'cuts'=waistcuts, 'cutnames'=waist_names,
               'ukbname'='Waist_circumference'),
  'bmi'=list('name'='bmi', 'cuts'=bmicuts, 'cutnames'=bmi_names,
             'ukbname'='Body_mass_index_BMI'),
  'waisthip'=list('name'='waisthip', 'cuts'=waisthipcuts, 'cutnames'=waisthip_names,
                  'ukbname'='waist_hip')
)

# Continuous / response phenotypes
contphenos <- data.frame(
  'name'=c('hba1c', 'ldlc', 'trig'),
  'ukbname'=c('Glycated_haemoglobin_HbA1c', 'LDL_direct', 'Triglycerides')
)

hasdiab <- ukb$t2d == 1
sumstats <- data.frame()
for (j in 1:length(cutphenos)){
  cutpheno <- cutphenos[[j]]
  ukb$pheno_cut <- cut(ukb[,cutpheno$ukbname], cutpheno$cuts)
  for (i in 1:nrow(contphenos)){
    contpheno <- contphenos[i,]
    ukb_coefs_all <- summary(lm(ukb[,contpheno$ukbname] ~ -1 + ukb$pheno_cut))$coefficients
    ukb_coefs_diab <- summary(lm(ukb[hasdiab,contpheno$ukbname] ~ -1 + ukb[hasdiab, 'pheno_cut']))$coefficients
    ukb_coefs_nodiab <- summary(lm(ukb[!hasdiab,contpheno$ukbname] ~ -1 + ukb[!hasdiab, 'pheno_cut']))$coefficients
    
    ncutgroups <- length(cutpheno$cutnames)
    sumstats <- rbind(sumstats, data.frame(
      'cutpheno'=cutpheno$name,
      'cutname'=cutpheno$cutnames,
      'contpheno'=contpheno$name,
      'diab'=rep(c('all', 'has_diab', 'no_diab'), each=ncutgroups),
      'mean'=c(ukb_coefs_all[,1], ukb_coefs_diab[,1], ukb_coefs_nodiab[,1]),
      'stderr'=c(ukb_coefs_all[,2], ukb_coefs_diab[,2], ukb_coefs_nodiab[,2])
    ))
  }
}
write.table(sumstats, '~/ukb_matched_sumstats.tsv', sep='\t', row.names = F)
