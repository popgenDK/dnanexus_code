fields <- data.table::fread('~/ukbb_fields.tsv', da=F, quote="",
  header=F, col.names = c('entityfield', 'title'))

foi <- rbind(
  fields[1,], #eid
  fields[grep('p31$', fields$entityfield),], # sex
  fields[grep('21022', fields$entityfield),], # age at recruitment
  fields[grep('22006', fields$entityfield),], # genetic ethnicity
  fields[grep('p21000_i0', fields$entityfield),], # ethnic background
  fields[grep('21001_i0', fields$entityfield),], # bmi
  fields[grep('p48_i0', fields$entityfield),], # waist
  fields[grep('p49_i0', fields$entityfield),], # hip
  fields[grep('p30780_i0', fields$entityfield),], # ldl
  fields[grep('p30870_i0', fields$entityfield),], # trig
  fields[grep('p2443_i0', fields$entityfield),], # diab
  fields[grep('p30750_i0', fields$entityfield),] # hba1c
)
write.table(foi[,'entityfield'], '~/foi.fields', row.names = F, quote = F, col.names = F)
foi$title <- gsub(' ','_',gsub(' \\| Instance 0|\\(|\\)','',foi$title))
write.table(foi, '~/foi.fieldnames', row.names = F, quote = F, sep='\t')
