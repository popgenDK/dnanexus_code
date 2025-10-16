dx extract_dataset app32683_20250501113033.dataset \
  --list-fields > ukbb_fields.tsv
  
Rscript phenotype_fields.R
dx extract_dataset app32683_20250501113033.dataset \
  --fields-file foi.fields \
  --delim '\t' \
  --output foi.tsv
