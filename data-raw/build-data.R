## Build datasets
devtools::load_all(".");
eia_bulk_cachelist <- eia_bulk_manifest();

## Write data sets files
usethis::use_data(eia_bulk_cachelist, overwrite=TRUE);

### =============================== EOF =====================================
