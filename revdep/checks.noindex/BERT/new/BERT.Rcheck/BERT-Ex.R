pkgname <- "BERT"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('BERT')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("BERT")
### * BERT

flush(stderr()); flush(stdout())

### Name: BERT
### Title: Adjust data using the BERT algorithm.
### Aliases: BERT

### ** Examples

# generate dataset with 1000 features, 5 batches, 10 samples per batch and
# two genotypes
data = generate_dataset(1000,5,10,0.1, 2)
corrected = BERT(data, cores=2)



cleanEx()
nameEx("compute_asw")
### * compute_asw

flush(stderr()); flush(stdout())

### Name: compute_asw
### Title: Compute the average silhouette width (ASW) for the dataset with
###   respect to both label and batch.
### Aliases: compute_asw

### ** Examples

# generate dataset with 1000 features, 5 batches, 10 samples per batch and
# two genotypes
data = generate_dataset(1000,5,10,0.1, 2)
asw = compute_asw(data)
asw



cleanEx()
nameEx("count_existing")
### * count_existing

flush(stderr()); flush(stdout())

### Name: count_existing
### Title: Count the number of numeric features in this dataset. Columns
###   labeled "Batch", "Sample" or "Label" will be ignored.
### Aliases: count_existing

### ** Examples

# generate dataset with 1000 features, 5 batches, 10 samples per batch and
# two genotypes
data = generate_dataset(1000,5,10, 0.1, 2)
count_existing(data)



cleanEx()
nameEx("generate_data_covariables")
### * generate_data_covariables

flush(stderr()); flush(stdout())

### Name: generate_data_covariables
### Title: Generate dataset with batch-effects and 2 classes with a
###   specified imbalance.
### Aliases: generate_data_covariables

### ** Examples

# generate dataset with 1000 features, 5 batches, 10 samples per batch and
# two genotypes. The class ratio will either be 7:3 or 3:7 per batch.
data = generate_data_covariables(1000,5,10, 0.1, 0.3)



cleanEx()
nameEx("generate_dataset")
### * generate_dataset

flush(stderr()); flush(stdout())

### Name: generate_dataset
### Title: Generate dataset with batch-effects and biological labels using
###   a simple LS model
### Aliases: generate_dataset

### ** Examples

# generate dataset with 1000 features, 5 batches, 10 samples per batch and
# two genotypes
data = generate_dataset(1000,5,10, 0.1, 2)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
