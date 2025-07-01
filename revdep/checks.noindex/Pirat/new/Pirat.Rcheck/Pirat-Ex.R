pkgname <- "Pirat"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('Pirat')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("estimate_gamma")
### * estimate_gamma

flush(stderr()); flush(stdout())

### Name: estimate_gamma
### Title: Estimate missingness parameters Gamma
### Aliases: estimate_gamma

### ** Examples

data(subbouyssie)
estimate_gamma(subbouyssie$peptides_ab)




cleanEx()
nameEx("estimate_psi_df")
### * estimate_psi_df

flush(stderr()); flush(stdout())

### Name: estimate_psi_df
### Title: Estimate psi and degrees of freedom
### Aliases: estimate_psi_df

### ** Examples

data(subbouyssie)
obj <- subbouyssie
# Keep only fully observed peptides
obs2NApep <- obj$peptides_ab[ ,colSums(is.na(obj$peptides_ab)) <= 0] 
estimate_psi_df(obs2NApep)





cleanEx()
nameEx("get_indexes_embedded_prots")
### * get_indexes_embedded_prots

flush(stderr()); flush(stdout())

### Name: get_indexes_embedded_prots
### Title: Indexes of PGs embedded in each others
### Aliases: get_indexes_embedded_prots

### ** Examples

data(subbouyssie)
get_indexes_embedded_prots(subbouyssie$adj)




cleanEx()
nameEx("impute_block_llk_reset")
### * impute_block_llk_reset

flush(stderr()); flush(stdout())

### Name: impute_block_llk_reset
### Title: Impute each PG.
### Aliases: impute_block_llk_reset

### ** Examples





cleanEx()
nameEx("impute_block_llk_reset_PG")
### * impute_block_llk_reset_PG

flush(stderr()); flush(stdout())

### Name: impute_block_llk_reset_PG
### Title: Impute each PG.
### Aliases: impute_block_llk_reset_PG

### ** Examples






cleanEx()
nameEx("impute_from_blocks")
### * impute_from_blocks

flush(stderr()); flush(stdout())

### Name: impute_from_blocks
### Title: Impute abundance table from PGs results
### Aliases: impute_from_blocks

### ** Examples

Py_impute_block_llk_reset <- function(data.pep.rna.mis, psi) { 
proc <- basilisk::basiliskStart(envPirat)

func <- basilisk::basiliskRun(proc, 
    fun = function(arg1, arg2) {
        
        imputed_pgs <- Pirat::impute_block_llk_reset(arg1, arg2)
        imputed_pgs 
    }, arg1 = data.pep.rna.mis, arg2 = psi)

basilisk::basiliskStop(proc)
func
}


data(subbouyssie)
obj <- subbouyssie
# Keep only fully observed peptides
obs2NApep <- obj$peptides_ab[ ,colSums(is.na(obj$peptides_ab)) <= 0] 
res_hyperparam <- estimate_psi_df(obs2NApep)
psi <- res_hyperparam$psi
imputed_pgs <- Py_impute_block_llk_reset(obj, psi)
impute_from_blocks(imputed_pgs, obj)





cleanEx()
nameEx("pipeline_llkimpute")
### * pipeline_llkimpute

flush(stderr()); flush(stdout())

### Name: pipeline_llkimpute
### Title: Pirat imputation function
### Aliases: pipeline_llkimpute my_pipeline_llkimpute

### ** Examples

# Pirat classical mode
data(subbouyssie)
myResult <- my_pipeline_llkimpute(subbouyssie)

# Pirat with transcriptomic integration for singleton PGs
data(subropers)
nsamples = nrow(subropers$peptides_ab)
myResult <- my_pipeline_llkimpute(subropers, 
extension = "T",
rna.cond.mask = seq(nsamples), 
pep.cond.mask = seq(nsamples),
max.pg.size.pirat.t = 1)

## Not run: 
##D myResult <- pipeline_llkimpute(subbouyssie)
## End(Not run)



cleanEx()
nameEx("pirat2SE")
### * pirat2SE

flush(stderr()); flush(stdout())

### Name: pirat2SE
### Title: COnvert Pirat dataset to SummarizedExperiment
### Aliases: pirat2SE

### ** Examples

data(subbouyssie)
peptides_ab <- subbouyssie$peptides_ab
adj <- subbouyssie$adj
mask_prot_diff <- subbouyssie$mask_prot_diff
mask_pep_diff <- subbouyssie$mask_pep_diff
obj <- pirat2SE(peptides_ab, adj, mask_prot_diff, mask_pep_diff )
obj




cleanEx()
nameEx("plot2hists")
### * plot2hists

flush(stderr()); flush(stdout())

### Name: plot2hists
### Title: Plot 2 histograms
### Aliases: plot2hists

### ** Examples

v1 <- 1:10
v2 <- 5:25
plot2hists(v1, v2)




cleanEx()
nameEx("plot_pep_correlations")
### * plot_pep_correlations

flush(stderr()); flush(stdout())

### Name: plot_pep_correlations
### Title: Empirical density of peptide correlations
### Aliases: plot_pep_correlations

### ** Examples

data(subbouyssie)
plot_pep_correlations(subbouyssie, 'test')




cleanEx()
nameEx("rm_pg_from_idx_merge_pg")
### * rm_pg_from_idx_merge_pg

flush(stderr()); flush(stdout())

### Name: rm_pg_from_idx_merge_pg
### Title: Remove PGs by index and merge
### Aliases: rm_pg_from_idx_merge_pg

### ** Examples

data(ropers)
idxs_emb_prot = get_indexes_embedded_prots(ropers$adj)
ropers_wo_emb_prot = rm_pg_from_idx_merge_pg(ropers, idxs_emb_prot)




cleanEx()
nameEx("split_large_pg")
### * split_large_pg

flush(stderr()); flush(stdout())

### Name: split_large_pg
### Title: Split too large PGs
### Aliases: split_large_pg

### ** Examples

data(subbouyssie)
split.obj <- split_large_pg(subbouyssie$adj, 5)




cleanEx()
nameEx("split_large_pg_PG")
### * split_large_pg_PG

flush(stderr()); flush(stdout())

### Name: split_large_pg_PG
### Title: Splits too large PGs in proteogenomics context
### Aliases: split_large_pg_PG

### ** Examples

data(subropers)
split.obj <- split_large_pg_PG(subropers$adj, 5, subropers$adj_rna_pg)




cleanEx()
nameEx("wrapper_pipeline_llkimpute")
### * wrapper_pipeline_llkimpute

flush(stderr()); flush(stdout())

### Name: wrapper_pipeline_llkimpute
### Title: Imputation method using SummarizedExperiment dataset
### Aliases: wrapper_pipeline_llkimpute

### ** Examples

data(subbouyssie)
obj <- pirat2SE(subbouyssie$peptides_ab, subbouyssie$adj, 
subbouyssie$mask_prot_diff, subbouyssie$mask_pep_diff )
res <- wrapper_pipeline_llkimpute(obj)




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
