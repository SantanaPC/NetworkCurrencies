#############################################################
##################Network Comparisons #######################
#############################################################

library(bipartite)
library(iNEXT)
library(vegan)
library(ggplot2)
library(igraph)
library("RColorBrewer")

# Script for network comparisons 
#(frequency, deposition, removal, performance) 

# Author: Lorena Valadão & Pamela Santana
# Date: 2025-11-02

# ---------------------------
# Setup
# ---------------------------
set.seed(123)
options(stringsAsFactors = FALSE)

required_pkgs <- c("bipartite", "iNEXT", "vegan", "ggplot2", "igraph", "RColorBrewer")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Please install packages: ", paste(missing_pkgs, collapse = ", "), 
       "\nExample: install.packages(c('", paste(missing_pkgs, collapse="','"), "'))", sep = "")
}
library(bipartite)
library(iNEXT)
library(vegan)
library(ggplot2)
library(igraph)
library(RColorBrewer)

# ---------------------------
# Functions
# ---------------------------

read_network <- function(path, sep = ",", header = TRUE, row.names = 1, dec = ".") {
  if (!file.exists(path)) stop("File not found: ", path)
  
  df <- tryCatch({
    read.csv(path,
             header = header,
             row.names = row.names,
             sep = sep,
             dec = dec,
             check.names = FALSE,
             stringsAsFactors = FALSE)
  }, error = function(e) {
    # fallback to read.table if read.csv fails
    read.table(path,
               header = header,
               row.names = row.names,
               sep = sep,
               dec = dec,
               check.names = FALSE,
               stringsAsFactors = FALSE)
  })
  
  mat <- as.matrix(df)
  
  # Ensure all columns are numeric
  if (!is.numeric(mat)) {
    suppressWarnings(mat <- apply(mat, 2, function(x) as.numeric(x)))
  }
  return(mat)
}

safe_plotweb <- function(mat, file = NULL, width = 12, height = 10, 
                         method = "normal", text.rot = 90,
                         pollinator_groups = NULL,
                         group_colors = c(
                           flow_buzz = "#63077D",
                           ant_buzz  = "#e72881",
                           theft     = "#1C796F",
                           robber    = "#FF6A00"
                         ),
                         col.low = "#999999", # plantas
                         bor.col.low = "#000000",
                         bor.col.high = "#000000",
                         col.interaction = "#CCCCCC",
                         bor.col.interaction = "#333333") {
  
  if (!is.null(file)) pdf(file, width = width, height = height)
  
  # ---- Define colors for pollinators (top level) ----
  col_high <- rep("#BDBDBD", ncol(mat))  # default grey if group not found
  
  if (!is.null(pollinator_groups)) {
    pols <- colnames(mat)
    # match pollinators to groups
    groups <- pollinator_groups[pols]
    # convert group to colors
    group_cols_vec <- group_colors[groups]
    # replace where match exists
    col_high[!is.na(group_cols_vec)] <- group_cols_vec[!is.na(group_cols_vec)]
  }
  
  tryCatch({
    plotweb(mat,
            method = method,
            text.rot = text.rot,
            col.low = col.low,       # plants
            col.high = col_high,     # pollinators colored by group
            bor.col.high = bor.col.high,
            bor.col.low = bor.col.low,
            bor.col.interaction = bor.col.interaction,
            col.interaction = col.interaction)
  }, error = function(e) {
    message("plotweb failed for file: ", file, " — ", e$message)
  })
  
  if (!is.null(file)) dev.off()
}

compute_null_distribution <- function(mat, n = 1000, method = "vaznull",
                                      metric_fun = function(x) networklevel(x, index = "H2")) {
  # create null models and compute metric on each
  nulls <- nullmodel(mat, N = n, method = method)
  vals <- unlist(sapply(nulls, metric_fun))
  return(vals)
}

# compute H2 and p-value against null distribution
test_metric_against_null <- function(mat, metric_name = "H2", nnull = 1000, null_method = "vaznull") {
  observed <- networklevel(mat, index = metric_name)
  null_vals <- compute_null_distribution(mat, n = nnull, method = null_method,
                                         metric_fun = function(x) networklevel(x, index = metric_name))
  p_value <- mean(null_vals >= observed) # one-sided: how often null >= observed (change if needed)
  ci <- quantile(null_vals, c(0.025, 0.975))
  list(observed = observed, null = null_vals, p_value = p_value, ci = ci)
}

# wrapper to compute a suite of network-level metrics
compute_metrics <- function(mat) {
  list(
    connectance = networklevel(mat, index = "connectance"),
    H2 = networklevel(mat, index = "H2"),
    weighted_NODF = networklevel(mat, index = "weighted NODF"),
    generality = networklevel(mat, index = "generality")
  )
}

df_obs <- read.csv(dados_obs_grupos, header = TRUE, stringsAsFactors = FALSE)

pollinator_groups <- setNames(df_obs$func_group, df_obs$bee_sp)

# Keep only unique mappings (avoid duplicates if bees appear multiple times)
pollinator_groups <- pollinator_groups[!duplicated(names(pollinator_groups))]

message("Loaded pollinator functional groups from dados_obs_group.csv (",
        length(pollinator_groups), " unique pollinators).")

# ---------------------------
# Load networks
# ---------------------------
rede_freq <- read_network("1.RawData/rede_frequencia.csv", sep = ",")
rede_deposicao <- read_network("1.RawData/rede_deposicao3.csv", sep = ";")
rede_performance_f <- read_network("1.RawData/rede_eficacia_f.csv", sep = ",")
rede_remocao <- read_network("1.RawData/rede_remocao7.txt", sep = "\t")  
rede_performance_m <- read_network("1.RawData/performace_masculina.csv", sep = ";", dec=",") 

# Quick sanity checks (prints)
message("Dimensions — rede_freq: ", paste(dim(rede_freq), collapse = " x "))
message("Dimensions — rede_deposicao: ", paste(dim(rede_deposicao), collapse = " x "))
message("Dimensions — rede_performance_f: ", paste(dim(rede_performance_f), collapse = " x "))
message("Dimensions — rede_remocao: ", paste(dim(rede_remocao), collapse = " x "))
message("Dimensions — rede_performance_m: ", paste(dim(rede_performance_m), collapse = " x "))

# ---------------------------
# Visualize networks and save PDFs
# ---------------------------
safe_plotweb(rede_freq, file = "3.Figures/rede_freq.pdf", pollinator_groups = pollinator_groups)
safe_plotweb(rede_deposicao, file = "3.Figures/rede_deposicao.pdf", pollinator_groups = pollinator_groups)
safe_plotweb(rede_performance_f, file = "3.Figures/rede_performance_f.pdf", pollinator_groups = pollinator_groups)
safe_plotweb(rede_remocao, file = "3.Figures/rede_remocao.pdf", pollinator_groups = pollinator_groups)
safe_plotweb(rede_performance_m, file = "3.Figures/rede_performance_m.pdf", pollinator_groups = pollinator_groups)

# ---------------------------
# Network-level metrics and null-model testing
# ---------------------------
n_nulls <- 1000

metrics_freq <- compute_metrics(rede_freq)
metrics_depos <- compute_metrics(rede_deposicao)
metrics_perf_f <- compute_metrics(rede_performance_f)
metrics_perf_m <- compute_metrics(rede_performance_m)
metrics_remov <- compute_metrics(rede_remocao)

# Print a concise summary
message("\n=== Summary metrics (observed) ===")
print(list(freq = metrics_freq, depos = metrics_depos, perf_f = metrics_perf_f, perf_m = metrics_perf_m, remov = metrics_remov))

# Test H2 against null models (vaznull) — returns observed, p-value, ci
h2_test_freq <- test_metric_against_null(rede_freq, metric_name = "H2", nnull = n_nulls)
h2_test_perf_f <- test_metric_against_null(rede_performance_f, metric_name = "H2", nnull = n_nulls)
h2_test_perf_m <- test_metric_against_null(rede_performance_m, metric_name = "H2", nnull = n_nulls)

message("\nH2 tests (observed, p-value, 95% CI from nulls):")
message("freq: ", round(h2_test_freq$observed, 4), " p=", round(h2_test_freq$p_value, 4), " CI=", paste(round(h2_test_freq$ci,4), collapse = ", "))
message("perf_f: ", round(h2_test_perf_f$observed, 4), " p=", round(h2_test_perf_f$p_value, 4), " CI=", paste(round(h2_test_perf_f$ci,4), collapse = ", "))
message("perf_m: ", round(h2_test_perf_m$observed, 4), " p=", round(h2_test_perf_m$p_value, 4), " CI=", paste(round(h2_test_perf_m$ci,4), collapse = ", "))

# Weighted NODF (nestedness) test against nulls
wnodf_test_freq <- test_metric_against_null(rede_freq, metric_name = "weighted NODF", nnull = n_nulls)
wnodf_test_perf_f <- test_metric_against_null(rede_performance_f, metric_name = "weighted NODF", nnull = n_nulls)
wnodf_test_perf_m <- test_metric_against_null(rede_performance_m, metric_name = "weighted NODF", nnull = n_nulls)

message("\nWeighted NODF tests:")
message("freq: ", round(wnodf_test_freq$observed, 4), " p=", round(wnodf_test_freq$p_value, 4))
message("perf_f: ", round(wnodf_test_perf_f$observed, 4), " p=", round(wnodf_test_perf_f$p_value, 4))
message("perf_m: ", round(wnodf_test_perf_m$observed, 4), " p=", round(wnodf_test_perf_m$p_value, 4))

# ---------------------------
# Modularity (computeModules / metaComputeModules)
# ---------------------------
# We run metaComputeModules to get a stable estimate, then compare against nulls.
compute_modularity_likelihood <- function(mat, metaN = 5, steps = 1e6, method = "Beckett") {
  mm <- metaComputeModules(mat, N = metaN, method = method, steps = steps)
  # metaComputeModules returns an object with slot likelihood (for bipartite's module object)
  val <- mm@likelihood
  return(list(meta = mm, likelihood = val))
}

message("\nComputing modularity (may take time)...")
mod_freq <- compute_modularity_likelihood(rede_freq, metaN = 5, steps = 1e6)
mod_perf_f <- compute_modularity_likelihood(rede_performance_f, metaN = 5, steps = 1e6)
mod_perf_m <- compute_modularity_likelihood(rede_performance_m, metaN = 5, steps = 1e6)

message("Modularity likelihoods (observed):")
message("freq: ", mod_freq$likelihood)
message("perf_f: ", mod_perf_f$likelihood)
message("perf_m: ", mod_perf_m$likelihood)

# Null-model modularity distribution (on null matrices)
nulls_freq <- nullmodel(rede_freq, N = n_nulls, method = "vaznull")
like_nulls_freq <- sapply(nulls_freq, function(x) {
  mm <- tryCatch(computeModules(x, method = "Beckett", steps = 1e6), error = function(e) NULL)
  if (!is.null(mm)) return(mm@likelihood) else return(NA)
})
like_nulls_freq <- na.omit(like_nulls_freq)

nulls_perf_f <- nullmodel(rede_performance_f, N = n_nulls, method = "vaznull")
like_nulls_perf_f <- sapply(nulls_perf_f, function(x) {
  mm <- tryCatch(computeModules(x, method = "Beckett", steps = 1e6), error = function(e) NULL)
  if (!is.null(mm)) return(mm@likelihood) else return(NA)
})
like_nulls_perf_f <- na.omit(like_nulls_perf_f)

nulls_perf_m <- nullmodel(rede_performance_m, N = n_nulls, method = "vaznull")
like_nulls_perf_m <- sapply(nulls_perf_m, function(x) {
  mm <- tryCatch(computeModules(x, method = "Beckett", steps = 1e6), error = function(e) NULL)
  if (!is.null(mm)) return(mm@likelihood) else return(NA)
})
like_nulls_perf_m <- na.omit(like_nulls_perf_m)

message("\nModularity null-model 95% CI (likelihood):")
message("freq: ", paste(round(quantile(like_nulls_freq, c(0.025, 0.975)), 4), collapse = " - "))
message("perf_f: ", paste(round(quantile(like_nulls_perf_f, c(0.025, 0.975)), 4), collapse = " - "))
message("perf_m: ", paste(round(quantile(like_nulls_perf_m, c(0.025, 0.975)), 4), collapse = " - "))

tryCatch({
  plotModuleWeb(mod_freq$meta)
  pdf("3.Figures/modularity_freq.pdf", width = 12, height = 8)
  plotModuleWeb(mod_freq$meta)
  dev.off()
}, error = function(e) message("Could not plot module web for freq: ", e$message))

library(ggplot2)
library(reshape2)

plot_network_metrics <- function(metrics_list, save = FALSE, output_dir = "3.Output/plots") {
   
  df <- do.call(rbind, lapply(metrics_list, function(x) unlist(x)))
  df <- as.data.frame(df)
  df$network <- rownames(df)
  
  df_long <- melt(df, id.vars = "network", variable.name = "metric", value.name = "value")
  
  p <- ggplot(df_long, aes(x = network, y = value, fill = metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal(base_size = 13) +
    labs(x = "Network", y = "Value", fill = "Metric",
         title = "Comparison of Network Metrics Across Networks") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  
   if (save) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    ggsave(file.path(output_dir, "network_metrics_comparison.png"), plot = p, width = 10, height = 6, dpi = 300)
  }
  
  return(p)
}

metrics_list <- list(
  freq      = compute_metrics(rede_freq),
  dep       = compute_metrics(rede_deposicao),
  perf_f    = compute_metrics(rede_performance_f),
  perf_m    = compute_metrics(rede_performance_m),
  rem       = compute_metrics(rede_remocao)
)

p <- plot_network_metrics(metrics_list)
p

# ---------------------------
# Save a RData with computed results 
# ---------------------------
save(metrics_freq, metrics_depos, metrics_perf_f, metrics_perf_m, metrics_remov,
     h2_test_freq, h2_test_perf_f, h2_test_perf_m,
     wnodf_test_freq, wnodf_test_perf_f, wnodf_test_perf_m,
     mod_freq, mod_perf_f, mod_perf_m,
     file = "network_metrics_summary.RData")
