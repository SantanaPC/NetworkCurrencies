
############################################################
#        Species-level analysis of plant-pollinator       #
#                 interactions networks                  #
############################################################

# ---- Libraries ----
library(bipartite)
library(ggplot2)

# ---- Load networks ----
# Matrices: rows = plants, columns = pollinators
rede_freq<- read.csv2("1.RawData/rede_frequencia.csv", row.names = 1, sep=",")
rede_deposicao<- read.csv2("1.RawData/rede_deposicao3.csv", row.names = 1)
rede_performance_f<- read.csv2("1.RawData/rede_eficacia_f.csv", row.names = 1, sep=",")
rede_performance_m<- read.csv2("1.RawData/performace_masculina.csv", row.names = 1, , dec=",")
rede_remocao<- read.table("1.RawData/rede_remocao7.txt", header = TRUE, row.names = 1, , sep = "\t")

# ---- Species-level metrics ----
# d' = specialization, species strength = contribution to network
compute_species_metrics <- function(mat) {
  specieslevel(mat, index = c("d", "species strength"))
}

species_metrics <- list(
  freq      = compute_species_metrics(rede_freq),
  dep       = compute_species_metrics(rede_deposicao),
  perf_f    = compute_species_metrics(rede_performance_f),
  perf_m    = compute_species_metrics(rede_performance_m),
  rem       = compute_species_metrics(rede_remocao)
)

# ---- Load Z-scores (precomputed) ----
z_plant <- read.csv2("1.RawData/z_score_plant.csv", row.names = 1)
z_bee   <- read.csv2("1.RawData/z_score_bee.csv", row.names = 1)

# ---- Paired t-tests for bees ----
t_tests_bee <- list(
  strength_freq_vs_perf_m = t.test(z_bee$z_strength_freq, z_bee$z_strength_perf_m, paired = TRUE),
  strength_freq_vs_perf_f = t.test(z_bee$z_strength_freq, z_bee$z_strength_perf_f, paired = TRUE),
  strength_perf_f_vs_perf_m = t.test(z_bee$z_strength_perf_f, z_bee$z_strength_perf_m, paired = TRUE),
  d_freq_vs_perf_f = t.test(z_bee$z_d_freq, z_bee$z_d_perf_f, paired = TRUE),
  d_freq_vs_perf_m = t.test(z_bee$z_d_freq, z_bee$z_d_perf_m, paired = TRUE),
  d_perf_f_vs_perf_m = t.test(z_bee$z_d_perf_f, z_bee$z_d_perf_m, paired = TRUE)
)

# ---- Correlations for bees ----
indices <- read.table("indices_especies.txt", header = TRUE)
cor_tests_bee <- list(
  strength_freq_vs_perf_f = cor.test(indices$z_strength_freq, indices$z_strength_perf_f),
  strength_freq_vs_perf_m = cor.test(indices$z_strength_freq, indices$z_strength_perf_m),
  strength_perf_f_vs_perf_m = cor.test(indices$z_strength_perf_f, indices$z_strength_perf_m),
  d_freq_vs_perf_f = cor.test(indices$z_d_freq, indices$z_d_perf_f),
  d_freq_vs_perf_m = cor.test(indices$z_d_freq, indices$z_d_perf_m),
  d_perf_f_vs_perf_m = cor.test(indices$z_d_perf_f, indices$z_d_perf_m),
  c_freq_vs_perf_fem = cor.test(indices$c_freq, indices$c_perf_fem),
  c_freq_vs_perf_masc = cor.test(indices$c_freq, indices$c_perf_masc),
  c_perf_fem_vs_perf_masc = cor.test(indices$c_perf_fem, indices$c_perf_masc),
  z_freq_vs_perf_fem = cor.test(indices$z_freq, indices$z_perf_fem),
  z_freq_vs_perf_masc = cor.test(indices$z_freq, indices$z_perf_masc),
  z_perf_fem_vs_perf_masc = cor.test(indices$z_perf_fem, indices$z_perf_masc)
)

# ---- Paired t-tests for plants ----
t_tests_plant <- list(
  strength_freq_vs_perf_m = t.test(z_plant$z_strength_freq, z_plant$z_strength_perf_m, paired = TRUE),
  strength_freq_vs_perf_f = t.test(z_plant$z_strength_freq, z_plant$z_strength_perf_f, paired = TRUE),
  strength_perf_f_vs_perf_m = t.test(z_plant$z_strength_perf_f, z_plant$z_strength_perf_m, paired = TRUE),
  d_freq_vs_perf_m = t.test(z_plant$z_d_freq, z_plant$z_d_perf_m, paired = TRUE),
  d_freq_vs_perf_f = t.test(z_plant$z_d_freq, z_plant$z_d_perf_f, paired = TRUE),
  d_perf_f_vs_perf_m = t.test(z_plant$z_d_perf_f, z_plant$z_d_perf_m, paired = TRUE)
)

# ---- Plot species-level metrics ----
plot_species_metrics <- function(z_scores, network_names = c("freq","dep","perf_f","perf_m","rem"), save_dir="3.Output/plots/species_metrics") {
  
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  metrics <- c("z_strength", "z_d")
  metric_labels <- c("Species Strength", "Specialization (d')")
  
  plots <- list()
  
  for (i in seq_along(metrics)) {
    metric <- metrics[i]
    df_long <- data.frame(
      species = rownames(z_scores),
      value = z_scores[[paste0(metric,"_freq")]],
      network_freq = "freq",
      value_perf_f = z_scores[[paste0(metric,"_perf_f")]],
      network_perf_f = "perf_f",
      value_perf_m = z_scores[[paste0(metric,"_perf_m")]],
      network_perf_m = "perf_m"
    )
    
    # Transform to long format
    df_long <- reshape2::melt(df_long, id.vars = "species", measure.vars = c("value","value_perf_f","value_perf_m"), variable.name = "network_metric", value.name = "value")
    df_long$network <- rep(c("freq","perf_f","perf_m"), each=nrow(z_scores))
    
    p <- ggplot(df_long, aes(x=species, y=value, fill=network)) +
      geom_bar(stat="identity", position="dodge") +
      theme_minimal(base_size=12) +
      labs(x="Species", y=metric_labels[i], fill="Network") +
      theme(axis.text.x = element_text(angle=45, hjust=1))
    
    plots[[metric]] <- p
    ggsave(filename = file.path(save_dir, paste0(metric,".png")), plot = p, width = 10, height = 6, dpi = 300)
  }
  
  return(plots)
}

# Generate plots
plots_bee <- plot_species_metrics(z_bee, save_dir = "3.Figures/plots/species_metrics/bee")
plots_plant <- plot_species_metrics(z_plant, save_dir = "3.Figures/plots/species_metrics/plant")

# ---- Compute c and z values for modularity ----
library(parallel)

compute_modules_cz <- function(mat, network_name = "freq", save_dir = "3.Output/modules", steps = 10E7, n_meta = 5) {
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("Computing modules for network: ", network_name)
  
  # Compute modules using metaComputeModules (robust estimate)
  mod <- metaComputeModules(mat, N = n_meta, method = "Beckett", steps = steps)
  
  # Extract likelihood index
  mod_likelihood <- mod@likelihood
  message("Modularity likelihood: ", round(mod_likelihood, 4))
  
  # Compute c and z values for species roles
  cz_higher <- czvalues(mod, level = "higher")  # pollinators
  cz_lower  <- czvalues(mod, level = "lower")   # plants
  
  # Save c/z tables
  write.csv(cz_higher, file.path(save_dir, paste0("czvalues_pollinators_", network_name, ".csv")))
  write.csv(cz_lower,  file.path(save_dir, paste0("czvalues_plants_", network_name, ".csv")))
  
  # Optionally plot module network
  pdf(file.path(save_dir, paste0("module_web_", network_name, ".pdf")), width = 12, height = 10)
  plotModuleWeb(mod)
  dev.off()
  
  return(list(modularity = mod_likelihood, cz_higher = cz_higher, cz_lower = cz_lower))
}

mod_freq <- compute_modules_cz(rede_freq, network_name = "freq")
mod_dep  <- compute_modules_cz(rede_deposicao, network_name = "dep")
mod_perf_f <- compute_modules_cz(rede_performance_f, network_name = "perf_f")
mod_perf_m <- compute_modules_cz(rede_performance_m, network_name = "perf_m")
mod_rem <- compute_modules_cz(rede_remocao, network_name = "rem")

# ---- Compute modules and species roles (c/z) for all networks ----

networks <- list(
  freq      = rede_freq,
  dep       = rede_deposicao,
  perf_f    = rede_performance_f,
  perf_m    = rede_performance_m,
  rem       = rede_remocao
)

mod_results <- list()
cz_results <- list()

for (net_name in names(networks)) {
  message("Processing network: ", net_name)
  
  # Compute modules and c/z values
  res <- compute_modules_cz(
    mat = networks[[net_name]],
    network_name = net_name,
    save_dir = "3.Output/modules"
  )
  
  # Salvar modularidade
  mod_results[[net_name]] <- res$modularity
  
  # Salvar c/z
  cz_results[[net_name]] <- list(
    higher = res$cz_higher,  # pollinators
    lower  = res$cz_lower    # plants
  )
}

#checking the results
#mod_results$freq
#cz_results$perf_f$higher
#cz_results$rem$lower

plot_cz_scatter_groups <- function(cz_higher, cz_lower, pollinator_groups = NULL,
                                   network_name = "network",
                                   save_dir = "3.Output/plots/cz_scatter_groups") {
  
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  cz_higher_df <- data.frame(
    species = names(cz_higher$c),
    c = unname(cz_higher$c),
    z = unname(cz_higher$z),
    type = "pollinator",
    stringsAsFactors = FALSE
  )
  
  cz_lower_df <- data.frame(
    species = names(cz_lower$c),
    c = unname(cz_lower$c),
    z = unname(cz_lower$z),
    type = "plant",
    stringsAsFactors = FALSE
  )
  
  if(!is.null(pollinator_groups)) {
    cz_higher_df$func_group <- pollinator_groups[cz_higher_df$species]
    cz_higher_df$func_group[is.na(cz_higher_df$func_group)] <- "unknown"
  } else {
    cz_higher_df$func_group <- "unknown"
  }
  
  cz_lower_df$func_group <- "plant"
  
  df_plot <- rbind(cz_higher_df, cz_lower_df)
  
  group_colors <- c(
    "flow_buzz" = "#63077D",
    "ant_buzz"  = "#e72881",
    "theft"     = "#1C796F",
    "robber"    = "#FF6A00",
    "plant"     = "#1C796F",
    "unknown"   = "grey50"
  )
  
  p <- ggplot(df_plot, aes(x = c, y = z, color = func_group)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0.62, linetype = "dashed", color = "gray50") +
    theme_minimal(base_size = 12) +
    labs(title = paste0("C vs Z Scatterplot - ", network_name),
         x = "Participation coefficient (c)",
         y = "Within-module degree (z)",
         color = "Functional group") +
    scale_color_manual(values = group_colors) +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  file_png <- file.path(save_dir, paste0("C_Z_scatter_groups_", network_name, ".png"))
  ggsave(file_png, p, width = 8, height = 6, dpi = 300)
  
  return(p)
}

plot_all_cz <- function(cz_results_list, pollinator_groups = NULL,
                        save_dir = "3.Figures/cz_scatter_groups") {
  
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  plots <- list()
  
  for(network_name in names(cz_results_list)) {
    
    cz_net <- cz_results_list[[network_name]]
    
    cz_higher <- cz_net$higher
    cz_lower  <- cz_net$lower
    
    if(length(cz_higher$c) == 0 | length(cz_lower$c) == 0) {
      warning(paste0("Network ", network_name, " has empty c/z values. Skipping."))
      next
    }
    
    cz_higher_df <- data.frame(
      species = names(cz_higher$c),
      c = unname(cz_higher$c),
      z = unname(cz_higher$z),
      type = "pollinator",
      stringsAsFactors = FALSE
    )
    
    cz_lower_df <- data.frame(
      species = names(cz_lower$c),
      c = unname(cz_lower$c),
      z = unname(cz_lower$z),
      type = "plant",
      stringsAsFactors = FALSE
    )
    
    # Adicionar grupo funcional
    if(!is.null(pollinator_groups)) {
      cz_higher_df$func_group <- pollinator_groups[cz_higher_df$species]
      cz_higher_df$func_group[is.na(cz_higher_df$func_group)] <- "unknown"
    } else {
      cz_higher_df$func_group <- "unknown"
    }
    
    cz_lower_df$func_group <- "plant"
    
    df_plot <- rbind(cz_higher_df, cz_lower_df)
    
    # Definir cores
    group_colors <- c(
      "flow_buzz" = "#63077D",
      "ant_buzz"  = "#e72881",
      "theft"     = "#1C796F",
      "robber"    = "#FF6A00",
      "plant"     = "darkgreen",
      "unknown"   = "grey50"
    )
    
    p <- ggplot(df_plot, aes(x = c, y = z, color = func_group)) +
      geom_point(size = 3, alpha = 0.8) +
      geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = 0.62, linetype = "dashed", color = "gray50") +
      theme_minimal(base_size = 12) +
      labs(title = paste0("C vs Z Scatterplot - ", network_name),
           x = "Participation coefficient (c)",
           y = "Within-module degree (z)",
           color = "Functional group") +
      scale_color_manual(values = group_colors) +
      theme(axis.text.x = element_text(angle=45, hjust=1))
    
    file_png <- file.path(save_dir, paste0("C_Z_scatter_groups_", network_name, ".png"))
    ggsave(file_png, p, width = 8, height = 6, dpi = 300)
    
    plots[[network_name]] <- p
  }
  
  return(plots)
}

plots_all <- plot_all_cz(cz_results, pollinator_groups = pollinator_groups)
