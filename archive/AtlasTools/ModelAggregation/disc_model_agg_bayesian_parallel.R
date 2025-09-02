library(brms)
library(lme4)
library(bayesplot)
library(tidybayes)
library(ggridges)
library(glue)
library(tidyverse)
library(future.apply)

disc <- fread("../../SEACAR_Trend_Analyses/WQ_Cont_Discrete/output/WQ_Discrete_All_KendallTau_Stats.txt", sep='|') %>%
  filter(SufficientData) %>%
  # select(ManagedAreaName, ParameterName, RelativeDepth, ActivityType, N_Years, N_Data, tau, p, SennSlope, SennIntercept, ub, lb, Trend) %>%
  mutate(SamplingFrequency="Discrete",
         se = (ub - lb)/3.92) %>% distinct()

websiteParams <- fread("../../SEACAR_Trend_Analyses/WQ_Cont_Discrete/data/WebsiteParameters.csv") %>% 
  select(IndicatorName, ParameterName, ParameterShort, RelativeDepth, ActivityType, SamplingFrequency, Website)

disc_model_results <- left_join(disc, websiteParams %>% select(-Website), by=c("ParameterName", "RelativeDepth", "ActivityType", "SamplingFrequency")) %>%
  arrange(IndicatorName, ParameterShort) %>% 
  filter(Website==1) %>% as.data.table()

fwrite(disc_model_results, file = "output/disc_model_results.csv")

library(future.apply)
plan(multisession)

# Create a list of (ma, indicator) pairs to iterate over
ma_indicator_list <- disc_model_results %>%
  filter(IndicatorName != "Water Quality") %>%
  distinct(ManagedAreaName, IndicatorName) %>%
  split(., seq(nrow(.)))

# Define function to process each pair
fit_bayes_model <- function(row){
  library(data.table)
  library(stats)
  library(brms)
  library(tidyverse)
  ma <- row$ManagedAreaName
  indicator <- row$IndicatorName
  
  ma_abrev <- MA_All[ManagedAreaName == ma, Abbreviation]
  indicator_short <- ifelse(indicator == "Water Clarity", "WC", "NUT")
  fileName <- paste0("output/models/", ma_abrev, "_", indicator_short, "_model")
  
  subset <- disc_model_results %>%
    filter(ManagedAreaName == ma, IndicatorName == indicator)
  if(ma_abrev=="RBSPAP"){
    subset <- subset %>% filter(!ParameterName=="Chlorophyll a, Uncorrected for Pheophytin")
  }
  
  if(!file.exists(paste0(fileName, ".rds"))) {
    b_mod <- brm(
      tau | se(se) ~ 1 + (1 | ParameterName),
      data = subset,
      iter = 6000, warmup = 1000, chains = 4, cores = 4, threads = threading(3),
      control = list(adapt_delta = 0.999, max_treedepth = 20),
      file = fileName
    )
  } else {
    b_mod <- readRDS(paste0(fileName, ".rds"))
  }
  
  return(list(
    ManagedAreaName = ma,
    IndicatorName = indicator,
    Model = b_mod
  ))
}

# Run in parallel
model_results_list <- future_lapply(ma_indicator_list, fit_bayes_model)

# Nest results into bayes_model_results
bayes_model_results <- list()
for(res in model_results_list){
  bayes_model_results[[res$ManagedAreaName]][[res$IndicatorName]] <- res$Model
}

# Perform hypothesis testing
hypothesis_results <- data.table()
for(ma in names(bayes_model_results)){
  for(indicator in names(bayes_model_results[[ma]])){
    mod <- bayes_model_results[[ma]][[indicator]]
    # What is the probability that our aggregate trend is increasing?
    h_pos <- hypothesis(mod, "Intercept > 0.0")
    h_results <- data.table(
      "ManagedAreaName" = ma,
      "IndicatorName" = indicator,
      "IncreasingProb" = round(h_pos$hypothesis$Post.Prob,2),
      "DecreasingProb" = round(1-h_pos$hypothesis$Post.Prob,2),
      "Estimate" = round(h_pos$hypothesis$Estimate,3),
      "Est.Error" = round(h_pos$hypothesis$Est.Error,3),
      "CI.Lower" = round(h_pos$hypothesis$CI.Lower,3),
      "CI.Upper" = round(h_pos$hypothesis$CI.Upper,3),
      "Parameters" = paste(sort(unique(mod$data$ParameterName)), collapse = "|")
    )
    hypothesis_results <- bind_rows(hypothesis_results, h_results)
  }
}

hypothesis_results <- hypothesis_results %>% mutate(
  Trend = ifelse(IncreasingProb>=0.8, 1, ifelse(DecreasingProb>=0.8, -1, 0))
)

openxlsx::write.xlsx(hypothesis_results, file = "output/WQ_agg_results.xlsx")

# Plot
plot_data <- hypothesis_results %>% group_by(Trend, IndicatorName) %>% summarise(n=n())
plot_data$Trend <- factor(plot_data$Trend, levels = c("-1", "0", "1"),
                          labels = c("Decreasing", "No Trend", "Increasing"))

plot <- ggplot(plot_data, aes(x = IndicatorName, y = n, fill = Trend)) +
  geom_col(position = "dodge") +
  labs(
    x = "Indicator",
    y = "Count of Trends",
    fill = "Trend Direction",
    title = "Trend Counts by Indicator"
  ) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(
    values = c("Decreasing" = "#D95F02", "No Trend" = "#7570B3", "Increasing" = "#1B9E77")
  ) +
  theme_bw()

ggsave(plot = plot, filename="output/Discrete_AggResults.png")

## Generate trend text
rmarkdown::render("Discrete_TrendStatus.Rmd")















bayes_model_results <- list()
# Loop through and apply model to each MA and indicator
for(ma in unique(disc_model_results$ManagedAreaName)){
  print(ma)
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  ma_data <- disc_model_results %>% filter(ManagedAreaName==ma)
  for(indicator in unique(ma_data$IndicatorName)){
    if(indicator=="Water Quality") next
    print(indicator)
    # Short indicator name for file name display
    indicator_short <- ifelse(indicator=="Water Clarity", "WC", "NUT")
    # model filename
    fileName <- paste0("output/models/",ma_abrev, "_", indicator_short, "_model")
    subset <- ma_data %>% filter(IndicatorName==indicator)
    if(!exists(fileName)){
      b_mod <- brm(
        tau | se(se) ~ 1 + (1 | ParameterName),
        data = subset,
        iter = 6000, warmup = 1000, chains = 4, cores = 4, threads = threading(3),
        control = list(adapt_delta = 0.999, max_treedepth=20),
        file = fileName
      )      
    } else {
      b_mod <- readRDS(fileName)
    }
    bayes_model_results[[ma]][[indicator]] <- b_mod
  }
}

hypothesis_results <- data.frame()
for(ma in names(bayes_model_results)){
  for(indicator in names(bayes_model_results[[ma]])){
    mod <- bayes_model_results[[ma]][[indicator]]
    # What is the probability that our aggregate trend is increasing?
    h_pos <- hypothesis(mod, "Intercept > 0.0")
    h_results <- data.frame(
      "ManagedAreaName" = ma,
      "IndicatorName" = indicator,
      "IncreasingProb" = round(h_pos$hypothesis$Post.Prob,2),
      "DecreasingProb" = round(1-h_pos$hypothesis$Post.Prob,2),
      "Estimate" = round(h_pos$hypothesis$Estimate,3),
      "Est.Error" = round(h_pos$hypothesis$Est.Error,3),
      "CI.Lower" = round(h_pos$hypothesis$CI.Lower,3),
      "CI.Upper" = round(h_pos$hypothesis$CI.Upper,3)
    )
    hypothesis_results <- bind_rows(hypothesis_results, h_results)
  }
}

hypothesis_results <- hypothesis_results %>% mutate(
  Trend = ifelse(IncreasingProb>=0.8, 1, ifelse(DecreasingProb>=0.8, -1, 0))
)







### PLOTTING RESULTS
skt_stats_disc <- readRDS("../../SEACAR_Trend_Analyses/WQ_Cont_Discrete/output/tables/disc/skt_stats_disc.rds")
db_thresholds <- setDT(openxlsx::read.xlsx("../../IndicatorQuantiles/output/ScriptResults/Database_Thresholds.xlsx", startRow = 6))
db_thresholds <- db_thresholds[Habitat=="Water Column" & IndicatorName!="Nekton" & 
                                 ParameterName!=	"Fluorescent dissolved organic matter, FDOM" &
                                 ThresholdID!=31, 
                               c("CombinedTable", "ParameterName", "LowQuantile", "HighQuantile")]

set_view_window <- function(plot_data, type = "Discrete WQ"){
  params <- unique(plot_data$ParameterName)
  # Invert sign for Secchi Depth calculation
  if("Secchi Depth" %in% params){plot_data$Mean <- -plot_data$Mean}
  # Grab min and max Y values from data
  miny <- min(plot_data$Mean)
  maxy <- max(plot_data$Mean)
  # Grab quantile limits for a given parameter
  low_q <- min(db_thresholds[ParameterName %in% params & CombinedTable==type, LowQuantile])
  high_q <- max(db_thresholds[ParameterName %in% params & CombinedTable==type, HighQuantile])
  # Is the lowest value below quantile limit?
  if(miny<low_q){
    # If so, use the next lowest value not below limit
    miny <- plot_data %>% filter(!Mean < low_q) %>% pull(Mean) %>% min()
  }
  # Is the highest value above quantile limit?
  if(maxy>high_q){
    # If so, use the next highest value not above limit
    maxy <- plot_data %>% filter(!Mean > high_q) %>% pull(Mean) %>% max()
  }
  # Uninvert sign for Secchi Depth (flip min and max, ensure 0 at top)
  if("Secchi Depth" %in% params){
    miny <- -maxy
    maxy <- 0
  }
  return(c(miny, maxy))
}

all_mods <- list.files("output/models", full.names=T)
combined_plots <- list()
forest_plots <- list()
for(ma in MA_All$ManagedAreaName[1:15]){
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  if(ma_abrev %in% c("BBCFMCAP","CRCP", "CBAP")) next
  for(indicator in c("Nutrients", "Water Clarity")){
    small_ind <- ifelse(indicator=="Nutrients", "NUT", "WC")
    mod <- readRDS(str_subset(all_mods, paste0(ma_abrev, "_", small_ind, "_model")))
    og_models <- disc_model_results %>% 
      filter(ManagedAreaName==ma, IndicatorName==indicator) %>% as.data.table()
    ma_subset <- data.table()
    ma_skt <- data.table()
    for(i in 1:nrow(og_models)){
      parameter <- og_models[i]$ParameterName
      p <- websiteParams[ParameterName==parameter, unique(ParameterShort)]
      a <- og_models[i]$ActivityType
      d <- og_models[i]$RelativeDepth
      data <- readRDS(paste0("../../SEACAR_Trend_Analyses/WQ_Cont_Discrete/output/figuredata/discrete_", ma_abrev, "_", p, ".rds"))
      skt <- skt_stats_disc %>% filter(ManagedAreaName==ma, ParameterName==parameter, 
                                       ActivityType==a, RelativeDepth==d)
      ma_subset <- bind_rows(ma_subset, data)
      ma_skt <- bind_rows(ma_skt, skt)
    }
    
    ## Combined Plots
    plot <- ggplot(ma_subset, aes(x = YearMonthDec, y = Mean)) +
      geom_point(aes(color = ParameterName)) +
      coord_cartesian(ylim = set_view_window(ma_subset, "Discrete WQ"))
    
    for(p in unique(og_models$ParameterName)){
      plot <- plot +
        geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, color = ParameterName),
                     linewidth = 1.2, alpha = 0.7, show.legend = TRUE)
    }
    combined_plots[[ma]][[indicator]] <- plot
    
    ## Forest Plots
    post.samples <- posterior_samples(mod, c("^b", "^sd"))
    names(post.samples) <- c("smd","tau")
    
    study.draws <- spread_draws(mod, r_ParameterName[ParameterName,], b_Intercept) %>%
      mutate(b_Intercept = r_ParameterName + b_Intercept)
    
    pooled.effect.draws <- spread_draws(mod, b_Intercept) %>%
      mutate(ParameterName = "Pooled Effect")
    
    forest.data <- bind_rows(study.draws,
                             pooled.effect.draws) %>%
      ungroup() %>%
      mutate(ParameterName = str_replace_all(ParameterName, "[.]", " ")) %>%
      mutate(ParameterName = reorder(ParameterName, b_Intercept))
    
    forest.data.summary <- group_by(forest.data, ParameterName) %>%
      mean_qi(b_Intercept)
    
    forest_plot <- ggplot(aes(b_Intercept,
                              relevel(ParameterName, "Pooled Effect",
                                      after = Inf)),
                          data = forest.data) +
      # Add vertical lines for pooled effect and CI
      geom_vline(xintercept = fixef(mod)[1, 1],
                 color = "grey", size = 1) +
      geom_vline(xintercept = fixef(mod)[1, 3:4],
                 color = "grey", linetype = 2) +
      geom_vline(xintercept = 0, color = "black",
                 size = 1) +
      # Add densities
      geom_density_ridges(fill = "blue",
                          rel_min_height = 0.01,
                          col = NA, scale = 1,
                          alpha = 0.8) +
      geom_pointintervalh(data = forest.data.summary,
                          size = 1) +
      # Add text and labels
      geom_text(data = mutate_if(forest.data.summary,
                                 is.numeric, round, 2),
                aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"),
                    x = Inf), hjust = "inward") +
      labs(x = "Standardized Mean Difference", # summary measure
           y = element_blank()) +
      theme_minimal()
    forest_plots[[ma]][[indicator]] <- forest_plot
  }
}



forest_plots[["Alligator Harbor Aquatic Preserve"]][["Water Clarity"]]
forest_plots[["Alligator Harbor Aquatic Preserve"]][["Nutrients"]]
forest_plots[["Apalachicola Bay Aquatic Preserve"]][["Water Clarity"]]
forest_plots[["Apalachicola Bay Aquatic Preserve"]][["Nutrients"]]
forest_plots[["Apalachicola National Estuarine Research Reserve"]][["Water Clarity"]]
forest_plots[["Apalachicola National Estuarine Research Reserve"]][["Nutrients"]]
forest_plots[["Banana River Aquatic Preserve"]][["Water Clarity"]]
forest_plots[["Banana River Aquatic Preserve"]][["Nutrients"]]
forest_plots[["Big Bend Seagrasses Aquatic Preserve"]][["Water Clarity"]]
forest_plots[["Big Bend Seagrasses Aquatic Preserve"]][["Nutrients"]]








ma <- "Alligator Harbor Aquatic Preserve"
ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
indicator <- "Nutrients"

mod <- bayes_model_results[[ma]][[indicator]]
og_models <- disc_model_results %>% 
  filter(ManagedAreaName==ma, IndicatorName==indicator) %>% as.data.table()

ma_subset <- data.table()
ma_skt <- data.table()
for(i in 1:nrow(og_models)){
  parameter <- og_models[i]$ParameterName
  p <- websiteParams[ParameterName==parameter, unique(ParameterShort)]
  a <- og_models[i]$ActivityType
  d <- og_models[i]$RelativeDepth
  data <- readRDS(paste0("../../SEACAR_Trend_Analyses/WQ_Cont_Discrete/output/figuredata/discrete_", ma_abrev, "_", p, ".rds"))
  skt <- skt_stats_disc %>% filter(ManagedAreaName==ma, ParameterName==parameter, 
                                   ActivityType==a, RelativeDepth==d)
  ma_subset <- bind_rows(ma_subset, data)
  ma_skt <- bind_rows(ma_skt, skt)
}

plot <- ggplot(ma_subset, aes(x = YearMonthDec, y = Mean)) +
  geom_point(aes(color = ParameterName)) +
  coord_cartesian(ylim = set_view_window(ma_subset, "Discrete WQ"))

for(p in unique(og_models$ParameterName)){
  plot <- plot +
    geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, color = ParameterName),
                 linewidth = 1.2, alpha = 0.7, show.legend = TRUE)
}


###############################
###############################
###############################
###############################
###############################
###############################
post.samples <- posterior_samples(mod, c("^b", "^sd"))
names(post.samples) <- c("smd","tau")

study.draws <- spread_draws(mod, r_ParameterName[ParameterName,], b_Intercept) %>%
  mutate(b_Intercept = r_ParameterName + b_Intercept)

pooled.effect.draws <- spread_draws(mod, b_Intercept) %>%
  mutate(ParameterName = "Pooled Effect")

forest.data <- bind_rows(study.draws,
                         pooled.effect.draws) %>%
  ungroup() %>%
  mutate(ParameterName = str_replace_all(ParameterName, "[.]", " ")) %>%
  mutate(ParameterName = reorder(ParameterName, b_Intercept))

forest.data.summary <- group_by(forest.data, ParameterName) %>%
  mean_qi(b_Intercept)

ggplot(aes(b_Intercept,
           relevel(ParameterName, "Pooled Effect",
                   after = Inf)),
       data = forest.data) +

  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(mod)[1, 1],
             color = "grey", size = 1) +
  geom_vline(xintercept = fixef(mod)[1, 3:4],
             color = "grey", linetype = 2) +
  geom_vline(xintercept = 0, color = "black",
             size = 1) +

  # Add densities
  geom_density_ridges(fill = "blue",
                      rel_min_height = 0.01,
                      col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointintervalh(data = forest.data.summary,
                      size = 1) +

  # Add text and labels
  geom_text(data = mutate_if(forest.data.summary,
                             is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"),
                x = Inf), hjust = "inward") +
  labs(x = "Standardized Mean Difference", # summary measure
       y = element_blank()) +
  theme_minimal()
# 
# 
# 
# # bayes_model3 <- brm(
# #   tau | se(se) ~ 1 + (1 | N_Years),
# #   data = subset,
# #   iter = 4000, warmup = 1000, chains = 4, cores = 4,
# #   control = list(adapt_delta = 0.999, max_treedepth=20),
# #   prior = c(prior(normal(0,1), class = Intercept),
# #             prior(cauchy(0,0.5), class = sd))
# # )
# # bayes_model4 <- brm(
# #   tau | se(se) ~ 1 + (1 | N_Years),
# #   data = subset,
# #   iter = 4000, warmup = 1000, chains = 4, cores = 4,
# #   control = list(adapt_delta = 0.999, max_treedepth=20)
# # )
# # pp_check(bayes_model4)
