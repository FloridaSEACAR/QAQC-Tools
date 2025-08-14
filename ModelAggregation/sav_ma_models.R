library(lme4)
library(nlme)
library(glue)
library(tidyverse)
library(data.table)

##### SAV Model aggregation overview #####
## Isolate MAs with Total Seagrass/Total SAV values.
## 1.) In these MAs default to using Total SAV > Total Seagrass where available, if not use Total Seagrass.
## 2.) For all others: exclude "Drift algae" and "No grass in quadrat" model results.
## 3.) If only 1 species remains, use that species' model result as the "aggregate" value.
## 4.) If more than 1 species remains, run the model aggregation below.

# Read in stats outputs
sav <- openxlsx::read.xlsx("../../SEACAR_Trend_Analyses/SAV/output/website/SAV_BBpct_LMEresults_All.xlsx") %>% distinct()

# All SAV managed areas
managedareas <- unique(sav$ManagedAreaName)

# Managed areas with Total SAV or Total Seagrass
# total_ma <- sav %>% filter(Species %in% c("Total SAV", "Total seagrass"))
# # Temp fix (too many entries for St Andrews due to name change [SAAP -> SBAP])
# total_ma <- total_ma[1:28,]
# sav_ma <- total_ma %>% filter(Species=="Total SAV")
# seagrass_ma <- total_ma %>% filter(!ManagedAreaName %in% unique(sav_ma$ManagedAreaName))
# total_ma_combined <- bind_rows(sav_ma, seagrass_ma)

# Create subset of MAs with successful models
ma_subset <- MA_All[ManagedAreaName %in% managedareas]
sav_mod_locs <- list.files("../../SEACAR_Trend_Analyses/SAV/output/models/", pattern = "SAV_BBpct_", full.names=T)
failedmodslist <- readRDS("../../SEACAR_Trend_Analyses/SAV/output/models/failedmodslist.rds")
# Exclude Drift Algae, Total Seagrass, and Total SAV from aggregate model
short_sp_to_exlcude <- c("DrAl", "ToSe", "ToSa")

#Managed areas that should have Halophila species combined:
ma_halspp <- c("Banana River Aquatic Preserve", "Indian River-Malabar to Vero Beach Aquatic Preserve", 
               "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve", "Jensen Beach to Jupiter Inlet Aquatic Preserve",
               "Loxahatchee River-Lake Worth Creek Aquatic Preserve", "Mosquito Lagoon Aquatic Preserve", 
               "Biscayne Bay Aquatic Preserve", "Florida Keys National Marine Sanctuary")

all_data <- data.frame()
sav_results <- data.frame()
for(ma in managedareas){
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  ma_mods <- str_subset(sav_mod_locs, ma_abrev)
  ma_data <- data.frame()
  for(m in ma_mods){
    filename <- tail(str_split_1(m, "/"),1)
    if(filename %in% failedmodslist$model) next
    sp <- str_split_1(tail(str_split_1(filename, "_"),1), ".rds")[1]
    if(sp %in% short_sp_to_exlcude) next
    mod <- readRDS(m)
    # Ensure that MAs with halophila species combined use the correct column
    if(ma %in% ma_halspp){
      column <- as.name("analysisunit")
    } else {
      column <- as.name("analysisunit_halid")
    }
    # Store raw data for each species within the MA, convert common names to match previous model results
    temp_data <- mod$data
    temp_data <- temp_data %>% mutate(
      common_name = case_when(
        get(column) == "Halodule wrightii" ~ "Shoal grass",
        get(column) == "Syringodium filiforme" ~ "Manatee grass",
        get(column) == "Thalassia testudinum" ~ "Turtle grass",
        get(column) == "Ruppia maritima" ~ "Widgeon grass",
        get(column) == "Halophila johnsonii" ~ "Johnson's seagrass",
        get(column) == "Halophila decipiens" ~ "Paddle grass",
        get(column) == "Halophila engelmannii" ~ "Star grass",
        .default = as.character(get(column))
      )
    )
    ma_data <- bind_rows(ma_data, temp_data)
  }
  # Only run model when there are >1 species available
  if(length(unique(ma_data$analysisunit))<2){
    temp_ma <- sav %>% filter(ManagedAreaName==ma, 
                              Species %in% unique(ma_data$common_name))
    
    ma_model_results <- data.frame(
      "ManagedAreaName" = ma,
      "Intercept" = temp_ma$LME_Intercept,
      "Slope" = temp_ma$LME_Slope,
      "p" = round(temp_ma$p,6),
      "Source" = unique(ma_data$common_name),
      "SpIncluded" = unique(ma_data$common_name)
    )
    # Store model result overviews
    sav_results <- bind_rows(sav_results, ma_model_results)
  }
  # Run a fixed effects model for each MA, using species (common_name) as factor
  model_fixed <- try(nlme::lme(BB_pct ~ relyear + common_name, 
                               random = list(SiteIdentifier = ~relyear),
                               control = list(msMaxIter = 1000, msMaxEval = 1000, 
                                              sing.tol=1e-20),
                               na.action = na.omit,
                               data = ma_data),
                     silent = TRUE)
  species_included <- paste(unique(ma_data$common_name), collapse = "|")
  
  if(class(try(model_fixed)) != "try-error"){
    ma_mod_results <- broom.mixed::tidy(model_fixed) %>% filter(effect == "fixed") %>% as.data.table()
    ma_model_results <- data.frame(
      "ManagedAreaName" = ma,
      "Intercept" = ma_mod_results[term=="(Intercept)", estimate],
      "Slope" = ma_mod_results[term=="relyear", estimate],
      "p" = round(ma_mod_results[term=="relyear", p.value],6),
      "Source" = "aggregate",
      "SpIncluded" = species_included
    )
    
    # Store model result overviews
    sav_results <- bind_rows(sav_results, ma_model_results)
  } else if(!ma %in% unique(sav_results$ManagedAreaName)){
    avail_sp <- sav %>% filter(ManagedAreaName==ma) %>% pull(Species)
    if(any(c("Total SAV", "Total Seagrass") %in% avail_sp)){
      if("Total SAV" %in% avail_sp){
        temp_ma <- sav %>% filter(ManagedAreaName==ma, Species=="Total SAV")
      } else if("Total Seagrass" %in% avail_sp){
        temp_ma <- sav %>% filter(ManagedAreaName==ma, Species=="Total Seagrass")
      }
      ma_model_results <- data.frame(
        "ManagedAreaName" = ma,
        "Intercept" = temp_ma$LME_Intercept,
        "Slope" = temp_ma$LME_Slope,
        "p" = round(temp_ma$p,6),
        "Source" = unique(temp_ma$Species),
        "SpIncluded" = unique(temp_ma$Species)
      ) 
    } else {
      ma_model_results <- data.frame(
        "ManagedAreaName" = ma,
        "Intercept" = NA,
        "Slope" = NA,
        "p" = NA,
        "Source" = NA,
        "SpIncluded" = NA
      )
    }
    
    # Store model result overviews for Total SAV, Total Seagrass, or NA results
    sav_results <- bind_rows(sav_results, ma_model_results)
  }
  
  # Store all data for each MA
  all_data <- bind_rows(all_data, ma_data)
  
  # Extract results and compile
  # model_summary <- summary(model_fixed)
  # ma_model_results <- data.frame(
  #   "ManagedAreaName" = ma,
  #   "Intercept" = coef(model_fixed)["(Intercept)"],
  #   "Slope" = coef(model_fixed)["relyear"],
  #   "p" = round(model_summary$coefficients["relyear", "Pr(>|t|)"],6),
  #   "Source" = "aggregate"
  # )

}

sav_results <- sav_results %>% rowwise() %>%
  mutate(Trend = ifelse(p<=0.05 & Slope>0, 1, ifelse(p<=0.05 & Slope<0, -1, 0)))

# total_ma_combined2 <- total_ma_combined %>% 
#   select(ManagedAreaName, LME_Intercept, LME_Slope, p, StatisticalTrend, Species) %>% 
#   rename("Intercept" = "LME_Intercept", "Slope" = "LME_Slope", "Trend" = "StatisticalTrend", "Source" = "Species") %>%
#   mutate(Trend = ifelse(Trend=="Significantly decreasing trend", -1, ifelse(Trend=="Significantly increasing trend", 1, 0)))
# 
# all_sav_results <- bind_rows(sav_results, total_ma_combined2)

# Add additional info which may be used in indicator summaries text
extra_info <- sav %>% 
  filter(!N_Data==0) %>% 
  group_by(ManagedAreaName) %>% 
  summarise(N_Data = sum(N_Data), 
            N_Programs = max(N_Programs),
            minYear = min(EarliestYear),
            maxYear = max(LatestYear))
# openxlsx::write.xlsx(sav_results, file = "SAV_MA_Results.xlsx", asTable = T)
openxlsx::write.xlsx(merge(sav_results, extra_info), file = "output/all_SAV_MA_Results.xlsx", asTable = T)
# openxlsx::write.xlsx(all_sav_results, file = "output/all_SAV_MA_Results.xlsx", asTable = T)

# Create visualizations of aggregate results, overlaid on previous simplified sav plots
simple_plots <- list.files("../../SEACAR_Trend_Analyses/SAV/output/Figures/BB/", pattern = "trendplot", full.names=T)
simple_plots <- str_subset(simple_plots, "_BBpct_")

all_plots <- list()
for(ma in sav_results$ManagedAreaName){
  subset <- sav_results %>% filter(ManagedAreaName==ma)
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  
  plot <- readRDS(str_subset(simple_plots, paste0("_",ma_abrev,"_")))
  plot <- plot +
    geom_abline(aes(linewidth = subset$Source, intercept = subset$Intercept, 
                    slope = subset$Slope), color = "blue") +
    labs(linewidth = "model source")
  ggsave(filename = paste0("output/agg_model_", ma_abrev, ".png"), plot, height = 10, width = 10)
  all_plots[[ma]] <- plot
}

# plot <- ggplot(data, aes(x = relyear, y = BB_pct)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(aes(color = "aggregate"), method = "lm", formula = y ~ x, se = TRUE) +
#   labs(title = glue("Seagrass Cover Trend Over Time in {ma}"),
#        x = "Year",
#        y = "Median Percent Cover") +
#   scale_color_manual(values = c("aggregate" = "blue"))