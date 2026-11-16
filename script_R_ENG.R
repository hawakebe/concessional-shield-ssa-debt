# =============================================================================
#  The Concessional Shield: Financing Structure and the Cost of Sovereign
#  Debt in Sub-Saharan Africa
#
#  Replication script (ENG vers.) - Hawa KEBE, M2 International Economics &
#  Development (111), Université Paris Dauphine-PSL, 2025-2026
#
#  Structure:
#    1. Construction of the World Bank panel (IDS, WDI, WGI) + GRD
#    2. Aggregation of the African Debt Database (ADD) to country-year-segment
#    3. Merge: analysis_dataset.csv
#    4. Descriptive statistics and figures of Chapter 2
#    5. Regressions of Chapter 3 (H1 to H4) and LaTeX tables
#
#  Raw data:
#    - DataBank exports (*_Data.csv): IDS, WDI, WGI
#    - grd-data.csv (UNU-WIDER Government Revenue Dataset)
#    - ADD_v2026May_a.xlsx (African Debt Database, May 2026 version)
#    - Long-term-ODA.xlsx, 2025-preliminary-data.xlsx (OECD-DAC)
#
#  Outputs: ./output/ (tables .csv, figures .png/.pdf, tables .tex)
#
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
packages <- c("tidyverse","readxl","scales","countrycode","fixest","ggcorrplot")
inst <- setdiff(packages, rownames(installed.packages()))
if (length(inst) > 0) install.packages(inst)
invisible(lapply(packages, library, character.only = TRUE))

# Adjust the path to the folder containing the raw data:
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/M2 Économie Internationale & Développement/Mémoire/FINAL Mémoire Hawa KEBE/Données brutes")
dir.create("output", showWarnings = FALSE)
out <- function(f) file.path("output", f)

# Common graphical charter
pal <- c(Multilateral = "#2A9D8F", Bilateral = "#E8A33D", China = "#3D348B",
         External_market = "#E76F51", Domestic = "#9E2A2B")
seg_lv  <- c("Multilateral","Bilateral","China","External_market","Domestic")
seg_lab <- c("Multilateral","Bilateral","China","External market (eurobonds)",
             "Domestic debt")
grp_lv  <- c("Sub-Saharan Africa", "Other developing countries")
ACC1 <- "#264653"; ACC2 <- "#E76F51"
theme_thesis <- theme_minimal(base_size = 12) +
  theme(legend.title = element_text(face = "bold", size = 10),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 12, 8, 10))
save_fig <- function(p, name, w = 9, h = 5.5) {
  ggsave(out(paste0(name, ".png")), p, width = w, height = h, dpi = 300)
  if (isTRUE(capabilities("cairo")))
    ggsave(out(paste0(name, ".pdf")), p, width = w, height = h,
           device = cairo_pdf)
}
wm <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (any(ok)) weighted.mean(x[ok], w[ok]) else NA_real_
}
winsor <- function(x, p = 0.01) {
  b <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  pmin(pmax(x, b[1]), b[2])
}

# =============================================================================
# 1. World Bank panel (IDS + WDI + WGI) + GRD
# =============================================================================
series <- c(
  "DT.INR.DPPG"="rate_total","DT.INR.OFFT"="rate_official","DT.INR.PRVT"="rate_private",
  "DT.DOD.DECT.GN.ZS"="ext_debt_gni","DT.TDS.DECT.EX.ZS"="debt_service_x",
  "DT.MAT.DPPG"="maturity_commit","DT.DOD.DPPG.CD"="ppg_debt_usd",
  "NY.GDP.MKTP.KD.ZG"="gdp_growth","FP.CPI.TOTL.ZG"="inflation",
  "FI.RES.TOTL.MO"="reserves_months","BN.CAB.XOKA.GD.ZS"="current_account",
  "NY.GDP.PCAP.KD"="gdp_pc","GC.TAX.TOTL.GD.ZS"="tax_rev_wdi",
  "DT.ODA.ODAT.GN.ZS"="oda_gni","DT.ODA.ODAT.PC.ZS"="oda_pc","DT.ODA.ODAT.CD"="oda_usd",
  "CC.EST"="gov_corruption","GE.EST"="gov_effectiveness","PV.EST"="gov_stability",
  "RQ.EST"="gov_reg_quality","RL.EST"="gov_rule_of_law","VA.EST"="gov_voice")

files <- list.files(".", pattern = "_Data\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No DataBank export (*_Data.csv) found.")

raw <- bind_rows(lapply(files, function(f)
  suppressWarnings(read_csv(f, show_col_types = FALSE, na = c("", "..", "NA"),
                            col_types = cols(.default = "c")))))

col_years   <- names(raw)[str_detect(names(raw), "^(19|20)\\d{2}")]
col_country <- names(raw)[str_detect(names(raw), regex("^country name", TRUE))][1]
col_iso     <- names(raw)[str_detect(names(raw), regex("^country code", TRUE))][1]
col_sc      <- names(raw)[str_detect(names(raw), regex("^series code", TRUE))][1]

# IDS: keep only the aggregate counterpart "World"
col_cp <- names(raw)[str_detect(names(raw), regex("counterpart-area name", TRUE))]
if (length(col_cp) >= 1) {
  raw <- raw %>%
    filter(is.na(.data[[col_cp[1]]]) |
             str_detect(.data[[col_cp[1]]], regex("world", TRUE)))
}

long <- raw %>%
  select(iso3c = all_of(col_iso), country = all_of(col_country),
         series_code = all_of(col_sc), all_of(col_years)) %>%
  filter(!is.na(iso3c), !is.na(series_code)) %>%
  pivot_longer(all_of(col_years), names_to = "year_txt", values_to = "value") %>%
  mutate(year = as.integer(str_extract(year_txt, "(19|20)\\d{2}")),
         value = as.numeric(value),
         series_code = str_remove(series_code, "^GOV_WGI_"),
         series_name = recode(series_code, !!!series)) %>%
  filter(series_name %in% unname(series)) %>%
  select(iso3c, country, year, series_name, value)

panel <- long %>%
  distinct(iso3c, year, series_name, .keep_all = TRUE) %>%
  pivot_wider(names_from = series_name, values_from = value) %>%
  mutate(region = countrycode(iso3c, "iso3c", "region", warn = FALSE),
         ssa = ifelse(!is.na(region) & region == "Sub-Saharan Africa", 1L, 0L))

# Total revenue (% of GDP), UNU-WIDER GRD
if (file.exists("grd-data.csv")) {
  grd <- read_delim("grd-data.csv", delim = ";", show_col_types = FALSE,
                    na = c("", "NA")) %>%
    transmute(iso3c = ISO, year = as.integer(YEAR),
              total_rev_grd = as.numeric(`TOTAL REVENUE`) * 100)
  panel <- panel %>% left_join(grd, by = c("iso3c", "year"))
}

# Cleaning: removal of regional aggregates, derived variables
gov <- c("gov_corruption","gov_effectiveness","gov_stability",
         "gov_reg_quality","gov_rule_of_law","gov_voice")
panel <- panel %>%
  filter(!is.na(region)) %>%
  mutate(governance_mean = rowMeans(across(all_of(gov)), na.rm = TRUE),
         governance_mean = ifelse(is.nan(governance_mean), NA, governance_mean),
         log_gdp_pc = ifelse(!is.na(gdp_pc) & gdp_pc > 0, log(gdp_pc), NA)) %>%
  arrange(iso3c, year)

# =============================================================================
# 2. Aggregation of the African Debt Database (five creditor segments)
# =============================================================================
add <- read_excel("ADD_v2026May_a.xlsx", sheet = 1) %>%
  mutate(segment = case_when(
    CreditorGroup == "Multilateral"     ~ "Multilateral",
    CreditorGroup == "Bilateral"        ~ "Bilateral",
    CreditorGroup == "China, all"       ~ "China",
    CreditorGroup == "Private"          ~ "External_market",
    CreditorGroup == "Domestic Markets" ~ "Domestic",
    TRUE ~ NA_character_))

# Amount-weighted average cost, at the country x year x segment level
cost_segment <- add %>%
  filter(!is.na(segment), !is.na(interest), !is.na(Amount_musd), Amount_musd > 0) %>%
  group_by(iso3c = ISO3, year, segment) %>%
  summarise(weighted_avg_cost = sum(interest * Amount_musd) / sum(Amount_musd),
            volume_musd = sum(Amount_musd), .groups = "drop")

cost_wide <- cost_segment %>%
  select(iso3c, year, segment, weighted_avg_cost) %>%
  pivot_wider(names_from = segment, values_from = weighted_avg_cost,
              names_prefix = "cost_")
vol_wide <- cost_segment %>%
  select(iso3c, year, segment, volume_musd) %>%
  pivot_wider(names_from = segment, values_from = volume_musd,
              names_prefix = "vol_")
mix_wide <- add %>%
  filter(!is.na(segment), !is.na(Amount_musd), Amount_musd > 0) %>%
  group_by(iso3c = ISO3, year, segment) %>%
  summarise(volume = sum(Amount_musd), .groups = "drop") %>%
  group_by(iso3c, year) %>% mutate(share = volume / sum(volume)) %>% ungroup() %>%
  select(iso3c, year, segment, share) %>%
  pivot_wider(names_from = segment, values_from = share,
              names_prefix = "share_", values_fill = 0)

# =============================================================================
# 3. Merge: analysis dataset (one row = one country x one year, 2000-2024)
# =============================================================================
base <- panel %>%
  left_join(cost_wide, by = c("iso3c", "year")) %>%
  left_join(vol_wide,  by = c("iso3c", "year")) %>%
  left_join(mix_wide,  by = c("iso3c", "year")) %>%
  filter(year >= 2000, year <= 2024) %>%
  mutate(inflation_w = winsor(inflation))
write_csv(base, out("analysis_dataset.csv"))

# Comparison sample: countries with at least one IDS observation
ids_iso <- unique(base$iso3c[!is.na(base$rate_total)])
wb <- base %>% filter(iso3c %in% ids_iso) %>%
  mutate(group = factor(ifelse(ssa == 1, grp_lv[1], grp_lv[2]), levels = grp_lv))
ssa_iso <- unique(base$iso3c[base$ssa == 1])

# ADD subset restricted to Sub-Saharan Africa, 2000-2024
add_ssa <- add %>%
  filter(!is.na(segment), year >= 2000, year <= 2024, ISO3 %in% ssa_iso) %>%
  mutate(period = cut(year, c(1999, 2009, 2019, 2024),
                      labels = c("2000-2009", "2010-2019", "2020-2024")),
         segment = factor(segment, levels = seg_lv),
         external = segment != "Domestic")

# =============================================================================
# 4. Chapter 2: descriptive statistics and figures
#    (figures without titles; titles and sources are in the thesis captions)
# =============================================================================
vars <- c(rate_total="Average cost of new commitments (%)",
          rate_official="Cost, official creditors (%)",
          rate_private="Cost, private creditors (%)",
          gdp_growth="GDP growth (%)", inflation="Inflation (%)",
          reserves_months="Reserves (months of imports)",
          current_account="Current account (% GDP)",
          gdp_pc="GDP per capita (constant USD)",
          ext_debt_gni="External debt (% GNI)",
          governance_mean="Governance (mean WGI index)",
          oda_gni="Net ODA (% GNI)", oda_pc="Net ODA per capita (USD)",
          tax_rev_wdi="Tax revenue, WDI (% GDP)",
          total_rev_grd="Total revenue, GRD (% GDP)")

# Table 2.2: descriptive statistics
T22 <- map_dfr(names(vars), function(v) {
  x <- wb[[v]][!is.na(wb[[v]])]
  tibble(Variable = vars[[v]], N = length(x), Mean = mean(x),
         `Std. dev.` = sd(x), Min = min(x), Median = median(x), Max = max(x))
}) %>% mutate(across(where(is.numeric), ~round(.x, 2)))
write_csv(T22, out("Table_2_2_descriptive_stats.csv"))

# Table 2.3: compared means, SSA / other developing countries
T23 <- wb %>% group_by(group) %>%
  summarise(across(all_of(names(vars)), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-group, names_to = "var", values_to = "val") %>%
  pivot_wider(names_from = group, values_from = val) %>%
  mutate(Variable = vars[var], across(where(is.numeric), ~round(.x, 2))) %>%
  select(Variable, everything(), -var)
write_csv(T23, out("Table_2_3_group_means.csv"))

# Figure 2.1: average cost of new external commitments, SSA vs other developing countries
fs1 <- wb %>% group_by(year, group) %>%
  summarise(cost = mean(rate_total, na.rm = TRUE), .groups = "drop")
f1 <- ggplot(fs1, aes(year, cost, color = group, linetype = group)) +
  geom_line(linewidth = 1.2) + geom_point(size = 1.5) +
  scale_color_manual(values = c("Sub-Saharan Africa" = ACC2,
                                "Other developing countries" = ACC1), name = NULL) +
  scale_linetype_manual(values = c("solid", "longdash"), name = NULL) +
  labs(x = NULL, y = "Average interest rate (%)") +
  theme_thesis + theme(legend.position = "top")
save_fig(f1, "Figure_2_1_avg_cost_SSA_vs_other")

# Table 2.4 and Figure 2.2: weighted average cost by segment
T24 <- add_ssa %>% filter(!is.na(interest), !is.na(Amount_musd), Amount_musd > 0) %>%
  group_by(segment, period) %>%
  summarise(cost = wm(interest, Amount_musd), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = cost) %>%
  left_join(add_ssa %>% group_by(segment) %>%
              summarise(`2000-2024` = wm(interest, Amount_musd),
                        `Instruments with rate` = sum(!is.na(interest)),
                        Countries = n_distinct(ISO3)), by = "segment") %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))
write_csv(T24, out("Table_2_4_cost_by_segment.csv"))

fs2 <- add_ssa %>% group_by(year, segment) %>%
  summarise(cost = wm(interest, Amount_musd), .groups = "drop")
f2 <- ggplot(fs2, aes(year, cost, color = segment)) +
  geom_line(linewidth = 1.1) + geom_point(size = 1.3) +
  scale_color_manual(values = pal, labels = seg_lab, name = "Segment") +
  labs(x = NULL, y = "Interest rate (%)") +
  theme_thesis
save_fig(f2, "Figure_2_2_cost_scale_segments")

# Table 2.5 and Figure 2.3: composition of external financing
ext <- add_ssa %>% filter(external)
T25 <- ext %>% group_by(period, segment) %>%
  summarise(v = sum(Amount_musd, na.rm = TRUE), .groups = "drop") %>%
  group_by(period) %>%
  mutate(share = 100 * v / sum(v), total_bn = sum(v) / 1000) %>% ungroup() %>%
  select(period, segment, share, total_bn) %>%
  pivot_wider(names_from = segment, values_from = share) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)))
write_csv(T25, out("Table_2_5_external_composition.csv"))

fs3 <- ext %>% group_by(year, segment) %>%
  summarise(v = sum(Amount_musd, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>% mutate(share = v / sum(v)) %>% ungroup()
f3 <- ggplot(fs3, aes(year, share, fill = segment)) +
  geom_area(position = "fill", alpha = 0.92) +
  scale_fill_manual(values = pal[1:4], labels = seg_lab[1:4], name = "Segment") +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = "Share of external financing") +
  theme_thesis
save_fig(f3, "Figure_2_3_external_composition")

# Figure 2.4: multilateral share and cost of external financing
cy <- ext %>% group_by(ISO3, year) %>%
  summarise(cost_ext = wm(interest, Amount_musd),
            share_multi = 100 * sum(Amount_musd[segment == "Multilateral"],
                                    na.rm = TRUE) / sum(Amount_musd, na.rm = TRUE),
            vol = sum(Amount_musd, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(cost_ext))
f4 <- ggplot(cy, aes(share_multi, cost_ext)) +
  geom_point(alpha = 0.35, color = ACC1, aes(size = vol)) +
  geom_smooth(method = "lm", se = TRUE, color = ACC2, linewidth = 1) +
  scale_size(range = c(0.5, 5), guide = "none") +
  labs(x = "Share of multilateral loans in external financing (%)",
       y = "Weighted average cost of external financing (%)") +
  theme_thesis
save_fig(f4, "Figure_2_4_concessionality_vs_cost")

# Figure 2.4bis: annual multilateral share
fs4b <- fs3 %>% filter(segment == "Multilateral")
f4b <- ggplot(fs4b, aes(year, share)) +
  geom_area(fill = pal["Multilateral"], alpha = 0.15) +
  geom_line(color = pal["Multilateral"], linewidth = 1.3) +
  geom_point(color = pal["Multilateral"]) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = "Share (%)") + theme_thesis
save_fig(f4b, "Figure_2_4bis_concessional_share")

# Figure 2.5: net ODA received by Sub-Saharan Africa (WDI)
fs5 <- base %>% filter(ssa == 1) %>% group_by(year) %>%
  summarise(oda_gni = mean(oda_gni, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(oda_gni))
f5 <- ggplot(fs5, aes(year, oda_gni)) +
  geom_area(fill = ACC1, alpha = 0.12) +
  geom_line(color = ACC1, linewidth = 1.3) + geom_point(color = ACC1) +
  labs(x = NULL, y = "Net ODA (% of GNI)") + theme_thesis
save_fig(f5, "Figure_2_5_oda_decline")

# Figures 2.5bis / 2.5ter: DAC ODA (OECD, April 2026)
if (file.exists("Long-term-ODA.xlsx")) {
  ge <- read_excel("Long-term-ODA.xlsx", sheet = "ODA GE 2018 2025", skip = 3)
  names(ge)[1] <- "donor"
  ge_tot <- ge %>% filter(donor == "TOTAL DAC")
  dac <- tibble(year = 2018:2025,
                current  = as.numeric(unlist(ge_tot[1, 2:9])),
                constant = as.numeric(unlist(ge_tot[1, 11:18])))
  write_csv(dac, out("ODA_DAC_2018_2025.csv"))
  f5b <- dac %>% pivot_longer(-year, names_to = "measure", values_to = "bn") %>%
    mutate(measure = recode(measure, current = "USD current",
                            constant = "USD constant 2024")) %>%
    ggplot(aes(year, bn / 1000, color = measure, linetype = measure)) +
    geom_line(linewidth = 1.2) + geom_point(size = 1.6) +
    scale_color_manual(values = c(ACC1, ACC2), name = NULL) +
    scale_linetype_manual(values = c("solid", "longdash"), name = NULL) +
    labs(x = NULL, y = "USD bn") +
    theme_thesis + theme(legend.position = "top")
  save_fig(f5b, "Figure_2_5bis_DAC_ODA_2018_2025")
}
if (file.exists("2025-preliminary-data.xlsx")) {
  pre <- read_excel("2025-preliminary-data.xlsx", sheet = "ADV", col_names = FALSE)
  donors  <- unlist(pre[4, ])
  ssa_row <- unlist(pre[45, ])
  oda_donor <- tibble(donor = as.character(donors),
                      oda_ssa = suppressWarnings(as.numeric(gsub("e$", "",
                                                                 as.character(ssa_row))))) %>%
    filter(!is.na(donor), !is.na(oda_ssa),
           !donor %in% c("DAC countries", "DAC DONORS")) %>%
    arrange(desc(oda_ssa)) %>% slice_head(n = 12)
  write_csv(oda_donor, out("Bilateral_ODA_SSA_2025_by_donor.csv"))
  f5c <- ggplot(oda_donor, aes(reorder(donor, oda_ssa), oda_ssa / 1000)) +
    geom_col(fill = ACC1) + coord_flip() +
    labs(x = NULL, y = "USD bn") + theme_thesis
  save_fig(f5c, "Figure_2_5ter_ODA_SSA_2025_donors")
}

# Table 2.6 and Figure 2.6: domestic debt
dom <- add_ssa %>% filter(segment == "Domestic")
T26 <- dom %>% group_by(period) %>%
  summarise(volume_bn = sum(Amount_musd, na.rm = TRUE) / 1000,
            cost = wm(interest, Amount_musd),
            countries = n_distinct(ISO3), instruments = n(), .groups = "drop") %>%
  left_join(add_ssa %>% group_by(period) %>%
              summarise(tot = sum(Amount_musd, na.rm = TRUE) / 1000),
            by = "period") %>%
  mutate(share_total = 100 * volume_bn / tot,
         across(where(is.numeric), ~round(.x, 2))) %>% select(-tot)
write_csv(T26, out("Table_2_6_domestic_debt.csv"))

fs6 <- dom %>% group_by(year) %>%
  summarise(vol = sum(Amount_musd, na.rm = TRUE) / 1000,
            cost = wm(interest, Amount_musd), .groups = "drop")
f6 <- ggplot(fs6, aes(year)) +
  geom_col(aes(y = vol), fill = pal["Domestic"], alpha = 0.75) +
  geom_line(aes(y = cost * max(vol) / 15), color = "black", linewidth = 1) +
  scale_y_continuous(name = "Volume of new issues (USD bn)",
                     sec.axis = sec_axis(~ . * 15 / max(fs6$vol),
                                         name = "Weighted average cost (%)")) +
  labs(x = NULL) + theme_thesis
save_fig(f6, "Figure_2_6_domestic_debt")

# Figure 2.7: eurobond issues
eb <- add_ssa %>% filter(segment == "External_market")
fs7 <- eb %>% group_by(year) %>%
  summarise(vol = sum(Amount_musd, na.rm = TRUE) / 1000,
            cost = wm(interest, Amount_musd), .groups = "drop")
f7 <- ggplot(fs7, aes(year)) +
  geom_col(aes(y = vol), fill = pal["External_market"], alpha = 0.75) +
  geom_line(aes(y = cost * max(vol) / 10), color = "black", linewidth = 1) +
  geom_point(aes(y = cost * max(vol) / 10)) +
  scale_y_continuous(name = "Volume of issues (USD bn)",
                     sec.axis = sec_axis(~ . * 10 / max(fs7$vol),
                                         name = "Weighted average cost (%)")) +
  labs(x = NULL) + theme_thesis
save_fig(f7, "Figure_2_7_eurobonds")

# =============================================================================
# 5. Chapter 3: regressions (H1 to H4) and LaTeX tables
#    Standard errors clustered at the country level in all estimations.
# =============================================================================
base <- base %>%
  mutate(period = cut(year, c(1999, 2009, 2019, 2024),
                      labels = c("2000-2009", "2010-2019", "2020-2024")),
         covid = as.integer(year %in% c(2020, 2021)))
wb <- base %>% filter(iso3c %in% ids_iso)

dict_en <- c(ssa = "Sub-Saharan Africa", ext_debt_gni = "External debt / GNI",
             gdp_growth = "GDP growth", inflation = "Inflation",
             inflation_w = "Inflation (winsorised)",
             reserves_months = "Reserves (months of imp.)",
             governance_mean = "Governance (WGI)",
             log_gdp_pc = "Log GDP per capita",
             maturity_commit = "Average maturity",
             oda_gni = "Net ODA / GNI", year = "Trend (year)",
             covid = "Covid (2020-2021)", period = "Period",
             segment = "Segment", cost = "Cost (%)",
             cost_w = "Winsorised cost (%)",
             rate_total = "Average cost (%)", rate_private = "Private cost (%)",
             cost_External_market = "Cost of the market segment (%)",
             share_Multilateral = "Multilateral share",
             share_Domestic = "Domestic share",
             share_Multi_ext = "Multilateral share (external)",
             p2 = "Period 2010-2019", p3 = "Period 2020-2024")
style_aca <- style.tex("aer", model.format = "(i)")
etable_tex <- function(models, file, title, label) {
  etable(models, tex = TRUE, style.tex = style_aca, dict = dict_en,
         fitstat = ~ n + r2, digits = 3, digits.stats = 3,
         title = title, label = label, replace = TRUE,
         notes = "Standard errors clustered at the country level in parentheses.",
         file = out(file))
}

# --- H1: the concessional shield (average cost of new commitments) -----------
h1a <- feols(rate_total ~ ssa, wb, cluster = ~iso3c)
h1b <- feols(rate_total ~ ssa + ext_debt_gni + gdp_growth + inflation +
               reserves_months | year, wb, cluster = ~iso3c)
h1c <- feols(rate_total ~ ssa + ext_debt_gni + gdp_growth + inflation +
               reserves_months + governance_mean | year, wb, cluster = ~iso3c)
h1d <- feols(rate_total ~ ssa + ext_debt_gni + gdp_growth + inflation_w +
               reserves_months + governance_mean | year, wb, cluster = ~iso3c)
h1e <- feols(rate_total ~ ssa * period + ext_debt_gni + gdp_growth +
               inflation_w + reserves_months + governance_mean, wb, cluster = ~iso3c)
h1f <- feols(rate_total ~ ssa + covid + ext_debt_gni + gdp_growth +
               inflation_w + reserves_months + governance_mean, wb, cluster = ~iso3c)
etable(h1a, h1b, h1c, h1d, h1e, h1f, se.below = TRUE, dict = dict_en)
etable_tex(list(h1a, h1b, h1c, h1d, h1e, h1f), file = "tab_H1.tex",
           title = "H1: average cost and Sub-Saharan Africa membership",
           label = "tab:h1")

# --- H2: the cost scale by segment (reference: multilateral) -----------------
long <- base %>%
  filter(ssa == 1) %>%
  select(iso3c, year, inflation_w, starts_with("cost_")) %>%
  pivot_longer(starts_with("cost_"), names_to = "segment", values_to = "cost",
               names_prefix = "cost_") %>%
  filter(!is.na(cost)) %>%
  mutate(segment = relevel(factor(segment, levels = seg_lv), ref = "Multilateral"),
         cost_w = winsor(cost))
h2a <- feols(cost   ~ i(segment) | year, long, cluster = ~iso3c)
h2b <- feols(cost_w ~ i(segment) | year, long, cluster = ~iso3c)
h2c <- feols(cost   ~ i(segment) + inflation_w | year, long, cluster = ~iso3c)
etable(h2a, h2b, h2c, se.below = TRUE, dict = dict_en)
etable_tex(list(h2a, h2b, h2c), file = "tab_H2.tex",
           title = "H2: cost premium of each segment relative to multilateral financing",
           label = "tab:h2")

# --- H3: shift in the financing mix and the role of aid ----------------------
ssa_df <- base %>% filter(ssa == 1) %>%
  mutate(vol_ext = coalesce(vol_Multilateral, 0) + coalesce(vol_Bilateral, 0) +
           coalesce(vol_China, 0) + coalesce(vol_External_market, 0),
         share_Multi_ext = ifelse(vol_ext > 0,
                                  coalesce(vol_Multilateral, 0) / vol_ext, NA_real_),
         p2 = as.integer(year >= 2010 & year <= 2019),
         p3 = as.integer(year >= 2020))
h3a <- feols(share_Multilateral ~ year, ssa_df, cluster = ~iso3c)
h3b <- feols(share_Multilateral ~ year + oda_gni, ssa_df, cluster = ~iso3c)
h3c <- feols(share_Domestic     ~ year + oda_gni, ssa_df, cluster = ~iso3c)
h3d <- feols(share_Multi_ext    ~ year + oda_gni, ssa_df, cluster = ~iso3c)
h3e <- feols(share_Multi_ext    ~ p2 + p3 + oda_gni, ssa_df, cluster = ~iso3c)
etable(h3a, h3b, h3c, h3d, h3e, se.below = TRUE, dict = dict_en)
etable_tex(list(h3a, h3b, h3c, h3d, h3e), file = "tab_H3.tex",
           title = "H3: financing composition and the role of aid",
           label = "tab:h3")

# --- H4: the premium on the market segment, two measures ---------------------
# Version A: cost of the market segment (ADD). The comparison group is
# limited to the North African issuers present in the ADD (only African eurobond 
# issuers have a market-segment cost in the ADD, so the SSA dummy contrasts 
# Sub-Saharan issuers with the North African issuers present in the sample 
# (Egypt, Morocco,Tunisia)).
h4a1 <- feols(cost_External_market ~ ssa, base, cluster = ~iso3c)
h4a2 <- feols(cost_External_market ~ ssa + gdp_growth + inflation_w +
                governance_mean, base, cluster = ~iso3c)
h4a3 <- feols(cost_External_market ~ ssa + gdp_growth + inflation_w +
                governance_mean + maturity_commit, base, cluster = ~iso3c)
# Version B: IDS private rate (worldwide comparison). Zero values correspond
# to the absence of private borrowing and are excluded.
priv <- wb %>% filter(!is.na(rate_private), rate_private > 0)
h4b1 <- feols(rate_private ~ ssa, priv, cluster = ~iso3c)
h4b2 <- feols(rate_private ~ ssa + ext_debt_gni + gdp_growth + inflation_w +
                governance_mean | year, priv, cluster = ~iso3c)
h4b3 <- feols(rate_private ~ ssa + ext_debt_gni + gdp_growth + inflation_w +
                governance_mean + log_gdp_pc | year, priv, cluster = ~iso3c)
etable(h4a1, h4a2, h4a3, h4b1, h4b2, h4b3, se.below = TRUE, dict = dict_en)
etable_tex(list(h4a1, h4a2, h4a3, h4b1, h4b2, h4b3), file = "tab_H4.tex",
           title = "H4: market premium, two measures",
           label = "tab:h4")

# --- Figure 3.1: correlation matrix of the regression variables --------------
Mvars <- c(`Total rate` = "rate_total", `Market cost` = "cost_External_market",
           `Debt/GNI` = "ext_debt_gni", `Growth` = "gdp_growth",
           `Inflation` = "inflation", `Reserves` = "reserves_months",
           `Governance` = "governance_mean", `ODA/GNI` = "oda_gni",
           `Revenue/GDP` = "total_rev_grd", `GDP/cap. (log)` = "log_gdp_pc")
Mdat <- wb[, Mvars]; names(Mdat) <- names(Mvars)
M <- cor(Mdat, use = "pairwise.complete.obs")
f31 <- ggcorrplot(M, type = "lower", lab = TRUE, lab_size = 3,
                  colors = c("#9E2A2B", "white", "#2A9D8F"))
save_fig(f31, "Figure_3_1_correlation_matrix", w = 8.5, h = 7.5)

message("Done. Tables (.csv, .tex) and figures (.png, .pdf) are in ./output/.")
