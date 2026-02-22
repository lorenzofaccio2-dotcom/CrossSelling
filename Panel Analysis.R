# analisi del prodotto
library(tidyverse)
library(readxl)
library(plm)
library(vars)

# ============================================================
# BLOCCO 1 — DATI PRODOTTO PROMOSSO
# ============================================================

data <- read_excel("C:/Users/Lorenzo/Downloads/282c7983-f02c-486f-a64e-e7fcd6ee89d2_0.xlsx")

data <- data %>%
  mutate(
    Trattamento = ifelse(`Codice Farmacia` %in% c('4','7','8','13','15','17'), 1, 0),
    Controllo   = ifelse(`Codice Farmacia` %in% c('2','10','34','35','48'), 1, 0),
    Dopo        = ifelse(Anno >= 2025, 1, 0),
    DiD         = Trattamento * Dopo,
    Gruppo      = ifelse(Trattamento == 1, "Trattato", "Controllo"),
    Periodo     = ifelse(Dopo == 1, "Dopo", "Prima")
  )

names(data) <- make.names(names(data))

# ---- Parallel trends (visivo) ----
df_pre <- data %>% filter(Dopo == 0)

ggplot(df_pre, aes(x = Mese, y = Qta..Venduta, group = Gruppo, color = Gruppo)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  geom_point(stat = "summary", fun = mean, size = 2) +
  labs(title = "Trend vendite nel periodo pre-promozione",
       x = "Mese", y = "Vendite medie") +
  theme_minimal()

# ---- Grafico pre/post ----
df_plot <- data %>%
  group_by(Gruppo, Periodo) %>%
  summarise(MediaVendite = mean(Qta..Venduta), .groups = "drop")

ggplot(df_plot, aes(x = Periodo, y = MediaVendite, fill = Gruppo)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(MediaVendite, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Vendite pre/post promozione per gruppo",
       x = "Periodo", y = "Vendite medie") +
  theme_minimal() +
  scale_fill_manual(values = c("Trattato" = "#1f77b4", "Controllo" = "#ff7f0e"))

# ---- DiD prodotto promosso ----
pdata_promosso <- pdata.frame(data, index = c("Codice.Farmacia", "Mese"))

fe_model <- plm(Qta..Venduta ~ Trattamento * Dopo,
                data = pdata_promosso, model = "within", effect = "twoways")
summary(fe_model)

re_model <- plm(Qta..Venduta ~ Trattamento * Dopo,
                data = pdata_promosso, model = "random", effect = "twoways")
summary(re_model)

phtest(fe_model, re_model)


# ============================================================
# BLOCCO 2 — DATI COMBINAZIONI (promosso + complementari)
# ============================================================

Data_Combinazioni_2024 <- read_excel("C:/Users/Lorenzo/Downloads/8f551c73-e41c-4918-831a-1b55a459f123_0.xlsx")
names(Data_Combinazioni_2024) <- make.names(names(Data_Combinazioni_2024))

Data_Combinazioni_2025 <- read_excel("C:/Users/Lorenzo/Downloads/6f4ce3ed-57df-42fb-9652-8ea04f4f7a73_0.xlsx")
names(Data_Combinazioni_2025) <- make.names(names(Data_Combinazioni_2025))

Data_Combinazioni <- rbind(Data_Combinazioni_2024, Data_Combinazioni_2025) %>%
  mutate(
    Trattamento = ifelse(Codice.Farmacia %in% c(4,7,8,13,15,17), 1, 0),
    Controllo   = ifelse(Codice.Farmacia %in% c(2,10,34,35,48), 1, 0),
    Dopo        = ifelse(Anno >= 2025, 1, 0),
    DiD         = Trattamento * Dopo,
    Periodo_ID  = paste0(Anno, "-", Mese)
  )


# ============================================================
# BLOCCO 3 — ISOLA I COMPLEMENTARI (combinazioni - promosso)
# ============================================================

promosso_agg <- data %>%
  group_by(Codice.Farmacia, Anno, Mese) %>%
  summarise(Qta_Promosso = sum(Qta..Venduta), .groups = "drop") %>%
  mutate(Periodo_ID = paste0(Anno, "-", Mese),
         Codice.Farmacia = as.character(Codice.Farmacia))

totale_agg <- Data_Combinazioni %>%
  filter(Codice.Farmacia %in% c(4,7,8,13,15,17,2,10,34,35,48)) %>%
  group_by(Codice.Farmacia, Periodo_ID, Trattamento, Dopo, DiD) %>%
  summarise(Qta_Totale = sum(Qta..Venduta), .groups = "drop") %>%
  mutate(Codice.Farmacia = as.character(Codice.Farmacia))

complementari_agg <- totale_agg %>%
  left_join(promosso_agg %>% dplyr::select(Codice.Farmacia, Periodo_ID, Qta_Promosso),
            by = c("Codice.Farmacia", "Periodo_ID")) %>%
  mutate(Qta_Complementari = Qta_Totale - Qta_Promosso)

# ---- DiD sui complementari (cross-selling) ----
pdata_comp <- pdata.frame(complementari_agg, 
                          index = c("Codice.Farmacia", "Periodo_ID"))

fe_cross <- plm(Qta_Complementari ~ Trattamento * Dopo,
                data = pdata_comp, model = "within", effect = "twoways")
summary(fe_cross)

re_cross <- plm(Qta_Complementari ~ Trattamento * Dopo,
                data = pdata_comp, model = "random", effect = "twoways")
summary(re_cross)

phtest(fe_cross, re_cross)


var_data <- complementari_agg %>%
  filter(Trattamento == 1) %>%
  group_by(Periodo_ID) %>%
  summarise(
    Promosso      = sum(Qta_Promosso, na.rm = TRUE),
    Complementari = sum(Qta_Complementari, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Periodo_ID)

var_matrix <- ts(var_data[, c("Promosso", "Complementari")], 
                 start = c(2024, 1), frequency = 12)

# Selezione lag ottimale
VARselect(var_matrix, lag.max = 6, type = "const")

# Stima VAR
var_model <- VAR(var_matrix, p = 2, type = "const")
summary(var_model)

# Stabilità
roots(var_model)

# Granger causality
causality(var_model, cause = "Promosso")
causality(var_model, cause = "Complementari")

# IRF: shock sul promosso -> effetto sui complementari
irf_result <- irf(var_model, 
                  impulse  = "Promosso", 
                  response = "Complementari",
                  n.ahead  = 12, 
                  boot     = TRUE, 
                  runs     = 500)
plot(irf_result)

