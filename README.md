# Analisi Promozione e Cross-Selling

Questo repository contiene script R per analizzare l’effetto di una promozione su un prodotto target e sui prodotti complementari in un contesto retail/farmacie.

## Cosa include
- **Difference-in-Differences (DiD)** su panel di farmacie per stimare l’effetto diretto della promozione.
- **Modelli VAR** per analizzare effetti dinamici sulle vendite dei prodotti complementari.
- Grafici e aggregazioni per visualizzare trend pre/post promozione.

## Struttura degli script

### 01_data_promosso.R
- Caricamento dati prodotto promosso
- Creazione variabili `Trattamento`, `Controllo`, `Dopo`, `DiD`, `Gruppo`, `Periodo`
- Grafici parallel trends e pre/post promozione
- Stima DiD con modelli **fixed effects** e **random effects**
- Test Hausman per scelta modello

### 02_combinazioni.R
- Caricamento dati combinazioni 2024-2025
- Unione dataset e creazione variabili DiD e `Periodo_ID`
- Preparazione dati aggregati per analisi cross-selling

### 03_crossselling_VAR.R
- Isolamento vendite complementari (`Qta_Complementari`)
- Stima DiD sui complementari (cross-selling)
- Creazione serie temporale aggregata
- Selezione lag ottimale, stima **VAR**
- Analisi radici, causalità di Granger e IRF per misurare impatto dinamico del promosso sui complementari

## Obiettivo
Fornire insight pratici su come le promozioni influenzano sia i prodotti principali sia quelli correlati, supportando decisioni di marketing data-driven.

Nota: i dati originali non sono inclusi per motivi di privacy.
Fornire insight pratici su come le promozioni influenzano sia i prodotti principali sia quelli correlati, supportando decisioni di marketing data-driven.

> ⚠️ Nota: i dati originali non sono inclusi per motivi di privacy.
