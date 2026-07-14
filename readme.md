# PMAAD: Anàlisi de Dades i Modelat Avançat - Airbnb Amsterdam

Aquest repositori conté el codi font i la documentació del projecte final de l'assignatura PMAAD (Preprocessament i Models Avançats d'Anàlisi de Dades, UPC). L'objectiu és analitzar i extreure coneixement de negoci del mercat d'Airbnb al centre d'Amsterdam aplicant tècniques avançades de mineria de dades, geoestadística i anàlisi textual.

![Dashboard Power BI](images/dashboard_powerbi_valor_preu.png)

## Estructura del Repositori
*   **`data/`**: Datasets preprocessats, conjunts intermedis i models (.rds, .csv).
*   **`images/`**: Visualitzacions clau extretes per a aquesta documentació.
*   **`powerbi/`**: Fitxer original, exportació del quadre de comandament interactiu.
*   **`profiling/`**: Resultats gràfics i taules generades durant la fase de perfilat avançat (CPG, TLP, aTLP).
*   **`reports/`**: Informes tècnic-gerencials, presentacions i annexos de les entregues.
*   **`scripts/`**: Codi font seqüenciat en R i Python per reproduir totes les fases del projecte.

---

## Fases del Projecte

### PART I: Marc de Referència
Introducció al problema del mercat de lloguer turístic a Amsterdam, descripció de la font de dades i definició de l'abast del projecte basat en objectius de negoci.

### PART II & III: Qualitat, Preprocessament de dades i EDA
Neteja profunda i preparació de la base de dades per al modelatge:
*   **Perfilament i Missings:** Avaluació de la qualitat, detecció d'errors i estratègies d'imputació de valors perduts.
*   **Outliers i Enginyeria de Variables:** Tractament de valors atípics i creació de noves variables significatives.
*   **AED:** Anàlisi Exploratòria de Dades univariant i bivariant sobre la base neta.

### PART IV & V: Clustering i Profiling Avançat
Segmentació del mercat per extreure perfils d'allotjaments:
*   **Clustering:** Aplicació d'algorismes avançats com CURE (amb distància Gower per a dades mixtes).
*   **Profiling:** Construcció del *Class Profiling Graph* (CPG) i *Targeted Lexical Profiling* (TLP / aTLP) per definir la semàntica dels clústers trobats.

### PART VI: Anàlisi Factorial (ACM i FAMD)
Reducció de dimensionalitat i extracció de components principals:
*   **ACM:** Anàlisi de Correspondències Múltiple sobre variables categòriques amb anàlisi d'individus i variables (Biplot).
*   **FAMD:** *Factor Analysis of Mixed Data* per estructurar un nou clustering basat en les coordenades factorials.

### PART VII: Estadística Descriptiva amb MAPES (Geoestadística Descriptiva)
Desenvolupament d'un quadre de comandament interactiu amb Microsoft Power BI per explicar una història visual i georeferenciada de les dades d'Airbnb, estratificant per zones i variables clau.

### PART VIII: Geoestadística
Modelatge espacial (Procés de Tipus I i II):
*   **Kriging Ordinari:** Disseny i validació d'un model d'interpolació espacial per predir el preu dels allotjaments en malles regulars.
*   **Processos de Punts:** Anàlisi de la intensitat i distribució geogràfica segregada per tipus d'amfitrió.

![Mapa amb marca de Propietari](images/mapa_particulars_vs_professionals.png)

### PART IX: Textual Analysis
Processament de Llenguatge Natural (NLP) sobre una mostra de 1.000 ressenyes d'usuaris:
*   **Preprocessament textual:** Neteja, stemming i creació de la matriu DTM (Document-Term Matrix).
*   **LSA / MCA Adaptat:** Anàlisi de Correspondències Simple (CA) i CA-GALT per vincular els termes a variables categòriques suplementàries.
*   **Topic Modelling (LDA):** *Latent Dirichlet Allocation* per descobrir els tòpics latents a les experiències dels usuaris.

![Anàlisi Textual](images/lda_wordclouds.png)

### PART X: Gestió del Projecte i Conclusions
*   **Gestió:** Resum del desenvolupament, incidències i eines de planificació (Gantt) utilitzades.
*   **Conclusions:** Highlights i insights rellevants orientats a la presa de decisions de negoci per a un client final.

---

## Entorn d'Execució

El projecte combina R (llenguatge principal per a l'EDA, geoestadística i NLP) i Python (per a algorismes específics de clustering com CURE amb matrius de Gower). Cal tenir instal·lades les següents dependències per reproduir els scripts:

### Entorn R

```R
# 1. Manipulació, Neteja i Transformació de Dades
install.packages(c("tidyverse", "dplyr", "tidyr", "reshape2", "Hmisc"))

# 2. Exploració de Dades (EDA) i Visualització Avançada
install.packages(c("visdat", "inspectdf", "skimr", "DataExplorer", "SmartEDA", 
                   "dataReporter", "patchwork", "ggcorrplot", "corrplot", 
                   "ggplot2", "ggrepel", "ggpubr", "grid", "gridExtra"))

# 3. Tractament de Valors Faltants i Imputació
install.packages(c("naniar", "VIM", "mice", "missForest", "StatMatch"))

# 4. Detecció d'Outliers
install.packages(c("EnvStats", "outliers", "chemometrics", "dbscan", "isotree", "tclust"))

# 5. Selecció de Variables i Random Forest
install.packages(c("vcd", "rcompanion", "caret", "mlbench", "XICOR", "randomForest"))

# 6. Clustering, Profiling i Anàlisi Factorial (MCA, FAMD, CA)
install.packages(c("dtwclust", "dendextend", "proxy", "cluster", "gower", 
                   "clustMixType", "factoextra", "flexclust", "FactoMineR", "psych"))

# 7. Geoestadística i Modelatge Espacial
install.packages(c("spatstat", "sp", "sf", "gstat", "geoR", "raster", "OpenStreetMap", "rJava"))

# 8. Anàlisi Textual i NLP
install.packages(c("tm", "SnowballC", "topicmodels", "tidytext", "cld3", "lda", "wordcloud", "RColorBrewer"))
remotes::install_github("nikita-moor/ldatuning")

```

### Entorn Python

Per executar la integració d'algorismes i l'exportació de dades a R, s'utilitzen els següents paquets:

```bash
pip install pandas numpy gower scipy matplotlib scikit-learn pyreadr
```
