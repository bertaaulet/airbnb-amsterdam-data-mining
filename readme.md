# Anàlisi del Mercat d'Airbnb a Amsterdam mitjançant Mineria de Dades

Aquest repositori conté el codi font i la documentació del projecte final de l'assignatura PMAAD (Preprocessament i Models Avançats d'Anàlisi de Dades, UPC). L'objectiu és analitzar i extreure coneixement de negoci del mercat d'Airbnb al centre d'Amsterdam aplicant tècniques avançades de mineria de dades, geoestadística i anàlisi textual.

![Dashboard Power BI](images/dashboard_powerbi_valor_preu.png)



## Estructura del Repositori
*   **`data/`**: Datasets preprocessats, conjunts intermedis i models (.rds, .csv).
*   **`images/`**: Visualitzacions clau extretes per a aquesta documentació.
*   **`powerbi/`**: Fitxer original, exportació del quadre de comandament interactiu.
*   **`profiling/`**: Resultats gràfics i taules generades durant la fase de perfilat avançat (CPG, TLP, aTLP).
*   **`reports/`**: Informes tècnic-gerencials, presentacions i annexos de les entregues.
*   **`scripts/`**: Codi font seqüenciat en R i Python per reproduir totes les fases del projecte.


## Obtenció de les dades

Els datasets originals **no s'inclouen** en aquest repositori perquè la seva mida supera els límits recomanats per GitHub.

Les dades es poden descarregar des del projecte **Inside Airbnb**:

https://insideairbnb.com/get-the-data/

Aquest projecte utilitza el conjunt de dades d'**Amsterdam**. Un cop descarregats els fitxers `listings.csv` i `reviews.csv`, cal copiar-los al directori `data/` abans d'executar els scripts.

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


---

# English Version

# Airbnb Amsterdam Data Mining & Advanced Analytics

This repository contains the source code and documentation for the final project of the **PMAAD (Advanced Data Preprocessing and Data Mining Models)** course at the Universitat Politècnica de Catalunya (UPC).

The objective of the project is to analyze the Airbnb market in central Amsterdam and extract business insights using advanced data mining, geostatistics and natural language processing techniques.

## Repository Structure

- **`data/`**: Preprocessed datasets, intermediate datasets and serialized models (.rds, .csv).
- **`images/`**: Figures and visualizations used throughout the documentation.
- **`powerbi/`**: Interactive Power BI dashboard.
- **`profiling/`**: Outputs generated during advanced profiling (CPG, TLP and aTLP).
- **`reports/`**: Technical reports, presentations and appendices.
- **`scripts/`**: R and Python source code reproducing every stage of the project.


## Dataset

The original datasets are **not included** in this repository because their size exceeds GitHub's recommended limits.

They can be downloaded from the **Inside Airbnb** project:

https://insideairbnb.com/get-the-data/

This project uses the **Amsterdam** dataset. Download the files `listings.csv` and `reviews.csv` and place them inside the `data/` directory before executing the scripts.

---

## Project Phases

### Part I – Project Background

Introduction to the Airbnb rental market in Amsterdam, description of the data source, and definition of the project's business objectives and scope.

### Parts II & III – Data Quality, Preprocessing and Exploratory Data Analysis

Comprehensive data cleaning and preparation before model development.

Main tasks include:

* **Data profiling and missing values:** Assessment of data quality, anomaly detection and missing value imputation strategies.
* **Outlier detection and feature engineering:** Identification and treatment of outliers together with the creation of meaningful derived variables.
* **Exploratory Data Analysis (EDA):** Univariate and bivariate statistical analysis of the cleaned dataset.

### Parts IV & V – Clustering and Advanced Profiling

Market segmentation to identify different accommodation profiles.

The analysis includes:

* **Clustering:** Application of advanced clustering algorithms such as **CURE**, using **Gower distance** to handle mixed-type data.
* **Cluster profiling:** Construction of **Class Profiling Graphs (CPG)** together with **Targeted Lexical Profiling (TLP)** and **Advanced Targeted Lexical Profiling (aTLP)** to characterize the semantic meaning of each cluster.

### Part VI – Factor Analysis (MCA and FAMD)

Dimensionality reduction and extraction of latent components.

This stage includes:

* **Multiple Correspondence Analysis (MCA):** Analysis of categorical variables together with individual and variable projections using biplots.
* **Factor Analysis of Mixed Data (FAMD):** Extraction of mixed-data latent factors used to perform an alternative clustering approach.

### Part VII – Descriptive Geostatistics and Interactive Mapping

Development of an interactive **Microsoft Power BI** dashboard providing a geographical overview of the Airbnb market in Amsterdam.

The dashboard allows users to explore spatial patterns by neighborhood and key business indicators through interactive visualizations.

### Part VIII – Geostatistics

Spatial statistical modeling based on Type I and Type II spatial processes.

The analysis includes:

* **Ordinary Kriging:** Development and validation of a spatial interpolation model for predicting Airbnb prices across a regular spatial grid.
* **Spatial Point Processes:** Analysis of the geographical intensity and distribution of listings according to host type.

![Host Type Map](images/mapa_particulars_vs_professionals.png)

### Part IX – Textual Analysis

Natural Language Processing (NLP) performed on a sample of 1,000 Airbnb user reviews.

The workflow includes:

* **Text preprocessing:** Cleaning, stemming and construction of the Document-Term Matrix (DTM).
* **Correspondence Analysis:** Standard Correspondence Analysis (CA) together with CA-GALT to relate textual terms with supplementary categorical variables.
* **Topic Modeling (LDA):** Application of **Latent Dirichlet Allocation (LDA)** to identify the latent topics discussed by Airbnb guests.

![Text Analysis](images/lda_wordclouds.png)

### Part X – Project Management and Conclusions

* **Project Management:** Summary of project development, encountered issues and planning tools (including Gantt charts).
* **Conclusions:** Key findings and business insights intended to support strategic decision-making.

---

## Execution Environment

The project combines **R** (used for data preprocessing, exploratory analysis, geostatistics and NLP) with **Python** (used for specific clustering algorithms such as CURE using Gower distance matrices).

Install the following dependencies before executing the scripts.

### R Environment

```R
# 1. Data Manipulation and Transformation
install.packages(c("tidyverse", "dplyr", "tidyr", "reshape2", "Hmisc"))

# 2. Exploratory Data Analysis and Visualization
install.packages(c("visdat", "inspectdf", "skimr", "DataExplorer", "SmartEDA",
                   "dataReporter", "patchwork", "ggcorrplot", "corrplot",
                   "ggplot2", "ggrepel", "ggpubr", "grid", "gridExtra"))

# 3. Missing Data Treatment
install.packages(c("naniar", "VIM", "mice", "missForest", "StatMatch"))

# 4. Outlier Detection
install.packages(c("EnvStats", "outliers", "chemometrics", "dbscan",
                   "isotree", "tclust"))

# 5. Feature Selection
install.packages(c("vcd", "rcompanion", "caret", "mlbench",
                   "XICOR", "randomForest"))

# 6. Clustering, Profiling and Factor Analysis
install.packages(c("dtwclust", "dendextend", "proxy", "cluster", "gower",
                   "clustMixType", "factoextra", "flexclust",
                   "FactoMineR", "psych"))

# 7. Geostatistics
install.packages(c("spatstat", "sp", "sf", "gstat", "geoR",
                   "raster", "OpenStreetMap", "rJava"))

# 8. Text Mining and NLP
install.packages(c("tm", "SnowballC", "topicmodels", "tidytext",
                   "cld3", "lda", "wordcloud", "RColorBrewer"))

remotes::install_github("nikita-moor/ldatuning")
```

### Python Environment

The Python environment is used for the integration of specific clustering algorithms and data exchange with R.

Install the required packages using:

```bash
pip install pandas numpy gower scipy matplotlib scikit-learn pyreadr
```
