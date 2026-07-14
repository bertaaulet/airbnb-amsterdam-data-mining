###############################################################################
# Profiling
################################################################################

rm(list = ls()) # eliminar objectes de memòria

################################################################################
# Paquets
################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(FactoMineR)
library(psych)

################################################################################
# Llegir dades
################################################################################
datos <- readRDS("../data/dataset_cure_python.rds")

# Directori on es guardaran les sortides principals
pathProfiling <- "../profiling"

str(datos)

if (!"cluster" %in% colnames(datos)) {
  stop("La base de dades ha de contenir una columna anomenada 'cluster' amb la partició obtinguda amb CURE.")
}

datos$cluster <- as.factor(datos$cluster)

################################################################################
# Eliminar variables no adequades per al profiling
################################################################################
excluded_vars <- c(
  "id",
  "listing_url",
  "scrape_id",
  "last_scraped",
  "source",
  "name",
  "description",
  "neighborhood_overview",
  "picture_url",
  "host_id",
  "host_url",
  "host_name",
  "host_since",
  "host_location",
  "host_about",
  "host_thumbnail_url",
  "host_picture_url",
  "host_verifications",
  "neighbourhood",
  "amenities",
  "license",
  "calendar_last_scraped",
  "first_review",
  "last_review"
)

# Eliminem també les variables que comencen per log
log_vars <- grep("^log_", colnames(datos), value = TRUE)

excluded_vars <- c(excluded_vars, log_vars)
excluded_vars <- intersect(excluded_vars, colnames(datos))

if (length(excluded_vars) > 0) {
  datos <- datos[, !colnames(datos) %in% excluded_vars, drop = FALSE]
}

################################################################################
# Tipus de variables
################################################################################
candidate_vars <- setdiff(colnames(datos), "cluster")

# Eliminar variables constants
candidate_vars <- candidate_vars[
  sapply(datos[, candidate_vars, drop = FALSE], function(x) {
    length(unique(x[!is.na(x)])) > 1
  })
]

# logical -> factor
varLog <- candidate_vars[sapply(datos[, candidate_vars, drop = FALSE], is.logical)]
for (vL in varLog) { datos[, vL] <- as.factor(datos[, vL]) }

# character -> factor
varChr <- candidate_vars[sapply(datos[, candidate_vars, drop = FALSE], is.character)]
for (vC in varChr) { datos[, vC] <- as.factor(datos[, vC]) }

varNum <- candidate_vars[sapply(datos[, candidate_vars, drop = FALSE], is.numeric)]
varCat <- candidate_vars[sapply(datos[, candidate_vars, drop = FALSE], function(x) is.factor(x) || is.character(x))]

str(datos)

################################################################################
# Funcions auxiliars
################################################################################

ClassPanelGraph <- function(var, data, cluster_hier) {
  # Crea el gràfic corresponent segons el tipus de variable
  if (is.numeric(data[, var])) {
    plot <- ggplot(data = data, aes(x = data[, var])) +
      geom_histogram(fill = "gray", color = "black") +
      facet_grid(get(cluster_hier) ~ .) +
      ylab("") + xlab(var)
  } else {
    plot <- ggplot(data = data, aes(x = data[, var])) +
      geom_bar(fill = "gray", color = "black") +
      facet_grid(get(cluster_hier) ~ .) +
      ylab("") + xlab(var) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(plot)
}

get_mode <- function(v) {
  # Calcula la moda ignorant NAs
  v <- v[!is.na(v)]
  if (length(v) == 0) return(NA)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

cv <- function(x, na.rm = TRUE) {
  # Coeficient de variació
  media <- mean(x, na.rm = na.rm)
  if (is.na(media) || media == 0) return(NA_real_)
  sd(x, na.rm = na.rm) / media
}

safe_shapiro <- function(x, max_n = 5000) {
  # Shapiro robust per mostres grans
  x <- x[!is.na(x)]
  
  if (length(x) < 3) {
    return(list(p.value = 0))
  }
  
  if (length(unique(x)) < 3) {
    return(list(p.value = 0))
  }
  
  if (length(x) > max_n) {
    set.seed(123)
    x <- sample(x, max_n)
  }
  
  shapiro.test(x)
}

################################################################################
# Configuració manual del TLP
################################################################################

# Variables numèriques on valors alts són bons:
# LOW BAD / HIGH GOOD -> rojo -> amarillo -> verde
vermell2verd_manual <- c(
  "accommodates",
  "bathrooms",
  "bedrooms",
  "beds",
  "price",
  "number_of_reviews",
  "estimated_occupancy_l365d",
  "estimated_revenue_l365d",
  "reviews_per_month",
  "dies_antiguitat_listing",
  "dies_antiguitat_host",
  "amenities_score"
)

vermell2verd_manual <- intersect(vermell2verd_manual, varNum)

# Colors manuals per a variables categòriques
# Les variables no incloses aquí queden en groc per defecte
cat_color_manual <- list(
  host_is_superhost = c(
    "FALSE" = "red",
    "TRUE" = "green"
  ),
  instant_bookable = c(
    "FALSE" = "yellow",
    "TRUE" = "green"
  ),
  host_type = c(
    "Particular" = "yellow",
    "Professional" = "green"
  ),
  cat_acceptance_rate = c(
    "Molt Restrictiu" = "red",
    "Selectiu" = "yellow",
    "Total (100%)" = "green"
  ),
  cat_response_rate = c(
    "Deficient" = "red",
    "Alta" = "yellow",
    "Immediata (100%)" = "green"
  ),
  cat_review_scores_rating = c(
    "Millorable / Risc" = "red",
    "Estàndard" = "yellow",
    "Excel·lent (Top)" = "green",
    "Nou (Sense Ressenyes)" = "yellow"
  ),
  room_type = c(
    "Entire home/apt" = "green",
    "Private room" = "yellow",
    "Other/Shared" = "red"
  )
)

################################################################################
# Funcions per construir la configuració del TLP
################################################################################

crear_config_numerica <- function(datos, varNum, vermell2verd) {
  # Crea la configuració inicial de les variables numèriques
  if (length(varNum) == 0) return(list())
  
  numeric_config <- lapply(varNum, function(var) {
    vals <- datos[[var]]
    vals <- vals[!is.na(vals)]
    
    min_val <- min(vals)
    max_val <- max(vals)
    
    q1 <- as.numeric(quantile(vals, 0.33, na.rm = TRUE))
    q2 <- as.numeric(quantile(vals, 0.66, na.rm = TRUE))
    
    if (q1 <= min_val) q1 <- min_val + (max_val - min_val) / 3
    if (q2 <= q1) q2 <- min_val + 2 * (max_val - min_val) / 3
    if (q2 >= max_val) q2 <- min_val + 2 * (max_val - min_val) / 3
    
    # -1: rojo -> amarillo -> verde
    #  1: verde -> amarillo -> rojo
    direccion <- if (var %in% vermell2verd) -1 else 1
    
    list(
      variable = var,
      min = min_val,
      max = max_val,
      cut1 = q1,
      cut2 = q2,
      direccion = direccion
    )
  })
  
  names(numeric_config) <- varNum
  return(numeric_config)
}

crear_config_categorica <- function(datos, varCat) {
  # Crea la configuració inicial de les variables categòriques
  if (length(varCat) == 0) return(list())
  
  cat_config <- lapply(varCat, function(vC) {
    mods <- sort(unique(as.character(datos[[vC]])))
    colors <- rep("yellow", length(mods))
    names(colors) <- mods
    colors
  })
  names(cat_config) <- varCat
  return(cat_config)
}

aplicar_mapeo_categorico_manual <- function(cat_config, cat_color_manual) {
  # Aplica els colors manuals sobre la configuració categòrica inicial
  if (length(cat_config) == 0) return(cat_config)
  
  for (v in names(cat_color_manual)) {
    if (v %in% names(cat_config)) {
      mods_manuals <- names(cat_color_manual[[v]])
      mods_existents <- names(cat_config[[v]])
      mods_a_canviar <- intersect(mods_manuals, mods_existents)
      
      for (m in mods_a_canviar) {
        cat_config[[v]][m] <- unname(cat_color_manual[[v]][m])
      }
    }
  }
  
  return(cat_config)
}

aplicar_direcciones_numericas_manuales <- function(numeric_config, vermell2verd_manual) {
  # Força les direccions manuals acordades
  if (length(numeric_config) == 0) return(numeric_config)
  
  for (v in names(numeric_config)) {
    if (v %in% vermell2verd_manual) {
      numeric_config[[v]]$direccion <- -1
    } else {
      numeric_config[[v]]$direccion <- 1
    }
  }
  
  return(numeric_config)
}

build_dfCate <- function(cat_config) {
  # Construeix una taula plana de la configuració categòrica
  if (length(cat_config) == 0) {
    return(data.frame(
      variables = character(0),
      modalidades = character(0),
      color = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  bind_rows(lapply(names(cat_config), function(v) {
    data.frame(
      variables = v,
      modalidades = names(cat_config[[v]]),
      color = unname(cat_config[[v]]),
      stringsAsFactors = FALSE
    )
  }))
}

build_varNumColor <- function(datos, varNum, numeric_config) {
  # Construeix els colors del TLP per a les variables numèriques
  # S'usa la mediana condicionada al clúster
  if (length(varNum) == 0) {
    return(data.frame(
      cluster = character(0),
      variable = character(0),
      color = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  df_clustered <- datos %>%
    group_by(cluster) %>%
    summarise(across(all_of(varNum), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
    data.frame()
  
  datos_modelo <- df_clustered %>%
    pivot_longer(!cluster, names_to = "variable", values_to = "valor") %>%
    data.frame()
  
  listaDatos <- list()
  
  for (var in varNum) {
    cfg <- numeric_config[[var]]
    
    subtabla <- datos_modelo[datos_modelo$variable == var, c("cluster", "variable", "valor")]
    subtabla$direccion <- cfg$direccion
    
    subtabla$grupo <- dplyr::case_when(
      subtabla$valor <= cfg$cut1 ~ "1",
      subtabla$valor <= cfg$cut2 ~ "2",
      TRUE ~ "3"
    )
    
    subtabla$color <- ifelse(
      subtabla$direccion == -1,
      ifelse(subtabla$grupo == "1", "red",
             ifelse(subtabla$grupo == "2", "yellow", "green")),
      ifelse(subtabla$grupo == "1", "green",
             ifelse(subtabla$grupo == "2", "yellow", "red"))
    )
    
    subtabla <- subtabla[, c("cluster", "variable", "color")]
    listaDatos[[var]] <- subtabla
  }
  
  bind_rows(listaDatos)
}

build_varCatColor <- function(datos, varCat, cat_config) {
  # Construeix els colors del TLP per a les variables categòriques
  if (length(varCat) == 0) {
    return(data.frame(
      cluster = character(0),
      variable = character(0),
      color = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  dfCate <- build_dfCate(cat_config)
  dfCate$id <- paste0(dfCate$variables, "_", dfCate$modalidades)
  
  df_moda <- datos %>%
    select(cluster, all_of(varCat)) %>%
    pivot_longer(cols = -cluster, names_to = "variable", values_to = "valor") %>%
    mutate(valor = as.character(valor)) %>%
    group_by(cluster, variable) %>%
    summarise(moda = get_mode(valor), .groups = "drop") %>%
    data.frame()
  
  df_moda$id <- paste0(df_moda$variable, "_", df_moda$moda)
  
  m <- match(df_moda$id, dfCate$id)
  df_moda$color <- dfCate[m, "color"]
  
  df_moda %>%
    select(cluster, variable, color) %>%
    data.frame()
}

construir_dfColor_desde_config <- function(datos, varNum, varCat, numeric_config, cat_config) {
  # Construeix el dfColor complet a partir de la configuració
  varNumColor <- build_varNumColor(datos, varNum, numeric_config)
  varCatColor <- build_varCatColor(datos, varCat, cat_config)
  
  dfColor <- rbind(varNumColor, varCatColor)
  
  return(list(
    dfColor = dfColor,
    varNumColor = varNumColor,
    varCatColor = varCatColor,
    dfCate = build_dfCate(cat_config)
  ))
}

################################################################################
# Funcions de postprocessat del TLP i aTLP
################################################################################

eliminar_variables_no_discriminatorias <- function(dfColor) {
  # Elimina variables amb el mateix color a tots els clústers
  dfColorRed <- dfColor
  
  for (var in unique(dfColorRed$variable)) {
    subset_var <- dfColorRed[dfColorRed$variable == var, ]
    
    if (length(unique(subset_var$color)) == 1) {
      dfColorRed <- dfColorRed %>%
        dplyr::filter(variable != var)
    }
  }
  
  return(dfColorRed)
}

aplicar_color_hex_con_cv <- function(dfColor, datos, cv_fun, varNum) {
  # Converteix el TLP en aTLP modulant el color amb el CV
  
  color_to_rgb <- function(col) {
    if (col == "red")    return(c(255,   0,   0))
    if (col == "green")  return(c(  0, 255,   0))
    if (col == "yellow") return(c(255, 255,   0))
    return(c(255, 255,   0))
  }
  
  dfColor_base <- dfColor
  
  if (length(varNum) > 0) {
    dfCV <- datos %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarise(
        dplyr::across(all_of(varNum), cv_fun, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(!cluster, names_to = "variable", values_to = "cv") %>%
      data.frame()
  } else {
    dfCV <- data.frame(
      cluster = character(0),
      variable = character(0),
      cv = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  
  m <- match(
    paste0(dfColor_base$cluster, dfColor_base$variable),
    paste0(dfCV$cluster, dfCV$variable)
  )
  dfColor_base[, "cv"] <- dfCV[m, "cv"]
  
  rgb_base <- t(sapply(dfColor_base$color, color_to_rgb))
  dfColor_base$R <- rgb_base[, 1]
  dfColor_base$G <- rgb_base[, 2]
  dfColor_base$B <- rgb_base[, 3]
  
  idx_num <- dfColor_base$variable %in% varNum
  idx_cv  <- idx_num & !is.na(dfColor_base$cv)
  
  sx  <- 80 + 125 * (1 - dfColor_base$cv) + 50 * (1 - dfColor_base$cv)^2
  sxa <- 180 + 180 * (1 - dfColor_base$cv) - 143 * (1 - dfColor_base$cv)^2 + 38 * (1 - dfColor_base$cv)^3
  
  dfColor_base[idx_cv & dfColor_base$color == "red", "R"]    <- sx[idx_cv & dfColor_base$color == "red"]
  dfColor_base[idx_cv & dfColor_base$color == "green", "G"]  <- sx[idx_cv & dfColor_base$color == "green"]
  dfColor_base[idx_cv & dfColor_base$color == "yellow", "G"] <- sx[idx_cv & dfColor_base$color == "yellow"]
  dfColor_base[idx_cv & dfColor_base$color == "yellow", "R"] <- sxa[idx_cv & dfColor_base$color == "yellow"]
  
  dfColor_base$R <- pmin(pmax(dfColor_base$R, 0), 255)
  dfColor_base$G <- pmin(pmax(dfColor_base$G, 0), 255)
  dfColor_base$B <- pmin(pmax(dfColor_base$B, 0), 255)
  
  dfColor_base[, "color"] <- rgb(dfColor_base$R, dfColor_base$G, dfColor_base$B, maxColorValue = 255)
  dfColor_base[, c("cv", "R", "G", "B")] <- NULL
  
  return(dfColor_base)
}

################################################################################
# Etapa 1: significació de variables
################################################################################
columns_validate <- c(varNum, varCat)
significant_vars <- c()

# Assegurem que existeix el directori per a les imatges abans de començar
if (!dir.exists("../profiling")) {
  dir.create("../profiling")
}

sink(file = "../reports/test_results.txt")

for (cV in columns_validate) {
  
  current_var <- datos[, cV]
  
  # Variables numèriques --------------------------------------------------------
  if (is.numeric(current_var)) {
    
    testSH <- safe_shapiro(current_var)
    
    if (testSH$p.value > 0.05) {
      
      anova <- aov(current_var ~ datos$cluster)
      cat("============ ", cV, " ================\n")
      print(summary(anova)); cat("\n")
      
      tryCatch({
        p_valor <- summary(anova)[[1]][["Pr(>F)"]][1]
        if (!is.na(p_valor) && p_valor <= 0.05) {
          significant_vars <- c(significant_vars, cV)
        }
      }, error = function(e) {
        cat("No s'ha pogut avaluar l'ANOVA per aquesta variable.\n")
      })
      
    } else {
      
      test <- kruskal.test(current_var ~ datos[, "cluster"])
      cat("============ ", cV, " ================\n")
      print(test); cat("\n")
      if (!is.na(test$p.value) && test$p.value <= 0.05) {
        significant_vars <- c(significant_vars, cV)
      }
    }
    
    # Si la variable és molt discreta, la representem com categòrica
    n_unique <- length(unique(current_var[!is.na(current_var)]))
    
    if (n_unique <= 5) {
      
      tabla_num <- data.frame(table(Var1 = as.factor(current_var), cluster = datos$cluster))
      
      gr <- ggplot(tabla_num, aes(x = Var1, y = Freq, fill = cluster)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "", x = cV, y = "Freqüència") +
        theme_minimal() +
        theme(text = element_text(size = 8))
      
    } else {
      
      gr_Boxplot <- ggboxplot(datos, "cluster", cV, fill = "cluster") +
        theme(text = element_text(size = 8))
      
      gr_Hist <- gghistogram(
        datos, x = cV,
        add = "mean", rug = TRUE,
        color = "cluster", fill = "cluster"
      ) +
        theme(text = element_text(size = 8))
      
      gr <- ggarrange(
        gr_Boxplot, gr_Hist,
        heights = c(2, 0.7), ncol = 2, nrow = 1, align = "v"
      )
    }
    
    ggsave(
      filename = file.path("../profiling", paste0("clustering_var_", cV, ".png")),
      plot = gr, bg = "white", width = 6, height = 3
    )
  }
  
  # Variables categòriques ------------------------------------------------------
  if (is.factor(current_var) || is.character(current_var)) {
    
    test <- suppressWarnings(
      chisq.test(current_var, datos[, "cluster"], simulate.p.value = TRUE)
    )
    
    cat("============ ", cV, " ================\n")
    print(test); cat("\n")
    
    tabla <- data.frame(table(Var1 = current_var, datos$cluster))
    colnames(tabla)[which(colnames(tabla) == "Var2")] <- "cluster"
    
    if (!is.na(test$p.value) && test$p.value <= 0.05) {
      significant_vars <- c(significant_vars, cV)
    }
    
    gr <- ggplot(tabla, aes(x = Var1, y = Freq, fill = cluster)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "", x = "", y = "") +
      theme_minimal() +
      theme(
        text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
      )
    
    ggsave(
      filename = file.path("../profiling", paste0("clustering_var_", cV, ".png")),
      plot = gr, bg = "white", width = 6, height = 3
    )
  }
}
sink()

################################################################################
# Centroides i modes
################################################################################
sink(file = "../reports/centroides.txt")

sel_num <- significant_vars[which(significant_vars %in% varNum)]
if (length(sel_num) > 0) {
  print(describeBy(datos[, sel_num, drop = FALSE], datos$cluster))
}

sel_cat <- significant_vars[which(significant_vars %in% varCat)]
listaModa <- list()
if (length(sel_cat) > 0) {
  for (vC in sel_cat) {
    tabla <- data.frame(table(datos[, vC], datos$cluster))
    calModa <- tabla %>%
      group_by(Var2) %>%
      filter(Freq == max(Freq)) %>%
      select(Var1, Var2) %>%
      as.data.frame()
    colnames(calModa) <- c(vC, "cluster")
    cat("\n============ ", vC, " ================\n")
    print(calModa); cat("\n")
    listaModa[[vC]][["moda"]] <- calModa
  }
}
sink()

################################################################################
# Etapa 2: significació de modalitats
################################################################################

# La variable resposta ha d'anar a l'última columna
datos_catdes <- datos[, c(setdiff(c(varNum, varCat, "cluster"), "cluster"), "cluster")]

res_catdes <- catdes(datos_catdes, num.var = ncol(datos_catdes))
res_catdes

png(filename = file.path(pathProfiling, "catdes_quanti.png"), width = 1200, height = 800, bg = "white")
plot(
  res_catdes, show = "quanti",
  col.upper = "red", col.lower = "blue",
  barplot = TRUE, cex.names = 1
)
dev.off()

png(filename = file.path(pathProfiling, "catdes_quali.png"), width = 1200, height = 800, bg = "white")
par(mfrow = c(1, 1))
plot(
  res_catdes, show = "quali",
  col.upper = "red", col.lower = "blue",
  barplot = FALSE, cex.names = 2
)
dev.off()

png(filename = file.path(pathProfiling, "catdes_all.png"), width = 1200, height = 800, bg = "white")
plot(
  res_catdes, show = "all",
  col.upper = "red", col.lower = "blue",
  barplot = FALSE, cex.names = 2
)
dev.off()

################################################################################
# CPG ==========================================================================
################################################################################
vars_all <- c(varNum, varCat)
plots <- lapply(vars_all, ClassPanelGraph, data = datos, cluster_hier = "cluster")

# Engraellat de 2x2 (4 imatges per pàgina)
n_cols <- 2
n_rows <- 2
plots_per_page <- n_cols * n_rows
total_plots <- length(plots)
num_pages <- ceiling(total_plots / plots_per_page)

for (i in 1:num_pages) {
  
  start_idx <- ((i - 1) * plots_per_page) + 1
  end_idx <- min(i * plots_per_page, total_plots)
  
  CPG_page <- ggarrange(
    plotlist = plots[start_idx:end_idx],
    ncol = n_cols,
    nrow = n_rows
  )
  
  ggsave(
    filename = file.path("../profiling", paste0("CPG_Completo_pag_", i, ".png")),
    plot = CPG_page,
    width = 12,
    height = 8,
    bg = "white"
  )
}

################################################################################
# TLP ==========================================================================
################################################################################

# Tota la configuració surt directament del codi
numeric_config <- crear_config_numerica(datos, varNum, vermell2verd_manual)
numeric_config <- aplicar_direcciones_numericas_manuales(numeric_config, vermell2verd_manual)

cat_config <- crear_config_categorica(datos, varCat)
cat_config <- aplicar_mapeo_categorico_manual(cat_config, cat_color_manual)

# Construcció automàtica del dfColor a partir de la configuració manual
res_tlp <- construir_dfColor_desde_config(
  datos = datos,
  varNum = varNum,
  varCat = varCat,
  numeric_config = numeric_config,
  cat_config = cat_config
)

dfColor <- res_tlp$dfColor

if (is.null(dfColor) || nrow(dfColor) == 0) {
  stop("No s'ha pogut construir 'dfColor'.")
}

## TLP complet -----------------------------------------------------------------
gr_TLP <- ggplot(dfColor, aes(x = variable, y = factor(cluster), fill = color)) +
  geom_tile(color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("red" = "red", "yellow" = "yellow", "green" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    title = "Traffic Light Panel (TLP)",
    x = "Variables",
    y = "Clusters"
  )

gr_TLP
ggsave(
  filename = file.path(pathProfiling, "TLP_Completo.png"),
  plot = gr_TLP, width = 11, height = 7, bg = "white"
)

## TLP variables discriminants -------------------------------------------------
dfColorRed <- eliminar_variables_no_discriminatorias(dfColor)

gr_TLP_red <- ggplot(dfColorRed, aes(x = variable, y = factor(cluster), fill = color)) +
  geom_tile(color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("red" = "red", "yellow" = "yellow", "green" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    title = "Traffic Light Panel (TLP)",
    x = "Variables",
    y = "Clusters"
  )

gr_TLP_red
ggsave(
  filename = file.path(pathProfiling, "TLP_VariablesDiscriminantes.png"),
  plot = gr_TLP_red, width = 11, height = 7, bg = "white"
)

################################################################################
# aTLP =========================================================================
################################################################################

dfColor_aTLP <- aplicar_color_hex_con_cv(dfColor, datos, cv, varNum)

## aTLP complet ----------------------------------------------------------------
gr_aTLP <- ggplot(dfColor_aTLP, aes(x = variable, y = factor(cluster), fill = color)) +
  geom_tile(color = "black", linewidth = 0.5) +
  scale_fill_identity() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Annotated Traffic Light Panel (aTLP)",
    x = "Variables",
    y = "Clusters"
  )

gr_aTLP
ggsave(
  filename = file.path(pathProfiling, "aTLP_Completo.png"),
  plot = gr_aTLP, width = 11, height = 7, bg = "white"
)

## aTLP variables discriminants ------------------------------------------------
dfColorRed_aTLP <- eliminar_variables_no_discriminatorias(dfColor_aTLP)

gr_aTLP_red <- ggplot(dfColorRed_aTLP, aes(x = variable, y = factor(cluster), fill = color)) +
  geom_tile(color = "black", linewidth = 0.5) +
  scale_fill_identity() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Annotated Traffic Light Panel (aTLP)",
    x = "Variables",
    y = "Clusters"
  )

gr_aTLP_red
ggsave(
  filename = file.path(pathProfiling, "aTLP_VariablesDiscriminantes.png"),
  plot = gr_aTLP_red, width = 11, height = 7, bg = "white"
)