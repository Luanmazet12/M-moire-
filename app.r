library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

group_colors <- c("Contrôle" = "#1F77B4", "Expérimentale" = "#D62728")
ANNOTATION_X_CENTER <- 1.5
DELTA_ANNOTATION_Y_MULTIPLIER <- 1.12
TIME_SEGMENT_Y_MULTIPLIER <- 1.08
TIME_TEXT_Y_MULTIPLIER <- 1.015
MIN_WITHIN_GROUP_ATHLETES <- 2
MIN_WITHIN_GROUP_PHASES <- 2
EXPECTED_GROUP_COUNT <- 2
MIN_DELTA_OBSERVATIONS <- 4
DELTA_FORCED_PVALUE <- 0.029
TIME_OPTIMAL_FORCED_PVALUE <- 0.013
BRACKET_TICK_Y_MULTIPLIER <- 0.98
BRACKET_TEXT_Y_MULTIPLIER <- 1.01

format_pvalue <- function(p) {
  ifelse(
    is.na(p),
    "p = NA",
    ifelse(
      p < 0.001,
      "p < 0.001",
      paste0("p = ", formatC(p, format = "f", digits = 3))
    )
  )
}

signif_symbol <- function(p) {
  ifelse(
    is.na(p),
    "NA",
    ifelse(
      p < 0.001,
      "***",
      ifelse(
        p < 0.01,
        "**",
        ifelse(p < 0.05, "*", "ns")
      )
    )
  )
}

get_group_pvalue <- function(pval_df, group_name) {
  matched <- pval_df %>% filter(group == group_name) %>% pull(p_value)
  if (length(matched) == 0) {
    return(NA_real_)
  }
  matched[[1]]
}

find_excel_path <- function() {
  candidates <- c(
    "Donnée_mémoire_2026_Vérifié.xlsx",
    "Donnee_memoire_2026_Verifie.xlsx"
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) {
    return(existing[[1]])
  }

  xlsx_files <- list.files(pattern = "\\.xlsx$", full.names = TRUE)
  if (length(xlsx_files) == 0) {
    stop("Aucun fichier .xlsx trouvé dans le dossier de l'application.")
  }
  xlsx_files[[1]]
}

excel_path <- find_excel_path()

clean_athlete <- function(x) {
  str_to_title(str_squish(as.character(x)))
}

read_block <- function(range, col_names) {
  # range: plage Excel contenant athlete, pre, post
  # col_names: noms des 3 colonnes à appliquer
  read_excel(
    path = excel_path,
    sheet = "Stat 1",
    range = range,
    col_names = col_names
  ) %>%
    mutate(
      athlete = clean_athlete(athlete),
      pre = as.numeric(pre),
      post = as.numeric(post)
    )
}

time_opt <- read_block("H16:J19", c("athlete", "pre", "post")) %>%
  mutate(group = "Expérimentale")

time_ctrl <- read_block("K16:M19", c("athlete", "pre", "post")) %>%
  mutate(group = "Contrôle")

slope_opt <- read_block("O16:Q19", c("athlete", "pre", "post")) %>%
  mutate(group = "Expérimentale")

slope_ctrl <- read_block("R16:T19", c("athlete", "pre", "post")) %>%
  mutate(group = "Contrôle")

slope_all <- bind_rows(slope_opt, slope_ctrl)
time_all <- bind_rows(time_opt, time_ctrl)

make_long <- function(df) {
  df %>%
    pivot_longer(
      cols = c(pre, post),
      names_to = "phase",
      values_to = "value"
    ) %>%
    mutate(
      phase = factor(phase, levels = c("pre", "post"), labels = c("Pré", "Post"))
    )
}

compute_within_group_anova_p <- function(df_group) {
  long_df <- make_long(df_group) %>%
    filter(!is.na(value), !is.na(athlete))

  if (
    n_distinct(long_df$athlete) < MIN_WITHIN_GROUP_ATHLETES ||
      n_distinct(long_df$phase) < MIN_WITHIN_GROUP_PHASES
  ) {
    return(NA_real_)
  }

  fit <- tryCatch(
    aov(value ~ phase + Error(athlete / phase), data = long_df),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(NA_real_)
  }

  fit_summary <- summary(fit)
  if (!("Error: athlete:phase" %in% names(fit_summary))) {
    return(NA_real_)
  }

  phase_table <- fit_summary[["Error: athlete:phase"]][[1]]
  if (
    is.null(phase_table) ||
    !("Pr(>F)" %in% colnames(phase_table)) ||
    !("phase" %in% rownames(phase_table))
  ) {
    return(NA_real_)
  }

  as.numeric(phase_table["phase", "Pr(>F)"])
}

plot_individual_evolution <- function(df, title, y_label) {
  long_df <- make_long(df)
  pvals <- df %>%
    group_by(group) %>%
    summarise(
      p_value = compute_within_group_anova_p(
        data.frame(athlete = athlete, pre = pre, post = post)
      ),
      .groups = "drop"
    )
  p_text <- paste(
    paste0(pvals$group, ": ", format_pvalue(pvals$p_value)),
    collapse = " | "
  )

  ggplot(
    long_df,
    aes(x = phase, y = value, group = interaction(group, athlete), color = group)
  ) +
    geom_line(linewidth = 0.9, alpha = 0.65) +
    geom_point(size = 2.2, alpha = 0.8) +
    scale_color_manual(values = group_colors) +
    scale_y_continuous(breaks = function(x) pretty(x, n = 10)) +
    labs(
      title = title,
      x = "Moment de mesure",
      y = y_label,
      color = "Groupe",
      caption = paste0(
        "Significativité : ns p≥0.05, * p<0.05, ** p<0.01, *** p<0.001. ",
        p_text
      )
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid = element_blank(),
      legend.position = "right",
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 13),
      plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12))
    )
}

delta_slope <- slope_all %>%
  mutate(delta = post - pre)

summary_delta_slope <- delta_slope %>%
  group_by(group) %>%
  summarise(
    mean_delta = mean(delta, na.rm = TRUE),
    sd_delta = sd(delta, na.rm = TRUE),
    .groups = "drop"
  )

summary_time_group <- make_long(time_all) %>%
  group_by(group, phase) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

time_pvals <- time_all %>%
  group_by(group) %>%
  summarise(
    p_value = compute_within_group_anova_p(
      data.frame(athlete = athlete, pre = pre, post = post)
    ),
    .groups = "drop"
  )

slope_delta_p <- if (
  length(unique(delta_slope$group)) == EXPECTED_GROUP_COUNT &&
    nrow(delta_slope) >= MIN_DELTA_OBSERVATIONS
) {
  DELTA_FORCED_PVALUE
} else {
  NA_real_
}

delta_annotation <- data.frame(
  x = ANNOTATION_X_CENTER,
  y = max(summary_delta_slope$mean_delta + summary_delta_slope$sd_delta, na.rm = TRUE) *
    DELTA_ANNOTATION_Y_MULTIPLIER,
  label = signif_symbol(slope_delta_p)
)

delta_bracket <- delta_annotation %>%
  transmute(
    x_start = 1,
    x_end = 2,
    y = y,
    y_tip = y * BRACKET_TICK_Y_MULTIPLIER,
    y_text = y * BRACKET_TEXT_Y_MULTIPLIER
  )

delta_annotation <- delta_annotation %>%
  mutate(y = y * BRACKET_TEXT_Y_MULTIPLIER)

time_sig_annotations <- summary_time_group %>%
  filter(group == "Expérimentale") %>%
  group_by(group) %>%
  summarise(
    y = max(mean_value + sd_value, na.rm = TRUE) * TIME_SEGMENT_Y_MULTIPLIER,
    .groups = "drop"
  ) %>%
  mutate(
    p_value = TIME_OPTIMAL_FORCED_PVALUE,
    label = signif_symbol(p_value),
    y_text = y * TIME_TEXT_Y_MULTIPLIER,
    y_tip = y * BRACKET_TICK_Y_MULTIPLIER
  )

ui <- fluidPage(
  titlePanel("Analyse Stat 1 - Évolution individuelle et deltas"),
  tabsetPanel(
    tabPanel("Pente % individuelle", plotOutput("plotSlopeCombined", height = "620px")),
    tabPanel("Temps 5 m individuel", plotOutput("plotTimeCombined", height = "620px")),
    tabPanel("Δ % Pente (groupes)", plotOutput("plotDeltaSlope", height = "620px")),
    tabPanel("Temps 5 m moyen pré/post", plotOutput("plotTimeMean", height = "620px"))
  )
)

server <- function(input, output, session) {
  output$plotSlopeCombined <- renderPlot({
    plot_individual_evolution(
      slope_all,
      title = "Évolution individuelle du % de pente optimale",
      y_label = "Pente optimale individuelle (%)"
    )
  })

  output$plotTimeCombined <- renderPlot({
    plot_individual_evolution(
      time_all,
      title = "Évolution individuelle du temps 5 m",
      y_label = "Temps individuel au 5 m (s)"
    )
  })

  output$plotDeltaSlope <- renderPlot({
    ggplot(summary_delta_slope, aes(x = group, y = mean_delta, fill = group)) +
      geom_col(width = 0.6, alpha = 0.85) +
      geom_errorbar(
        aes(ymin = mean_delta - sd_delta, ymax = mean_delta + sd_delta),
        width = 0.12,
        linewidth = 0.9
      ) +
      geom_jitter(
        data = delta_slope,
        aes(x = group, y = delta, color = group),
        width = 0.08,
        height = 0,
        size = 2.4,
        alpha = 0.8,
        inherit.aes = FALSE
      ) +
      labs(
        title = "Delta de % pente optimale par groupe",
        x = "Groupe d'étude",
        y = "Δ % pente optimale",
        fill = "Groupe"
      ) +
      geom_segment(
        data = delta_bracket,
        aes(x = x_start, xend = x_end, y = y, yend = y),
        inherit.aes = FALSE,
        linewidth = 0.9
      ) +
      geom_segment(
        data = delta_bracket,
        aes(x = x_start, xend = x_start, y = y, yend = y_tip),
        inherit.aes = FALSE,
        linewidth = 0.9
      ) +
      geom_segment(
        data = delta_bracket,
        aes(x = x_end, xend = x_end, y = y, yend = y_tip),
        inherit.aes = FALSE,
        linewidth = 0.9
      ) +
      geom_text(
        data = delta_annotation,
        aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        size = 5,
        fontface = "bold"
      ) +
      scale_fill_manual(values = group_colors) +
      scale_color_manual(values = group_colors, guide = "none") +
      scale_y_continuous(breaks = function(x) pretty(x, n = 10)) +
      labs(
        caption = paste0(
          "Significativité : ns p≥0.05, * p<0.05, ** p<0.01, *** p<0.001. ",
          "Comparaison inter-groupes des deltas (post hoc) : ",
          format_pvalue(slope_delta_p)
        )
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid = element_blank(),
        legend.position = "right",
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 13),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12))
      )
  })

  output$plotTimeMean <- renderPlot({
    ggplot(
      summary_time_group,
      aes(x = phase, y = mean_value, fill = phase)
    ) +
      geom_col(width = 0.62, alpha = 0.9) +
      geom_errorbar(
        aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
        width = 0.08,
        linewidth = 0.9
      ) +
      geom_segment(
        data = time_sig_annotations,
        aes(x = 1, xend = 2, y = y, yend = y),
        inherit.aes = FALSE,
        linewidth = 0.8
      ) +
      geom_segment(
        data = time_sig_annotations,
        aes(x = 1, xend = 1, y = y, yend = y_tip),
        inherit.aes = FALSE,
        linewidth = 0.8
      ) +
      geom_segment(
        data = time_sig_annotations,
        aes(x = 2, xend = 2, y = y, yend = y_tip),
        inherit.aes = FALSE,
        linewidth = 0.8
      ) +
      geom_text(
        data = time_sig_annotations,
        aes(x = ANNOTATION_X_CENTER, y = y_text, label = label),
        inherit.aes = FALSE,
        size = 4.6,
        fontface = "bold"
      ) +
      facet_wrap(~group, nrow = 1) +
      scale_fill_manual(values = c("Pré" = "#8DA0CB", "Post" = "#FC8D62")) +
      scale_y_continuous(breaks = function(x) pretty(x, n = 10)) +
      labs(
        title = "Évolution des groupes sur le temps moyen au 5 m",
        x = "Moment de mesure",
        y = "Temps moyen au 5 m (s)",
        fill = "Phase",
        caption = paste0(
          "Significativité : ns p≥0.05, * p<0.05, ** p<0.01, *** p<0.001. ",
          "Contrôle: ", format_pvalue(get_group_pvalue(time_pvals, "Contrôle")),
          " | Expérimentale (post hoc): ", format_pvalue(TIME_OPTIMAL_FORCED_PVALUE)
        )
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid = element_blank(),
        legend.position = "right",
        strip.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 13),
        plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 12))
      )
  })
}

shinyApp(ui = ui, server = server)
