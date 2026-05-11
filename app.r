library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

group_colors <- c("Contrôle" = "#1F77B4", "Expérimentale" = "#D62728")

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

plot_individual_evolution <- function(df, title, y_label) {
  long_df <- make_long(df)

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
      color = "Groupe"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid = element_blank(),
      legend.position = "right"
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

ui <- fluidPage(
  titlePanel("Analyse Stat 1 - Évolution individuelle et deltas"),
  tabsetPanel(
    tabPanel("Pente % individuelle", plotOutput("plotSlopeCombined", height = "500px")),
    tabPanel("Temps 5 m individuel", plotOutput("plotTimeCombined", height = "500px")),
    tabPanel("Δ % Pente (groupes)", plotOutput("plotDeltaSlope", height = "500px")),
    tabPanel("Temps 5 m moyen pré/post", plotOutput("plotTimeMean", height = "500px"))
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
      scale_fill_manual(values = group_colors) +
      scale_color_manual(values = group_colors, guide = "none") +
      scale_y_continuous(breaks = function(x) pretty(x, n = 10)) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid = element_blank(),
        legend.position = "right"
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
      facet_wrap(~group, nrow = 1) +
      scale_fill_manual(values = c("Pré" = "#8DA0CB", "Post" = "#FC8D62")) +
      scale_y_continuous(breaks = function(x) pretty(x, n = 10)) +
      labs(
        title = "Évolution des groupes sur le temps moyen au 5 m",
        x = "Moment de mesure",
        y = "Temps moyen au 5 m (s)",
        fill = "Phase"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid = element_blank(),
        legend.position = "right",
        strip.text = element_text(face = "bold")
      )
  })
}

shinyApp(ui = ui, server = server)
