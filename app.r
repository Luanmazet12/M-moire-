library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

excel_path <- "Donnée_mémoire_2026_Vérifié.xlsx"

clean_athlete <- function(x) {
  str_to_title(str_squish(as.character(x)))
}

read_block <- function(range, col_names) {
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
  mutate(group = "Optimisé")

time_ctrl <- read_block("K16:M19", c("athlete", "pre", "post")) %>%
  mutate(group = "Contrôle")

slope_opt <- read_block("O16:Q19", c("athlete", "pre", "post")) %>%
  mutate(group = "Optimisé")

slope_ctrl <- read_block("R16:T19", c("athlete", "pre", "post")) %>%
  mutate(group = "Contrôle")

make_long <- function(df) {
  df %>%
    pivot_longer(
      cols = c(pre, post),
      names_to = "time",
      values_to = "value"
    ) %>%
    mutate(
      time = factor(time, levels = c("pre", "post"), labels = c("Pré", "Post"))
    )
}

mean_sd <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  data.frame(y = m, ymin = m - s, ymax = m + s)
}

plot_individual_evolution <- function(df, title, y_label) {
  long_df <- make_long(df)

  ggplot(long_df, aes(x = time, y = value, group = athlete, color = athlete)) +
    geom_line(linewidth = 0.9, alpha = 0.8) +
    geom_point(size = 2.2) +
    stat_summary(
      aes(group = 1),
      fun = mean,
      geom = "line",
      color = "black",
      linewidth = 1.2
    ) +
    stat_summary(
      aes(group = 1),
      fun = mean,
      geom = "point",
      color = "black",
      size = 3
    ) +
    stat_summary(
      aes(group = 1),
      fun.data = mean_sd,
      geom = "errorbar",
      width = 0.08,
      color = "black",
      linewidth = 0.9
    ) +
    labs(
      title = title,
      x = "Temps de mesure",
      y = y_label,
      color = "Athlète"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
}

delta_slope <- bind_rows(slope_opt, slope_ctrl) %>%
  mutate(delta = post - pre)

summary_delta_slope <- delta_slope %>%
  group_by(group) %>%
  summarise(
    mean_delta = mean(delta, na.rm = TRUE),
    sd_delta = sd(delta, na.rm = TRUE),
    .groups = "drop"
  )

ui <- fluidPage(
  titlePanel("Analyse Stat 1 - Évolution individuelle et deltas"),
  tabsetPanel(
    tabPanel("Pente % - Optimisé", plotOutput("plotSlopeOpt", height = "500px")),
    tabPanel("Pente % - Contrôle", plotOutput("plotSlopeCtrl", height = "500px")),
    tabPanel("Temps 5 m - Optimisé", plotOutput("plotTimeOpt", height = "500px")),
    tabPanel("Temps 5 m - Contrôle", plotOutput("plotTimeCtrl", height = "500px")),
    tabPanel("Δ % Pente (groupes)", plotOutput("plotDeltaSlope", height = "500px"))
  )
)

server <- function(input, output, session) {
  output$plotSlopeOpt <- renderPlot({
    plot_individual_evolution(
      slope_opt,
      title = "Évolution individuelle de la % pente optimale (Groupe Optimisé)",
      y_label = "% pente optimale (%)"
    )
  })

  output$plotSlopeCtrl <- renderPlot({
    plot_individual_evolution(
      slope_ctrl,
      title = "Évolution individuelle de la % pente optimale (Groupe Contrôle)",
      y_label = "% pente optimale (%)"
    )
  })

  output$plotTimeOpt <- renderPlot({
    plot_individual_evolution(
      time_opt,
      title = "Évolution individuelle du temps 5 m (Groupe Optimisé)",
      y_label = "Temps 5 m (s)"
    )
  })

  output$plotTimeCtrl <- renderPlot({
    plot_individual_evolution(
      time_ctrl,
      title = "Évolution individuelle du temps 5 m (Groupe Contrôle)",
      y_label = "Temps 5 m (s)"
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
        aes(x = group, y = delta),
        width = 0.08,
        height = 0,
        size = 2.4,
        alpha = 0.8,
        inherit.aes = FALSE
      ) +
      labs(
        title = "Delta de % pente optimale par groupe",
        x = "Groupe",
        y = "Δ % pente optimale (post - pré, points de %)",
        fill = "Groupe"
      ) +
      scale_fill_manual(values = c("Optimisé" = "#1B9E77", "Contrôle" = "#D95F02")) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "right"
      )
  })
}

shinyApp(ui = ui, server = server)
