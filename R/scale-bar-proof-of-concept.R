library(ggchicklet)
library(tidyverse)
library(qualtr)


yss_input_survey <- get_responses("SV_9z95CIZ3Y2Ou3pb")

yss_input_labels <- yss_input_survey %>% map(attr, "label")


  scale_match <- "prog_mot_1"
  middle_level <- "Neutral"

  scale_data <- yss_input_survey %>%
    filter(recipient_last_name != "") %>%
    select(matches(scale_match)) %>%
    mutate_all(~fct_explicit_na(., "No Response"))

  scale_data_labels <- yss_input_labels[names(scale_data)]

  levs <- scale_data[[1]] %>% levels()


  positive_levels <- levs[-1:-which(levs == middle_level)]
  positive_levels <- positive_levels[positive_levels != "No Response"]

  negative_levels <- levs[1:(which(levs == middle_level) - 1)]


  scale_data_long <- scale_data %>%
    gather(var, val) %>%
    mutate(
      val = fct_expand(val, !!!levs) %>% fct_relevel(!!!levs)
    )

  dplt_scale_data <- scale_data_long %>%
    mutate(var = str_split_fixed(recode(var, !!!scale_data_labels), "-", 2)[, 2]) %>%
    group_by(var, val, drop = FALSE) %>%
    summarize(n = n()) %>%
    mutate(
      y = case_when(
        val %in% negative_levels ~ -n,
        TRUE ~ n
      ),
      facet = case_when(
        val %in% middle_level ~ "Neutral",
        val == "No Response" ~ "NR",
        TRUE ~ "Affirmative or Negative"
        ) %>%
        fct_relevel("Neutral", "Affirmative or Negative", "No Response"),
      level_cat = case_when(
        val %in% positive_levels ~ "Positive",
        val %in% middle_level ~ "Middle",
        val == "No Response"  ~ "No Response",
        val %in% negative_levels ~ "Negative"
      ),
      abs_level_dist = abs_level_dist(val, middle_level, levs)
    ) %>%
    group_by(
      var, level_cat
    ) %>%
    arrange(
      abs_level_dist
    ) %>%
    mutate(
      yend = lag(y, default = 0),
      y = y + yend,
      hjust = ifelse(val == "No Response", "left", "right")
    ) %>%
    ungroup()



  bputils::fonts()

  dplt_scale_data %>%
    ggplot(aes(x = var, xend = var, y = y, yend = yend)) +
    geom_segment(aes(colour = val), lwd = 7) +
    geom_text(
      aes(y = y - 0.01 * y, label = y, hjust = hjust),
      vjust = "center",
      colour = "grey20",
      data = dplt_scale_data %>%
        group_by(facet) %>%
        filter(as.numeric(val) == max(as.numeric(val)) | val == middle_level)) +
    coord_flip() +
    facet_grid(.~facet, space = "free_x", scales = "free_x") +
    scale_x_discrete(
      labels = function(x) str_wrap(x, 45)
    ) +
    scale_y_continuous(
      labels = function(x) abs(x),
      breaks = function(x) seq(round(x[1] / 50) * 50, x[2], by = 50),
      limits = function(x) c(x[1], max(x[2], 50))
    ) +
    bptheme::theme_blueprint(grid = "Xx", axis = FALSE) +
    scale_color_manual(values = c("#9BB9DC", "#D2E6F5", "grey", "#FFEB82", "#FABE78", "grey20")) +
    labs(y = "Participants", x = NULL, colour = "")



abs_level_dist <- function(val, middle_level, levs) {

  val_num <- which(levs == val)
  middle_level_num <- which(levs == middle_level)

  res <- abs(val_num - middle_level_num)

  res

}
