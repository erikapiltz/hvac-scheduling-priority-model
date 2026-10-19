# ============================================================
# 02_roby_visualizations.R
#
# Run after the consolidated 01_roby_priority_model.R.
# No sports/activity-category breakdown.
# All usage charts use eligible indoor room data.
# Indoor gym bookings remain included.
# ============================================================


# 0. Packages -------------------------------------------------

required_pkgs <- c(
  "readr", "dplyr", "tidyr", "stringr",
  "ggplot2", "forcats", "scales", "ggrepel"
)

missing_pkgs <- required_pkgs[
  !vapply(
    required_pkgs,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_pkgs)) {
  stop(
    "Install packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(forcats)
  library(scales)
  library(ggrepel)
})


# 1. Read one successful model run -----------------------------

source_paths <- unlist(
  lapply(
    sys.frames(),
    function(f) {
      z <- f$ofile
      
      if (
        is.character(z) &&
        length(z) == 1L &&
        file.exists(z)
      ) {
        z
      } else {
        NULL
      }
    }
  ),
  use.names = FALSE
)

if (length(source_paths)) {
  setwd(
    dirname(
      normalizePath(
        tail(source_paths, 1),
        winslash = "/"
      )
    )
  )
}

output_dir <- "roby_outputs_v3"
figure_dir <- file.path(output_dir, "figures")
rds_path <- file.path(output_dir, "roby_analysis.rds")

if (!file.exists(rds_path)) {
  stop(
    "Run the revised 01 script successfully before 02."
  )
}

model <- readRDS(rds_path)

stopifnot(
  identical(
    model$metadata$model_version,
    "KSD_Roby_v3"
  ),
  
  all(
    model$qa_checks$passed[
      model$qa_checks$severity == "ERROR"
    ]
  )
)

if (
  !identical(
    model$metadata$revision,
    "Ginger_FMX_Compass_Kentlake_2026-09-14"
  )
) {
  stop(
    "Run the consolidated Kentlake 01 script first; ",
    "the saved results are from another revision."
  )
}

if (is.null(model$metadata$priority_tier_labels)) {
  stop(
    "Run the consolidated Ginger/Kentlake 01 script first."
  )
}

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

school_priority <- model$school_priority
room_priority <- model$room_priority
top_232_rooms <- model$top_232_rooms

selected_n <- nrow(top_232_rooms)

as_of_label <- as.character(
  model$metadata$analysis_as_of
)

stopifnot(
  all(room_priority$indoor_outdoor == "Indoor"),
  
  !any(
    top_232_rooms$building ==
      "Mattson Middle School"
  )
)

# Remove only obsolete generated chart names in this folder.
for (
  stub in c(
    "07_activity_mix_by_school",
    "13_already_deployed_exclusions"
  )
) {
  
  old_files <- file.path(
    figure_dir,
    paste0(stub, c(".png", ".pdf"))
  )
  
  if (any(file.exists(old_files))) {
    unlink(
      old_files[file.exists(old_files)]
    )
  }
}


# 2. Helpers --------------------------------------------------

clean_school_label <- function(x) {
  x |>
    str_replace(" Elementary School$", " ES") |>
    str_replace(" Middle School$", " MS") |>
    str_replace(" High School$", " HS")
}

for (
  nm in c(
    "school_priority",
    "room_priority",
    "top_232_rooms"
  )
) {
  
  x <- get(nm)
  x$building_short <- clean_school_label(x$building)
  assign(nm, x)
}

heating_colors <- c(
  "Electric" = "#2EAD73",
  "Mixed" = "#E5BE44",
  "Gas - whole-school boiler" = "#D95363",
  "Unknown" = "#909090"
)

notes <- paste0(
  "As of ",
  as_of_label,
  ". Scheduled room-hours are not measured HVAC runtime ",
  "or estimated savings."
)

theme_roby <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 15
      ),
      
      plot.subtitle = element_text(
        size = 10
      ),
      
      plot.caption = element_text(
        size = 9,
        hjust = 0
      ),
      
      axis.title = element_text(
        face = "bold"
      ),
      
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

chart_index <- tibble::tibble(
  chart = character(),
  file_stub = character()
)

save_chart <- function(
    p,
    stub,
    width = 12,
    height = 8
) {
  
  message("Saving chart: ", stub)
  
  for (ext in c("png", "pdf")) {
    ggsave(
      file.path(
        figure_dir,
        paste0(stub, ".", ext)
      ),
      
      plot = p,
      width = width,
      height = height,
      units = "in",
      dpi = 300,
      bg = "white"
    )
  }
  
  chart_index <<- bind_rows(
    chart_index,
    
    tibble::tibble(
      chart = p$labels$title,
      file_stub = stub
    )
  )
}

bar_plot <- function(
    data,
    label,
    value,
    title,
    subtitle,
    axis_title
) {
  
  ggplot(
    data,
    
    aes(
      x = reorder(
        .data[[label]],
        .data[[value]]
      ),
      y = .data[[value]]
    )
  ) +
    geom_col() +
    coord_flip() +
    
    scale_y_continuous(
      labels = label_number(
        big.mark = ","
      )
    ) +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = axis_title,
      caption = notes
    ) +
    
    theme_roby()
}


# 3. Recommended installs by school ---------------------------

installs_by_school <- top_232_rooms |>
  count(
    building,
    building_short,
    heating_system,
    name = "recommended_rooms"
  )

p <- ggplot(
  installs_by_school,
  
  aes(
    reorder(
      building_short,
      recommended_rooms
    ),
    recommended_rooms,
    fill = heating_system
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(label = recommended_rooms),
    hjust = -.15,
    size = 3
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = heating_colors
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0, .10)
    )
  ) +
  
  labs(
    title = "Recommended Roby Installs by School",
    
    subtitle = paste(
      selected_n,
      "new planning candidates; Mattson is separate.",
      "Colors describe heating, not savings."
    ),
    
    x = NULL,
    y = "Candidate rooms / provisional units",
    fill = "School heating",
    caption = notes
  ) +
  
  theme_roby()

save_chart(
  p,
  "01_recommended_installs_by_school",
  height = 10
)

write_csv(
  installs_by_school,
  
  file.path(
    output_dir,
    "roby_installs_by_school.csv"
  )
)


# 4. School and room scores -----------------------------------

p <- bar_plot(
  school_priority |>
    mutate(
      score_points =
        100 * school_priority_score
    ) |>
    slice_max(
      score_points,
      n = 25,
      with_ties = FALSE
    ),
  
  "building_short",
  "score_points",
  "School-Level Roby Priority",
  "Relative composite score; eligible indoor use only.",
  "Priority points (0-100)"
)

save_chart(
  p,
  "02_school_priority_score_top_25",
  height = 9
)

first_40 <- top_232_rooms |>
  arrange(district_rank) |>
  slice_head(n = 40) |>
  mutate(
    room_label = paste0(
      district_rank,
      ". ",
      building_short,
      " - ",
      resource
    ),
    
    room_label = factor(
      room_label,
      levels = rev(room_label)
    )
  )

p <- ggplot(
  first_40,
  
  aes(
    room_label,
    100 * room_priority_score
  )
) +
  
  geom_col() +
  coord_flip() +
  
  labs(
    title = "First 40 Candidates in Deployment Order",
    
    subtitle = paste(
      "Priority tiers determine order first;",
      "bars show within-model scores, not savings."
    ),
    
    x = NULL,
    y = "Room priority points (0-100)",
    caption = notes
  ) +
  
  theme_roby()

save_chart(
  p,
  "03_top_40_room_candidates",
  width = 14,
  height = 12
)


# 5. Energy priority vs indoor room demand ---------------------

# Larger chart, abbreviations, and no connector lines.
plot_schools <- school_priority |>
  mutate(
    school_abbrev = coalesce(
      na_if(allowed_prefix, ""),
      building_short
    )
  )

stopifnot(
  !anyDuplicated(plot_schools$school_abbrev)
)

write_csv(
  plot_schools |>
    distinct(
      school_abbrev,
      building
    ) |>
    arrange(school_abbrev),
  
  file.path(
    output_dir,
    "roby_school_abbreviation_key.csv"
  )
)

missing_energy_n <- sum(
  !is.finite(plot_schools$wneui)
)

plot_schools <- plot_schools |>
  filter(
    is.finite(wneui),
    is.finite(indoor_hours)
  )

p <- ggplot(
  plot_schools,
  
  aes(
    x = wneui,
    y = indoor_hours
  )
) +
  
  geom_point(
    aes(
      size = unique_indoor_resources,
      color = heating_system
    ),
    alpha = .85
  ) +
  
  geom_text_repel(
    aes(label = school_abbrev),
    
    color = "black",
    fontface = "bold",
    size = 4.3,
    box.padding = .25,
    point.padding = .30,
    force = .6,
    force_pull = 5,
    segment.color = NA,
    max.overlaps = Inf,
    max.time = 3,
    seed = 415
  ) +
  
  scale_color_manual(
    values = heating_colors
  ) +
  
  scale_size_area(
    max_size = 9
  ) +
  
  scale_x_continuous(
    expand = expansion(
      mult = c(.07, .07)
    )
  ) +
  
  scale_y_continuous(
    labels = label_number(big.mark = ","),
    limits = c(0, NA),
    
    expand = expansion(
      mult = c(.04, .08)
    )
  ) +
  
  labs(
    title = "Energy Priority vs Indoor Room Demand",
    
    subtitle = paste0(
      "Further right = higher energy intensity. ",
      "Higher = more indoor scheduling demand.\n",
      "Recent use receives more weight; future bookings are excluded."
    ),
    
    x = "School WNEUI (inherited lookup; reporting periods need verification)",
    y = "Recency-weighted indoor room-hours",
    color = "School heating",
    size = "Eligible room labels",
    
    caption = paste0(
      notes,
      
      "\nKentlake: eligible FMX-listed indoor rooms. ",
      "Other high schools: room-specific candidates.",
      
      "\nBubble size counts candidate FMX labels, ",
      "not verified installations.",
      
      if (missing_energy_n > 0) {
        paste0(
          " ",
          missing_energy_n,
          " school/building(s) with missing WNEUI are not plotted."
        )
      } else {
        ""
      }
    )
  ) +
  
  theme_roby() +
  
  theme(
    plot.title = element_text(
      size = 21,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 12
    ),
    
    axis.title = element_text(
      size = 14,
      face = "bold"
    ),
    
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.box = "vertical",
    
    plot.caption = element_text(
      size = 10,
      hjust = 0
    ),
    
    plot.margin = margin(
      14, 20, 14, 14
    )
  ) +
  
  guides(
    color = guide_legend(
      order = 1,
      nrow = 1
    ),
    
    size = guide_legend(
      order = 2,
      nrow = 1
    )
  )

save_chart(
  p,
  "04_wneui_vs_indoor_hours",
  width = 18,
  height = 11
)


# 6. After-hours and community/public demand -------------------

p <- bar_plot(
  school_priority |>
    slice_max(
      after_hours_hours,
      n = 25,
      with_ties = FALSE
    ),
  
  "building_short",
  "after_hours_hours",
  "Indoor After-Hours Demand by School",
  
  paste(
    "Outside ranking reference:",
    model$metadata$reference_window,
    "(not verified BAS hours)."
  ),
  
  "Recency-weighted room-hours outside reference window"
)

save_chart(
  p,
  "05_after_hours_demand_by_school",
  height = 9
)

p <- bar_plot(
  school_priority |>
    slice_max(
      community_events,
      n = 25,
      with_ties = FALSE
    ),
  
  "building_short",
  "community_events",
  "Indoor Community/Public Use by School",
  
  paste(
    "Text-based community/public classification;",
    "each school occurrence counted once across its indoor rooms."
  ),
  
  "Recency-weighted scheduled occurrences"
)

save_chart(
  p,
  "06_community_public_use_by_school",
  height = 9
)


# 7. Indoor room use by space type -----------------------------

# Not a sports-category breakdown.
# Indoor gyms remain; outdoor resources are absent from the model.
# Includes all eligible candidates, not only the selected rooms.

space_use <- room_priority |>
  group_by(
    building_short,
    resource_type
  ) |>
  summarise(
    hours = sum(hours),
    .groups = "drop"
  ) |>
  filter(hours > 0)

top_use_schools <- space_use |>
  group_by(building_short) |>
  summarise(
    total = sum(hours),
    .groups = "drop"
  ) |>
  slice_max(
    total,
    n = 20,
    with_ties = FALSE
  )

space_use <- space_use |>
  inner_join(
    top_use_schools,
    by = "building_short"
  )

p <- ggplot(
  space_use,
  
  aes(
    reorder(
      building_short,
      total
    ),
    
    hours,
    fill = resource_type
  )
) +
  
  geom_col() +
  coord_flip() +
  
  scale_y_continuous(
    labels = label_number(
      big.mark = ","
    )
  ) +
  
  labs(
    title = "Indoor Room Use by Space Type",
    
    subtitle = paste(
      "Sports-category breakdown removed.",
      "Gym, library, MPR and other eligible indoor use retained."
    ),
    
    x = NULL,
    y = "Recency-weighted indoor room-hours",
    fill = "Room type",
    caption = notes
  ) +
  
  theme_roby() +
  
  guides(
    fill = guide_legend(
      nrow = 3,
      byrow = TRUE
    )
  )

save_chart(
  p,
  "07_indoor_hours_by_room_type",
  width = 14,
  height = 10
)


# 8. Score distribution ---------------------------------------

p <- ggplot(
  room_priority,
  
  aes(
    100 * room_priority_score
  )
) +
  
  geom_histogram(bins = 25) +
  
  labs(
    title = "Room Priority Score Distribution",
    
    subtitle = paste(
      "All eligible indoor candidates;",
      "not savings probabilities."
    ),
    
    x = "Room priority points (0-100)",
    y = "Candidate rooms",
    caption = notes
  ) +
  
  theme_roby()

save_chart(
  p,
  "08_room_priority_score_distribution"
)


# 9. Selected room types --------------------------------------

room_type_counts <- top_232_rooms |>
  count(
    resource_type,
    name = "recommended_rooms"
  )

p <- bar_plot(
  room_type_counts,
  
  "resource_type",
  "recommended_rooms",
  "Recommended Rooms by Resource Type",
  
  paste(
    selected_n,
    "selected planning candidates;",
    "combined cafeteria/MPR labels remain combined."
  ),
  
  "Candidate rooms"
)

save_chart(
  p,
  "09_top_232_by_resource_type"
)


# 10-11. Optional QA charts -----------------------------------

if (nrow(model$excluded_rows)) {
  
  excluded_counts <- model$excluded_rows |>
    count(
      exclude_reason,
      name = "requests"
    )
  
  p <- bar_plot(
    excluded_counts,
    
    "exclude_reason",
    "requests",
    "Source Requests Excluded Before Room Scoring",
    
    paste(
      "Request-level exclusions;",
      "room/resource exclusions are documented separately",
      "in the QA CSVs."
    ),
    
    "Source requests"
  )
  
  save_chart(
    p,
    "10_excluded_rows_by_reason",
    width = 14
  )
}

if (nrow(model$missing_wneui)) {
  
  p <- ggplot(
    model$missing_wneui,
    
    aes(
      clean_school_label(building)
    )
  ) +
    
    geom_bar() +
    coord_flip() +
    
    labs(
      title = "Candidate Schools Missing WNEUI",
      x = NULL,
      y = "Schools"
    ) +
    
    theme_roby()
  
  save_chart(
    p,
    "11_missing_wneui_schools"
  )
}


# 12. Weighted school-score components ------------------------

component_labels <- c(
  hours = "Indoor use",
  wneui = "WNEUI",
  after_hours = "After-hours",
  community = "Community/public",
  rooms = "Active room demand"
)

components <- school_priority |>
  slice_head(n = 20) |>
  select(
    building_short,
    school_priority_score,
    hours_norm,
    wneui_norm,
    after_hours_norm,
    community_norm,
    rooms_norm
  ) |>
  pivot_longer(
    ends_with("_norm"),
    names_to = "input",
    values_to = "normalized_value"
  ) |>
  mutate(
    input = str_remove(
      input,
      "_norm$"
    ),
    
    contribution =
      100 *
      normalized_value *
      unname(
        model$metadata$school_weights[input]
      ),
    
    component = unname(
      component_labels[input]
    )
  )

p <- ggplot(
  components,
  
  aes(
    reorder(
      building_short,
      school_priority_score
    ),
    
    contribution,
    fill = component
  )
) +
  
  geom_col() +
  coord_flip() +
  
  labs(
    title = "What Drives School Priority",
    
    subtitle = paste(
      "Weighted components add to the school's priority score;",
      "these are not activity/sports categories."
    ),
    
    x = NULL,
    y = "Priority points",
    fill = "Score component",
    caption = notes
  ) +
  
  theme_roby()

save_chart(
  p,
  "12_school_score_components_top_20",
  height = 9
)


# 13. Evidence across the planning list ------------------------

evidence_by_rank <- top_232_rooms |>
  mutate(
    band_start =
      50L * ((district_rank - 1L) %/% 50L) + 1L,
    
    rank_band = paste0(
      band_start,
      "-",
      pmin(
        band_start + 49L,
        selected_n
      )
    ),
    
    evidence_confidence = factor(
      evidence_confidence,
      levels = c(
        "High",
        "Moderate",
        "Low"
      )
    )
  ) |>
  count(
    rank_band,
    band_start,
    evidence_confidence,
    name = "rooms"
  )

p <- ggplot(
  evidence_by_rank,
  
  aes(
    reorder(
      rank_band,
      band_start
    ),
    
    rooms,
    fill = evidence_confidence
  )
) +
  
  geom_col() +
  
  labs(
    title = "Evidence Behind the Candidate List",
    
    subtitle = paste(
      "Heuristic strength of room/schedule evidence.",
      "Compatibility and avoidable runtime remain separate checks."
    ),
    
    x = "Deployment-rank group",
    y = "Candidate rooms",
    fill = "Evidence confidence",
    caption = notes
  ) +
  
  theme_roby()

save_chart(
  p,
  "13_evidence_confidence_by_rank"
)


# 14. Raw historical coverage, separate from scoring -----------

age_totals <- room_priority |>
  summarise(
    across(
      c(
        hours_recent,
        hours_year_2,
        hours_year_3,
        hours_older
      ),
      sum
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "age_band",
    values_to = "hours"
  ) |>
  mutate(
    age_band = factor(
      age_band,
      
      levels = c(
        "hours_recent",
        "hours_year_2",
        "hours_year_3",
        "hours_older"
      ),
      
      labels = c(
        "Past 365 dates",
        "Previous 365 dates",
        "Third 365-date period",
        "Older history"
      )
    )
  )

p <- ggplot(
  age_totals,
  
  aes(
    age_band,
    hours
  )
) +
  
  geom_col() +
  
  scale_y_continuous(
    labels = label_number(
      big.mark = ","
    )
  ) +
  
  labs(
    title = "Documented Indoor Room-Hours by Age",
    
    subtitle = paste(
      "Unweighted historical totals, including nominal",
      "reconstructed dates. Future/undated repeats excluded."
    ),
    
    x = NULL,
    y = "Historical room-hours before recency weighting",
    caption = notes
  ) +
  
  theme_roby()

save_chart(
  p,
  "14_historical_hours_by_age",
  width = 13
)


# 15. Explain the tier-based deployment order ------------------

# Read the group descriptions from 01.
tier_labels <- model$metadata$priority_tier_labels

rank_view <- top_232_rooms |>
  arrange(district_rank) |>
  mutate(
    priority_group = factor(
      as.character(priority_tier),
      levels = names(tier_labels),
      labels = unname(tier_labels)
    )
  )

group_boundaries <- rank_view |>
  group_by(priority_tier) |>
  summarise(
    first_rank = min(district_rank),
    .groups = "drop"
  ) |>
  filter(
    first_rank > min(rank_view$district_rank)
  )

p <- ggplot(
  rank_view,
  
  aes(
    x = district_rank,
    y = 100 * room_priority_score
  )
) +
  
  geom_vline(
    data = group_boundaries,
    aes(xintercept = first_rank - .5),
    
    color = "grey75",
    linetype = "dashed",
    linewidth = .4
  ) +
  
  geom_point(
    aes(color = priority_group),
    size = 2.5,
    alpha = .9
  ) +
  
  scale_y_continuous(
    limits = c(0, NA),
    
    expand = expansion(
      mult = c(0, .10)
    )
  ) +
  
  labs(
    title = "Why Rooms Appear in This Deployment Order",
    
    subtitle = paste0(
      "Each dot is one candidate room. ",
      "Left to right shows the proposed deployment order.\n",
      "KSD's priority groups come first; ",
      "higher-scoring rooms come first within each group."
    ),
    
    x = "Position in the proposed room list",
    y = "Model priority points - not percent savings",
    color = "Deployment group",
    
    caption = paste(
      str_wrap(
        paste(
          "Dashed lines mark a new priority group.",
          "Scores can jump at these boundaries",
          "because the list is not sorted by score alone.",
          "Lower scores do not prove that a room is unused",
          "or that Roby would provide no benefit."
        ),
        width = 150
      ),
      
      "This run has no fuel penalty or school-level summer-program bonus.",
      
      notes,
      
      sep = "\n"
    )
  ) +
  
  theme_roby() +
  
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 12
    ),
    
    axis.title = element_text(
      size = 13,
      face = "bold"
    ),
    
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 10),
    
    plot.caption = element_text(
      size = 10,
      hjust = 0
    ),
    
    plot.margin = margin(
      14, 18, 14, 14
    )
  ) +
  
  guides(
    color = guide_legend(
      ncol = 2,
      byrow = TRUE
    )
  )

save_chart(
  p,
  "15_scores_along_deployment_order",
  width = 18,
  height = 10
)


# Finish ------------------------------------------------------

write_csv(
  chart_index,
  
  file.path(
    output_dir,
    "roby_visual_summary.csv"
  )
)

message(
  "Charts saved to: ",
  normalizePath(figure_dir)
)

message(
  "Boss-facing starting charts: 01, 04, and 09. ",
  "No sports/activity mix chart is created."
)

print(
  installs_by_school |>
    arrange(desc(recommended_rooms))
)