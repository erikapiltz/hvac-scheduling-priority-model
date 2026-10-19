# ============================================================
# 01_roby_priority_model.R
# Ginger/FMX/Compass model, including campus-wide Kentlake.
#
# Target: 232 NEW planning candidates.
# Mattson is tracked separately and is NOT deducted from 232.
#
# Every ranked room must exist in the FMX input.
# Campus Compass coverage supports candidate screening;
# individual room/controller mapping remains separately tracked.
#
# Run this file completely before running 02.
# ============================================================


# 0. Packages -------------------------------------------------

required_pkgs <- c(
  "readr", "dplyr", "tidyr", "stringr", "lubridate", "tibble"
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

if (packageVersion("dplyr") < "1.1.4") {
  stop("Update dplyr before running this script.")
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(tibble)
})

progress <- function(x) {
  message(
    format(Sys.time(), "%H:%M:%S"),
    " | ",
    x
  )
}


# 1. Settings -------------------------------------------------

install_limit <- 232L

# Fixed analysis date makes reruns reproducible.
analysis_as_of <- as.Date("2026-09-14")

model_timezone <- "America/Los_Angeles"

# Blank: find a schedule CSV or open a file picker.
schedule_file_manual <- ""

# Retain the output interface used by 02.
output_dir <- "roby_outputs_v3"

write_excel <- TRUE

# Keep the previously requested classroom-LAST fallback.
# Generic "Classroom", whole-wing labels, and classroom bundles
# do not count as precise rooms.
include_classrooms_after_shared <- TRUE

# FALSE retains the first-pass planning analysis.
# TRUE restricts candidates to individually confirmed rooms
# recorded in room_verification below.
require_individual_compass_confirmation <- FALSE

# Ginger's recommendation: fuel category alone does not
# penalize or exclude a room.
heating_score_weight <- 0

# Summer survey remains context only.
# Actual eligible summer bookings in FMX still count.
summer_program_year <- 2026L

room_weights <- c(
  hours = .30,
  after_hours = .25,
  wneui = .20,
  community = .15,
  events = .05,
  school = .05,
  summer = 0
)

school_weights <- c(
  hours = .35,
  wneui = .25,
  after_hours = .20,
  community = .10,
  rooms = .10
)

recency_weights <- c(
  recent = 1,
  year_2 = .50,
  year_3 = .25
)

future_preview_days <- 365L

# Ranking reference only.
# These are NOT verified bell/BAS/compliance operating hours.
reference_start_hour <- 7
reference_end_hour <- 16

# Add only documented closure dates.
reference_closed_dates <- as.Date(character())

max_reconstructed_span_days <- 730L

# Screening heuristics, not validated savings probabilities.
strong_recent_dates <- 12L
strong_recent_after_hours <- 24

moderate_recent_dates <- 3L
moderate_recent_after_hours <- 4

broad_bundle_threshold <- 10L

priority_tier_labels <- c(
  "1" = "1. KW/KR nominated or previously allowed shared spaces",
  "2" = "2. Middle-school shared spaces and nominated rooms",
  "3" = "3. Ginger's elementary/Kentlake spaces; verified Admin Board Room",
  "4" = "4. Other previously requested shared spaces",
  "5" = "5. Other shared spaces",
  "6" = "6. Other indoor rooms",
  "7" = "7. Classrooms / instructional rooms (last)"
)

stopifnot(
  abs(sum(room_weights) - 1) < 1e-10,
  abs(sum(school_weights) - 1) < 1e-10,
  heating_score_weight == 0,
  room_weights[["summer"]] == 0,
  reference_start_hour >= 0,
  reference_end_hour <= 24,
  reference_start_hour < reference_end_hour
)


# 2. Locate project/input -------------------------------------

# Resolve the sourced script's folder when available.
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

script_dir <- if (length(source_paths)) {
  dirname(
    normalizePath(
      tail(source_paths, 1),
      winslash = "/"
    )
  )
} else {
  getwd()
}

find_schedule_file <- function(manual_path) {
  
  if (nzchar(manual_path)) {
    candidates <- unique(
      c(
        path.expand(manual_path),
        file.path(script_dir, manual_path)
      )
    )
    
    candidates <- candidates[file.exists(candidates)]
    
    if (!length(candidates)) {
      stop("Schedule CSV not found: ", manual_path)
    }
    
    return(
      normalizePath(
        candidates[1],
        winslash = "/"
      )
    )
  }
  
  paths <- unique(
    unlist(
      lapply(
        unique(c(script_dir, getwd())),
        function(d) {
          list.files(
            d,
            pattern = "schedule.*request.*\\.csv$",
            ignore.case = TRUE,
            full.names = TRUE
          )
        }
      )
    )
  )
  
  paths <- unique(
    normalizePath(
      paths,
      winslash = "/",
      mustWork = FALSE
    )
  )
  
  if (length(paths) == 1L) {
    return(paths[1])
  }
  
  if (interactive()) {
    message(
      "Choose the original FMX schedule-request CSV ",
      "(not a model output)."
    )
    
    return(
      normalizePath(
        file.choose(),
        winslash = "/"
      )
    )
  }
  
  stop(
    "Set schedule_file_manual to the exact schedule CSV path."
  )
}

schedule_file <- find_schedule_file(schedule_file_manual)

# Use the project/script folder for this run and subsequent 02.
setwd(script_dir)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Back up existing outputs before replacing them.
# Do not modify the source CSV.
old_outputs <- list.files(
  output_dir,
  pattern = "^roby_.*\\.(csv|xlsx|rds)$",
  full.names = TRUE
)

if (length(old_outputs)) {
  
  backup_dir <- tempfile(
    paste0(
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      "_"
    ),
    tmpdir = file.path(output_dir, "previous_runs")
  )
  
  dir.create(
    backup_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (!all(file.copy(old_outputs, backup_dir))) {
    stop("Could not back up previous outputs.")
  }
}

rds_path <- file.path(output_dir, "roby_analysis.rds")

# Prevent 02 from using an old successful result after a failed run.
if (file.exists(rds_path)) {
  unlink(rds_path)
}

progress(paste("Reading:", schedule_file))


# 3-5. Input columns, dates and common helpers ------------------

clean_col_names <- function(x) {
  x <- str_replace_all(
    str_to_lower(x),
    "[^a-z0-9]+",
    "_"
  )
  
  make.unique(
    str_replace_all(x, "^_+|_+$", ""),
    sep = "_"
  )
}

raw_schedule <- read_csv(
  schedule_file,
  col_types = cols(.default = col_character()),
  name_repair = "unique",
  show_col_types = FALSE
)

names(raw_schedule) <- clean_col_names(names(raw_schedule))

get_col <- function(
    candidates,
    required = FALSE,
    preserve_lines = FALSE
) {
  
  hit <- intersect(
    clean_col_names(candidates),
    names(raw_schedule)
  )
  
  if (!length(hit) && required) {
    stop(
      "Missing column: ",
      paste(candidates, collapse = " / ")
    )
  }
  
  x <- if (length(hit)) {
    raw_schedule[[hit[1]]]
  } else {
    rep(NA_character_, nrow(raw_schedule))
  }
  
  x <- if (preserve_lines) {
    str_trim(x)
  } else {
    str_squish(x)
  }
  
  x[x == ""] <- NA_character_
  
  x
}

parse_dt <- function(x) {
  suppressWarnings(
    parse_date_time(
      x,
      orders = c(
        "mdy IMS p",
        "mdy IM p",
        "mdy HMS",
        "mdy HM",
        "ymd HMS",
        "ymd HM",
        "mdy",
        "ymd"
      ),
      tz = model_timezone,
      quiet = TRUE
    )
  )
}

key <- function(x) {
  str_to_lower(
    str_squish(
      coalesce(as.character(x), "")
    )
  )
}

collapse_values <- function(x) {
  paste(
    sort(unique(x[!is.na(x) & x != ""])),
    collapse = " | "
  )
}

safe_minmax <- function(x) {
  good <- is.finite(x)
  out <- rep(0, length(x))
  
  if (!any(good)) {
    return(out)
  }
  
  r <- range(x[good])
  
  out[good] <- if (r[1] == r[2]) {
    as.numeric(r[2] > 0)
  } else {
    (x[good] - r[1]) / diff(r)
  }
  
  out
}


# 6. Existing school/WNEUI inputs ------------------------------

# WNEUI retained from the original script.
# Do not substitute Site EUI or Source EUI into this column.
#
# Original heating classifications remain FALLBACK data.
# Ginger's corrections below take precedence.
#
# Heating codes are categories, not measured fuel percentages:
# 0 = electric; .5 = mixed; 1 = gas category; NA = unknown.

school_lookup <- tribble(
  ~canonical_building, ~school_level, ~allowed_prefix, ~wneui, ~heating_gas_code,
  
  "Canyon Ridge Middle School",       "Middle",     "CR", 58.3, 1,
  "Cedar Heights Middle School",      "Middle",     "CH", 27.2, 0,
  "Mattson Middle School",            "Middle",     "MA", 37.0, NA_real_,
  "Meeker Middle School",             "Middle",     "MK", 61.1, 1,
  "Meridian Middle School",           "Middle",     "MM", 40.4, 0,
  "Mill Creek Middle School",         "Middle",     "MC", 49.7, 0,
  "Northwood Middle School",          "Middle",     "NW", 27.1, 0,
  
  "Carriage Crest Elementary School", "Elementary", "CC", 28.4, 1,
  "Cedar Valley Elementary School",   "Elementary", "CV", 49.1, 0,
  "Covington Elementary School",      "Elementary", "CO", 29.6, 0,
  "Crestwood Elementary School",      "Elementary", "CW", 30.5, NA_real_,
  "Daniel Elementary School",         "Elementary", "DE", 34.1, 0,
  "East Hill Elementary School",      "Elementary", "EH", 29.5, .5,
  "Emerald Park Elementary School",   "Elementary", "EP", 37.5, 0,
  "Fairwood Elementary School",       "Elementary", "FW", 44.0, .5,
  "Glenridge Elementary School",      "Elementary", "GR", 36.1, 0,
  "Grass Lake Elementary School",     "Elementary", "GL", 46.1, 1,
  "Horizon Elementary School",        "Elementary", "HE", 26.7, 0,
  "Jenkins Creek Elementary School",  "Elementary", "JC", 35.7, 0,
  "Kent Elementary School",           "Elementary", "KE", 40.8, 1,
  "Lake Youngs Elementary School",    "Elementary", "LY", 48.0, 1,
  "Martin Sortun Elementary School",  "Elementary", "MS", 34.4, 0,
  "Meadow Ridge Elementary School",   "Elementary", "MR", 34.1, NA_real_,
  "Meridian Elementary School",       "Elementary", "ME", 41.5, 0,
  "Millennium Elementary School",     "Elementary", "ML", 31.5, 0,
  "Neely-O'Brien Elementary School",  "Elementary", "NO", 37.1, 1,
  "Panther Lake Elementary School",   "Elementary", "PL", 29.8, .5,
  "Park Orchard Elementary School",   "Elementary", "PO", 44.3, 0,
  "Pine Tree Elementary School",      "Elementary", "PT", 52.9, 1,
  "Ridgewood Elementary School",      "Elementary", "RW", 30.9, 1,
  "River Ridge Elementary School",    "Elementary", "RR", 22.9, 0,
  "Sawyer Woods Elementary School",   "Elementary", "SW", 37.0, 1,
  "Scenic Hill Elementary School",    "Elementary", "SH", 66.4, 1,
  "Soos Creek Elementary School",     "Elementary", "SC", 39.9, 0,
  "Springbrook Elementary School",    "Elementary", "SB", 45.4, 0,
  "Sunrise Elementary School",        "Elementary", "SR", 29.7, 0,
  
  "Kentwood High School",             "High",       "KW", 32.7, NA_real_,
  "Kentridge High School",            "High",       "KR", 69.6, NA_real_,
  "Kent-Meridian High School",        "High",       "KM", 57.4, NA_real_,
  "Kentlake High School",             "High",       "KL", 40.2, NA_real_,
  
  "Kent Laboratory Academy",          "Other",      NA,   16.3, NA_real_,
  "Kent Valley Early Learning Center","Other",      NA,   NA,   NA_real_,
  "The Outreach Program (TOP)",       "Other",      NA,   NA,   NA_real_,
  "KM Shop Building",                 "Support",    NA,   81.8, NA_real_,
  "Admin Building A",                 "Support",    NA,   NA,   NA_real_,
  "Admin Building D",                 "Support",    NA,   NA,   NA_real_,
  "Central Kitchen",                  "Support",    NA,   NA,   NA_real_,
  "Administration Center",            "Support",    NA,   NA,   NA_real_,
  "Administration Building",          "Support",    "ADMIN", NA, NA_real_,
  "Transportation Office",            "Support",    NA,   NA,   NA_real_,
  "Portables",                        "Other",      NA,   NA,   NA_real_,
  "IGrad",                            "Other",      NA,   NA,   NA_real_,
  "Kent Virtual Academy",             "Other",      NA,   NA,   NA_real_
)

# Summer survey snapshot: context only, not a scoring bonus.
# Source: Summer activities at elementary schools.xlsx, Sheet1.
# Unanswered and unlisted schools remain NA.
# "Cedar Vakket" in row 24 retains the previous CV mapping.

summer_lookup <- tribble(
  ~allowed_prefix, ~has_summer_program, ~summer_source_row,
  
  "GR", FALSE, 2L,
  "EP", TRUE, 3L,
  "KE", FALSE, 4L,
  "EH", FALSE, 5L,
  "SW", FALSE, 6L,
  "JC", FALSE, 7L,
  "CO", FALSE, 8L,
  "DE", FALSE, 9L,
  "CC", FALSE, 10L,
  "LY", FALSE, 11L,
  "HE", TRUE, 12L,
  "RW", FALSE, 13L,
  "SR", FALSE, 14L,
  "PT", FALSE, 15L,
  "ML", FALSE, 16L,
  "PL", TRUE, 17L,
  "NO", TRUE, 18L,
  "SH", FALSE, 19L,
  "FW", FALSE, 20L,
  "SC", FALSE, 21L,
  "SB", TRUE, 22L,
  "MR", TRUE, 23L,
  "CV", FALSE, 24L,
  "CW", FALSE, 25L,
  "RR", TRUE, 26L,
  "GL", FALSE, 27L,
  "PO", FALSE, 28L,
  "MS", NA, 29L,
  "ME", NA, 30L
)

summer_notes <- tribble(
  ~allowed_prefix, ~summer_location_notes,
  
  "EP", "KSD district kinder screener headed by Jennifer Antrobus",
  "HE", "Camp Wakapala (Kent Parks) gym, MPR, rooms 407 and 408, playground",
  "PL", "Port 4 , gym and MPR",
  "NO", "Gym/Classrooms",
  "SB", "MPR, Portable 4. Gym hallway 400 classrooms",
  "MR", "K Jump Start; K rooms and SC Primary room",
  "RR", "MPR, Library, classrooms"
)

already_roby_schools <- "Mattson Middle School"

ginger_source <-
  "Ginger Ott - Roby Room Breakdown emails, 2026-09-09"

ginger_electric <- c(
  "SC", "SB", "CV", "HE", "EH", "CC", "JC"
)

school_lookup <- school_lookup |>
  mutate(
    legacy_heating_gas_code = heating_gas_code,
    
    heating_gas_code = case_when(
      allowed_prefix %in% ginger_electric ~ 0,
      
      # Gas gym units contradict the earlier whole-campus
      # electric classification. Other MC rooms remain unknown.
      allowed_prefix == "MC" ~ NA_real_,
      
      TRUE ~ heating_gas_code
    ),
    
    heating_source = case_when(
      allowed_prefix %in% ginger_electric ~
        ginger_source,
      
      allowed_prefix == "MC" ~
        paste(ginger_source, "- gas gym units; rest unknown"),
      
      is.na(heating_gas_code) ~
        "Unknown",
      
      TRUE ~
        "Earlier user-marked chart; Ginger supplied no replacement for this campus"
    ),
    
    heating_system = case_when(
      heating_gas_code == 0 ~ "Electric",
      heating_gas_code == .5 ~ "Mixed",
      heating_gas_code == 1 ~ "Gas - whole-school boiler",
      TRUE ~ "Unknown"
    ),
    
    # Kentlake is confirmed campus-wide.
    # Other high schools remain room-specific.
    school_compass_confirmed = case_when(
      school_level %in% c("Elementary", "Middle") ~ TRUE,
      allowed_prefix %in% "KL" ~ TRUE,
      TRUE ~ NA
    ),
    
    school_compass_source = case_when(
      school_level %in% c("Elementary", "Middle") ~
        "KSD confirmation in this discussion",
      
      allowed_prefix %in% "KL" ~
        "Ginger confirmation relayed by Erika: all of Kentlake High School is on Compass",
      
      TRUE ~
        "School-wide coverage unconfirmed; use room-specific evidence"
    ),
    
    # Eligibility is NOT a room allocation or an FMX override.
    compass_school_eligible =
      school_level %in% c("Elementary", "Middle") |
      allowed_prefix %in% "KL",
    
    already_roby_excluded =
      canonical_building %in% already_roby_schools,
    
    wneui_source = if_else(
      allowed_prefix %in% "ADMIN",
      "No WNEUI supplied for this admin building label",
      "Original R lookup; reporting period not supplied"
    ),
    
    wneui_period_verified = FALSE
  ) |>
  left_join(
    summer_lookup,
    by = "allowed_prefix",
    na_matches = "never"
  ) |>
  left_join(
    summer_notes,
    by = "allowed_prefix",
    na_matches = "never"
  ) |>
  mutate(
    summer_year = if_else(
      !is.na(summer_source_row),
      summer_program_year,
      NA_integer_
    ),
    
    summer_score = 0,
    
    summer_source = if_else(
      is.na(summer_source_row),
      NA_character_,
      paste0(
        "Summer activities at elementary schools.xlsx | Sheet1 row ",
        summer_source_row
      )
    )
  )


# Map known campus labels.
# Outdoor/site labels are not treated as school buildings.

canonicalize_building <- function(x) {
  
  simple <- key(
    str_remove(
      school_lookup$canonical_building,
      " (Elementary School|Middle School|High School)$"
    )
  )
  
  vapply(
    key(x),
    function(z) {
      
      if (
        !nzchar(z) ||
        str_detect(z, "field|off.?campus")
      ) {
        return(NA_character_)
      }
      
      exact <- which(
        key(school_lookup$canonical_building) == z |
          key(school_lookup$allowed_prefix) == z
      )
      
      if (length(exact) == 1L) {
        return(school_lookup$canonical_building[exact])
      }
      
      if (str_detect(z, "meridian.*middle|middle.*meridian")) {
        return("Meridian Middle School")
      }
      
      if (str_detect(z, "meridian.*elementary|elementary.*meridian")) {
        return("Meridian Elementary School")
      }
      
      if (str_detect(z, "kent.?meridian")) {
        return("Kent-Meridian High School")
      }
      
      if (str_detect(z, "kentwood")) {
        return("Kentwood High School")
      }
      
      if (str_detect(z, "kentridge")) {
        return("Kentridge High School")
      }
      
      if (str_detect(z, "kentlake")) {
        return("Kentlake High School")
      }
      
      if (str_detect(z, "neely|o.?brien")) {
        return("Neely-O'Brien Elementary School")
      }
      
      if (str_detect(z, "daniel")) {
        return("Daniel Elementary School")
      }
      
      if (str_detect(z, "kent lab")) {
        return("Kent Laboratory Academy")
      }
      
      if (str_detect(z, "outreach|\\btop\\b")) {
        return("The Outreach Program (TOP)")
      }
      
      hits <- which(
        vapply(
          simple,
          function(s) str_detect(z, fixed(s)),
          logical(1)
        )
      )
      
      if (length(hits) == 1L) {
        school_lookup$canonical_building[hits]
      } else {
        NA_character_
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}


# 6A. Optional exact room verification -------------------------

# Add entries only after KSD/ATS confirms the actual FMX room.
# resource must match the source FMX resource label.
#
# TRUE = room-level Compass control confirmed.
# FALSE = room ruled out.
# NA = not individually verified.
#
# No verification rows are fabricated.
# Admin Board Room needs TRUE here before it enters recommendations.

room_verification <- tibble(
  canonical_building = character(),
  resource = character(),
  room_compass_confirmed = logical(),
  physical_room_id = character(),
  bas_zone_id = character(),
  verification_source = character()
)

# Prior KW/KR allowances remain room-specific.
# No directional gym is renamed "Annex" without confirmation.
# These entries cannot create rooms absent from FMX.

prior_hs_rooms <- tribble(
  ~allowed_prefix, ~resource,
  
  "KW", "KW Main Gym",
  "KW", "KW Aux Gym",
  "KW", "KW New Gym",
  "KW", "Cafeteria/Commons",
  "KW", "KW Performing Arts Center",
  
  "KR", "KR Main Gym",
  "KR", "KR East Gym",
  "KR", "KR North Gym",
  "KR", "KR West Gym",
  "KR", "Cafeteria",
  "KR", "KR Performing Arts Center"
) |>
  mutate(
    resource_key = key(resource),
    prior_hs_allowed = TRUE
  )


# 6B. Ginger's requests: match against FMX, never create rooms ----

# Patterns are matching rules, not aliases or controller proof.
# Matched candidates retain the actual FMX resource name.
# Missing requests remain visible in the coverage report.

request_definitions <- tribble(
  ~request_key, ~requested_space, ~pattern, ~type_override,
  
  "cafe",
  "Cafeteria",
  "^(?:[a-z]{1,3} )?cafeteria(?:/commons|/mpr(?: #?\\d+)?)?$",
  NA_character_,
  
  "cafecommons",
  "Cafeteria/commons",
  "^(?:(?:[a-z]{1,3} )?cafeteria(?:/commons|/mpr(?: #?\\d+)?)?|(?:the )?commons(?: #?\\d+)?)$",
  NA_character_,
  
  "addition",
  "Cafeteria addition",
  "^cafeteria addition$",
  "Cafeteria",
  
  "lgi159",
  "Large group instruction room 159",
  "^(?:(?:large group instruction|lgi)(?: room)? #?159|room #?159)$",
  "Shared Instruction",
  
  "rc200",
  "Resource Center room 200",
  "^(?:resource cent(?:er|re)(?: room)? #?200|room #?200)$",
  "Library",
  
  "pacfoyer",
  "PAC foyer",
  "^(?:[a-z]{1,3} )?pac (?:foyer|lobby)$",
  "PAC/Auditorium/Stage",
  
  "annexgym",
  "Annex gym",
  "^(?:[a-z]{1,3} )?annex gym$",
  "Gym",
  
  "annexweight",
  "Annex weight room",
  "^(?:[a-z]{1,3} )?annex weight room$",
  "Fitness/Changing",
  
  "pac",
  "PAC",
  "^(?:[a-z]{1,3} )?(?:pac|performing arts center)(?: room \\d+)?$",
  "PAC/Auditorium/Stage",
  
  "maingym",
  "Main gym",
  "^(?:[a-z]{1,3} )?main gym$",
  "Gym",
  
  "auxgym",
  "Aux gym",
  "^(?:[a-z]{1,3} )?(?:aux|auxiliary) gym$",
  "Gym",
  
  "gym",
  "Gym",
  "^(?:(?:[a-z]{1,3} )?(?:(?:main|aux|auxiliary|new|east|west|north|south|annex) )?gym|\\d{3,4} gym)$",
  "Gym",
  
  "library",
  "Library",
  "^(?:[a-z]{1,3} )?library(?: room #?\\d+)?$",
  "Library",
  
  "conference",
  "Conference room",
  "^(?:(?:mini|small) )?conference room(?:[ -]+#?[a-z]?\\d*[a-z]?)?$",
  "Conference",
  
  "flex",
  "Flex building",
  "^flex building$",
  "Flex Space",
  
  "mpr",
  "Multipurpose room",
  "^(?:m\\.?p\\.?r(?: #?\\d+)?|multi.?purpose(?: room)?|cafeteria/mpr(?: #?\\d+)?)$",
  "MPR",
  
  "mprcafe",
  "Multipurpose/cafeteria",
  "^(?:m\\.?p\\.?r(?: #?\\d+)?|multi.?purpose(?: room)?|cafeteria(?:/mpr(?: #?\\d+)?|/commons)?)$",
  NA_character_,
  
  "commons",
  "Commons",
  "^(?:(?:the )?commons(?: #?\\d+)?|cafeteria/commons)$",
  NA_character_,
  
  "lounge",
  "Staff lounge",
  "^(?:staff (?:lounge|room)|teacher'?s? lounge)$",
  "Staff/Lounge",
  
  "board",
  "Admin Board Room",
  "^(?:(?:admin|administration|school board) )?board ?room(?:[ -]+\\w+)?$",
  "Board Room"
)

requests_for <- function(prefixes, keys) {
  expand_grid(
    allowed_prefix = prefixes,
    request_key = keys
  )
}

ginger_requests <- bind_rows(
  requests_for(
    "KR",
    c(
      "cafe", "addition", "lgi159", "rc200",
      "pacfoyer", "annexgym", "annexweight"
    )
  ),
  
  requests_for(
    "KW",
    c(
      "pac", "maingym", "auxgym",
      "annexgym", "annexweight"
    )
  ),
  
  requests_for(
    "KL",
    c("gym", "pac", "cafecommons")
  ),
  
  requests_for(
    c("MC", "CH", "MM", "NW"),
    c("cafecommons", "library", "maingym", "auxgym")
  ),
  
  requests_for(
    "CR",
    c(
      "cafecommons", "conference", "library",
      "maingym", "auxgym"
    )
  ),
  
  requests_for(
    c("SC", "SB", "CV"),
    c("library", "gym", "flex", "mpr")
  ),
  
  requests_for(
    "RR",
    c("cafecommons", "library", "gym")
  ),
  
  requests_for(
    "SH",
    c("cafe", "gym", "library", "lounge")
  ),
  
  requests_for(
    c("HE", "EH", "CC", "SR", "PL", "EP", "PO", "PT"),
    c("gym", "mprcafe", "library")
  ),
  
  requests_for(
    "GL",
    c("mpr", "flex", "gym")
  ),
  
  requests_for(
    "JC",
    c("conference", "gym", "library", "commons")
  ),
  
  requests_for(
    "KE",
    c("cafe", "gym", "library")
  ),
  
  requests_for(
    "ADMIN",
    "board"
  )
) |>
  left_join(
    request_definitions,
    by = "request_key"
  ) |>
  mutate(
    request_item = paste(
      allowed_prefix,
      request_key,
      sep = "_"
    ),
    request_source = ginger_source
  )

stopifnot(
  !anyDuplicated(ginger_requests$request_item),
  !anyDuplicated(school_lookup$canonical_building),
  
  !anyDuplicated(
    paste(
      room_verification$canonical_building,
      key(room_verification$resource)
    )
  )
)


# 7. Resource parsing and classification -----------------------

atomic_resources <- function(x) {
  p <- str_squish(
    unlist(
      str_split(
        coalesce(x, ""),
        "[\\r\\n;|]+"
      )
    )
  )
  
  unique(p[nzchar(p)])
}

excluded_resource_pattern <- paste0(
  "parking|field|playground|track|outdoor|courtyard|portable|off.?campus|",
  "tennis.*court|stadium|turf|\\bport\\s*#?\\d|^other$|unknown room"
)

aggregate_resource_pattern <- paste0(
  "^classroom$|classrooms|all rooms|\\bwing\\b|\\bhall(?:way)?\\b|",
  "\\bcorridor\\b|entire building|whole school"
)

classify_resource_type <- function(resource) {
  r <- key(resource)
  
  case_when(
    str_detect(r, "cafeteria\\s*/\\s*mpr") ~
      "Cafeteria/MPR",
    
    str_detect(r, "cafeteria\\s*/\\s*commons") ~
      "Cafeteria/Commons",
    
    str_detect(
      r,
      "performing arts|\\bpac\\b|auditorium|theat(er|re)|\\bstage\\b"
    ) ~ "PAC/Auditorium/Stage",
    
    str_detect(r, "\\bgym\\b|gymnasium") ~
      "Gym",
    
    str_detect(r, "library|resource cent(er|re)") ~
      "Library",
    
    str_detect(r, "commons") ~
      "Commons",
    
    str_detect(r, "\\bm\\.?p\\.?r\\b|multi.?purpose") ~
      "MPR",
    
    str_detect(r, "cafeteria|\\bcafe\\b") ~
      "Cafeteria",
    
    str_detect(r, "board ?room") ~
      "Board Room",
    
    str_detect(r, "conference") ~
      "Conference",
    
    str_detect(r, "staff|lounge|teacher") ~
      "Staff/Lounge",
    
    str_detect(r, "weight|wrestling|locker|dance") ~
      "Fitness/Changing",
    
    str_detect(r, "^flex (?:building|room|space)") ~
      "Flex Space",
    
    str_detect(r, "large group instruction|\\blgi\\b") ~
      "Shared Instruction",
    
    str_detect(r, "kitchen") ~
      "Kitchen",
    
    str_detect(r, "office") ~
      "Office",
    
    str_detect(
      r,
      "classroom|(^|\\s)room\\s*#?[a-z]?\\d|cooking|band|choir|music|practice|ceramics"
    ) ~ "Classroom/Instructional",
    
    TRUE ~
      "Unclassified"
  )
}

core_shared_types <- c(
  "Gym",
  "Cafeteria",
  "Cafeteria/MPR",
  "Cafeteria/Commons",
  "Commons",
  "MPR",
  "Library",
  "PAC/Auditorium/Stage",
  "Flex Space",
  "Shared Instruction"
)

split_resource_bundle <- function(x) {
  
  if (is.na(x) || !nzchar(x)) {
    return("Unknown room/resource")
  }
  
  # Never split on dashes.
  parts <- str_squish(
    unlist(
      str_split(x, "[\\r\\n;|]+")
    )
  )
  
  parts <- parts[nzchar(parts)]
  
  if (!length(parts)) {
    return("Unknown room/resource")
  }
  
  starts <- c(
    "Cafeteria/MPR(?:\\s*#?\\d+)?",
    "Cafeteria/Commons",
    
    paste0(
      "(?:Mini |Small )?Conference Room",
      "(?:\\s*#?[A-Za-z]?\\d+[A-Za-z]?|",
      "\\s+[A-Za-z](?=\\s|$))?"
    ),
    
    "Prayer Room/Telehealth",
    "Outdoor Commons",
    "Commons(?:\\s*#?\\d+)?",
    "Staff Lounge",
    "Weight Room",
    "Locker Rooms?",
    "Band Room",
    "Choir Room",
    "Music Room",
    "Practice Room\\s*\\d+",
    
    paste0(
      "Room\\s*#?\\d+\\s*-\\s*",
      "(?:Cooking Classroom|Music Room|Woodshop)"
    ),
    
    "Room\\s*#?[A-Za-z]?\\d+[A-Za-z]?",
    "Office\\s*\\d+",
    "Classroom\\s*-\\s*\\d+\\s+(?:Wing|Hall)",
    "Classroom\\s*-\\s*Portable",
    
    paste0(
      "Classroom",
      "(?:\\s*#?\\d+[A-Za-z]?|\\s+[A-Za-z]-?\\d+)?"
    ),
    
    "[A-Z]{1,3}\\s+(?:Main |Aux |New |East |West |North |South )?Gym\\b",
    "[A-Z]{1,3}\\s+Performing Arts Center",
    
    paste0(
      "[A-Z]{1,3}\\s+",
      "(?:Football/Soccer|Football|Soccer|Baseball|Softball|Grass)",
      " Field\\s*\\d*"
    ),
    
    "Cafeteria",
    "Kitchen",
    "Library",
    "MPR(?:\\s*#?\\d+)?",
    "Stage",
    "Gym(?:nasium)?",
    "Courtyard",
    "Parking Lot",
    "Playground",
    "Portable\\s*\\d*",
    "Track",
    "Other"
  )
  
  pattern <- paste0(
    "(?<![A-Za-z0-9])(?:",
    paste(starts, collapse = "|"),
    ")\\b"
  )
  
  split_one <- function(p) {
    
    # Preserve known complete source labels and unresolved paths.
    if (
      p %in% resource_dictionary ||
      str_detect(p, fixed(">"))
    ) {
      return(p)
    }
    
    m <- str_locate_all(
      p,
      regex(pattern, ignore_case = TRUE)
    )[[1]]
    
    if (nrow(m) <= 1) {
      return(p)
    }
    
    # Preserve unmatched leading text rather than dropping it.
    begin <- sort(unique(c(1L, m[, 1])))
    
    str_squish(
      str_sub(
        p,
        begin,
        c(begin[-1] - 1L, nchar(p))
      )
    )
  }
  
  result <- unique(
    unlist(
      lapply(parts, split_one)
    )
  )
  
  result[nzchar(result)]
}

extract_resource_prefix <- function(x) {
  
  prefixes <- na.omit(school_lookup$allowed_prefix)
  
  pattern <- paste0(
    "^(",
    paste(prefixes, collapse = "|"),
    ")\\s+",
    "(?:(?:Main|Aux|New|East|West|North|South)\\s+Gym|",
    "Gym|Performing Arts Center|PAC|.*\\bField)\\b"
  )
  
  str_to_upper(
    str_match(
      str_squish(x),
      regex(pattern, ignore_case = TRUE)
    )[, 2]
  )
}


# 8. Read requests and build the observed FMX catalog -----------

progress(
  "Building the FMX resource catalog and matching Ginger's requests..."
)

resource_raw <- get_col(
  c("Resources", "Resource", "Room", "Rooms"),
  TRUE,
  TRUE
)

resource_dictionary <- unique(
  unlist(
    lapply(
      resource_raw[
        !is.na(resource_raw) &
          str_detect(resource_raw, "[\\r\\n;|]")
      ],
      atomic_resources
    )
  )
)

events_base <- tibble(
  source_row = seq_len(nrow(raw_schedule)),
  
  request_id = get_col(
    c("ID", "Request ID")
  ),
  
  building_original = get_col(
    c("Building", "School", "Facility"),
    TRUE,
    TRUE
  ),
  
  resource_raw = resource_raw,
  
  request_name = get_col(
    c("Name", "Event Name", "Title")
  ),
  
  request_type = get_col(
    c("Request type", "Type")
  ),
  
  status = get_col(
    "Status",
    TRUE
  ),
  
  schedule_text = get_col(
    c("Schedule", "Recurrence"),
    preserve_lines = TRUE
  ),
  
  event_start = parse_dt(
    get_col(c("Event starts", "Event Start", "Start"))
  ),
  
  event_end = parse_dt(
    get_col(c("Event ends", "Event End", "End"))
  ),
  
  reservation_start = parse_dt(
    get_col(c("Reservation starts", "Reservation Start"))
  ),
  
  reservation_end = parse_dt(
    get_col(c("Reservation ends", "Reservation End"))
  )
) |>
  mutate(
    request_id = coalesce(
      request_id,
      str_extract(request_name, "^\\d+"),
      paste0("row_", source_row)
    ),
    
    start_time = coalesce(
      event_start,
      reservation_start
    ),
    
    end_time = coalesce(
      event_end,
      reservation_end
    ),
    
    duration_hours = as.numeric(
      difftime(
        end_time,
        start_time,
        units = "hours"
      )
    ),
    
    canonical_building =
      canonicalize_building(building_original)
  )


# Catalog contains resource labels actually observed in FMX.
# Multi-building rows cannot reliably attribute individual rooms.

fmx_catalog <- events_base |>
  filter(
    !str_detect(
      coalesce(building_original, ""),
      "[\\r\\n;|]"
    )
  ) |>
  mutate(
    resource = lapply(
      resource_raw,
      atomic_resources
    )
  ) |>
  unnest_longer(resource) |>
  mutate(
    resource_key = key(resource)
  ) |>
  group_by(
    canonical_building,
    building_original,
    resource_key
  ) |>
  summarise(
    resource = first(resource),
    
    fmx_source_rows =
      collapse_values(as.character(source_row)),
    
    .groups = "drop"
  ) |>
  group_by(
    canonical_building,
    resource_key
  ) |>
  summarise(
    resource = first(resource),
    
    fmx_building_labels =
      collapse_values(building_original),
    
    fmx_source_rows =
      collapse_values(fmx_source_rows),
    
    .groups = "drop"
  ) |>
  left_join(
    school_lookup,
    by = "canonical_building",
    na_matches = "never"
  )


# Match requested rooms to source resource names, not event titles.

nomination_matches <- bind_rows(
  lapply(
    seq_len(nrow(ginger_requests)),
    function(i) {
      
      q <- ginger_requests[i, ]
      
      scope <- if (q$allowed_prefix == "ADMIN") {
        str_detect(
          key(fmx_catalog$canonical_building),
          "^admin"
        )
      } else {
        coalesce(
          fmx_catalog$allowed_prefix == q$allowed_prefix,
          FALSE
        )
      }
      
      fmx_catalog |>
        filter(
          .env$scope,
          str_detect(resource_key, .env$q$pattern)
        ) |>
        transmute(
          canonical_building,
          resource_key,
          request_item = q$request_item,
          requested_space = q$requested_space,
          type_override = q$type_override
        )
    }
  )
)

if (!nrow(nomination_matches)) {
  nomination_matches <- tibble(
    canonical_building = character(),
    resource_key = character(),
    request_item = character(),
    requested_space = character(),
    type_override = character()
  )
}

nomination_flags <- nomination_matches |>
  group_by(
    canonical_building,
    resource_key
  ) |>
  summarise(
    ginger_requested = TRUE,
    
    request_items =
      collapse_values(request_item),
    
    ginger_requested_spaces =
      collapse_values(requested_space),
    
    type_override = first(
      na.omit(type_override),
      default = NA_character_
    ),
    
    .groups = "drop"
  )


# Do not count an unresolved alternate PAC label as an extra room.
# Staff verification can resolve whether it is a distinct space.

alias_holds <- tribble(
  ~allowed_prefix, ~resource_key, ~alias_hold_reason,
  
  "KW",
  "performing arts center room 600",
  "Possible alternate label for KW Performing Arts Center; verify physical identity"
)


room_registry <- fmx_catalog |>
  left_join(
    nomination_flags,
    by = c("canonical_building", "resource_key"),
    na_matches = "never"
  ) |>
  left_join(
    prior_hs_rooms |> select(-resource),
    by = c("allowed_prefix", "resource_key"),
    na_matches = "never"
  ) |>
  left_join(
    alias_holds,
    by = c("allowed_prefix", "resource_key"),
    na_matches = "never"
  ) |>
  left_join(
    room_verification |>
      mutate(resource_key = key(resource)) |>
      select(-resource),
    
    by = c("canonical_building", "resource_key"),
    na_matches = "never"
  ) |>
  mutate(
    ginger_requested = coalesce(
      ginger_requested,
      FALSE
    ),
    
    prior_hs_allowed = coalesce(
      prior_hs_allowed,
      FALSE
    ),
    
    original_resource_type =
      classify_resource_type(resource),
    
    resource_type = case_when(
      str_detect(
        coalesce(request_items, ""),
        "lgi159|rc200"
      ) ~ coalesce(
        type_override,
        original_resource_type
      ),
      
      original_resource_type != "Unclassified" ~
        original_resource_type,
      
      TRUE ~
        coalesce(type_override, original_resource_type)
    ),
    
    board_requested =
      ginger_requested & resource_type == "Board Room",
    
    fmx_resource_present = TRUE,
    
    fmx_precise_resource =
      !str_detect(resource_key, aggregate_resource_pattern) &
      !str_detect(resource, fixed(">")),
    
    resource_prefix =
      extract_resource_prefix(resource),
    
    cross_school_conflict =
      !is.na(resource_prefix) &
      (
        is.na(allowed_prefix) |
          resource_prefix != allowed_prefix
      ),
    
    in_requested_scope =
      coalesce(compass_school_eligible, FALSE) |
      prior_hs_allowed |
      ginger_requested |
      coalesce(room_compass_confirmed, FALSE),
    
    # A nomination is not a verified controller mapping.
    compass_status = case_when(
      room_compass_confirmed %in% FALSE ~
        "Ruled out",
      
      room_compass_confirmed %in% TRUE ~
        "Confirmed",
      
      coalesce(school_compass_confirmed, FALSE) ~
        "Campus confirmed; room mapping pending",
      
      allowed_prefix %in% c("KW", "KR") &
        (prior_hs_allowed | ginger_requested) ~
        "Reported likely",
      
      TRUE ~
        "Unknown"
    ),
    
    compass_evidence_source = case_when(
      !is.na(room_compass_confirmed) ~
        verification_source,
      
      coalesce(school_compass_confirmed, FALSE) ~
        school_compass_source,
      
      allowed_prefix %in% c("KW", "KR") &
        prior_hs_allowed ~
        "Prior KSD room-specific Compass report; mapping pending",
      
      allowed_prefix %in% c("KW", "KR") &
        ginger_requested ~
        paste(
          ginger_source,
          "- nominated, not room-mapped"
        ),
      
      TRUE ~
        "No supporting Compass confirmation"
    ),
    
    nomination_match_note = if_else(
      str_detect(
        coalesce(request_items, ""),
        "pacfoyer"
      ) &
        str_detect(resource_key, "lobby"),
      
      "Foyer/lobby terminology match only; retain FMX name and verify requested-space identity",
      
      ""
    ),
    
    compass_planning_evidence =
      compass_status %in% c(
        "Confirmed",
        "Campus confirmed; room mapping pending",
        "Reported likely"
      ),
    
    needs_room_detail_review = str_detect(
      resource_key,
      
      paste0(
        "^(mpr|library|cafeteria|commons|gym|stage|",
        "conference room|kitchen|office|staff lounge|flex building)$"
      )
    ),
    
    # School coverage and individual room verification remain distinct.
    both_operational_conditions_confirmed =
      fmx_precise_resource &
      coalesce(room_compass_confirmed, FALSE),
    
    installation_ready =
      both_operational_conditions_confirmed &
      !is.na(physical_room_id) &
      physical_room_id != "" &
      !is.na(bas_zone_id) &
      bas_zone_id != "",
    
    room_exclude_reason = case_when(
      coalesce(already_roby_excluded, FALSE) ~
        "Mattson: prior deployment/pilot",
      
      str_detect(resource_key, excluded_resource_pattern) ~
        "Outdoor/portable/non-room resource",
      
      !fmx_precise_resource ~
        "Aggregate/generic classroom or unverified path: not a precise room",
      
      cross_school_conflict ~
        "Cross-school resource prefix conflict",
      
      resource_type == "Unclassified" ~
        "Unclassified resource; manual review",
      
      compass_status == "Ruled out" ~
        "Compass control ruled out",
      
      !is.na(alias_hold_reason) &
        !coalesce(room_compass_confirmed, FALSE) ~
        alias_hold_reason,
      
      !in_requested_scope ~
        "Outside approved school/room scope",
      
      board_requested &
        !coalesce(room_compass_confirmed, FALSE) ~
        "Board Room found in FMX; Compass control still needs confirmation",
      
      !compass_planning_evidence ~
        "No school/room Compass evidence",
      
      require_individual_compass_confirmation &
        !coalesce(room_compass_confirmed, FALSE) ~
        "Individual room Compass confirmation required",
      
      !include_classrooms_after_shared &
        resource_type == "Classroom/Instructional" ~
        "Classroom fallback disabled",
      
      TRUE ~
        NA_character_
    ),
    
    compass_room_candidate =
      is.na(room_exclude_reason),
    
    exception_allowed =
      !coalesce(compass_school_eligible, FALSE) &
      in_requested_scope,
    
    # Ginger's room-level heating evidence overrides the
    # school-level fallback for these identified spaces.
    ginger_room_gas =
      (
        allowed_prefix %in% c("MC", "KL") &
          resource_type == "Gym"
      ) |
      (
        allowed_prefix %in% "KR" &
          str_detect(
            resource_key,
            "\\bpac\\b|performing arts center"
          )
      ),
    
    room_heating_gas_code = if_else(
      ginger_room_gas,
      1,
      heating_gas_code
    ),
    
    room_heating_source = if_else(
      ginger_room_gas,
      ginger_source,
      heating_source
    ),
    
    room_heating_description = if_else(
      ginger_room_gas,
      "Gas-served equipment; whole-school boiler dependence NOT established",
      heating_system
    ),
    
    linked_equipment_notes = paste0(
      if_else(
        allowed_prefix %in%
          c("KW", "KR", "MC", "CH", "MM", "NW", "CR"),
        
        "Ginger: coordinate associated bathroom exhaust fans if possible. ",
        ""
      ),
      
      if_else(
        allowed_prefix %in% "KW" &
          resource_type == "PAC/Auditorium/Stage",
        
        "Ginger: coordinate PAC dressing-room units and exhaust fans. ",
        ""
      ),
      
      if_else(
        resource_type == "Flex Space",
        
        "Verify whether the flex-building calendar represents one controllable space. ",
        ""
      ),
      
      "Linked equipment is not counted as extra Roby rooms."
    )
  )


# Request-level cleaning.
# Indoor athletics remain. Field-trip requests are excluded.

duplicate_columns <- c(
  "request_id",
  "building_original",
  "resource_raw",
  "start_time",
  "end_time",
  "schedule_text",
  "status"
)

duplicate_rows <- events_base[
  duplicated(events_base[duplicate_columns]),
]

events_base <- events_base[
  !duplicated(events_base[duplicate_columns]),
] |>
  group_by(
    request_id,
    building_original,
    start_time
  ) |>
  mutate(
    conflicting_version = n() > 1L
  ) |>
  ungroup() |>
  mutate(
    status_ok = str_detect(
      key(status),
      "^finalized|^approved|^confirmed|^pending (invoice|payment)$"
    ),
    
    closure_or_support = str_detect(
      key(request_name),
      
      paste0(
        "building closed|school closed|holiday|closure|",
        "custodial|maintenance|test event|testing custodial|",
        "^\\d+ - test(?: \\d+)?$"
      )
    ),
    
    field_trip = str_detect(
      key(paste(request_name, request_type)),
      "\\bfield[ -]?trips?\\b"
    ),
    
    exclude_reason = case_when(
      conflicting_version ~
        "Conflicting versions of request/date",
      
      is.na(building_original) ~
        "Missing building",
      
      str_detect(
        coalesce(building_original, ""),
        "[\\r\\n;|]"
      ) ~ "Multiple building labels; attribution review",
      
      is.na(start_time) | is.na(end_time) ~
        "Missing/unparsed start or end",
      
      !is.finite(duration_hours) |
        duration_hours <= 0 |
        duration_hours > 16 ~
        "Invalid duration or duration over 16 hours",
      
      !status_ok ~
        "Canceled/declined/draft/pending approval or unknown status",
      
      closure_or_support ~
        "Closure/test/maintenance/custodial activity",
      
      field_trip ~
        "Field-trip request; no indoor hours credited",
      
      TRUE ~
        NA_character_
    )
  )

excluded_rows <- bind_rows(
  events_base |>
    filter(!is.na(exclude_reason)),
  
  duplicate_rows |>
    mutate(
      exclude_reason = "Duplicate request record"
    )
)

events_kept <- events_base |>
  filter(is.na(exclude_reason))

unmatched_buildings <- events_kept |>
  filter(is.na(canonical_building)) |>
  count(
    building_original,
    name = "source_requests"
  )

resource_map <- tibble(
  resource_raw = unique(events_kept$resource_raw)
) |>
  mutate(
    parsed_resource = lapply(
      resource_raw,
      split_resource_bundle
    )
  ) |>
  unnest_longer(parsed_resource) |>
  mutate(
    resource_key = key(parsed_resource)
  )

schedule_clean <- events_kept |>
  left_join(
    resource_map,
    by = "resource_raw",
    relationship = "many-to-many"
  ) |>
  left_join(
    room_registry,
    by = c("canonical_building", "resource_key"),
    na_matches = "never"
  ) |>
  mutate(
    # A parser-created fragment is not enough.
    # It must independently match an observed FMX resource.
    resource = coalesce(
      resource,
      parsed_resource
    ),
    
    fmx_resource_present = coalesce(
      fmx_resource_present,
      FALSE
    ),
    
    room_exclude_reason = if_else(
      !fmx_resource_present,
      
      "Parsed name not independently present as an FMX resource",
      
      room_exclude_reason
    ),
    
    compass_room_candidate =
      fmx_resource_present &
      coalesce(compass_room_candidate, FALSE),
    
    indoor_outdoor = if_else(
      compass_room_candidate,
      "Indoor",
      "Excluded/review"
    ),
    
    community_or_public = str_detect(
      key(paste(request_name, request_type)),
      
      paste0(
        "community|rental|pta|ptsa|booster|parks|church|",
        "league|club|external|ymca|champions"
      )
    )
  ) |>
  distinct(
    request_id,
    start_time,
    canonical_building,
    resource_key,
    .keep_all = TRUE
  )

cross_school_resource_conflicts <- schedule_clean |>
  filter(
    coalesce(cross_school_conflict, FALSE)
  )

resource_parse_review <- schedule_clean |>
  filter(
    !compass_room_candidate |
      coalesce(needs_room_detail_review, FALSE)
  ) |>
  select(
    source_row,
    request_id,
    canonical_building,
    resource_raw,
    resource,
    fmx_resource_present,
    compass_status,
    needs_room_detail_review,
    room_exclude_reason
  )

# These diagnostic files are written early.
# The final room lists appear after scoring and QA finish.
for (
  nm in c(
    "excluded_rows",
    "unmatched_buildings",
    "resource_parse_review",
    "cross_school_resource_conflicts"
  )
) {
  write_csv(
    get(nm),
    
    file.path(
      output_dir,
      paste0("roby_", nm, ".csv")
    )
  )
}

write_csv(
  room_registry,
  
  file.path(
    output_dir,
    "roby_fmx_resource_catalog.csv"
  )
)

write_csv(
  ginger_requests,
  
  file.path(
    output_dir,
    "roby_ginger_requested_spaces.csv"
  )
)

room_events <- schedule_clean |>
  filter(compass_room_candidate) |>
  group_by(
    request_id,
    start_time
  ) |>
  mutate(
    indoor_resources_in_request = n()
  ) |>
  ungroup() |>
  mutate(
    broad_bundle =
      indoor_resources_in_request >= broad_bundle_threshold
  )

if (!nrow(room_events)) {
  
  write_csv(
    tibble(
      rank = integer(),
      school = character(),
      roby_install_room = character()
    ),
    
    file.path(
      output_dir,
      "roby_simplified_rank_room_install.csv"
    )
  )
  
  stop(
    "No rooms pass the current FMX/Compass/scope filters. ",
    "Review roby_fmx_resource_catalog.csv; no rooms were invented."
  )
}

progress(
  paste(
    "Eligible request-room records:",
    nrow(room_events),
    "; expanding dated recurrences..."
  )
)


# 9. Dated occurrences and recurrence uncertainty --------------

# Reconstruct only bounded daily/weekly rules.
# Custom-date lists are not invented.
#
# Missing year in "until July 28" uses the next such
# calendar date after the start. That inference is flagged.
#
# Unexported exceptions and holiday cancellations remain unknown.

parse_recurrence <- function(
    text,
    start,
    exported_date_count = 1L
) {
  
  s <- key(text)
  first_date <- as.Date(start, tz = model_timezone)
  
  declared <- suppressWarnings(
    as.integer(
      str_extract(
        s,
        "\\d+(?=\\s*(?:custom\\s*)?(?:dates?|times|occurrences?)\\b)"
      )
    )
  )
  
  fallback <- function(note, count = declared) {
    
    list(
      dates = first_date,
      reconstructed = FALSE,
      review = TRUE,
      declared = count,
      
      undated = if (is.na(count)) {
        NA_integer_
      } else {
        max(0L, count - 1L)
      },
      
      method = note
    )
  }
  
  # Do not multiply separately exported dated rows by
  # the complete recurrence count again.
  if (exported_date_count > 1L) {
    
    unknown_remaining <-
      is.na(declared) && s != "once"
    
    left <- if (unknown_remaining) {
      NA_integer_
    } else if (s == "once") {
      0L
    } else {
      max(0L, declared - exported_date_count)
    }
    
    return(
      list(
        dates = first_date,
        reconstructed = FALSE,
        review = is.na(left) || left > 0L,
        declared = declared,
        undated = left,
        method = "Dated export rows; no additional dates invented"
      )
    )
  }
  
  if (
    str_detect(
      coalesce(text, ""),
      "[\\r\\n]"
    )
  ) {
    return(
      fallback(
        "Multiple recurrence descriptions; listed date only",
        NA_integer_
      )
    )
  }
  
  if (s == "once") {
    return(
      list(
        dates = first_date,
        reconstructed = FALSE,
        review = FALSE,
        declared = 1L,
        undated = 0L,
        method = "Listed occurrence"
      )
    )
  }
  
  if (s == "") {
    return(
      fallback(
        "Recurrence missing; listed date only",
        NA_integer_
      )
    )
  }
  
  if (str_detect(s, "custom date")) {
    
    if (!is.na(declared) && declared == 1L) {
      return(
        list(
          dates = first_date,
          reconstructed = FALSE,
          review = FALSE,
          declared = 1L,
          undated = 0L,
          method = "One listed custom date"
        )
      )
    }
    
    return(
      fallback(
        "Custom dates not exported; listed date only"
      )
    )
  }
  
  daily <- str_detect(
    s,
    "^daily(?:,|$)"
  )
  
  weekly <- str_detect(
    s,
    "^weekly on "
  )
  
  if (!(daily || weekly)) {
    return(
      fallback(
        "Unsupported recurrence; listed date only"
      )
    )
  }
  
  week_days <- if (daily) {
    
    1:7
    
  } else if (str_detect(s, "weekdays")) {
    
    1:5
    
  } else {
    
    which(
      vapply(
        c(
          "monday", "tuesday", "wednesday", "thursday",
          "friday", "saturday", "sunday"
        ),
        
        function(d) str_detect(s, fixed(d)),
        
        logical(1)
      )
    )
  }
  
  if (!length(week_days)) {
    return(
      fallback(
        "Weekly rule lacks interpretable weekdays"
      )
    )
  }
  
  end_date <- as.Date(NA)
  inferred_year <- FALSE
  
  end_text <- str_match(
    s,
    "until\\s+([a-z]+\\s+\\d{1,2}(?:,?\\s+\\d{4})?)"
  )[, 2]
  
  if (!is.na(end_text)) {
    
    inferred_year <-
      !str_detect(end_text, "\\d{4}")
    
    end_date <- suppressWarnings(
      as.Date(
        mdy(
          if (inferred_year) {
            paste(end_text, year(first_date))
          } else {
            end_text
          },
          
          quiet = TRUE
        )
      )
    )
    
    if (
      inferred_year &&
      !is.na(end_date) &&
      end_date < first_date
    ) {
      end_date <- end_date %m+% years(1)
    }
    
  } else if (str_detect(s, "for a week$")) {
    
    end_date <- first_date + 6L
  }
  
  if (!is.na(end_date)) {
    
    span <- as.numeric(
      end_date - first_date
    )
    
    if (
      span < 0 ||
      span > max_reconstructed_span_days
    ) {
      return(
        fallback(
          "Recurrence span requires review"
        )
      )
    }
    
    dates <- seq(
      first_date,
      end_date,
      by = "day"
    )
    
    dates <- dates[
      wday(dates, week_start = 1) %in% week_days
    ]
    
  } else if (
    !is.na(declared) &&
    declared > 0L &&
    declared <= max_reconstructed_span_days
  ) {
    
    dates <- seq(
      first_date,
      first_date + max_reconstructed_span_days,
      by = "day"
    )
    
    dates <- head(
      dates[
        wday(dates, week_start = 1) %in% week_days
      ],
      declared
    )
    
    if (length(dates) != declared) {
      return(
        fallback(
          "Count cannot be reconstructed within safety limit"
        )
      )
    }
    
  } else {
    
    return(
      fallback(
        "No interpretable recurrence end/count; listed date only"
      )
    )
  }
  
  if (
    !length(dates) ||
    !(first_date %in% dates)
  ) {
    return(
      fallback(
        "Listed date conflicts with recurrence rule"
      )
    )
  }
  
  list(
    dates = dates,
    reconstructed = TRUE,
    review = TRUE,
    declared = length(dates),
    undated = 0L,
    
    method = paste0(
      "Nominal daily/weekly dates; exceptions unverified",
      
      if (inferred_year) {
        "; end year inferred"
      } else {
        ""
      }
    )
  )
}

requests <- room_events |>
  distinct(source_row, .keep_all = TRUE) |>
  group_by(request_id) |>
  mutate(
    exported_date_count =
      n_distinct(start_time)
  ) |>
  ungroup()

recurrence_parts <- lapply(
  seq_len(nrow(requests)),
  function(i) {
    
    if (i %% 1000L == 0L) {
      progress(
        paste(
          "Recurrence requests:",
          i,
          "of",
          nrow(requests)
        )
      )
    }
    
    r <- requests[i, ]
    
    p <- parse_recurrence(
      r$schedule_text,
      r$start_time,
      r$exported_date_count
    )
    
    offset <- as.integer(
      as.Date(r$end_time, tz = model_timezone) -
        as.Date(r$start_time, tz = model_timezone)
    )
    
    starts <- as.POSIXct(
      paste(
        p$dates,
        
        format(
          r$start_time,
          "%H:%M:%S",
          tz = model_timezone
        )
      ),
      tz = model_timezone
    )
    
    ends <- as.POSIXct(
      paste(
        p$dates + offset,
        
        format(
          r$end_time,
          "%H:%M:%S",
          tz = model_timezone
        )
      ),
      tz = model_timezone
    )
    
    list(
      occurrences = tibble(
        source_row = r$source_row,
        occurrence_start = starts,
        occurrence_end = ends,
        
        date_is_reconstructed =
          p$reconstructed &
          p$dates != as.Date(
            r$start_time,
            tz = model_timezone
          )
      ),
      
      review = tibble(
        source_row = r$source_row,
        declared_occurrences = p$declared,
        dated_occurrences = length(p$dates),
        undated_occurrences = p$undated,
        recurrence_needs_review = p$review,
        recurrence_method = p$method
      )
    )
  }
)

occurrences <- bind_rows(
  lapply(
    recurrence_parts,
    `[[`,
    "occurrences"
  )
)

recurrence_status <- bind_rows(
  lapply(
    recurrence_parts,
    `[[`,
    "review"
  )
)

room_events <- room_events |>
  left_join(
    recurrence_status,
    by = "source_row"
  )

recurrence_review <- requests |>
  select(
    source_row,
    request_id,
    canonical_building,
    request_name,
    schedule_text
  ) |>
  left_join(
    recurrence_status,
    by = "source_row"
  ) |>
  filter(recurrence_needs_review)

dated_rooms <- room_events |>
  select(
    canonical_building,
    resource_key,
    source_row,
    request_id,
    community_or_public,
    broad_bundle
  ) |>
  left_join(
    occurrences,
    by = "source_row",
    relationship = "many-to-many"
  ) |>
  mutate(
    event_date = as.Date(
      occurrence_start,
      tz = model_timezone
    )
  ) |>
  distinct(
    canonical_building,
    resource_key,
    request_id,
    occurrence_start,
    occurrence_end,
    .keep_all = TRUE
  )

if (
  any(
    is.na(dated_rooms$occurrence_start) |
    is.na(dated_rooms$occurrence_end) |
    dated_rooms$occurrence_end <=
    dated_rooms$occurrence_start
  )
) {
  stop(
    "Invalid reconstructed local times; ",
    "review DST/date rules before scoring."
  )
}


# 10. Merge overlapping bookings and apply recency -------------

age_band <- function(d) {
  
  age <- as.integer(
    analysis_as_of - d
  )
  
  case_when(
    age < 0 ~ "future",
    age < 365 ~ "recent",
    age < 730 ~ "year_2",
    age < 1095 ~ "year_3",
    TRUE ~ "older"
  )
}

weight_for_band <- function(
    b,
    weights = recency_weights
) {
  
  result <- unname(weights[b])
  result[is.na(result)] <- 0
  
  result
}

# Split at local midnights for overnight bookings.
split_interval <- function(start, end) {
  
  days <- seq(
    as.Date(start, tz = model_timezone),
    as.Date(end, tz = model_timezone),
    by = "day"
  )
  
  a <- pmax(
    as.numeric(start),
    
    as.numeric(
      as.POSIXct(
        paste(days, "00:00:00"),
        tz = model_timezone
      )
    )
  )
  
  b <- pmin(
    as.numeric(end),
    
    as.numeric(
      as.POSIXct(
        paste(days + 1L, "00:00:00"),
        tz = model_timezone
      )
    )
  )
  
  tibble(
    day = days[b > a],
    a = a[b > a],
    b = b[b > a]
  )
}

union_hours <- function(a, b) {
  
  keep <-
    is.finite(a) &
    is.finite(b) &
    b > a
  
  a <- a[keep]
  b <- b[keep]
  
  if (!length(a)) {
    return(0)
  }
  
  o <- order(a, b)
  a <- a[o]
  b <- b[o]
  
  previous_end <- c(
    -Inf,
    head(cummax(b), -1L)
  )
  
  sum(
    pmax(
      0,
      b - pmax(a, previous_end)
    )
  ) / 3600
}

outside_hours <- function(a, b, day) {
  
  if (
    wday(day, week_start = 1) > 5 ||
    day %in% reference_closed_dates
  ) {
    return(union_hours(a, b))
  }
  
  midnight <- as.POSIXct(
    paste(day, "00:00:00"),
    tz = model_timezone
  )
  
  lo <- as.numeric(
    midnight + hours(reference_start_hour)
  )
  
  hi <- as.numeric(
    midnight + hours(reference_end_hour)
  )
  
  union_hours(
    a,
    pmin(b, lo)
  ) +
    union_hours(
      pmax(a, hi),
      b
    )
}

progress(
  "Merging overlapping bookings into indoor room-days..."
)

# Expand each occurrence once, then attach its rooms.
occurrence_segments <- bind_rows(
  lapply(
    seq_len(nrow(occurrences)),
    function(i) {
      
      if (i %% 3000L == 0L) {
        progress(
          paste(
            "Dated occurrences processed:",
            i,
            "of",
            nrow(occurrences)
          )
        )
      }
      
      split_interval(
        occurrences$occurrence_start[i],
        occurrences$occurrence_end[i]
      ) |>
        mutate(
          source_row = occurrences$source_row[i]
        )
    }
  )
)

segments <- room_events |>
  select(
    canonical_building,
    resource_key,
    source_row,
    community_or_public
  ) |>
  left_join(
    occurrence_segments,
    by = "source_row",
    relationship = "many-to-many"
  )

room_day <- segments |>
  group_by(
    canonical_building,
    resource_key,
    day
  ) |>
  group_modify(
    function(.x, .y) {
      
      h <- union_hours(
        .x$a,
        .x$b
      )
      
      ah <- outside_hours(
        .x$a,
        .x$b,
        .y$day
      )
      
      tibble(
        hours = h,
        after_hours_hours = ah,
        
        raw_booked_hours =
          sum(.x$b - .x$a) / 3600,
        
        community_hours = union_hours(
          .x$a[.x$community_or_public],
          .x$b[.x$community_or_public]
        )
      )
    }
  ) |>
  ungroup() |>
  mutate(
    age_band = age_band(day)
  )

dated_rooms <- dated_rooms |>
  mutate(
    age_band = age_band(event_date)
  )


# 11. Evidence and ranking ------------------------------------

progress(
  "Calculating school and room scores..."
)

room_keys <- c(
  "canonical_building",
  "resource_key"
)

room_attributes <- room_registry |>
  select(
    all_of(room_keys),
    resource,
    resource_type,
    ginger_requested,
    ginger_requested_spaces,
    request_items,
    prior_hs_allowed,
    exception_allowed,
    needs_room_detail_review,
    fmx_resource_present,
    fmx_precise_resource,
    fmx_source_rows,
    compass_status,
    compass_planning_evidence,
    compass_evidence_source,
    nomination_match_note,
    room_compass_confirmed,
    both_operational_conditions_confirmed,
    installation_ready,
    physical_room_id,
    bas_zone_id,
    verification_source,
    room_heating_gas_code,
    room_heating_description,
    room_heating_source,
    linked_equipment_notes
  )

room_evidence <- room_events |>
  group_by(
    canonical_building,
    resource_key
  ) |>
  summarise(
    source_requests =
      n_distinct(request_id),
    
    source_rows =
      collapse_values(as.character(source_row)),
    
    request_ids =
      collapse_values(request_id),
    
    activities =
      collapse_values(request_type),
    
    recurrence_needs_review =
      any(recurrence_needs_review),
    
    undated_recurrence = any(
      is.na(undated_occurrences) |
        undated_occurrences > 0
    ),
    
    only_broad_bundles =
      all(broad_bundle),
    
    .groups = "drop"
  ) |>
  left_join(
    room_attributes,
    by = room_keys
  ) |>
  left_join(
    dated_rooms |>
      group_by(
        canonical_building,
        resource_key
      ) |>
      summarise(
        recent_distinct_dates = n_distinct(
          event_date[age_band == "recent"]
        ),
        
        recent_listed_dates = n_distinct(
          event_date[
            age_band == "recent" &
              !date_is_reconstructed
          ]
        ),
        
        recent_reconstructed_dates = n_distinct(
          event_date[
            age_band == "recent" &
              date_is_reconstructed
          ]
        ),
        
        .groups = "drop"
      ),
    
    by = room_keys
  )

rank_model <- function(weights = recency_weights) {
  
  rd <- room_day |>
    mutate(
      recency_weight =
        weight_for_band(age_band, weights)
    )
  
  dr <- dated_rooms |>
    mutate(
      recency_weight =
        weight_for_band(age_band, weights)
    )
  
  room_use <- rd |>
    group_by(
      canonical_building,
      resource_key
    ) |>
    summarise(
      weighted_hours =
        sum(hours * recency_weight),
      
      weighted_after_hours =
        sum(after_hours_hours * recency_weight),
      
      hours_recent =
        sum(hours[age_band == "recent"]),
      
      after_hours_recent =
        sum(after_hours_hours[age_band == "recent"]),
      
      hours_year_2 =
        sum(hours[age_band == "year_2"]),
      
      hours_year_3 =
        sum(hours[age_band == "year_3"]),
      
      hours_older =
        sum(hours[age_band == "older"]),
      
      hours_historical_unweighted =
        sum(hours[age_band != "future"]),
      
      hours_upcoming = sum(
        hours[
          day > analysis_as_of &
            day <= analysis_as_of + future_preview_days
        ]
      ),
      
      overlap_hours_removed =
        sum(raw_booked_hours - hours),
      
      .groups = "drop"
    ) |>
    rename(
      hours = weighted_hours,
      after_hours_hours = weighted_after_hours
    )
  
  room_counts <- dr |>
    group_by(
      canonical_building,
      resource_key
    ) |>
    summarise(
      events =
        sum(recency_weight),
      
      community_events =
        sum(recency_weight * community_or_public),
      
      .groups = "drop"
    )
  
  room <- room_use |>
    left_join(
      room_counts,
      by = c("canonical_building", "resource_key")
    )
  
  # School hours are room-hours.
  # School event counts are deduplicated across indoor rooms.
  school_counts <- dr |>
    distinct(
      canonical_building,
      request_id,
      occurrence_start,
      .keep_all = TRUE
    ) |>
    group_by(canonical_building) |>
    summarise(
      community_events =
        sum(recency_weight * community_or_public),
      
      events =
        sum(recency_weight),
      
      .groups = "drop"
    )
  
  school <- room |>
    group_by(canonical_building) |>
    summarise(
      indoor_hours =
        sum(hours),
      
      after_hours_hours =
        sum(after_hours_hours),
      
      indoor_hours_recent =
        sum(hours_recent),
      
      unique_indoor_resources =
        n(),
      
      active_indoor_resources =
        sum(hours > 0),
      
      .groups = "drop"
    ) |>
    left_join(
      school_counts,
      by = "canonical_building"
    ) |>
    left_join(
      school_lookup,
      by = "canonical_building"
    ) |>
    mutate(
      hours_norm =
        safe_minmax(indoor_hours),
      
      wneui_norm =
        safe_minmax(wneui),
      
      after_hours_norm =
        safe_minmax(after_hours_hours),
      
      community_norm =
        safe_minmax(community_events),
      
      rooms_norm =
        safe_minmax(active_indoor_resources),
      
      school_priority_score =
        school_weights[["hours"]] * hours_norm +
        school_weights[["wneui"]] * wneui_norm +
        school_weights[["after_hours"]] * after_hours_norm +
        school_weights[["community"]] * community_norm +
        school_weights[["rooms"]] * rooms_norm
    ) |>
    arrange(
      desc(school_priority_score),
      canonical_building
    ) |>
    mutate(
      school_rank = row_number()
    )
  
  room <- room |>
    left_join(
      room_evidence,
      by = c("canonical_building", "resource_key")
    ) |>
    left_join(
      school_lookup,
      by = "canonical_building"
    ) |>
    left_join(
      school |>
        select(
          canonical_building,
          school_priority_score
        ),
      
      by = "canonical_building"
    ) |>
    mutate(
      hours_norm =
        safe_minmax(hours),
      
      after_hours_norm =
        safe_minmax(after_hours_hours),
      
      wneui_norm =
        safe_minmax(wneui),
      
      community_norm =
        safe_minmax(community_events),
      
      events_norm =
        safe_minmax(events),
      
      room_priority_score =
        room_weights[["hours"]] * hours_norm +
        room_weights[["after_hours"]] * after_hours_norm +
        room_weights[["wneui"]] * wneui_norm +
        room_weights[["community"]] * community_norm +
        room_weights[["events"]] * events_norm +
        room_weights[["school"]] * school_priority_score +
        room_weights[["summer"]] * summer_score,
      
      base_room_priority_score =
        room_priority_score,
      
      heating_scheduling_factor =
        1,
      
      is_core_shared_space =
        resource_type %in% core_shared_types,
      
      # Nominated shared-use rooms precede the classroom fallback.
      # No room absent from FMX can reach this point.
      priority_tier = case_when(
        allowed_prefix %in% c("KW", "KR") &
          (ginger_requested | prior_hs_allowed) ~
          1L,
        
        school_level == "Middle" &
          (ginger_requested | is_core_shared_space) ~
          2L,
        
        ginger_requested ~
          3L,
        
        resource_type == "Classroom/Instructional" ~
          7L,
        
        allowed_prefix %in%
          c(
            "NO", "RR", "GR", "EH", "PL",
            "HE", "JC", "MS", "ME", "SH"
          ) &
          is_core_shared_space ~
          4L,
        
        is_core_shared_space ~
          5L,
        
        TRUE ~
          6L
      )
    ) |>
    arrange(
      priority_tier,
      desc(room_priority_score),
      desc(hours_recent),
      canonical_building,
      resource_key
    ) |>
    mutate(
      district_rank =
        row_number(),
      
      recommended_for_roby =
        district_rank <= install_limit,
      
      planned_units =
        1L,
      
      indoor_outdoor =
        "Indoor",
      
      compass_room_candidate =
        TRUE
    )
  
  list(
    room = room,
    school = school
  )
}


# 12. Opportunity, confidence and sensitivity ------------------

ranked <- rank_model()

room_priority <- ranked$room

school_priority <- ranked$school |>
  rename(
    building = canonical_building
  )

zone_counts <- room_priority |>
  filter(
    !is.na(bas_zone_id),
    bas_zone_id != ""
  ) |>
  count(
    canonical_building,
    bas_zone_id,
    name = "rooms_on_known_zone"
  )

room_priority <- room_priority |>
  left_join(
    zone_counts,
    by = c("canonical_building", "bas_zone_id")
  ) |>
  mutate(
    shared_zone_review = coalesce(
      rooms_on_known_zone > 1L,
      FALSE
    ),
    
    scheduling_opportunity_band = case_when(
      recent_distinct_dates >= strong_recent_dates &
        after_hours_recent >= strong_recent_after_hours ~
        "Stronger",
      
      recent_distinct_dates >= moderate_recent_dates &
        after_hours_recent >= moderate_recent_after_hours ~
        "Moderate",
      
      recent_distinct_dates > 0 ~
        "Limited",
      
      undated_recurrence ~
        "Uncertain dates",
      
      hours_upcoming > 0 ~
        "Upcoming only",
      
      TRUE ~
        "Historical only"
    ),
    
    evidence_confidence = case_when(
      needs_room_detail_review |
        only_broad_bundles |
        recent_distinct_dates < 2L ~
        "Low",
      
      undated_recurrence |
        recurrence_needs_review |
        recent_listed_dates < 3L ~
        "Moderate",
      
      TRUE ~
        "High"
    ),
    
    selection_status = case_when(
      !recommended_for_roby ~
        "Not selected / alternative",
      
      !both_operational_conditions_confirmed ~
        "Planning candidate; precise room-to-Compass verification pending",
      
      !installation_ready |
        shared_zone_review ~
        "FMX/Compass conditions confirmed; implementation review pending",
      
      TRUE ~
        "Room-level mapping recorded; commissioning still required"
    ),
    
    review_reason = paste0(
      if_else(
        needs_room_detail_review,
        "Verify physical identity of named FMX resource; ",
        ""
      ),
      
      if_else(
        source_requests == 1L,
        "Only one source request; ",
        ""
      ),
      
      if_else(
        only_broad_bundles,
        "Supported only by broad resource bundles; ",
        ""
      ),
      
      if_else(
        undated_recurrence,
        "Some recurrence dates unavailable; ",
        ""
      ),
      
      if_else(
        recurrence_needs_review,
        "Recurrence/exception review needed; ",
        ""
      ),
      
      if_else(
        recent_distinct_dates == 0,
        "No recent dated booking found in extract; ",
        ""
      ),
      
      if_else(
        shared_zone_review,
        "Multiple candidates share a recorded BAS zone; ",
        ""
      ),
      
      if_else(
        is.na(room_heating_gas_code),
        "Room heating type unknown; ",
        ""
      ),
      
      if_else(
        !coalesce(room_compass_confirmed, FALSE),
        "Room-to-Compass mapping/compatibility check pending; ",
        ""
      ),
      
      if_else(
        nomination_match_note != "",
        paste0(nomination_match_note, "; "),
        ""
      ),
      
      "Existing BAS schedule and avoidable runtime not established."
    ),
    
    confidence_scope =
      "Schedule/room evidence only; not a savings probability",
    
    estimated_energy_savings =
      NA_real_
  )

progress(
  "Checking rank sensitivity to historical-use weights..."
)

recent_only <- rank_model(
  c(
    recent = 1,
    year_2 = 0,
    year_3 = 0
  )
)$room

lighter_discount <- rank_model(
  c(
    recent = 1,
    year_2 = .75,
    year_3 = .50
  )
)$room

sensitivity <- room_priority |>
  select(
    all_of(room_keys),
    district_rank,
    recommended_for_roby
  ) |>
  left_join(
    recent_only |>
      transmute(
        canonical_building,
        resource_key,
        rank_recent_only = district_rank
      ),
    
    by = room_keys
  ) |>
  left_join(
    lighter_discount |>
      transmute(
        canonical_building,
        resource_key,
        rank_lighter_discount = district_rank
      ),
    
    by = room_keys
  ) |>
  mutate(
    selected_in_scenarios =
      as.integer(district_rank <= install_limit) +
      as.integer(rank_recent_only <= install_limit) +
      as.integer(rank_lighter_discount <= install_limit),
    
    rank_span =
      pmax(
        district_rank,
        rank_recent_only,
        rank_lighter_discount
      ) -
      pmin(
        district_rank,
        rank_recent_only,
        rank_lighter_discount
      )
  )

room_priority <- room_priority |>
  left_join(
    sensitivity |>
      select(
        all_of(room_keys),
        selected_in_scenarios,
        rank_span
      ),
    
    by = room_keys
  ) |>
  rename(
    building = canonical_building
  )

top_232_rooms <- room_priority |>
  filter(recommended_for_roby)

simplified_rank_room_install <- top_232_rooms |>
  transmute(
    rank = district_rank,
    school = building,
    roby_install_room = resource
  )

simplified_rank_room_install_with_review <- top_232_rooms |>
  transmute(
    rank = district_rank,
    school = building,
    roby_install_room = resource,
    priority_tier,
    ginger_requested,
    fmx_resource_present,
    school_compass_confirmed,
    room_compass_confirmed,
    both_operational_conditions_confirmed,
    needs_room_detail_review,
    scheduling_opportunity_band,
    evidence_confidence,
    compass_status,
    selection_status,
    review_reason
  )

room_opportunity_review <- room_priority |>
  select(
    district_rank,
    building,
    resource,
    recommended_for_roby,
    priority_tier,
    ginger_requested,
    ginger_requested_spaces,
    room_priority_score,
    scheduling_opportunity_band,
    evidence_confidence,
    confidence_scope,
    fmx_resource_present,
    fmx_precise_resource,
    school_compass_confirmed,
    compass_status,
    compass_evidence_source,
    room_compass_confirmed,
    both_operational_conditions_confirmed,
    installation_ready,
    selection_status,
    recent_distinct_dates,
    recent_listed_dates,
    recent_reconstructed_dates,
    hours_recent,
    after_hours_recent,
    hours_upcoming,
    recurrence_needs_review,
    undated_recurrence,
    source_requests,
    only_broad_bundles,
    physical_room_id,
    bas_zone_id,
    shared_zone_review,
    room_heating_description,
    room_heating_source,
    linked_equipment_notes,
    selected_in_scenarios,
    rank_span,
    review_reason
  )


# Requested spaces stay visible even when absent from FMX.
# Unmatched requests are NOT inserted into the ranked room table.

coverage_detail <- nomination_matches |>
  left_join(
    room_registry |>
      select(
        all_of(room_keys),
        resource,
        compass_status,
        room_exclude_reason,
        nomination_match_note
      ),
    
    by = room_keys
  ) |>
  left_join(
    room_priority |>
      select(
        canonical_building = building,
        resource_key,
        district_rank,
        recommended_for_roby
      ),
    
    by = room_keys
  )

request_coverage <- bind_rows(
  lapply(
    seq_len(nrow(ginger_requests)),
    function(i) {
      
      q <- ginger_requests[i, ]
      
      m <- coverage_detail |>
        filter(
          request_item == q$request_item
        )
      
      tibble(
        request_item = q$request_item,
        school_abbreviation = q$allowed_prefix,
        requested_space = q$requested_space,
        source = q$request_source,
        
        matching_fmx_labels =
          nrow(m),
        
        matched_resources =
          collapse_values(m$resource),
        
        in_scored_candidate_pool =
          sum(!is.na(m$district_rank)),
        
        selected_rooms =
          sum(m$recommended_for_roby %in% TRUE),
        
        coverage_status = if (!nrow(m)) {
          
          "Not found as a resource in this FMX extract"
          
        } else if (any(!is.na(m$district_rank))) {
          
          "Matched; see selected count and verification status"
          
        } else {
          
          "FMX match exists; excluded or no qualifying event"
        },
        
        outstanding_review = paste(
          collapse_values(m$room_exclude_reason),
          collapse_values(m$compass_status),
          collapse_values(m$nomination_match_note),
          sep = " | "
        )
      )
    }
  )
)

admin_board_fmx_review <- room_registry |>
  filter(
    str_detect(
      resource_key,
      
      request_definitions$pattern[
        request_definitions$request_key == "board"
      ]
    )
  )

pending_compass_review <- room_registry |>
  filter(
    (ginger_requested | prior_hs_allowed) &
      !both_operational_conditions_confirmed
  )

installation_ready_rooms <- top_232_rooms |>
  filter(
    installation_ready,
    !shared_zone_review
  )


# 13. QA and exports ------------------------------------------

progress(
  "Running QA checks and writing the simplified room list..."
)

test_start <- as.POSIXct(
  "2026-09-01 13:00:00",
  tz = model_timezone
)

test_weekly <- parse_recurrence(
  "Weekly on Tuesday, 4 times",
  test_start
)

test_custom <- parse_recurrence(
  "On 165 custom dates",
  test_start
)

known_physical_duplicates <- top_232_rooms |>
  filter(
    !is.na(physical_room_id),
    physical_room_id != ""
  ) |>
  count(
    building,
    physical_room_id
  ) |>
  filter(n > 1)

missing_wneui <- school_priority |>
  filter(is.na(wneui))

check <- function(
    name,
    passed,
    severity = "ERROR"
) {
  tibble(
    check = name,
    passed = isTRUE(passed),
    severity = severity
  )
}

qa_checks <- bind_rows(
  check(
    "Dashes and combined resource names preserved",
    
    identical(
      split_resource_bundle(
        "Cafeteria Classroom - 100 Wing Classroom - 200 Wing Library MK Main Gym"
      ),
      
      c(
        "Cafeteria",
        "Classroom - 100 Wing",
        "Classroom - 200 Wing",
        "Library",
        "MK Main Gym"
      )
    ) &&
      
      identical(
        split_resource_bundle(
          "Cafeteria/MPR #1 Commons #1 Commons #2"
        ),
        
        c(
          "Cafeteria/MPR #1",
          "Commons #1",
          "Commons #2"
        )
      )
  ),
  
  check(
    "Four weekly dates reconstructed",
    
    identical(
      as.character(test_weekly$dates),
      
      c(
        "2026-09-01",
        "2026-09-08",
        "2026-09-15",
        "2026-09-22"
      )
    )
  ),
  
  check(
    "Custom dates not invented",
    
    length(test_custom$dates) == 1L &&
      test_custom$undated == 164L
  ),
  
  check(
    "Overlaps merged",
    
    abs(
      union_hours(
        c(0, 3600),
        c(7200, 10800)
      ) - 3
    ) < 1e-9
  ),
  
  check(
    "Future and old history not scored",
    
    identical(
      weight_for_band(
        age_band(
          analysis_as_of -
            c(0, 364, 365, 729, 730, 1094, 1095, -1)
        )
      ),
      
      c(
        1, 1,
        .5, .5,
        .25, .25,
        0, 0
      )
    )
  ),
  
  check(
    "All selected names exist in FMX",
    
    all(top_232_rooms$fmx_resource_present) &&
      
      nrow(
        anti_join(
          top_232_rooms |>
            select(
              canonical_building = building,
              resource_key
            ),
          
          fmx_catalog,
          by = room_keys
        )
      ) == 0L
  ),
  
  check(
    "Every ranked candidate has an observed FMX resource and qualifying request",
    
    all(room_priority$fmx_resource_present) &&
      all(room_priority$source_requests >= 1L) &&
      
      nrow(
        anti_join(
          room_priority |>
            select(
              canonical_building = building,
              resource_key
            ),
          
          fmx_catalog,
          by = room_keys
        )
      ) == 0L
  ),
  
  check(
    "Kentlake campus eligible; other HS not blanket eligible",
    
    identical(
      school_lookup$compass_school_eligible[
        school_lookup$allowed_prefix %in% "KL"
      ],
      TRUE
    ) &&
      
      !any(
        school_lookup$compass_school_eligible[
          school_lookup$school_level == "High" &
            !(school_lookup$allowed_prefix %in% "KL")
        ]
      )
  ),
  
  check(
    "Explicit room-level Compass rejection overrides campus coverage",
    
    !any(
      room_registry$compass_room_candidate &
        room_registry$room_compass_confirmed %in% FALSE
    )
  ),
  
  check(
    "No generic classroom or aggregate/path counted as a room",
    
    all(top_232_rooms$fmx_precise_resource)
  ),
  
  check(
    "All selected rooms have stated Compass screening evidence",
    
    all(top_232_rooms$compass_planning_evidence)
  ),
  
  check(
    "Unverified controls never called installation ready",
    
    all(
      !top_232_rooms$installation_ready |
        top_232_rooms$both_operational_conditions_confirmed
    )
  ),
  
  check(
    "No selected Mattson",
    
    !any(
      top_232_rooms$building %in%
        already_roby_schools
    )
  ),
  
  check(
    "No outdoor or portable selections",
    
    !any(
      str_detect(
        key(top_232_rooms$resource),
        excluded_resource_pattern
      )
    )
  ),
  
  check(
    "No duplicate selected FMX keys",
    
    !anyDuplicated(
      paste(
        top_232_rooms$building,
        top_232_rooms$resource_key
      )
    )
  ),
  
  check(
    "No duplicate verified physical rooms",
    
    nrow(known_physical_duplicates) == 0L
  ),
  
  check(
    "Ordinary classrooms remain last",
    
    all(
      top_232_rooms$priority_tier[
        top_232_rooms$resource_type ==
          "Classroom/Instructional" &
          !top_232_rooms$ginger_requested
      ] == 7L
    )
  ),
  
  check(
    "Board Room must have confirmed room-level Compass",
    
    all(
      top_232_rooms$room_compass_confirmed[
        top_232_rooms$resource_type == "Board Room"
      ] %in% TRUE
    )
  ),
  
  check(
    "No invalid score or after-hours total",
    
    all(
      is.finite(room_priority$room_priority_score)
    ) &&
      
      all(
        room_priority$after_hours_hours >= -1e-9 &
          room_priority$after_hours_hours <=
          room_priority$hours + 1e-9
      )
  ),
  
  check(
    "Ginger overrides East Hill and Carriage Crest",
    
    all(
      school_lookup$heating_gas_code[
        school_lookup$allowed_prefix %in%
          c("EH", "CC")
      ] == 0
    )
  ),
  
  check(
    "No fuel penalty or summer bonus",
    
    heating_score_weight == 0 &&
      room_weights[["summer"]] == 0
  ),
  
  check(
    "Target 232 reached without padding",
    
    nrow(top_232_rooms) == install_limit,
    
    "WARNING"
  ),
  
  check(
    "All requested items have an FMX match",
    
    all(
      request_coverage$matching_fmx_labels > 0
    ),
    
    "WARNING"
  ),
  
  check(
    "All selected WNEUI values present",
    
    !any(
      is.na(top_232_rooms$wneui)
    ),
    
    "WARNING"
  ),
  
  check(
    "All selected rooms individually verified for Compass",
    
    all(
      top_232_rooms$both_operational_conditions_confirmed
    ),
    
    "WARNING"
  )
)

write_csv(
  qa_checks,
  
  file.path(
    output_dir,
    "roby_qa_checks.csv"
  )
)

write_csv(
  request_coverage,
  
  file.path(
    output_dir,
    "roby_ginger_request_coverage.csv"
  )
)

if (
  any(
    !qa_checks$passed &
    qa_checks$severity == "ERROR"
  )
) {
  
  print(
    qa_checks |>
      filter(!passed)
  )
  
  stop(
    "QA error: final outputs not exported. ",
    "See roby_qa_checks.csv."
  )
}

if (any(!qa_checks$passed)) {
  warning(
    "Review QA warnings. ",
    "Missing or unverified rooms were not fabricated."
  )
}

priority_tier_summary <- top_232_rooms |>
  group_by(priority_tier) |>
  summarise(
    candidates = n(),
    first_rank = min(district_rank),
    last_rank = max(district_rank),
    .groups = "drop"
  )

candidate_summary <- tibble(
  analysis_as_of = analysis_as_of,
  target_new_rooms = install_limit,
  
  eligible_candidates =
    nrow(room_priority),
  
  selected_candidates =
    nrow(top_232_rooms),
  
  shortfall = max(
    0L,
    install_limit - nrow(top_232_rooms)
  ),
  
  selected_generic_labels =
    sum(top_232_rooms$needs_room_detail_review),
  
  selected_low_evidence =
    sum(top_232_rooms$evidence_confidence == "Low"),
  
  both_conditions_confirmed =
    sum(top_232_rooms$both_operational_conditions_confirmed),
  
  installation_ready_records =
    nrow(installation_ready_rooms),
  
  ginger_items_not_found =
    sum(request_coverage$matching_fmx_labels == 0)
)

already_roby_excluded_summary <- tibble(
  building = already_roby_schools,
  prior_rooms_approximate = 39L,
  note = "Prior pilot/deployment assumption; NOT deducted from 232"
)

heating_lookup <- school_lookup |>
  select(
    canonical_building,
    legacy_heating_gas_code,
    heating_system,
    heating_gas_code,
    heating_source
  )

summer_inputs <- school_lookup |>
  select(
    canonical_building,
    has_summer_program,
    summer_year,
    summer_location_notes,
    summer_source,
    summer_score
  )

hs_allowlist_not_found <- prior_hs_rooms |>
  anti_join(
    room_priority,
    by = c("allowed_prefix", "resource_key")
  )

cleaned_event_export <- room_events |>
  select(
    source_row,
    request_id,
    canonical_building,
    resource,
    resource_key,
    request_name,
    request_type,
    status,
    schedule_text,
    start_time,
    end_time,
    declared_occurrences,
    dated_occurrences,
    undated_occurrences,
    recurrence_method,
    recurrence_needs_review,
    broad_bundle,
    community_or_public,
    compass_room_candidate
  )

outputs <- list(
  roby_school_priority =
    school_priority,
  
  roby_room_priority_all_candidates =
    room_priority,
  
  roby_kentlake_all_candidates = room_priority |>
    filter(building == "Kentlake High School"),
  
  roby_top_232_room_candidates =
    top_232_rooms,
  
  roby_simplified_rank_room_install =
    simplified_rank_room_install,
  
  roby_simplified_rank_room_install_with_review =
    simplified_rank_room_install_with_review,
  
  roby_room_opportunity_review =
    room_opportunity_review,
  
  roby_rank_sensitivity =
    sensitivity,
  
  roby_candidate_summary =
    candidate_summary,
  
  roby_priority_tier_summary =
    priority_tier_summary,
  
  roby_already_deployed_excluded_schools =
    already_roby_excluded_summary,
  
  roby_cross_school_resource_conflicts =
    cross_school_resource_conflicts,
  
  roby_resource_parse_review =
    resource_parse_review,
  
  roby_excluded_rows =
    excluded_rows,
  
  roby_unmatched_buildings =
    unmatched_buildings,
  
  roby_missing_wneui =
    missing_wneui,
  
  roby_recurrence_review =
    recurrence_review,
  
  roby_hs_allowlist_not_found =
    hs_allowlist_not_found,
  
  roby_heating_lookup =
    heating_lookup,
  
  roby_summer_lookup =
    summer_inputs,
  
  roby_cleaned_room_events =
    cleaned_event_export,
  
  roby_ginger_request_coverage =
    request_coverage,
  
  roby_pending_compass_verification =
    pending_compass_review,
  
  roby_admin_board_room_fmx_review =
    admin_board_fmx_review,
  
  roby_installation_ready_rooms =
    installation_ready_rooms
)

for (nm in names(outputs)) {
  
  write_csv(
    outputs[[nm]],
    
    file.path(
      output_dir,
      paste0(nm, ".csv")
    ),
    
    na = ""
  )
}

if (
  write_excel &&
  requireNamespace("writexl", quietly = TRUE)
) {
  
  tryCatch(
    writexl::write_xlsx(
      list(
        Candidate_Summary = candidate_summary,
        QA_Checks = qa_checks,
        
        Simple_Rank_Room_Install =
          simplified_rank_room_install,
        
        Simple_Rank_With_Review =
          simplified_rank_room_install_with_review,
        
        Top_232_Rooms = top_232_rooms,
        Room_Priority_All = room_priority,
        School_Priority = school_priority,
        Opportunity_Review = room_opportunity_review,
        Ginger_Coverage = request_coverage,
        Compass_Review = pending_compass_review,
        Heating_Inputs = heating_lookup,
        Summer_Context = summer_inputs,
        Recurrence_Review = recurrence_review,
        Cleaned_Room_Events = cleaned_event_export
      ),
      
      file.path(
        output_dir,
        "roby_priority_outputs.xlsx"
      )
    ),
    
    error = function(e) {
      warning(
        "XLSX export failed; CSV/RDS outputs remain: ",
        conditionMessage(e)
      )
    }
  )
  
} else if (write_excel) {
  
  message(
    "CSV/RDS complete. Optional XLSX needs ",
    "install.packages('writexl')."
  )
}

metadata <- list(
  # v3 identifies the existing output interface used by 02.
  model_version =
    "KSD_Roby_v3",
  
  revision =
    "Ginger_FMX_Compass_Kentlake_2026-09-14",
  
  analysis_as_of =
    analysis_as_of,
  
  source_file =
    basename(schedule_file),
  
  source_md5 =
    unname(tools::md5sum(schedule_file)),
  
  install_limit =
    install_limit,
  
  timezone =
    model_timezone,
  
  room_weights =
    room_weights,
  
  school_weights =
    school_weights,
  
  recency_weights =
    recency_weights,
  
  heating_score_weight =
    heating_score_weight,
  
  summer_program_year =
    summer_program_year,
  
  priority_tier_labels =
    priority_tier_labels,
  
  include_classrooms_after_shared =
    include_classrooms_after_shared,
  
  require_individual_compass_confirmation =
    require_individual_compass_confirmation,
  
  reference_window = paste0(
    "Weekdays ",
    reference_start_hour,
    ":00-",
    reference_end_hour,
    ":00"
  )
)

# Written last so 02 cannot read a partially completed run.
saveRDS(
  list(
    metadata = metadata,
    school_priority = school_priority,
    room_priority = room_priority,
    top_232_rooms = top_232_rooms,
    candidate_summary = candidate_summary,
    qa_checks = qa_checks,
    excluded_rows = excluded_rows,
    missing_wneui = missing_wneui,
    priority_tier_summary = priority_tier_summary,
    sensitivity = sensitivity,
    ginger_request_coverage = request_coverage
  ),
  
  rds_path
)

writeLines(
  capture.output(sessionInfo()),
  
  file.path(
    output_dir,
    "roby_R_session_info.txt"
  )
)


# 14. Console preview -----------------------------------------

print(candidate_summary)
print(priority_tier_summary)

print(
  simplified_rank_room_install |>
    head(25)
)

print(
  request_coverage |>
    filter(school_abbreviation == "ADMIN")
)

progress(
  paste(
    "DONE. Saved to:",
    normalizePath(output_dir)
  )
)

message(
  "Planning candidates are not a verified install list. ",
  "See FMX/Compass checks and Ginger coverage."
)