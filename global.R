
# packages ----------------------------------------------------------------

# suppressPackageStartupMessages({

# shiny
library(shiny)
library(shinythemes)
library(shinydashboard)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(leaflet)

# tidyverse
library(tidyverse)

# visualizations
library(ggiraph)
library(ggrepel)
library(ggforce)
library(bggUtils)
#   library(plotly)

# read/write
library(pins)

# tables
library(reactable)
library(gt)
library(gtExtras)
library(DT)

tidyverse_conflicts()

# })

# authentication ----------------------------------------------------------

# authenticate via encrypted json 
googleCloudStorageR::gcs_auth(
        json_file = 
                gargle::secret_decrypt_json(
                        path = here::here(".secrets", "gcs_auth_file.json"),
                        key = "GCS_AUTH_KEY"
                )
)

# set bucket
googleCloudStorageR::gcs_global_bucket(config::get("gcs_bucket"))

# src ---------------------------------------------------------------

# load data
source(here::here("R", "visualizations.R"))
source(here::here("R", "tables.R"))
source(here::here("R", "data.R"))

# data ---------------------------------------------------------------
data_board = 
        pins::board_gcs(
                bucket = googleCloudStorageR::gcs_get_global_bucket(),
                prefix = "data/",
                versioned = T
        )

# games info 
games  = 
        pins::pin_read(
                board = data_board,
                name = "games_info"
        ) %>%
        # unnest some info
        unnest(c(bgg_outcomes, bgg_info)) %>%
        # arrange by geek rating
        arrange(desc(bayesaverage)) %>%
        mutate(bgg_rank = row_number())

# %>%
#         # playercounts
#         unnest(playercounts) %>%
#         prep_playercounts()

# playercounts
# games_playercounts = 
#         games %>%
#         select(game_id, name, yearpublished, playercounts) %>%
#         # prep playercounts
#         unnest(playercounts)

# predictions
games_predicted = 
        pins::pin_read(
                board = data_board,
                name = "games_predicted"
        )

# games categories 
games_categories = 
        games %>%
        unnest_categorical(variable = categories,
                           min = 1)

categories = 
        games_categories %>%
        pull_categorical()
                                
# games mechanics
games_mechanics = 
        games %>%
        unnest_categorical(variable = mechanics,
                           min = 1)

mechanics = 
        games_mechanics %>%
        pull_categorical()

games_designers = 
        games %>%
        unnest_categorical(variable = designers,
                           min = 5)

designers = 
        games_designers %>%
        pull_categorical()

games_publishers = 
        games %>%
        unnest_categorical(variable = publishers,
                           min = 25)

publishers = 
        games_publishers %>%
        pull_categorical()

# player counts
games_playercounts = 
        games %>%
        select(
                any_of(bgg_ids()),
                playercounts
        ) %>%
        unnest(playercounts) %>%
        pivot_longer(
                cols = c(playercount_best, playercount_rec, playercount_notrec),
                names_repair = "check_unique",
                names_to = c("type"),
                names_prefix = c("playercount_"),
                values_to = c("playercount")
        ) %>%
        separate_longer_delim(
                cols = c("playercount"),
                delim = ","
        ) %>%
        mutate(playercount = 
                       fct_lump_n(
                               fct_inseq(playercount),
                               n = 15
                       )) %>%
        mutate(type = case_when(
                type == 'best' ~ 'Best',
                type == 'rec' ~ 'Recommended',
                type == 'notrec' ~ 'Not Recommended')) %>%
        mutate(type = factor(type,
                             levels = c('Best', 'Recommended', 'Not Recommended')))

theme_set(bggUtils::theme_bgg()+
                  theme(axis.text.y = element_text(size = 10),
                        axis.text.x = element_text(size = 10)))

my_theme = 
        bslib::bs_theme() %>%
        bs_theme_update(version = 5,
                        preset = "cerulean")

# library(htmltools)
# 
# anim_width <- function(x, width1, width2) {
#         x |> tagAppendAttributes(
#                 class = "animate-width",
#                 style = css(
#                         `--width1` = validateCssUnit(width1),
#                         `--width2` = validateCssUnit(width2),
#                 ),
#         )
# }
# 
# anim_height <- function(x, height1, height2) {
#         # Wrap in a div fixed at the height of height2, so the rest of
#         # the content on the page doesn't shift up and down
#         div(style = css(height = validateCssUnit(height2)),
#             x |> tagAppendAttributes(
#                     class = "animate-height",
#                     style = css(
#                             `--height1` = validateCssUnit(height1),
#                             `--height2` = validateCssUnit(height2),
#                     ),
#             )
#         )
# }
