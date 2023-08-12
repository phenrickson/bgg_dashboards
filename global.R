
# packages ----------------------------------------------------------------

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
library(bggUtils)

# read/write
library(pins)

# tables
library(gt)
library(gtExtras)
library(DT)

# gcs
library(googleCloudStorageR)

# authenticate
googleCloudStorageR::gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))


# functions ---------------------------------------------------------------

# helper functions
source(here::here("src", "functions.R"))

# load data from gcs ---------------------------------------------------------------
gcs_board = 
        pins::board_gcs(
                bucket = Sys.getenv("GCS_DEFAULT_BUCKET"),
                prefix = "data/",
                versioned = T
        )

# load games data
# info
games = 
        pins::pin_read(
                board = gcs_board,
                name = "games_info"
        ) %>%
        # unnest some info
        unnest(c(bgg_outcomes, bgg_info)) %>%
        # arrange by geek rating
        arrange(desc(bayesaverage)) %>%
        # # playercounts
        unnest(playercounts) %>%
        prep_playercounts() %>%
        ungroup()

# predictions
games_predictions = 
        pins::pin_read(
                board = gcs_board,
                name = "games_predicted"
        ) 

theme_set(bggUtils::theme_bgg()+
        theme(axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 10)))
