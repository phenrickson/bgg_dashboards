
# packages ----------------------------------------------------------------

# shiny
library(shiny)
library(shinythemes)
library(shinydashboard)
library(bslib)
library(shinyWidgets)
library(leaflet)

# tidyverse
library(tidyverse)

# visualizations
library(ggiraph)

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


# load data from gcs ---------------------------------------------------------------
gcs_board = 
        pins::board_gcs(
                bucket = Sys.getenv("GCS_DEFAULT_BUCKET"),
                prefix = "data/",
                versioned = T
        )

# load games data
games = 
        pins::pin_read(
                board = gcs_board,
                name = "games_info"
        )

# src code ----------------------------------------------------------------

