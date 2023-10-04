# connect to gcs

# authenticates via info set in .Renviron
#library(googleCloudStorageR)

get_gcs_board = function(board,
                         bucket = googleCloudStorageR::gcs_get_global_bucket(),
                         ...) {
        
        require(googleCloudStorageR)
        
        board_prefix = paste0(board, "/")
        
        pins::board_gcs(
                bucket = "bgg_bucket",
                prefix = board_prefix,
                versioned = T)
        
}

# 
# googleCloudStorageR::gcs_auth(
#         json_file = 
# # 
# googleCloudStorageR::gcs_auth(
#         json_file = Sys.getenv("GCS_AUTH_FILE")
# )

# get_gcs_board('data')

# prep playercounts for table
prep_playercounts = function(data) {
        
        data %>%
                mutate(across(c("playercount_best", "playercount_rec"),
                              ~ gsub(pattern = paste(as.character(seq(9, 100, by = 1)), collapse = "|"),
                                     replacement = "8+",
                                     x = .x))) %>%
                rowwise() %>%
                mutate(across(c("playercount_best", "playercount_rec"),
                              ~ str_split(.x, ",", ) %>%
                                      unlist() %>%
                                      unique() %>%
                                      sort() %>%
                                      paste(collapse = ","))) %>%
                ungroup()
}

        
                