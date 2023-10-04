reactable_style = function() {
        
        list(
                #  fontFamily = "Work Sans, sans-serif", 
                fontSize = "0.8rem")
        
}

bar_chart <- function(label, width = "100%", height = "0.875rem", fill = "dodgerblue", background = NULL) {
        bar <- div(style = list(background = fill, width = width, height = height))
        chart <- div(style = list(flexGrow = 1, marginLeft = "0.375rem", background = background), bar)
        div(style = list(display = "flex", alignItems = "center"), label, chart)
}

outcome_columns = function(games) {
        list(
                bayesaverage = outcome_column(
                        name = "Geek Rating",
                        format = colFormat(digits = 2),
                        cell = function(value) {
                                scaled <- (value - min(games$bayesaverage, na.rm=T)) / (max(games$bayesaverage, na.rm=T) - min(games$bayesaverage, na.rm=T))
                                color <- ifelse(is.na(value), "#CCCCCC", rating_color(scaled))
                                value <- format(round(value, 1), nsmall = 1)
                                div(
                                        style = 
                                                c(
                                                        list(background = color),
                                                        outcome_style()
                                                ),
                                        value)
                        }
                ),
                average = 
                        outcome_column(
                                name = "Average Rating",
                                format = colFormat(digits = 2),
                                cell = function(value) {
                                        scaled <- (value - min(games$average, na.rm=T)) / (max(games$average, na.rm=T) - min(games$average, na.rm=T))
                                        color <-  ifelse(is.na(value), "#CCCCCC", rating_color(scaled))
                                        value <- format(round(value, 1), nsmall = 1)
                                        div(
                                                style = 
                                                        c(
                                                                list(background = color),
                                                                outcome_style()
                                                        ),
                                                value)
                                }
                        ),
                averageweight = 
                        outcome_column(
                                name = "Average Weight",
                                format = colFormat(digits = 2),
                                cell = function(value) {
                                        scaled <- (value - min(games$averageweight, na.rm=T)) / (max(games$averageweight, na.rm=T) - min(games$averageweight, na.rm=T))
                                        color <-  ifelse(is.na(value), "#CCCCCC", weight_color(scaled))
                                        value <- format(round(value, 1), nsmall = 1)
                                        div(
                                                style = 
                                                        c(
                                                                list(background = color),
                                                                outcome_style()
                                                        ),
                                                value)
                                }
                        ),
                usersrated =
                        colDef(
                                minWidth = 200,
                                name = "User Ratings",
                                defaultSortOrder = "desc",
                                cell = function(value) {
                                        width <- paste0(value * 100 / max(games$usersrated, na.rm=T), "%")
                                        # Add thousands separators
                                        value <- format(value, big.mark = ",")
                                        bar_chart(value, width = width, fill = "#1C86EE")
                                },
                                align = "left",
                                # Use the operating system's default monospace font, and
                                # preserve white space to prevent it from being collapsed by default
                                style = list(whiteSpace = "pre")
                        )
        )
}

outcome_style = function() {
        
        list(
                `width` = "1.875rem",
                `height` = "1.875rem",
                `border-radius` = '50%',
                display = "flex",
                `align-items` = "center",
                `justify-content` = "center",
                margin = "auto",
                width = "1.875rem",
                height = "1.875rem",
                border = "1px solid rgba(0, 0, 0, 0.1)",
                color = '#000',
                `font-size` = '0.8125rem',
                `letter-spacing`= '1px')
}

outcome_column <- function(maxWidth = 90, ...) {
        colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
}

make_color_pal <- function(colors, bias = 1) {
        get_color <- colorRamp(colors, bias = bias)
        function(x) rgb(get_color(x), maxColorValue = 255)
}

rating_color <- make_color_pal(c("firebrick", "#f8fcf8", "dodgerblue2"), bias = 1.3)
weight_color <- make_color_pal(c("deepskyblue1","#f8fcf8", "darkred"), bias = 1.3)
usersrated_color = make_color_pal(c("navy", "#f8fcf8"), bias = 0.6)

usersrated_column <- 
        function(maxWidth = 70, class = NULL, ...) {
                colDef(
                        maxWidth = maxWidth,
                        class = paste("cell number", class),
                        cell = function(value) {
                                scaled <- (value - min(games$usersrated, na.rm=T)) / (max(games$usersrated, na.rm=T) - min(games$usersrated, na.rm=T))
                                color <- usersrated_color(scaled)
                                value <- format(round(value, 1), nsmall = 1)
                                div(
                                        style = list(background = color),
                                        value)
                        },
                        ...
                )
        }

select_summary_cols = 
        function(data) {
                
                data %>%
                        unnest(playercounts) %>%
                        select(bgg_rank,
                               game_id,
                               yearpublished,
                               name,
                               bayesaverage,
                               averageweight,
                               average,
                               usersrated)
                
                # playercount_best,
                # playercount_rec,
                # categories,
                # mechanics,
                # designers,
                # artists,
                # publishers,
                # minplaytime,
                # maxplaytime,
                # minplayers,
                # maxplayers,
                # minplaytime,
                # maxplaytime)
        }

game_field <- function(name, ...) {
        if (any(is.na(...))) NULL
        else tagList(div(class = "detail-label", name), ...)
}

prep_summary_cols = 
        function(data) {
                
                data %>%
                        mutate(
                                across(
                                        c("categories",
                                          "mechanics",
                                          "designers",
                                          "artists",
                                          "publishers"), 
                                        ~ map_chr(.x, unnest_summary_string)
                                )
                        )
                
        }

unnest_summary_string = 
        function(x,
                 string = "value") {
                
                x %>%
                        pluck(string) %>% 
                        toString
        }

summary_table_columns = 
        function(data,
                 bgg_outcomes_maxWidth = 90,
                 name_minWidth = 125) {
                
                list(
                        bgg_rank = colDef(show=F),
                        game_id = colDef(show = F),
                        yearpublished = colDef(name = "Year",
                                               minWidth = 70,
                                               maxWidth = 70),
                        name = colDef(name = "Name",
                                      minWidth = name_minWidth),
                        bayesaverage = 
                                colDef(name = "Geek Rating",
                                       format = colFormat(digits = 2),
                                       maxWidth = bgg_outcomes_maxWidth),
                        averageweight = colDef(name = "Average Weight",
                                               format = colFormat(digits = 2),
                                               maxWidth = bgg_outcomes_maxWidth),
                        average = colDef(name = "Average Rating",
                                         format = colFormat(digits = 2),
                                         maxWidth = bgg_outcomes_maxWidth),
                        usersrated = colDef(name = "Users Rated",
                                            maxWidth = bgg_outcomes_maxWidth),
                        bgg_rank = colDef(show = F)
                )
        }

summary_table_details = 
        function(index) {
                
                game = summary[index,]
                
                detail = div(
                        class = "game-detail",
                        div(class = "detail-header", 
                            game$name, 
                            span(class = "detail-title", paste(game$yearpublished, game$game_id, sep = "")
                            )
                        ),
                        game_field("Geek Rank", game$bgg_rank),
                        game_field("Playing Time", paste(game$minplaytime, game$maxplaytime, sep = "-")),
                        game_field("Player Count", paste(game$minplayers, game$maxplayers, sep = "-")),
                        game_field("Best Player Count", game$playercount_best),
                        game_field("Recommended Player Count", game$playercount_rec)
                        # game_field("Categories", game$categories),
                        # game_field("Mechanics", game$mechanics),
                        # game_field("Publishers", game$publishers),
                        # game_field("Designers", game$designers)
                )
                
                detail
        }

build_summary_table = function(data,
                               ...,
                               simple = T,
                               games) {
        
        if (simple == T) {
                
                data %>%
                        reactable(
                                ...,
                                defaultSorted = 'bayesaverage',
                                defaultSortOrder = "desc",
                                defaultColDef = colDef(
                                        vAlign = "center",
                                        headerVAlign = "bottom",
                                        class = "cell",
                                        headerClass = "header"
                                ),
                                columns = 
                                        # c(
                                        #         list(
                                        #                 bgg_rank = colDef(show = F),
                                        #                 game_id = colDef(show = F),
                                        #                 yearpublished = colDef(name = "Published",
                                        #                                        minWidth = 100,
                                        #                                        maxWidth = 100),
                                        #                 name = colDef(name = "Name",
                                        #                               minWidth = 200)
                                        #         ),
                                        summary_table_columns()
                        )
        } else {
                
                data %>%
                        reactable(
                                ...,
                                defaultSorted = 'bayesaverage',
                                defaultSortOrder = "desc",
                                defaultColDef = colDef(
                                        vAlign = "center",
                                        headerVAlign = "bottom",
                                        class = "cell",
                                        headerClass = "header"
                                ),
                                columns =
                                        c(
                                                list(
                                                        bgg_rank = colDef(show = F),
                                                        game_id = colDef(show = F),
                                                        yearpublished = colDef(name = "Published",
                                                                               minWidth = 100,
                                                                               maxWidth = 100),
                                                        name = colDef(name = "Name",
                                                                      minWidth = 200)
                                                ),
                                                outcome_columns(games)
                                        )
                        )
        }
        
        
}

# # # 
# games %>%
#         sample_n(100) %>%
#         select_summary_cols() %>%
#         build_summary_table(
#                 style = reactable_style(),
#                 games = games,
#                 defaultPageSize = 25
#         )
