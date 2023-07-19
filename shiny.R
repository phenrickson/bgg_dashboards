# selectors
# 
# options(DT.options = list(pageLength = 10,
#                           initComplete = htmlwidgets::JS("function(settings, json) {",
#                                                          "$(this.api().table().container()).css({'font-size': '", '10pt', "'});};",
#                                                          "}")
#                           )
# )


# sidebar ---------------------------------------------------------------


picker_options = 
        list(`actions-box` = TRUE,
             `live-search` = T)


# filter games in analysis by bgg rank
select_game_ranks = 
        sliderInput(
                "game_ranks",
                label = "BGG Game Ranks:",
                value = 10000,
                min = 0,
                step = 500,
                ticks = F,
                max = games %>% 
                        filter(!is.na(bayesaverage)) %>%
                        nrow()
        )

# filter games in analysis by bgg rank
select_minimum_users = 
        sliderInput(
                "minimum_users",
                label = "Minimum User Ratings:",
                value = 100,
                min = 0,
                max = 1000,
                step = 100,
                ticks = F
        )

# categories
select_categories = 
        shinyWidgets::pickerInput(
                "categories",
                label = "Select Categories",
                choices =  get_distinct_var(games, categories),
                selected = get_distinct_var(games, categories),
                options = picker_options,
                multiple = T)

# mechanics
select_mechanics = 
        shinyWidgets::pickerInput(
                "mechanics",
                label = "Select Mechanics",
                choices =  get_distinct_var(games, mechanics),
                selected = get_distinct_var(games, mechanics),
                options = picker_options,
                multiple = T)


# publishers
select_publishers = 
        shinyWidgets::pickerInput(
                "publishers",
                label = "Select Publishers",
                choices =  get_distinct_var(games, publishers),
                selected = get_distinct_var(games, publishers),
                options = picker_options,
                multiple = T)

# designers
select_designers = 
        shinyWidgets::pickerInput(
                "designers",
                label = "Select Designers",
                choices =  get_distinct_var(games, designers),
                selected = get_distinct_var(games, designers),
                options = picker_options,
                multiple = T)

# recommended player counts
select_rec_playercounts = 
        selectInput(
                "rec_playercount",
                label = 'Recommended Playercounts',
                choices = c(seq(1,8), "8+"),
                selected = c(seq(1,8), "8+"),
                multiple = T
        )



# cards -------------------------------------------------------------------







# ui ----------------------------------------------------------------------

ui <- 
        page_sidebar(
                title = "Find a Boardgame",
                # sidebar options
                sidebar = 
                        sidebar(
                                bg = 'white',
                                accordion(
                                        accordion_panel(
                                                "Filters",
                                                select_categories,
                                                #  select_publishers,
                                                #  select_designers,
                                                select_mechanics
                                                #   select_designers
                                        ),
                                        accordion_panel(
                                                "Other",
                                                select_game_ranks,
                                                select_minimum_users
                                        )
                                ),
                                width = 300
                        ),
                # top level cards
                layout_columns(
                        height = 100,
                        fill = FALSE,
                        # number of games
                        value_box(
                                title = "Games:",
                                value = textOutput("number_games"),
                                showcase = bsicons::bs_icon("bookshelf"),
                                theme_color = "dark"
                        ),
                        # average rating
                        value_box(
                                title = "Avg. Rating:",
                                value = textOutput("average_rating"),
                                theme_color = "secondary"
                        ),
                        # average complexity
                        value_box(
                                title = "Avg. Complexity:",
                                value = textOutput("average_complexity"),
                                theme_color = "secondary"
                        ),
                        # average ratings
                        value_box(
                                title = "Avg. Users",
                                value = textOutput("user_ratings"),
                                theme_color = "secondary"
                        )
                ),
                # main layout
                # table on the left
                layout_column_wrap(
                        width = 1/2,
                        card(
                                dataTableOutput("games_table")
                        ),
                        layout_column_wrap(
                                width = 1,
                                heights_equal = "row",
                                card(
                                        plotOutput("plot1")
                                ),
                                card(
                                        plotOutput("plot2")
                                )
                        )
                )
        )

server<- 
        function(input, output, session) {
                
                
                # set filters based on selected games
                observeEvent(input$game_ranks, {
                        
                        # filter games based on ranks
                        selected_games = 
                                games %>%
                                filter(row_number() <= input$game_ranks) %>%
                                filter(usersrated >= input$minimum_users)
                        
                        # update categories
                        list_categories = 
                                get_distinct_var(selected_games,
                                                 categories)
                        
                        # update mechanics
                        list_mechanics = 
                                get_distinct_var(selected_games,
                                                 mechanics)
                        
                        # # update designers
                        # list_designers = 
                        #         get_distinct_var(selected_games,
                        #                          designers)
                        
                        # update for categories
                        updatePickerInput(session = session, 
                                          inputId = "categories",
                                          selected = list_categories,
                                          choices =  list_categories
                        )
                        
                        # update for mechanics
                        updatePickerInput(session = session, 
                                          inputId = "mechanics",
                                          selected = list_mechanics,
                                          choices =  list_mechanics
                        )
                        
                        # # update for designers
                        # updatePickerInput(session = session, 
                        #                   inputId = "designers",
                        #                   selected = list_designers,
                        #                   choices =  list_designers
                        # )
                }
                
                )
                
                # games in categories
                games_with_categories = 
                        reactive({
                                games %>%
                                        unnest(categories) %>%
                                        filter(value %in% input$categories) %>%
                                        pull(game_id)
                        })
                
                # games in mechanics
                games_with_mechanics = 
                        reactive({
                                games %>%
                                        unnest(mechanics) %>%
                                        filter(value %in% input$mechanics) %>%
                                        pull(game_id)
                        })
                
                # games in designers
                games_with_designers = 
                        reactive({
                                games %>%
                                        unnest(designers) %>%
                                        filter(value %in% input$designers) %>%
                                        pull(game_id)
                        })
                
                # filter games based on selections
                games_obj = 
                        reactive({
                                games %>%
                                        filter(row_number() <= input$game_ranks) %>%
                                        filter(usersrated >= input$minimum_users) %>%
                                        filter(game_id %in% games_with_categories()) %>%
                                        filter(game_id %in% games_with_mechanics())
                        })
                
                
                ### outputs
                
                # cards
                output$average_rating =
                        renderText(
                                round_text(
                                        mean(games_obj()$average, na.rm=T)
                                )
                        )
                
                output$average_complexity =
                        renderText(
                                round_text(
                                        mean(games_obj()$averageweight, na.rm=T)
                                )
                        )
                
                output$number_games =
                        renderText(
                                nrow(games_obj())
                        )
                
                output$user_ratings =
                        renderText(
                                round_text(
                                        median(games_obj()$usersrated, na.rm=T)
                                )
                        )
                
                # table
                output$games_table =
                        DT::renderDT(
                                games_obj() %>%
                                        make_games_datatable()
                                 #       datatable()
                                        # DT::datatable(
                                        #         rownames = F,
                                        #         options = list(
                                        #                 initComplete = JS(
                                        #                         "function(settings, json) {",
                                        #                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                        #                         "}")
                                        #         ))
                        )
                
                # plots
                output$plot1 =
                        renderPlot(
                                # girafe(ggobj = 
                                games_obj() %>%
                                        ggplot(aes(x=averageweight,
                                                   y=average,
                                                   size = log(usersrated)))+
                                        geom_point(alpha = 0.35)+
                                        guides(size = 'none')+
                                        coord_cartesian(xlim = c(0.95, 5.05),
                                                        ylim = c(5.5, 9.5))
                                
                                #  width = 10
                                # )
                        )
                
                
               sum_categories = 
                       reactive({
                       games_obj() %>%
                       select(game_id, categories) %>% 
                       unnest(categories)
                       })
               
               output$plot2 = 
                       renderPlot(
                               if( 
                                       nrow(sum_categories()) < 1) {
                                       ggplot()
                               } else {
                                       sum_categories() %>%
                                               group_by(value) %>%
                                               summarize(n = n_distinct(game_id)) %>%
                                               slice_max(order_by = n, n = 10) %>%
                                               ggplot(aes(x=n,
                                                          y = reorder(present_text(value), n)))+
                                               geom_col()+
                                               ylab("")                               
                                       }
                       )
                
        }

rownames = FALSE) %>%
        formatStyle(c("sell_value", "buy_value"), backgroundColor = styleInterval(brks, clrs))

