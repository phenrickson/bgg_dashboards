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
        pickerInput(
                "rec_playercount",
                label = 'Recommended Playercounts',
                choices = c(seq(1,8), "8+"),
                selected = c(seq(1,8), "8+"),
                options = picker_options,
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
                                                "Game Types",
                                                select_categories,
                                                select_mechanics                                              #  select_designers,
                                                #   select_designers
                                        ),
                                        accordion_panel(
                                                "Publisher/Designer",
                                                select_designers,
                                                select_publishers
                                        ),
                                        accordion_panel(
                                                "Player Counts",
                                                select_rec_playercounts
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
                                        girafeOutput("plot1")
                                ),
                                card(
                                        girafeOutput("plot2")
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
                        
                        # update publishers
                        list_publishers = 
                                get_distinct_var(selected_games,
                                                 publishers)
                        
                        # update designers
                        list_designers = 
                                get_distinct_var(selected_games,
                                                 designers)
                        
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
                        
                        # update for designers
                        updatePickerInput(session = session,
                                          inputId = "designers",
                                          selected = list_designers,
                                          choices =  list_designers
                        )
                        
                        # update for publishers
                        updatePickerInput(session = session,
                                          inputId = "publishers",
                                          selected = list_publishers,
                                          choices =  list_publishers
                        )
                        
                        
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
                
                # games in designers
                games_with_publishers = 
                        reactive({
                                games %>%
                                        unnest(publishers) %>%
                                        filter(value %in% input$publishers) %>%
                                        pull(game_id)
                        })
                
                # # games in playercounts
                # games_with_rec_playercount = 
                #         reactive({
                #                 games %>%
                #                         filter(input$rec_playercount %in% playercount_rec)
                #                 
                #         })
                
                # filter games based on selections
                games_obj = 
                        reactive({
                                games %>%
                                        filter(row_number() <= input$game_ranks) %>%
                                        filter(usersrated >= input$minimum_users) %>%
                                        filter(game_id %in% games_with_categories()) %>%
                                        filter(game_id %in% games_with_mechanics()) %>%
                                        filter(game_id %in% games_with_designers()) %>%
                                        filter(game_id %in% games_with_publishers())
                                #%>%
                                     #   filter(game_id %in% games_with_rec_playercount())
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
                                        #  datatable()
                                        make_games_datatable(pageLength = 10) %>%
                                        format_games_datatable()
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
                        #  renderPlot(
                        renderGirafe(
                                # girafe(ggobj = 
                                girafe(ggobj = 
                                               games_obj() %>%
                                               mutate(highlight = case_when(row_number() %in% input$games_table_rows_selected ~ 'yes',
                                                                            TRUE ~ 'no')) %>%
                                               ggplot(aes(x=averageweight,
                                                          y=average,
                                                          alpha = highlight,
                                                          color = average,
                                                          size = log(usersrated),
                                                          tooltip = paste0(name,
                                                                           '\n',
                                                                           'Average: ', round(average,2),
                                                                           '\n',
                                                                           'Complexity: ', round(averageweight, 2))))+
                                               geom_point_interactive()+
                                               guides(size = 'none')+
                                               coord_cartesian(xlim = c(0.95, 5.05),
                                                               ylim = c(5.5, 9.5))+
                                               xlab("Complexity")+
                                               ylab("Average")+
                                               scale_color_gradient2(low = 'red', 
                                                                     mid = 'grey65',
                                                                     high = 'dodgerblue2',
                                                                     midpoint = 7,
                                                                     limits = c(5, 9),
                                                                     oob=scales::squish)+
                                               guides(color = guide_colorbar(barheight=0.5,
                                                                             title.vjust = 1,
                                                                             barwidth=15,
                                                                             title.position = 'top'))+
                                               guides(color = 'none',
                                                      alpha = 'none')+
                                               scale_alpha_manual(values = c(0.3, 1)),
                                       width = 9)
                        )
                
                # 
                # sum_categories = 
                #         reactive({
                #                 games_obj() %>%
                #                         select(game_id, categories) %>% 
                #                         unnest(categories)
                #         })
                # 
                output$plot2 = 
                        renderGirafe({
                                sum_categories =  games_obj() %>%
                                        select(game_id, categories) %>% 
                                        unnest(categories)
                                
                                if (nrow(sum_categories) < 1) {
                                        ggplot()
                                } else {
                                        
                                        girafe(ggobj = 
                                                       sum_categories %>%
                                                       group_by(value) %>%
                                                       summarize(n = n_distinct(game_id)) %>%
                                                       slice_max(order_by = n, n = 10) %>%
                                                       ggplot(aes(x=n,
                                                                  y = reorder(present_text(value), n)))+
                                                       geom_col()+
                                                       ylab(""),
                                               width = 9
                                        )
                                }
                        })
                
        }

shinyApp(ui, server)
