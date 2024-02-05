# sidebar ---------------------------------------------------------------

# # set picker options
# picker_options_default =
#         pickerOptions(
#                 actionsBox = T,
#                 liveSearch = T,
#                 selectedTextFormat = "count > 5",
#                 container = 'body',
#                 width = 'auto',
#                 size = 15
#         )

button_style = 
        'padding:8px; font-size:80%'

# set defaults
default_inputs =
        list(
                bgg_ranks = list("min" = 1,
                                 "max" = games %>%
                                         filter(!is.na(bayesaverage)) %>%
                                         nrow()
                ),
                min_users = list("value" = 25),
                bgg_years = list("min" = min(games$yearpublished, na.rm = T),
                                 "max" = max(games %>%
                                                     filter(is.na(bayesaverage)) %>%
                                                     pull(yearpublished), na.rm=T)
                )
        )

# global options that will define universe of games included in display
global_filters = 
        list(
                # bgg ranks
                sliderInput(
                        "bgg_ranks",
                        label = "Include Games Ranked:",
                        value = c(default_inputs$bgg_ranks$min,
                                  default_inputs$bgg_ranks$max),
                        min = 1,
                        step = 5,
                        ticks = F,
                        max = games %>%
                                filter(!is.na(bayesaverage)) %>%
                                nrow()
                ),
                numericInput(
                        "min_users",
                        label = 'Minimum User Ratings',
                        value = default_inputs$min_users$value
                ),
                # years
                sliderInput(
                        "bgg_years",
                        label = "Published Between:",
                        value = c(default_inputs$bgg_years$min,
                                  default_inputs$bgg_years$max),
                        min = min(games$yearpublished, na.rm=T),
                        step = 1,
                        ticks = F,
                        max = max(games$yearpublished, na.rm = T)
                )
        )

game_type_filters = 
        list(
                shinyWidgets::virtualSelectInput(
                        "categories",
                        label = "Select Categories",
                        search = T,
                        selected = categories,
                        choices = categories,
                        multiple = T
                ),
                shinyWidgets::virtualSelectInput(
                        "mechanics",
                        label = "Select Mechanics",
                        search = T,
                        selected = mechanics,
                        choices =  mechanics,
                        multiple = T
                ),
                sliderInput(
                        "complexity",
                        label = "BGG Weight",
                        min = 1,
                        max = 5,
                        step = 0.1,
                        value = c(1, 5),
                        ticks = F
                ),
                shinyWidgets::virtualSelectInput(
                        "playercounts",
                        label = "Select Player Counts",
                        selected = unique(games_playercounts$playercount) %>% sort(),
                        choices = unique(games_playercounts$playercount) %>% sort(),
                        multiple = T
                )
        )

game_creator_filters = 
        list(
                shinyWidgets::virtualSelectInput(
                        "publishers",
                        label = "Select Publishers",
                        search = T,
                        selected = publishers,
                        choices =  publishers,
                        multiple = T 
                ),
                shinyWidgets::virtualSelectInput(
                        "designers",
                        label = "Select Designers",
                        search = T,
                        choices =  designers,
                        multiple = T
                )
        )

main_sidebar = 
        sidebar(
                accordion(
                        accordion_panel(
                                "Game Types",
                                game_type_filters
                        ),
                        # apply filters
                        actionButton("update_filters",
                                     "Apply Filters",
                                     class = "btn-primary m-2"),
                        accordion_panel(
                                "Publisher/Designer",
                                game_creator_filters,
                                # apply filters
                                actionButton("reset_filters",
                                             "Reset Filters",
                                             class = "btn-primary m-2"),
                        ),
                        accordion_panel(
                                "Games to Include:",
                                global_filters,
                                actionButton("update_games",
                                             "Update Games",
                                             class = "btn-primary m-2")
                        )
                )
        )


# filters
ui =
        page_navbar(
                theme = my_theme %>%
                        bs_add_rules(sass::sass_file("www/tables.scss")),
                title = "Explore BGG",
                collapsible = FALSE,
                fillable = c("BGG Ratings"),
                id = "navbar",
                # find a board game
                nav_panel(
                        height = "500px",
                        fillable = F,
                        title = 'Find a Board Game',
                        layout_sidebar(
                                sidebar = main_sidebar,
                                # layout
                                layout_columns(
                                        layout_columns(
                                                fillable = F,
                                                max_height = "200px",
                                                # number of games
                                                value_box(
                                                        title = "Games:",
                                                        value = textOutput("number_games"),
                                                        showcase = bsicons::bs_icon("bookshelf"),
                                                        theme_color = "primary",
                                                ),
                                                # average rating
                                                value_box(
                                                        title = "Avg. Rating:",
                                                        value = textOutput("average_rating"),
                                                        showcase = bsicons::bs_icon("bar-chart"),
                                                        theme_color = "secondary"
                                                ),
                                                # average complexity
                                                value_box(
                                                        title = "Avg. Complexity:",
                                                        value = textOutput("average_complexity"),
                                                        showcase = bsicons::bs_icon("dice-6"),
                                                        theme_color = "secondary"
                                                ),
                                                # average ratings
                                                value_box(
                                                        title = "Avg. Ratings",
                                                        value = textOutput("average_usersrated"),
                                                        showcase = bsicons::bs_icon("people"),
                                                        theme_color = "secondary"
                                                )
                                        )
                                ),
                                # main visualizations
                                layout_column_wrap(
                                        width = 1/2,
                                        # card(
                                        #         reactableOutput("games_table"),
                                        #         height = "800px"
                                        # ),
                                        layout_column_wrap(
                                                width = 1,
                                                # table
                                                card(
                                                        reactableOutput("games_table"),
                                                        height = "600px"
                                                )
                                                # # distributions
                                                # card(
                                                #         girafeOutput("outcomes"),
                                                #         height = "400px"
                                                # )
                                        ),
                                        layout_column_wrap(
                                                width = 1,
                                                heights_equal = "row",
                                                # average vs weight
                                                card(
                                                        card_header("Average Rating and Complexity"),
                                                        girafeOutput("average_vs_weight"),
                                                        height = "300px"
                                                ),
                                                # # average vs users ratings
                                                # card(
                                                #         card_header("Average Rating and Complexity"),
                                                #         girafeOutput("average_vs_weight"),
                                                #         height = "400px"
                                                # ),
                                                # recommended player counts
                                                card(
                                                        card_header("Player Counts"),
                                                        girafeOutput("summary_playercounts",
                                                                     height = '300px')
                                                )
                                                # # categorical
                                                # navset_card_tab(
                                                #         title = "Most Frequent",
                                                #         nav_panel("Categories", 
                                                #                   girafeOutput("top_categories"),
                                                #                   height = "300px"
                                                #         ),
                                                #         nav_panel("Mechanics", 
                                                #                   girafeOutput("top_mechanics"),
                                                #                   height = "300px"),
                                                # )
                                        )
                                )
                        )
                ),
                # examine the geek rating
                nav_panel(
                        title = "BGG Ratings",
                        layout_columns(
                                card(
                                        card(height = 200, lorem::ipsum(15)),
                                        card_header("Some explanation of how exactly..."),
                                        plotOutput("bgg_outcomes"),
                                )
                        )
                        #         card(),
                        #         layout_column_wrap(
                        #                 card(
                        #                         plotOutput("bgg_outcomes")
                        #                 ),
                        #                 card(),
                        #                 card(),
                        #         )
                        # )
                ),
                # examine a game
                nav_panel(
                        title = "Examine a Game",
                        layout_columns(
                                card(width = 300)
                        )
                ),
                # Designers
                nav_panel(
                        title = "Designers",
                        layout_columns(
                                card(
                                        
                                )
                        )
                ),
                # Designers
                nav_panel(
                        title = "Publishers",
                        layout_columns(
                                card(
                                        
                                )
                        )
                )
                # player counts
                # publishers
                # designers
                # mechanics
        )


server <- function(input, output, session) {
        
        #  bslib::bs_themer()
        output$check_filters =
                renderText({
                        print(input$reset_filters)
                })
        
        gamesInput =
                eventReactive(
                        c(input$update_games,
                          input$reset_filters),
                        {
                                games %>%
                                        #filter based on min users
                                        filter(usersrated >= input$min_users[1])
                                #  %>%
                                # # filter to selected ranks
                                # filter(bgg_rank >=input$bgg_ranks[1] & bgg_rank <=input$bgg_ranks[2]) %>%
                                # # filter years
                                # filter(yearpublished >= input$bgg_years[1] & yearpublished <= input$bgg_years[2])
                        },
                        ignoreNULL = F
                )
        
        # on reset filters, flip games back to gamesInput
        # initial_games = 
        #         eventReactive(
        #                 input$reset_filters,
        #                 {
        #                         gamesInput()
        #                 }
        #         )
        
        # update filters based on selection of games
        observeEvent(
                c(input$update_games,
                  input$reset_filters),
                {
                        # categories
                        categories_options =
                                gamesInput() %>%
                                unnest_categorical(categories, min = 1) %>%
                                pull_categorical()
                        
                        # mechanics
                        mechanics_options = 
                                gamesInput() %>%
                                unnest_categorical(mechanics, min = 1) %>%
                                pull_categorical()
                        
                        # publishers
                        publishers_options = 
                                gamesInput() %>%
                                unnest_categorical(publishers, min = 25) %>%
                                pull_categorical()
                        
                        # designers
                        designers_options = 
                                gamesInput() %>%
                                unnest_categorical(designers, min = 25) %>%
                                pull_categorical()
                        
                        # update picker
                        updatePickerInput(
                                session = session,
                                inputId = "categories",
                                selected = categories_options,
                                choices = categories_options
                        )
                        
                        updatePickerInput(
                                session = session,
                                inputId = "mechanics",
                                selected = mechanics_options,
                                choices = mechanics_options
                        )
                        
                        updatePickerInput(
                                session = session,
                                inputId = "publishers",
                                selected = publishers_options,
                                choices = publishers_options
                        )
                        
                        updatePickerInput(
                                session = session,
                                inputId = "designers",
                                selected = designers_options,
                                choices = designers_options
                        )
                        
                },
                ignoreNULL = F
        )
        
        # filter games based on selections
        filtered_games =
                eventReactive(
                        c(input$update_filters,
                          input$update_games),
                        {
                                gamesInput() %>%
                                        # filter complexity
                                        filter(averageweight >= input$complexity[1] & averageweight <= input$complexity[2]) %>%
                                        # filter categories
                                        filter(game_id %in% (games_categories %>%
                                                                     filter(value %in% input$categories) %>%
                                                                     pull(game_id))
                                        ) %>%
                                        # filter mechanics
                                        filter(game_id %in% (games_mechanics %>%
                                                                     filter(value %in% input$mechanics) %>%
                                                                     pull(game_id))
                                        ) %>%
                                        # filter publishers
                                        filter(game_id %in% (games_publishers %>%
                                                                     filter(value %in% input$publishers) %>%
                                                                     pull(game_id))
                                        ) %>%
                                        # filter designers
                                        filter(game_id %in% (games_designers %>%
                                                                     filter(value %in% input$designers) %>%
                                                                     pull(game_id))
                                        ) %>%
                                        # filter player counts
                                        filter(game_id %in% (games_playercounts %>%
                                                                     filter(playercount_votes >=5) %>%
                                                                     filter(type %in% c('Best', 'Recommended')) %>%
                                                                     filter(playercount %in% input$playercounts) %>%
                                                                     pull(game_id))
                                        )
                        },
                        ignoreNULL = FALSE
                                        )
                                
                                # list of game ids in selection
                                filtered_game_ids =
                                        reactive({
                                                filtered_games() %>%
                                                        pull(game_id)
                                        })
                                
                                ### tables
                                output$games_table =
                                        renderReactable({
                                                filtered_games() %>%
                                                        select_summary_cols() %>%
                                                        build_summary_table(
                                                                style = reactable_style(),
                                                                games = games,
                                                                simple = T,
                                                                defaultPageSize = 25
                                                        )
                                        }
                                        )
                                
                                # output$outcomes =
                                #         renderGirafe({
                                #                 girafe(
                                #                         ggobj =
                                #                                 filtered_games() %>%
                                #                                 plot_outcomes_histograms(),
                                #                         width = 9)
                                #         }
                                #         )
                                # 
                                # plot_average_vs_weight_background = 
                                #         reactive({
                                #                 gamesInput() %>%
                                #                         plot_average_vs_weight(engine = 'ggplot',
                                #                                                y_range = c(0, 10))+
                                #                         geom_point()
                                #         })
                                # 
                                
                                ### plots
                                # average vs weight
                                output$average_vs_weight =
                                        renderGirafe(
                                                {
                                                        girafe(
                                                                ggobj = filtered_games() %>%
                                                                        plot_average_vs_weight(engine = 'ggplot',
                                                                                               y_range = c(0, 10)),
                                                                width = 12
                                                        )
                                                }
                                        )
                                
                                # top categorical variables
                                categorical_vars = c("categories",
                                                     "mechanics")
                                # "publishers",
                                # "designers",
                                # "artists")
                                
                                categorical_plots =
                                        reactive({
                                                plots =
                                                        map(categorical_vars,
                                                            ~ filtered_games() %>%
                                                                    count_categorical(
                                                                            variable = .x,
                                                                            slice_n = 15,
                                                                            other = F) %>%
                                                                    mutate(type = .x) %>%
                                                                    plot_categorical()
                                                        )
                                                
                                                names(plots) = categorical_vars
                                                
                                                plots
                                                
                                        })
                                
                                output$top_categories =
                                        renderGirafe({
                                                girafe(
                                                        ggobj = categorical_plots()$categories                        
                                                )
                                        })
                                
                                output$top_mechanics =
                                        renderGirafe({
                                                girafe(
                                                        ggobj = categorical_plots()$mechanics
                                                )
                                        })
                                
                                # output$top_mechanics =
                                #         renderGirafe({
                                #                 girafe(
                                #                         ggobj = categorical_plots()$mechanics
                                #                 )
                                #         })
                                
                                # output$top_artists =
                                #         renderGirafe({
                                #                 ggobj = categorical_plots()$artists
                                #         })
                                # 
                                # output$top_designers =
                                #         renderGirafe({
                                #                 ggobj = categorical_plots()$designers
                                #         })
                                
                                # output$top_publishers =
                                #         renderGirafe({
                                #                 ggobj = categorical_plots()$publishers
                                #         })
                                
                                output$summary_playercounts =
                                        renderGirafe({
                                                girafe(
                                                        ggobj =
                                                                games_playercounts %>%
                                                                filter(game_id %in% filtered_game_ids()) %>%
                                                                plot_playercount_recommendations(),
                                                        width = 9
                                                )
                                        }
                                        )
                                
                                ### value boxes
                                # display number of games
                                output$number_games =
                                        reactive({
                                                nrow(filtered_games())
                                        })
                                
                                # display average
                                output$average_rating =
                                        reactive({
                                                mean(filtered_games()$average, na.rm=T) %>%
                                                        round(., digits = 2)
                                        })
                                
                                # display average complexity
                                output$average_complexity =
                                        reactive({
                                                mean(filtered_games()$averageweight, na.rm=T) %>%
                                                        round(., digits = 2)
                                        })
                                
                                # display average complexity
                                output$average_usersrated =
                                        reactive({
                                                mean(filtered_games()$usersrated, na.rm=T) %>%
                                                        round(digits = 0)
                                        })
                                
                                # bgg outcomes
                                output$bgg_outcomes = 
                                        renderPlot({
                                                filtered_games() %>%
                                                        filter(!is.na(bayesaverage)) %>%
                                                        mutate(usersrated = log(usersrated)) %>%
                                                        select(
                                                                game_id,
                                                                any_of(bgg_outcomes())
                                                        ) %>%
                                                        ggplot(aes(x=.panel_x,
                                                                   y = .panel_y)) +
                                                        geom_autopoint(size = 0.75)+
                                                        geom_autodensity(alpha = 0.6)+
                                                        facet_matrix(
                                                                vars(average, averageweight, usersrated),
                                                                layer.diag = 2,
                                                                grid.y.diag = F,
                                                        )
                                        })
}

shinyApp(ui, server)
