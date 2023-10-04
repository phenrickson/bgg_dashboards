bgg_ids = function() {
        c("game_id", "name", "yearpublished")
}

bgg_outcomes = function() {
        c("averageweight", "average", "bayesaverage", "usersrated")
}


prep_playercount_recommendations = function(data) {
        
        data %>%
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
}

plot_playercount_recommendations =  function(data,
                                             pal = colorRampPalette(c("dodgerblue", "white", "firebrick3"))(10)) {
        
        data %>%
                group_by(type, playercount) %>%
                filter(!is.na(playercount)) %>%
                count() %>%
                ggplot(aes(x=playercount,
                           y=n,
                           fill = type,
                           tooltip = paste0(
                                   paste('Games:', n),
                                   '\n',
                                   paste('Player Count:', playercount),
                                   '\n',
                                   paste(type)
                           )))+
                geom_col_interactive()+
                scale_fill_manual(
                        values = c(pal[1], pal[3], pal[8])
                )+
                #    scale_y_discrete(limits = rev)+
                xlab("Player Count")+
                ylab("Games")
}

plot_outcomes_histograms = function(data) {
        
        data %>%
                select(
                        any_of(bgg_ids()),
                        any_of(bgg_outcomes())
                ) %>%
                mutate(usersrated = log1p(usersrated)) %>%
                pivot_longer(
                        cols = -c(bgg_ids()),
                        names_to = "outcome",
                        values_to = "value"
                ) %>%
                mutate(outcome = 
                               case_when(
                                       outcome == 'average' ~ 'average rating',
                                       outcome == 'averageweight' ~ 'average weight',
                                       outcome == 'usersrated' ~ 'user ratings (logged)',
                                       outcome == 'bayesaverage' ~ 'geek rating')
                ) %>%
                ggplot(aes(x=value,
                           tooltip =  paste0("value: [",round(..xmin..,2),",",
                                             round(..xmax..,2),"]",
                                             "\n",
                                             "count: ",..count..)))+
                geom_histogram_interactive(bins = 60)+
                facet_wrap(
                        outcome ~.,
                        scales = "free"
                )
        
}

# make scatter plot for average ratings vs complexity
plot_average_vs_weight = function(data,
                                  x_range = c(0.8, 5),
                                  y_range = c(3, 10),
                                  color = T,
                                  engine = "ggplot") {
        
        if (engine == 'plotly') {
                
                require(plotly)
                
                data %>%
                        plotly::plot_ly(
                                x = ~ jitter(averageweight, amount = 0.05),
                                y = ~ average,
                                size = ~ log1p(usersrated),
                                type = 'scatter',
                                hoverinfo = "text",
                                mode = 'markers',
                                marker = list(
                                        # color = 'darkgrey',
                                        # fill = 'darkgrey',
                                        #  symbol = 'circle-open'
                                ),
                                text =  ~ paste(
                                        paste0(name, ' (', yearpublished, ')'),
                                        paste("Average Weight", round(averageweight, 2)),
                                        paste("Average Rating:", round(average, 2)),
                                        paste("User Ratings:", usersrated),
                                        paste("Geek Rating", round(bayesaverage, 2)),
                                        sep = "\n"),
                                name = 'All Games'
                        ) %>%
                        hide_colorbar() %>%
                        layout(
                                xaxis = list(title = "Average Weight",
                                             range = x_range),
                                yaxis = list(title = "Average Rating",
                                             range = y_range)
                        ) %>%
                        layout(legend = list(orientation = "h",   # show entries horizontally
                                             xanchor = "center",  # use center of legend as anchor
                                             x = 0.5,
                                             y = 11))
                
        }
        else if (engine == 'ggplot') {
                
                data %>%
                        filter(!is.na(bayesaverage)) %>%
                        # mutate(highlight = case_when(row_number() %in% input$games_highlighted~ 'yes',
                        #                              TRUE ~ 'no')) %>%
                        ggplot(aes(x=averageweight,
                                   y=average,
                                   color = average,
                                   label = name,
                                   size = log(usersrated),
                                   tooltip = paste0(name,
                                                    '\n',
                                                    'Complexity: ', round(averageweight, 2),
                                                    '\n',
                                                    'Average Rating: ', round(average,2),
                                                    '\n',
                                                    'User Ratings: ', usersrated,
                                                    '\n',
                                                    'Geek Rating: ', round(bayesaverage, 2),
                                                    '\n',
                                                    'BGG Rank: ', bgg_rank)))+
                        geom_point_interactive(
                                alpha = 0.5,
                                shape = 16
                        )+
                        guides(size = 'none')+
                        coord_cartesian(xlim = x_range,
                                        ylim = y_range)+
                        xlab("Average Weight")+
                        ylab("Average Rating")+
                        scale_color_gradient2(low = 'red',
                                              mid = 'grey65',
                                              high = 'dodgerblue2',
                                              midpoint = 7,
                                              limits = c(5, 9),
                                              oob=scales::squish)+
                        # guides(color = guide_colorbar(barheight=0.5,
                        #                               title.vjust = 1,
                        #                               barwidth=15,
                        #                               title.position = 'top'))+
                        guides(color = 'none',
                               alpha = 'none')
                # scale_alpha_manual(values = c(0.3, 1))
        }
        
}


# for visualizaing categorical data
count_categorical = function(data, 
                             variable,
                             ids = bgg_ids(),
                             other = T,
                             other_n = 10,
                             slice = T,
                             slice_n = 40) {
        
        unnested = 
                data %>%
                select(any_of(ids),
                       {{variable}}) %>%
                unnest({{variable}})
        
        if (other == T) {
                
                summarized = 
                        unnested %>%
                        mutate(value = fct_lump_n(fct_infreq(value), other_n)) %>%    
                        group_by(value) %>%
                        summarize(games = n_distinct(game_id),
                                  .groups = 'drop') %>%
                        arrange(desc(games)) %>%
                        left_join(.,
                                  unnested %>%
                                          distinct(id, value),
                                  by = join_by(value)
                        )
        } else {
                summarized = 
                        unnested %>%
                        group_by(id, value) %>%
                        summarize(games = n_distinct(game_id),
                                  .groups = 'drop') %>%
                        arrange(desc(games))
        }
        
        if (slice == T) {
                
                out = summarized  %>%
                        mutate(
                                id,
                                value,
                                games,
                                .keep = 'none'
                        ) %>%
                        slice_max(order_by = games, n = slice_n, with_ties = FALSE) 
        } else {
                out = summarized  %>%
                        mutate(
                                id,
                                value,
                                games,
                                .keep = 'none'
                        )
        }
        
        attr(out, "categorical") <- rlang::englue("{{variable}}")
        
        out
        
        
}

table_categorical =  function(data,
                              ...) {
        
        
        #  variable = enquo(attr(data, "variable"))
        
        categorical = attr(data, "categorical")
        
        data %>% 
                mutate(
                        !!categorical := value,
                        games,
                        .keep = 'none',
                        .after = games
                ) %>%
                relocate(
                        !!categorical,
                        games
                )
        
}


plot_categorical = function(data) {
        
        data %>%
                ggplot(aes(x=games,
                           tooltip = paste0(value,
                                            '\n',
                                            'Games:', games),
                           y=reorder(value, games))) +
                geom_col_interactive(fill = 'grey60')+
                ylab("")
}


# games %>%
#         filter(!is.na(bayesaverage)) %>%
#         count_categorical(publishers,
#                           other = F,
#                           other_n = 10,
#                           slice = T,
#                           slice_n = 10) %>%
#         plot_categorical()
#         table_categorical()

# games %>%
#         filter(!is.na(bayesaverage)) %>%
#         summarize_categorical(publishers)
# 
# games %>%
#         filter(!is.na(bayesaverage)) %>%
#         summarize_categorical(mechanics)
# 
# games %>%
#         filter(!is.na(bayesaverage)) %>%
#         summarize_categorical(artists)
# 
# games %>%
#         filter(!is.na(bayesaverage)) %>%
#         summarize_categorical(designers)



# set.seed(1)
# games %>%
#         filter(!is.na(bayesaverage)) %>%
#         plot_average_vs_weight()

# games %>%
#         filter(!is.na(bayesaverage)) %>%
#         select(name, averageweight, usersrated, average, bayesaverage) %>%
#         mutate_if(is.numeric, round, 2) %>%
#         datatable()
# make_games_datatable()
# format_games_datatable()
#  
# games %>%
#         filter(!is.na(bayesaverage)) %>%
#     #    mutate(highlight = 'yes') %>%
#         plot_average_vs_weight(engine = 'plotly')

add_trace_games = function(plot,
                           game_ids,
                           color = 'blue',
                           ax = 10,
                           ay = 20,
                           trace_label = "test") {
        
        plot %>%
                add_trace(
                        data = plotly_data(plot, 1) %>%
                                filter(game_id %in% game_ids),
                        mode = 'markers',
                        textposition = 'topright',
                        marker = list(
                                color = color,
                                symbol = 'circle',
                                opacity = 1
                        ),
                        name = trace_label
                ) %>%
                add_annotations(
                        data = plotly_data(plot, 1) %>%
                                filter(game_id %in% game_ids),
                        text = ~ name,
                        ax = ax,
                        ay = ay,
                        arrowhead = 0,
                        font = list(
                                size = 12,
                                color = color)
                )
        
}

# get distinct values for categorical variables
get_distinct_var = function(data = games,
                            select_var,
                            select_value = value) {
        
        temp = data %>%
                select({{ select_var }}) %>%
                unnest({{ select_var}})
        
        if (nrow(temp) ==0) {
                character(0)
        } else {
                temp %>%
                        distinct({{ select_value}}) %>%
                        pull() %>%
                        sort()
        }
        
}

# round text for presenting with renderText
round_text = function(var,
                      digits = 2) {
        
        if (is.nan(var) | is.na(var)) {""}
        else {
                round(var, digits)
        }
        
}

# make plot for average vs ratings
plot_average_vs_ratings = function(data) {
        
        
        data %>% 
                ggplot(aes(x=average,
                           y=averageweight, 
                           size = log(usersrated))) + 
                geom_point_interactive(alpha = 0.5,
                                       tooltip = paste0(game_id))+guides(size = 'none')
}

# datatable code
datatable_format = function() {
        
        htmlwidgets::JS("function(settings, json) {",
                        paste0("$(this.api().table().container()).css({'font-size': '", '10pt', "'});"), "}")
        
}

# datatable default
datatable_default = function(data) {
        
        data %>%
                DT::datatable(
                        escape = F,
                        rownames = F,
                        class = list(stripe = FALSE),
                        selection = 'none',
                        options = list(pageLength = 10,
                                       scrollY = T,
                                       autowidth = T,
                                       initComplete = datatable_format(),
                                       scrollX=F),
                        height = "100%"
                )
}


prep_games_datatable = 
        function(data) {
                
                data %>%
                        mutate(Rank = bgg_rank,
                               Published = yearpublished,
                               Name = name,
                               Complexity = averageweight,
                               Ratings = usersrated,
                               Average = average,
                               Geek = bayesaverage,
                               .keep = 'none',
                               .before = 'Published'
                        ) %>%
                        mutate_if(is.numeric, round, 2)
        }

make_games_datatable = function(data,
                                pageLength = 10,
                                ...) {
        
        
        datatable(escape=F,
                  ...,
                  #   rownames = F,
                  #  class = list(stripe = FALSE),
                  # filter = list(position = 'top'),
                  options = list(
                          pageLength = pageLength,
                          lengthChange = F,
                          initComplete = htmlwidgets::JS(
                                  "function(settings, json) {",
                                  paste0("$(this.api().table().container()).css({'font-size': '", '8pt', "'});"),
                                  "}"),
                          scrollX=F,
                          stripe = F,
                          autowidth=T,
                          info = F,
                          columnDefs = list(list(className = 'dt-center',
                                                 targets=c("Published",
                                                           "Complexity",
                                                           "Ratings",
                                                           "Average",
                                                           "Geek")))))
        
}





format_games_datatable = function(datatable) {
        
        complexity_breaks = seq(1, 5, 0.01)
        complexity_colors = colorRampPalette(c("deepskyblue1", "white", "orange"))(length(complexity_breaks) + 1)
        
        average_breaks = c(2, 3, 4, 5, 6, seq(7, 8, 0.1), 8.5, 9, 10)
        average_colors = colorRampPalette(c("red", "white", "dodgerblue2"))(length(average_breaks) + 1)
        
        geek_breaks = c(4, seq(5, 7, 0.1), 7.2, 7.4, 7.5, 8, 9)
        geek_colors = colorRampPalette(c("red", "white", "dodgerblue2"))(length(geek_breaks) + 1)
        
        ratings_breaks= c(0, 50, 100, 1000, 2000, 3000, 5000, 10000, 20000, 30000, 40000, 50000, 75000, 100000, 500000)
        ratings_colors = colorRampPalette(c("white", "dodgerblue2"))(length(ratings_breaks) + 1)
        
        datatable %>%
                formatStyle(c("Average"),
                            backgroundColor = styleInterval(cuts = average_breaks, values = average_colors)) %>%
                formatStyle(c("Complexity"),
                            backgroundColor = styleInterval(cuts = complexity_breaks, values = complexity_colors)) %>%
                formatStyle(c("Ratings"),
                            backgroundColor = styleInterval(cuts = ratings_breaks, values = ratings_colors)) %>%
                formatStyle(c("Geek"),
                            backgroundColor = styleInterval(cuts = geek_breaks, values = geek_colors))
}


make_plot_average_complexity = function(data) {
        
        data %>%
                ggplot(aes(x=averageweight,
                           y=average,
                           # alpha = highlight,
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
                scale_alpha_manual(values = c(0.3, 1))
}

# games %>%
#         head(1000) %>%
#         make_plot_average_complexity() %>%
#         girafe(ggobj = .)
#         make_ga

# games %>%
#         head(10000) %>%
#         make_games_datatable() %>%
#         format_games_datatable()
# 
# # quantiles = seq(0, 1, 0.05)
# # 
# games %>%
#         filter(!is.na(bayesaverage)) %>%
#         summarise(rating = quantile(usersrated, quantiles),
#                   quantile = quantiles) %>%
#         print(n = 50)


unnest_categorical = function(data,
                              variable,
                              min = 5) {
        
        data %>%
                select(game_id, name) %>% 
                left_join(.,
                          data %>%
                                  select(game_id, name, {{variable}}) %>% 
                                  unnest({{variable}})
                ) %>%
                mutate(value = replace_na(value, 'Other')) %>%
                mutate(value = as.character(fct_lump_min(value, min))) %>%
                select(game_id, name, value) %>%
                distinct()
        
}

pull_categorical = function(data) {
        
        data %>%
                pull(value) %>%
                unique() %>%
                sort()
}

find_games_with_categorical = function(data,
                                       levels) {
        
        data %>%
                filter(value %in% levels) %>%
                pull(game_id)
}

