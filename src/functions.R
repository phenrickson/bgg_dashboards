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
                                      paste(collapse = ",")))
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
                                       scrollX=F)
                )
        
}



make_games_datatable = function(data,
                                pageLength = 15) {
        
        
        data %>%
                mutate(Published = yearpublished,
                       Name = name,
                       Complexity = averageweight,
                       Ratings = usersrated,
                       Average = average,
                       Geek = bayesaverage,
                       .keep = 'none',
                       .before = 'Published'
                ) %>%
                mutate_if(is.numeric, round, 2) %>%
                datatable(escape=F,
                          rownames = F,
                          class = list(stripe = FALSE),
                          # filter = list(position = 'top'),
                          options = list(pageLength = pageLength,
                                         initComplete = htmlwidgets::JS(
                                                 "function(settings, json) {",
                                                 paste0("$(this.api().table().container()).css({'font-size': '", '8pt', "'});"),
                                                 "}"),
                                         scrollX=F,
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
        complexity_colors = colorRampPalette(c("white", "orange"))(length(complexity_breaks) + 1)
        
        average_breaks = c(2, seq(3, 9, 0.1), 10)
        average_colors = colorRampPalette(c("red", "white", "dodgerblue2"))(length(average_breaks) + 1)
        
        geek_breaks = c(seq(4, 7, 0.1), 7.2, 7.4, 7.5, 8, 9)
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

games %>%
        make_games_datatable() %>%
        format_games_datatable()

# quantiles = seq(0, 1, 0.05)
# 
# games %>%
#         filter(!is.na(bayesaverage)) %>%  
#         summarise(rating = quantile(usersrated, quantiles),
#                   quantile = quantiles) %>%
#         print(n = 50)


