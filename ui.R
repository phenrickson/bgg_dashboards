# make multi page layout
page_navbar(
        title = "Explore Board Games",
        theme = my_theme,
        fillable = F,
        selected = 'Find a Game',
        nav_panel(
                title = 'Find a Game',
                # title = "Find Neighbors",
                # findNeighborsUI("neighbors")
        ),
        nav_panel(
                title = 'Examine a Game',
        ),
        nav_panel(
                title = 'Publishers & Designers'
        ),
        nav_panel(
                title = "Mechanics"
        ),
        nav_panel(
                title = 'Player Counts',
                # title = "Patents Overview",
                # patentHistoryUI("history")
        )
)
