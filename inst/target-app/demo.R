# define variables for the demo
demo <- list(
    peaks = list(
        file = 'sim_peaks.tsv',
        name_column = 'peak_name',
        distance = 100
    ),
    expression = list(
        file = 'sim_transcripts.tsv',
        name_column = 'tx_id',
        factor_number = 'Two',
        stat_column1 = 'stat1',
        stat_column2 = 'stat2'
    ),
    genome = list(
        file = 'sim_genome.tsv',
        name_column = 'tx_id',
        type = 'Custome'
    )
)

demo_plot <- list(
    ranking = 'score_rank',
    grouping = 'stat',
    groupby = 'top',
    group_breaks = '-100,100',
    group_colors = 'green,gray,red',
    group_labels = 'comp,none,coop',
    xlabel = 'Ranking',
    ylabel = 'ECDF',
    main = 'Aggregated Functions'
)

demo_test <- list(
    ranking = 'score_rank',
    grouping = 'stat',
    groupby = 'top',
    group_breaks = '-100,100',
    group_labels = 'comp,none,coop',
    compare = 'coop,none',
    alternative = 'greater'
)