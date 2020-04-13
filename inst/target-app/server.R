# load required libraries
require(shiny)
require(shinyBS)
require(DT)
require(readr)
require(target)
require(GenomicRanges)
require(GenomicFeatures)
require(broom)

source('modules.R')

# define server
server <- function(input, output, session) {
    # read inputs
    peaks <- list(
        data = callModule(
            tsvFile, 
            'peaks_file',
            'sim_peaks.tsv'
        ),
        name_column = callModule(
            columnSelect,
            id = 'peaks_name_column',
            table = peaks$data()
        ),
        distance = callModule(
            numberSlider,
            id = 'peaks_distance'
        )
    )
    
    output$peaks_table <- renderDataTable(peaks$data())
    
    expression <- list(
        data = callModule(
            tsvFile,
            'expression_file',
            'sim_transcripts.tsv'
        ),
        name_column = callModule(
            columnSelect,
            id = 'expression_name_column',
            table = expression
        ),
        number_factor = callModule(
            characterRadioInput,
            id = 'expression_factor_number'
        ),
        stat_column1 = callModule(
            columnSelect,
            id = 'expression_stat_column1',
            table = expression
        ),
        stat_column2 = callModule(
            columnSelect,
            id = 'expression_stat_column2',
            table = expression
        )
    )
    
    output$expression_table <- renderDataTable(expression$data())
    
    genome <- list(
        data = callModule(
            tsvFile, 
            'genome_file', 
            'sim_genome.tsv'
        ),
        name_column = callModule(
            columnSelect,
            id = 'genome_name_column',
            table = genome$data()
        )
    )
    output$genome_table <- renderDataTable(genome$data())
    
    # calculate associated peaks
    output$associated_peaks <- renderDataTable({
        run_target(
            return = 'associated_peaks',
            peaks = peaks$data(),
            expression = expression$data(),
            genome = genome$data(),
            distance = peaks$distance(),
            expression_name = expression$name_column(),
            genome_name = genome$name_column()
        )
    })
    
    # calculate direct targets
    dt <- reactive(
        run_target(
            return = 'direct_targets',
            peaks = peaks$data(),
            expression = expression$data(),
            genome = genome$data(),
            expression_name = expression$name_column(),
            genome_name = genome$name_column(),
            distance = peaks$distance(),
            factor_num = expression$number_factor(),
            stat1 = expression$stat_column1(),
            stat2 = expression$stat_column2()
        )
    )
    output$direct_targets <- renderDataTable({
        dt()
    })
    
    # to be replaced by call to the input module
    params1 <- callModule(
        predictionVariables,
        return = 'plot'
    )
    params1 <- list(
        ranking = 'score_rank',
        grouping = 'stat',
        groupby = 'top',
        group_breaks = '10000',
        group_colors = 'green,gray,red',
        group_labels = 'comp,none,coop',
        xlabel = 'Ranking',
        ylabel = 'ECDF',
        main = 'Aggregated Functions'
    )
    
    # make prediction plot
    output$predicted_plot <- renderPlot({
        make_prediction(dt(), params1, return = 'plot')
    })
    
    # to be replaced by call to the input module
    params2 <- callModule(
        predictionVariables,
        return = 'test'
    )
    params2 <- list(
        ranking = 'score_rank',
        grouping = 'stat',
        groupby = 'top',
        group_breaks = '10000',
        group_labels = 'comp,none,coop',
        compare = 'coop,none',
        alternative = 'greater'
    )
    
    # make prediction testing
    output$predicted_testing <- renderDataTable({
        make_prediction(dt(), params2, return = 'test')
    })
}