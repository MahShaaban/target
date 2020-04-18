# load required libraries
require(shiny)
require(shinyBS)
require(DT)
require(readr)
require(target)
require(GenomicRanges)
require(GenomicFeatures)
require(broom)
require(TxDb.Hsapiens.UCSC.hg19.knownGene)
require(TxDb.Hsapiens.UCSC.hg38.knownGene)
require(TxDb.Mmusculus.UCSC.mm10.knownGene)
require(TxDb.Mmusculus.UCSC.mm9.knownGene)

source('modules.R')
source('demo.R')

# define server
server <- function(input, output, session) {
    # read inputs
    peaks <- list()
    peaks$data <- callModule(
        tsvFile, 
        'peaks_file',
        demo$peaks$file
    )
    peaks$name_column <- callModule(
        columnSelect,
        id = 'peaks_name_column',
        path = demo$peaks$file
    )
    peaks$distance <- callModule(
        numberSlider,
        id = 'peaks_distance'
    )
    
    output$peaks_table <- renderDataTable(peaks$data())
    
    expression <- list()
    
    expression$data <- callModule(
        tsvFile,
        'expression_file',
        demo$expression$file
    )
    expression$name_column <- callModule(
        columnSelect,
        id = 'expression_name_column',
        path = demo$expression$file
    )
    # expression$number_factor <- callModule(
    #     characterRadioInput,
    #     id = 'expression_factor_number'
    # )
    expression$number_factor <- reactive(input$expression_factor_number)
    expression$stat_column1 <- callModule(
        columnSelect,
        id = 'expression_stat_column1',
        path = demo$expression$file
    )
    expression$stat_column2 <- callModule(
        columnSelect,
        id = 'expression_stat_column2',
        path = demo$expression$file
    )
    
    output$expression_table <- renderDataTable(expression$data())
    
    genome <- list()
    
    genome$data <- callModule(
        tsvFile,
        'genome_file',
        demo$genome$file
    )
    
    observe({
        if (input$genome_type != 'Custome') {
            genome$data <- reactive({
                load_genome(input$genome_type)
            })
        }
    })
    
    genome$name_column <- callModule(
        columnSelect,
        id = 'genome_name_column',
        path = demo$genome$file
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
    # params1 <- callModule(
    #     predictionVariables,
    #     return = 'plot'
    # )
    params1 <- demo_plot
    
    # make prediction plot
    output$predicted_plot <- renderPlot({
        make_prediction(dt(), params1, return = 'plot')
    })
    
    # to be replaced by call to the input module
    # params2 <- callModule(
    #     predictionVariables,
    #     return = 'test'
    # )
    params2 <- demo_test
    
    # make prediction testing
    output$predicted_testing <- renderDataTable({
        make_prediction(dt(), params2, return = 'test')
    })
}