# load required libraries -----------------------------------------------------
require(shiny)
require(shinyBS)
require(DT)
require(GenomicRanges)
require(GenomicFeatures)
require(TxDb.Hsapiens.UCSC.hg19.knownGene)
require(TxDb.Hsapiens.UCSC.hg38.knownGene)
require(TxDb.Mmusculus.UCSC.mm10.knownGene)
require(TxDb.Mmusculus.UCSC.mm9.knownGene)
require(target)
require(dplyr)
require(readr)
#require(markdown)

# define ui -------------------------------------------------------------------
ui <- navbarPage(
    title = 'target',
    tabPanel(
        # Main Page, contains:
        # 1. Input panels
        # 2. Output tabs
        'main',
        # Input panels ----
        # 1. Binding data
        # 2. Expression data
        # 3. Reference genome
        splitLayout(
            # Binding data input panel
            # Upload peaks file in bed format
            # Choose distance between peaks and regions
            wellPanel(
                tags$h4('Binding Data'),
                tags$p("Peaks file from ChIP experiment."),
                fileInput(
                    'peaks',
                    label = 'Upload File'
                    ),
                bsTooltip('peaks',
                          'Tab separated text file with at least three columns.',
                          'top', options = list(container = "body")),
                selectInput(
                    'peak_id_col',
                    label = 'Name Column',
                    choices = 'Pending'
                ),
                bsTooltip('peak_id_col',
                          'Name of the column in the uploaded file that contains peak names.',
                          'top', options = list(container = "body")),
                sliderInput(
                    'distance',
                    label = 'Choose Distance',
                    min = 0,
                    max = 500,
                    value = 100
                    ),
                bsTooltip('distance',
                          'Distance in kb to get peaks within.',
                          'top', options = list(container = "body"))
            ),
            # Expression data input panel
            # Upload differential expression table
            # Choose the name of the column of the regions IDs
            # Choose the number of factors
            # Choose the name of the column/s of the statistics of the factor/s
            wellPanel(
                tags$h4('Expression Data'),
                tags$p("Statistics from purturbed expression."),
                fileInput(
                    'expression',
                    label = 'Upload File'
                    ),
                bsTooltip('expression',
                          'Tab separated text file with at least three columns.',
                          'top', options = list(container = "body")),
                selectInput(
                    'region_id',
                    label = 'Name Column',
                    choices = 1
                    ),
                bsTooltip('region_id',
                          'Name of the column in the uploaded file that contains region IDs.',
                          'top', options = list(container = "body")),
                radioButtons(
                    'type',
                    label = 'Number of Factors',
                    choices = c('One', 'Two'),
                    selected = 'One',
                    inline = TRUE
                    ),
                selectInput(
                    'stat_id',
                    label = 'Statistics Column',
                    choices = 'Pending'
                    ),
                bsTooltip('stat_id',
                          'Name of the column in the uploaded file that contains region statistics.',
                          'top', options = list(container = "body")),
                selectInput(
                    'stat_id2',
                    label = 'Statistics Column (second factor)',
                    choices = 'Pending'
                ),
                bsTooltip('stat_id2',
                          'Name of the column in the uploaded file that contains region statistics.',
                          'top', options = list(container = "body"))
            ),
            # Reference genome input panel
            # Upload the genome file in bed format when selected genome is custome
            # Choose genome from the built in options
            # Choose the name of the column of the regions IDs in the genome file
            wellPanel(
                tags$h4('Reference Genome'),
                tags$p('Genomic coordinates of the regions.'),
                fileInput(
                    'genome',
                    label = 'Upload File'
                    ),
                bsTooltip('genome',
                          'Tab separated text file with at least four columns.',
                          'top', options = list(container = "body")),
                selectInput(
                    'genome_id_col',
                    label = 'Name Column',
                    choices = 1
                ),
                bsTooltip('genome_id_col',
                          'Name of the column in the uploaded file that contains region IDs.',
                          'top', options = list(container = "body")),
                selectInput(
                    'genome_id',
                    label = '(or) Select Genome',
                    choices = c('Custome', 'mm10', 'mm9', 'hg19', 'hg38')
                ),
                bsTooltip('genome_id',
                          'Alternative built-in genomes.',
                          'top', options = list(container = "body"))
            )
        ),
        # Output tabs ----
        # 1. Peaks, the uploaded file
        # 2. Expression, the uploaded file
        # 3. Genome, the uploaded file or the built in option
        # 4. Associated peaks, the ouput of calling associated_peaks
        # 5. Direct targets, the output of calling direct_targets
        # 6. Prediction plot, the output of calling plot_predictions
        tabsetPanel(
            # Peaks, the uploaded file
            tabPanel(
                'Peaks',
                tags$br(),
                tags$head(
                    tags$style(
                        type="text/css",
                        "#inline label{ display: table-cell; text-align: center; vertical-align: middle; }
                        #inline .form-group { display: table-row;}")
                ),
                tags$div(
                    id = 'inline',
                    checkboxGroupInput(
                        'peak_columns',
                        label = 'Show Columns:  ',
                        choices = 'All',
                        selected = 'All',
                        inline = TRUE
                    )
                ),
                tags$br(),
                dataTableOutput('peaks_tab')),
            # Expression, the uploaded file
            tabPanel(
                'Expression',
                tags$br(),
                tags$div(
                    id = 'inline',
                    checkboxGroupInput(
                        'expression_columns',
                        label = 'Show Columns:  ',
                        choices = 'All',
                        selected = 'All',
                        inline = TRUE
                    )
                ),
                tags$br(),
                dataTableOutput('expression_tab')),
            # Genome, the uploaded file or the built in option
            tabPanel(
                'Genome',
                tags$br(),
                tags$div(
                    id = 'inline',
                    checkboxGroupInput(
                        'genome_columns',
                        label = 'Show Columns:  ',
                        choices = 'All',
                        selected = 'All',
                        inline = TRUE
                    )
                ),
                tags$br(),
                 dataTableOutput('genome_tab')),
            # Associated peaks, and a download button
            tabPanel(
                'Associated Peaks',
                tags$br(),
                dataTableOutput('ap'),
                tags$br(),
                downloadButton('download_ap')),
            # Direct targets, and a download button
            tabPanel(
                'Direct Targets',
                tags$br(),
                dataTableOutput('dt'),
                tags$br(),
                downloadButton('download_dt')),
            # Prediction plot
            # Select plot parameters
            # Show the plot function output
            # Export the plot to file
            tabPanel(
                'Prediction Plot',
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            'plot_rank',
                            'Rank Column',
                            choices = 'Pending',
                            selected = 'score_rank'),
                        tags$hr(),
                        selectInput(
                            'plot_group',
                            'Grouping Column',
                            choices = 'Pending',
                            selected = 'stat'),
                        textInput(
                            'plot_breaks',
                            'Breaks',
                            value = '-100,-.5,.5,100'),
                        textInput(
                            'plot_colors',
                            'Colors',
                            value = 'green,gray,red'),
                        textInput(
                            'plot_labels',
                            'Labels',
                            value = 'down,none,up'),
                        tags$hr(),
                        textInput(
                            'plot_main',
                            'Main Title',
                            value = 'Predicted Function'),
                        textInput(
                            'plot_xlab',
                            'x-axis label',
                            value = 'Regulatory Potential'),
                        textInput(
                            'plot_ylab',
                            'y-axis label',
                            value = 'ECDF')
                    ),
                    mainPanel(
                        plotOutput('plot'),
                        tags$br(),
                        downloadButton('download_plot')
                    )
                )
            )
        )
    ),
    # Tutorial Page
    tabPanel(
        'Tutorial',
#        includeMarkdown('./tutorial/tutorial.md')
         tags$h1('Tutorial')
    )
)


# define server  --------------------------------------------------------------
server <- function(input, output, session) {
    # Input tabs ----
    # 1. peaks
    # 2. expression
    # 3. genome

    # load peaks file
    peaks <- reactive({
        if (is.null(input$peaks)) {
            return(NULL)
        } else {
            read_tsv(input$peaks$datapath)
        }
    })

    peak_cols <- reactive({
        if(!is.null(input$peaks)) {
            names(peaks())
        } else {
            return(NULL)
        }
    })

    # load expression file
    expression <- reactive({
        if (is.null(input$expression)) {
            return(NULL)
        } else {
            read_tsv(input$expression$datapath)
        }
    })

    # get column names of the expression files
    expression_cols <- reactive({
        if(!is.null(input$expression)) {
            names(expression())
        } else {
            'Pending'
        }
    })

    # load genome file, if custome
    # or else, load genome from txdb

    # define helper function
    # takes, txdb name and distance
    # returns, data.frame of distances within transcripts of txdb
    get_regions <- function(txdb, distance) {
        # get columns in txdb
        txdb_columns <- columns(txdb)

        # get transcripts
        trans <- transcripts(txdb, columns = txdb_columns)

        # get regions
        as.data.frame(
            promoters(trans,
                      downstream = distance * 1000,
                      upstream = distance * 1000)
        )
    }
    genome <- reactive({
        switch(input$genome_id,
               'mm10' = {get_regions(TxDb.Mmusculus.UCSC.mm10.knownGene, input$distance)},
               'mm9' = {get_regions(TxDb.Mmusculus.UCSC.mm9.knownGene, input$distance)},
               'hg19' = {get_regions(TxDb.Hsapiens.UCSC.hg19.knownGene, input$distance)},
               'hg38' = {get_regions(TxDb.Hsapiens.UCSC.hg38.knownGene, input$distance)},
               'Custome' = {
                   if (is.null(input$genome)) {
                       return(NULL)
                   } else {
                       # load genome file
                       trans <- read_tsv(input$genome$datapath)

                       # get regions
                       as.data.frame(
                           promoters(makeGRangesFromDataFrame(trans, keep.extra.columns = TRUE),
                                     upstream = input$distance * 1000,
                                     downstream = input$distance * 1000)
                       )
                   }
               })
    })

    # get column names of genome file
    genome_cols <- reactive({
        if (!is.null(genome())) {
            names(genome())
        } else {
            'Pending'
        }
    })

    # observers for choosing columns names
    observe({
        updateSelectInput(session,
                          'peak_id_col',
                          choices = peak_cols())
    })

    observe({
        updateSelectInput(session,
                          'region_id',
                          choices = expression_cols())
    })

    observe({
        updateSelectInput(session,
                          'stat_id',
                          choices = expression_cols())
    })

    observe({
        updateSelectInput(session,
                          'stat_id2',
                          choices = expression_cols())
    })

    observe({
        updateSelectInput(session,
                          'genome_id_col',
                          choices = genome_cols())
    })

    observe({
        updateCheckboxGroupInput(session,
                                 'peak_columns',
                                 choices = c('All', names(peaks())),
                                 selected = 'All',
                                 inline = TRUE)
    })

    observe({
        updateCheckboxGroupInput(session,
                                 'expression_columns',
                                 choices = c('All', names(expression())),
                                 selected = 'All',
                                 inline = TRUE)
    })

    observe({
        updateCheckboxGroupInput(session,
                                 'genome_columns',
                                 choices = c('All', names(genome())),
                                 selected = 'All',
                                 inline = TRUE)
    })

    # render tabs ----
    ## peaks table
    filtered_peaks <- reactive({
        if (is.null(input$peak_columns)) {
            return(NULL)
        } else if (input$peak_columns == 'All') {
            peaks()
        } else {
            peaks()[, input$peak_columns]
        }
    })

    output$peaks_tab <- renderDataTable({
        validate(
            need(filtered_peaks(),
                 'Pleas upload peaks file to show here.')
        )
        filtered_peaks()
    })

    # expression table
    filtered_expression <- reactive({
        if (is.null(input$expression_columns)) {
            return(NULL)
        } else if (input$expression_columns == 'All') {
            expression()
        } else {
            expression()[, input$expression_columns]
        }
    })

    output$expression_tab <- renderDataTable({
        validate(
            need(filtered_expression(),
                'Pleas upload expression file to show here.')
        )
        filtered_expression()
    })

    # genome table
    filtered_genome <- reactive({
        if (is.null(input$genome_columns)) {
            return(NULL)
        } else if (input$genome_columns == 'All') {
            genome()
        } else {
            genome()[, input$genome_columns]
        }
    })

    output$genome_tab <- renderDataTable({
        validate(
            need(filtered_genome(),
                 'Pleas select or upload a custome genome to show here.')
        )
        filtered_genome()
    })

    ## merge genome and regions ----
    expression_genome <- reactive({
        if (is.null(genome()) | is.null(expression())) {
            return(NULL)
        } else {
           try(
               merge(x = expression(),
                     y = genome(),
                     by.y = input$genome_id_col,
                     by.x = input$region_id)
           )
        }
    })

    ## get associated peaks ----
    ap <- reactive({
        if (is.null(input$peaks) | is.null(input$expression) | is.null(input$genome)) {
            return(NULL)
        } else {
            peaks_gr <- makeGRangesFromDataFrame(peaks(), keep.extra.columns = TRUE)
            regions_gr <- makeGRangesFromDataFrame(expression_genome(), keep.extra.columns = TRUE)

            try(
                associated_peaks(peaks_gr,
                                 regions_gr,
                                 input$region_id,
                                 base = input$distance * 1000)
            )
        }
    })

    ## render associated peaks tab
    output$ap <- renderDataTable({
        expression_genome()
        validate(
            need(ap(), 'Please upload files to show associated peaks here.')
        )

        if (is.null(input$peaks) | is.null(input$expression) | is.null(input$genome)) {
            return(NULL)
        } else {
            as.data.frame(ap())
        }
    })

    ## get direct targets ----
    dt <- reactive({
        if (is.null(peaks()) | is.null(expression_genome())) {
            return(NULL)
        } else {
            if (input$type == 'Two') {
                stat <- c(input$stat_id, input$stat_id2)
            } else {
                stat <- input$stat_id
            }
                tryCatch({
                    direct_targets(peaks = makeGRangesFromDataFrame(peaks(), keep.extra.columns = TRUE),
                                   regions = makeGRangesFromDataFrame(expression_genome(), keep.extra.columns = TRUE),
                                   regions_col = input$region_id,
                                   stats_col = stat,
                                   base = input$distance * 1000)
                }, error = function(err) print('Error'))
        }
    })

    ## render direct targets tab
    output$dt <- renderDataTable({
        validate(
            need(dt(), 'Please upload files to show direct targets here.')
        )
        if (is.null(input$peaks) | is.null(input$expression) | is.null(input$genome)) {
            return(NULL)
        } else {
            as.data.frame(dt())
        }
    })

    ## plot tab ----
    dt_cols <- reactive({
        names(as.data.frame(dt()))
    })

    # observer for the column names to use in plot
    observe({
        updateSelectInput(session, 'plot_rank', choices = dt_cols())
        updateSelectInput(session, 'plot_group', choices = dt_cols())
    })

    # render plot
    make_plot <- reactive({
        if (is.null(dt())) {
            return(NULL)
        } else {
            # get direct_target data.frame
            dt <- as.data.frame(dt())

            # unpack selected columns of direct target
            rank <- unlist(dt[, input$plot_rank])
            group <- unlist(dt[, input$plot_group])
            breaks <- unlist(strsplit(input$plot_breaks, ','))
            colors <- unlist(strsplit(input$plot_colors, ','))
            labels <- unlist(strsplit(input$plot_labels, ','))

            # make a group factor
            fac <- cut(group,
                       breaks = breaks,
                       labels = labels)

            # plot axes
            xlab <- input$plot_xlab
            ylab <- input$plot_ylab
            main <- input$plot_main

            # make plot
            plot_predictions(rank,
                             fac,
                             colors,
                             labels,
                             xlab = xlab,
                             ylab = ylab,
                             main = main)
        }
    })

    output$plot <- renderPlot({
        validate(
            need(dt(),
                 'Please upload files and choose prameters to show the graph here.')
        )

        make_plot()
    })

    # download buttons
    # 1. associated peaks
    # 2. direct targets
    # 3. plot (missing)

    # download associated peaks
    output$download_ap <- downloadHandler(
        filename = function() format(Sys.time(), 'ap_%y.%m.%d_%H.%M.%S.tsv'),
        content = function(con) {
            write.table(as.data.frame(ap()),
                        con,
                        sep = '\t',
                        row.names = FALSE,
                        quote = FALSE)
        }
    )

    # download direct tragets
    output$download_dt <- downloadHandler(
        filename = function() {
            format(Sys.time(), 'dt_%y.%m.%d_%H.%M.%S.tsv')
        },
        content = function(con) {
            write.table(as.data.frame(dt()),
                        con,
                        sep = '\t',
                        row.names = FALSE,
                        quote = FALSE)
        }
    )

    # download plot
    output$download_plot <- downloadHandler(
        filename = function() {
            format(Sys.time(), 'plot_%y.%m.%d_%H.%M.%S.png')
        },
        content = function(file) {
            png(file)
            make_plot()
            dev.off()
        }
    )
}

# run the app  ----------------------------------------------------------------
shinyApp(ui = ui, server = server)
