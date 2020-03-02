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
                    label = 'Upload File',
                    accept = 'tsv',
                    placeholder = 'sim_peaks.tsv'),
                bsTooltip('peaks',
                          'Tab separated text file with at least three columns.',
                          'top', options = list(container = "body")),
                selectInput(
                    'peak_id_col',
                    label = 'Name Column',
                    choices = 'peak_name',
                    selected = 'peak_name'
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
                    label = 'Upload File',
                    accept = 'tsv',
                    placeholder = 'sim_transcripts.tsv'
                ),
                bsTooltip('expression',
                          'Tab separated text file with at least three columns.',
                          'top', options = list(container = "body")),
                selectInput(
                    'region_id',
                    label = 'Name Column',
                    choices = 'tx_id',
                    selected = 'tx_id'
                ),
                bsTooltip('region_id',
                          'Name of the column in the uploaded file that contains region IDs.',
                          'top', options = list(container = "body")),
                radioButtons(
                    'type',
                    label = 'Number of Factors',
                    choices = c('One', 'Two'),
                    selected = 'Two',
                    inline = TRUE
                ),
                selectInput(
                    'stat_id',
                    label = 'Statistics Column',
                    choices = 'stat1',
                    selected = 'stat1'
                ),
                bsTooltip('stat_id',
                          'Name of the column in the uploaded file that contains region statistics.',
                          'top', options = list(container = "body")),
                selectInput(
                    'stat_id2',
                    label = 'Statistics Column (second factor)',
                    choices = 'stat2',
                    selected = 'stat2'
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
                    label = 'Upload File',
                    accept = 'tsv',
                    placeholder = 'sim_genome.tsv'
                ),
                bsTooltip('genome',
                          'Tab separated text file with at least four columns.',
                          'top', options = list(container = "body")),
                selectInput(
                    'genome_id_col',
                    label = 'Name Column',
                    choices = 'tx_id',
                    selected = 'tx_id'
                ),
                bsTooltip('genome_id_col',
                          'Name of the column in the uploaded file that contains region IDs.',
                          'top', options = list(container = "body")),
                selectInput(
                    'genome_id',
                    label = '(or) Select Genome',
                    choices = c('Custome', 'mm10', 'mm9', 'hg19', 'hg38'),
                    selected = 'Custome'
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
                            choices = 'score',
                            selected = 'score'),
                        tags$hr(),
                        selectInput(
                            'plot_group',
                            'Grouping Column',
                            choices = 'stat',
                            selected = 'stat'),
                        textInput(
                            'plot_breaks',
                            'Breaks (Quantiles)',
                            value = '0,.25,.75,1'),
                        textInput(
                            'plot_colors',
                            'Colors',
                            value = 'green,gray,red'),
                        textInput(
                            'plot_labels',
                            'Labels',
                            value = 'comp,none,coop'),
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
        includeMarkdown('./tutorial/tutorial.md')
    )
)


# define server  --------------------------------------------------------------
server <- function(input, output, session) {
    # Input tabs ----
    # 1. peaks
    # 2. expression
    # 3. genome

    # define a loading function
    load_file <- function(input, demo) {
        path <- ifelse(!is.null(input), input, demo)
        read_tsv(path)
    }

    # load peaks file
    peaks <- reactive({
        load_file(input$peaks$datapath, 'sim_peaks.tsv')
    })

    # load expression file
    expression <- reactive({
        load_file(input$expression$datapath, 'sim_transcripts.tsv')
    })

    # load genome file, if custome
    # or else, load genome from txdb

    # define a helper function
    get_regions <- function(txdb) {
        transcripts(txdb, columns = columns(txdb))
    }

    genome <- reactive({
        trans <- switch(
            input$genome_id,
            'mm10' = get_regions(TxDb.Mmusculus.UCSC.mm10.knownGene),
            'mm9' = get_regions(TxDb.Mmusculus.UCSC.mm9.knownGene),
            'hg19' = get_regions(TxDb.Hsapiens.UCSC.hg19.knownGene),
            'hg38' = get_regions(TxDb.Hsapiens.UCSC.hg38.knownGene),
            'Custome' = {
                trans <- load_file(input$genome$datapath, 'sim_genome.tsv')
                makeGRangesFromDataFrame(trans,
                                         keep.extra.columns = TRUE)
            }
        )

        # get regions
        as_tibble(
            promoters(trans,
                      upstream = input$distance * 1000,
                      downstream = input$distance * 1000)
        )
    })

    # observers for choosing columns names
    observe({
        updateSelectInput(
            session,
            'peak_id_col',
            choices = names(peaks()),
            selected = 'peak_name'
        )

        updateCheckboxGroupInput(
            session,
            'peak_columns',
            choices = c('All', names(peaks())),
            selected = 'All',
            inline = TRUE
        )
    })

    observe({
        updateSelectInput(
            session,
            'region_id',
            choices = names(expression()),
            selected = 'tx_id'
        )

        updateSelectInput(
            session,
            'stat_id',
            choices = names(expression()),
            selected = 'stat1'
        )

        updateSelectInput(
            session,
            'stat_id2',
            choices = names(expression()),
            selected = 'stat2'
        )

        updateCheckboxGroupInput(
            session,
            'expression_columns',
            choices = c('All', names(expression())),
            selected = 'All',
            inline = TRUE
        )
    })

    observe({
        updateSelectInput(session,
                          'genome_id_col',
                          choices = names(genome()),
                          selected = 'tx_id')

        updateCheckboxGroupInput(
            session,
            'genome_columns',
            choices = c('All', names(genome())),
            selected = 'All',
            inline = TRUE
        )
    })

    # render tabs ----
    ## peaks table
    filtered_peaks <- reactive({
        if (is.null(input$peak_columns)) {
            return(NULL)
        } else if ('All' %in% input$peak_columns) {
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
        } else if ('All' %in% input$expression_columns) {
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
        } else if ('All' %in% input$genome_columns) {
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
    # expression_genome <- reactive({
    #     tryCatch({
    #         merge(x = expression(),
    #               y = genome(),
    #               by.y = input$genome_id_col,
    #               by.x = input$region_id)
    #     }, error = function(err) {
    #         message("expression and genome merge failed.")
    #         message(paste("expression is", nrow(expression()), "by", ncol(expression())))
    #         message(paste("genome is", nrow(genome()), "by", ncol(genome())))
    #         message(paste("merge attempted by", input$region_id, "and", input$genome_id_col))
    #     })
    # })

    ## get associated peaks ----
    ap <- reactive({
        try({
            pp <- makeGRangesFromDataFrame(peaks(),
                                           keep.extra.columns = TRUE)

            expression_genome <- merge(x = expression(),
                                       y = genome(),
                                       by.y = input$genome_id_col,
                                       by.x = input$region_id)

            rr <- makeGRangesFromDataFrame(expression_genome,
                                           keep.extra.columns = TRUE)
            associated_peaks(peaks = pp,
                             regions = rr,
                             regions_col = input$region_id,
                             base = input$distance * 1000)
        })
        # tryCatch({
        #     pp <- makeGRangesFromDataFrame(peaks(),
        #                                    keep.extra.columns = TRUE)
        #     rr <- makeGRangesFromDataFrame(expression_genome(),
        #                                    keep.extra.columns = TRUE)
        #     associated_peaks(peaks = pp,
        #                      regions = rr,
        #                      regions_col = input$region_id,
        #                      base = input$distance * 1000)
        # }, error = function(err) {
        #     message('The call to associated_peaks failed.')
        #     message(paste('peaks is a ', as.character(class(pp)),'object with length', length(pp)))
        #     message(paste('regions is a ', as.character(class(rr)),'object with length', length(rr)))
        # })
    })

    ## render associated peaks tab
    output$ap <- renderDataTable({
        validate(
            need(ap(), 'Please upload files to show associated peaks here.')
        )

        as_tibble(ap())
    })

    ## get direct targets ----
    dt <- reactive({
        if (input$type == 'Two') {
            stat <- c(input$stat_id, input$stat_id2)
        } else {
            stat <- input$stat_id
        }
        try({
            pp <- makeGRangesFromDataFrame(peaks(),
                                           keep.extra.columns = TRUE)

            expression_genome <- merge(x = expression(),
                                       y = genome(),
                                       by.y = input$genome_id_col,
                                       by.x = input$region_id)

            rr <- makeGRangesFromDataFrame(expression_genome,
                                           keep.extra.columns = TRUE)
            direct_targets(peaks = pp,
                           regions = rr,
                           regions_col = input$region_id,
                           stats_col = stat,
                           base = input$distance * 1000)
        })
        # tryCatch({
        #     pp <- makeGRangesFromDataFrame(peaks(),
        #                                    keep.extra.columns = TRUE)
        #     rr <- makeGRangesFromDataFrame(expression_genome(),
        #                                    keep.extra.columns = TRUE)
        #     direct_targets(peaks = pp,
        #                    regions = rr,
        #                    regions_col = input$region_id,
        #                    stats_col = stat,
        #                    base = input$distance * 1000)
        # }, error = function(err) {
        #     message('The call to direct_targets failed.')
        #     message(paste('peaks is a ', as.character(class(pp)),'object with length', length(pp)))
        #     message(paste('regions is a ', as.character(class(rr)),'object with length', length(rr)))
        # })
    })

    ## render direct targets tab
    output$dt <- renderDataTable({
        validate(
            need(dt(), 'Please upload files to show direct targets here.')
        )
        as_tibble(dt())
    })

    ## plot tab ----

    # observer for the column names to use in plot
    observe({
        validate(
            need(dt(), 'Please upload files to show direct targets here.')
        )
        updateSelectInput(
            session,
            'plot_rank',
            choices = names(as.data.frame(dt())),
            selected = 'score'
        )
        updateSelectInput(
            session,
            'plot_group',
            choices = names(as.data.frame(dt())),
            selected = 'stat'
        )
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
            tryCatch({
                fac <- cut(group,
                           breaks = quantile(rank, as.numeric(breaks)),
                           labels = labels)
            }, error = function(err) {
                message(paste("cutting", input$plot_group, "by", input$plot_breaks, "failed."))
            })

            # plot axes
            xlab <- input$plot_xlab
            ylab <- input$plot_ylab
            main <- input$plot_main

            # make plot
            tryCatch({
                plot_predictions(rank,
                                 fac,
                                 colors,
                                 labels,
                                 xlab = xlab,
                                 ylab = ylab,
                                 main = main)
            }, error = function(err) {
                message("the call to plot_predictions failed.")
            })
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
