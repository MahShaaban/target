# load required libraries
require(shiny)
require(shinyBS)
require(DT)
source('modules.R')

# define ui
ui <- navbarPage(
    title = 'target',
    tabPanel(
        title = 'main',
        splitLayout(
            wellPanel(
               'Binding Data',
               'Peaks file from ChIP experiment.',
               tsvFileInput(
                   id = 'peaks_file',
                   demo = 'sim_peaks.tsv',
                   tip = 'Tab separated text file with at least three columns.'
               ),
               columnSelectInput(
                   id = 'peaks_name_column',
                   label = 'Name Column',
                   default = 'peak_name',
                   tip = 'Name of the column in the uploaded file that contains peak names.'
               ),
               numberSliderInput(
                   id = 'peaks_distance',
                   label = 'Distance',
                   min = 0,
                   max = 500,
                   value = 100,
                   tip = 'A number of the distances in kb.'
               )
            ),
            wellPanel(
                'Expression Data',
                'Statistics from purturbed expression.',
                tsvFileInput(
                    id = 'expression_file',
                    demo = 'sim_transcripts.tsv',
                    tip = 'Tab separated text file with at least three columns.'
                ),
                columnSelectInput(
                    id = 'expression_name_column',
                    label = 'Name Column',
                    default = 'tx_id',
                    tip = 'Name of the column in the uploaded file that contains region IDs.'
                ),
                characterRadioInput(
                    id = 'expression_factor_number',
                    label = 'Number of Factors',
                    choices = c('One', 'Two'),
                    selected = 'Two',
                    tip = 'The number of factors.'
                ),
                columnSelectInput(
                    id = 'expression_stat_column1',
                    label = 'Statistics Column',
                    default = 'stat1',
                    tip = 'Name of the column in the uploaded file that contains region statistics.'
                ),
                columnSelectInput(
                    id = 'expression_stat_column2',
                    label = 'Statistics Column (second factor)',
                    default = 'stat2',
                    tip = 'Name of the column in the uploaded file that contains region statistics.'
                )
            ),
            wellPanel(
                'Reference Genome',
                'Genomic coordinates of the regions.',
                tsvFileInput(
                    id = 'genome_file',
                    demo = 'sim_genome.tsv',
                    tip = 'Tab separated text file with at least four columns.'
                ),
                columnSelectInput(
                    id = 'genome_name_column',
                    label = 'Name Column',
                    default = 'tx_id',
                    tip = 'Name of the column in the uploaded file that contains region IDs.'
                ),
                selectInput(
                    inputId = 'genome_type',
                    label = 'Genom Type',
                    choices = c('Custome', 'mm9', 'mm10', 'hg19', 'hg38'),
                    selected = 'Custome'
                )
            )
        )
    ),
    tabsetPanel(
        tabPanel(
            title = 'Peaks',
            dataTableOutput('peaks_table')
        ),
        tabPanel(
            title = 'Expression',
            dataTableOutput('expression_table')
        ),
        tabPanel(
            title = 'Genome',
            dataTableOutput('genome_table')
        ),
        tabPanel(
            title = 'Associated Peaks',
            dataTableOutput('associated_peaks')
        ),
        tabPanel(
            title = 'Direct Targets',
            dataTableOutput('direct_targets')
        ),
        tabPanel(
            title = 'Prediction Plot',
            sidebarLayout(
                sidebarPanel(
                    'Plot Parameters',
                    predictionVariablesInput(
                        id = 'plot',
                        return = 'plot'
                    )
                ),
                mainPanel(
                    plotOutput('predicted_plot'),
                    downloadButton(
                        'download_plot',
                        label = 'Download'
                    )
                )
            )
        ),
        tabPanel(
            title = 'Prediction Testing',
            sidebarLayout(
                sidebarPanel(
                    'Testing Parameters',
                    predictionVariablesInput(
                        id = 'test',
                        return = 'test'
                    )
                ),
                mainPanel(
                    dataTableOutput('predicted_testing'),
                    downloadButton(
                        'download_test',
                        label = 'Download'
                    )
                )
            )
        )
    ),
    tabPanel(
        title = 'Tutorial',
        includeHTML('./tutorial/tutorial.md')
    ),
    tabPanel(
        title = 'Contact Us'
    )
)
