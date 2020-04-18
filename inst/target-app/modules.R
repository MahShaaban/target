#' File input user interface
#'
#' @param id A character, to specifiy the name space.
#' @param tip A string, to describe the module function.
#' @param demo A string, default link to a demo file.
#'
#' @return A tagList containing UI elements.
tsvFileInput <- function(id, tip, demo) {
    # declare name space
    ns <- NS(id)
    
    # make ui
    tagList(
        fileInput(
            ns("file"),
            label = 'Upload File',
            accept = 'tsv',
            placeholder = demo
        ),
        bsTooltip(
            ns("file"),
            tip,
            'top',
            options = list(container = "body")
        )
    )
}

#' File input server processing
#'
#' @param input, output, session Shiny stuff 
#' @inheritParams tsvFileInput
#'
#' @return A data.frame
tsvFile <- function(input, ouput, session, demo) {
     reactive({
        validate(need(input$file, message = input$file$datapath))
        
         read_tsv(
            ifelse(
                is.null(input$file$datapath),
                demo,
                input$file$datapath
            )
        )
    })
}

#' Column selection interface
#'
#' @inheritParams tsvFileInput
#' @param label A string, to use as a label for the input field.
#' @param default A string, to indicate the default choice for the demo.
#'
#' @return A tagList containing UI elements
columnSelectInput <- function(id, label, default, tip) {
    # declare name space
    ns <- NS(id)
    
    # make ui
    tagList(
        selectInput(
            ns('column'),
            label = label,
            choices = default,
            selected = default
        ),
        bsTooltip(
            ns("column"),
            tip,
            'top',
            options = list(container = "body")
        )
    )
}

#' Column selection server processing
#'
#' @inheritParams tsvFile
#' @param table A data.frame to select a column name from.
#'
#' @return A reactive character string.
columnSelect <- function(input, output, session, path) {
    column_names <- names(read_tsv(path, n_max = 1))
    observe(
        updateSelectInput(
            inputId = input$column,
            choices = column_names,
            session = session
        )
    )
    return(
        reactive({ input$column })
    )
}

#' Slider selection interface
#'
#' @inheritParams tsvFileInput
#' @inheritParams columSelectionInput
#' @param min An integer
#' @param max An integer
#' @param value An integer
#' 
#' @return A tagList containing UI elements
numberSliderInput <- function(id, label, min, max, value, tip) {
    # declare name space
    ns <- NS(id)
    
    # make ui
    tagList(
        sliderInput(
            inputId = ns("slider"),
            label = label,
            min = min,
            max = max,
            value = value
        ),
        bsTooltip(
            ns("slider"),
            tip,
            'top',
            options = list(container = "body")
        )
    )
}

#' Slider selection server processing
#'
#' @inheritParams tsvFile
#'
#' @return A reactive character string.
numberSlider <- function(input, output, session) {
    return(
        reactive({
            input$slider
        })
    )
}

#' Radio buttons selection interface
#'
#' @inheritParams tsvFileInput
#' @inheritParams columSelectionInput
#' @param choices A character vector
#' @param selected A character string
#' 
#' @return A tagList containing UI elements
characterRadioInput <- function(id, label, choices, selected, tip) {
    # declare name space
    ns <- NS(id)

    # make ui
    tagList(
        radioButtons(
            inputId = ns("radio"),
            label = label,
            choices = choices,
            selected = selected,
            inline = TRUE
        ),
        bsTooltip(
            ns("radio"),
            tip,
            'top',
            options = list(container = "body")
        )
    )
}

#' Radio buttons selection server processing
#'
#' @inheritParams tsvFile
#'
#' @return A reactive character string.
characterRadio <- function(input, output, session) {
    return(
        reactive({
            input$radio
        })
    )
}

#' Input plotting variables and parameters interface
#'
#' @inheritParams tsvFileInput
#' @param return A character string, default 'plot'.
#'
#' @return A tagList containing UI elements
predictionVariablesInput <- function(id, return = 'plot') {
    # declare name space
    ns <- NS(id)
    
    # make ui
    ## shared part
    shared <- tagList(
        selectInput(
            inputId = ns('ranking'),
            label = 'Ranking variable',
            choices = 'score_rank',
            selected = 'score_rank'
        ),
        selectInput(
            inputId = ns('grouping'),
            label = 'Ranking variable',
            choices = 'Grouping Variable',
            selected = 'stat'
        ),
        radioButtons(
            inputId = ns('groupby'),
            label = 'Group By',
            choices = c('top', 'quantiles', 'values'),
            selected = 'top',
            inline = TRUE
        ),
        textInput(
            inputId = ns('group_breaks'),
            label = 'Breaks',
            value = '100'
        ),
        textInput(
            inputId = ns('group_labels'),
            label = 'Colors',
            value = 'comp,none,coop'  
            )
    )
    
    # for plot
    plot <- tagList(
        textInput(
            inputId = ns('group_colors'),
            label = 'Colors',
            value = 'green,gray,red'
        ),
        textInput(
            inputId = ns('xlabel'),
            label = 'X-axis Label',
            value = 'Ranking'
        ),
        textInput(
            inputId = ns('ylabel'),
            label = 'Y-axis Label',
            value = 'ECDF'
        ),
        textInput(
            inputId = ns('mainlabel'),
            label = 'Graph Title',
            value = 'Aggregated Functions'
        )
    )
    
    # for test
    test <- tagList(
        textInput(
            inputId = ns('compare'),
            label = 'Compare',
            value = 'Coop,none'
        ),
        textInput(
            inputId = ns('alternative'),
            label = 'Test Direction',
            value = 'alternative'
        )
    )
    
    # return tag list
    switch(
        return,
        'plot' = return(tagList(shared, plot)),
        'test' = return(tagList(shared, test))
    )
}

#' Plotting variables server processing
#'
#' @inheritParams tsvFile
#' @inheritParams predictionVariablesInput
#'
#' @return A list of reactive strings
predictionVariables <- function(input, output, session, return = 'plot') {
    switch(return, 
           'plot' = {
               return(
                   list(
                       ranking = reactive(input$ranking),
                       grouping = reactive(input$grouping),
                       groupby = reactive(input$groupby),
                       group_breaks = reactive(group_breaks),
                       group_colors = reactive(input$group_colors),
                       group_labels = reactive(input$group_labels),
                       xlabel = reactive(input$xlabel),
                       ylabel = reactive(input$ylabel),
                       mainlabel = reactive(mainlabel)
                   )
               )
           },
           'test' = {
               return(
                   list(
                       ranking = reactive(input$ranking),
                       grouping = reactive(input$grouping),
                       groupby = reactive(input$groupby),
                       group_breaks = reactive(input$group_breaks),
                       group_labels = reactive(input$group_labels),
                       compare = reactive(input$compare),
                       alternative = reactive(input$alternative)
                   )
               )
           })
    
}

#' Call main target functions
#' 
#' A wrapper for calling functions from the target pacakge. 
#' Inputs are reshaped and passed to one of the two main functions in target:
#' associated peaks or direct_targets.
#' 
#' @param return A character string, 'associated_peaks' (default) or 
#' 'direct_targets'.
#' @param peaks A data.frame
#' @param expression A data.frame
#' @param genome A data.frame
#' @param expression_name A string
#' @param genome_name A string
#' @param factor_num A string
#' @param stat1 A string
#' @param stat2 A string
#' @param distance An integer
#'
#' @return A data.frame
run_target <- function(return = 'associated_peaks', peaks, expression,
                       genome, expression_name, genome_name, factor_num = NULL,
                       stat1 = NULL, stat2 = NULL, distance = NULL) {
    # get ranges from genome
    genome_ranges <- as.data.frame(
        promoters(
            makeGRangesFromDataFrame(genome, keep.extra.columns = TRUE),
            upstream = distance * 1000,
            downstream = distance * 1000
        )
    )
    
    # merge the expression and genome data.frames
    regions <- merge(y = expression,
                     x = genome_ranges,
                     by.y = expression_name,
                     by.x = genome_name)
    
    # perform the calculations
    res <- switch(
        return,
        'associated_peaks' = {
            try({
                associated_peaks(
                    peaks = makeGRangesFromDataFrame(peaks, keep.extra.columns = TRUE),
                    regions = makeGRangesFromDataFrame(regions, keep.extra.columns = TRUE),
                    regions_col = expression_name,
                    base = distance * 1000
                )
            })
        },
        'direct_targets' = {
            try({
                # combine the two selected columns names in one vector
                stats <- ifelse(factor_num == 'One', stat1, c(stat1, stat2))
                
                direct_targets(
                    peaks = makeGRangesFromDataFrame(peaks, keep.extra.columns = TRUE),
                    regions = makeGRangesFromDataFrame(regions, keep.extra.columns = TRUE),
                    regions_col = expression_name,
                    stats_col = stats,
                    base = distance * 1000
                )
            })
        }
    )
    
    return(as.data.frame(res))
}

#' Call prediction target functions
#' 
#' A wrapper for calling functions from the target pacakge. 
#' Inputs are reshaped and passed to one of the two prediction functions in 
#' target: plot_predictions or test_predictions. 
#' 
#' @param return A character string, 'associated_peaks' (default) or 
#' 'direct_targets'.
#' @param df A data.frame
#' @param params A list of parameters.
#'
#' @return A plot or a data.frame.
make_prediction <- function(df, params, return = 'plot') {
    
    # unpack the shared inputs
    rank <- subset(df, select = params$ranking, drop = TRUE)
    grouping <- subset(df, select = params$grouping, drop = TRUE)
    breaks <- unpack(params$group_breaks, as = 'numeric')
    labels <- unpack(params$group_labels)
    
    # make a grouping variables
    group <- switch(
        params$groupby,
        'quantiles' = make_groups_quantiles(grouping, breaks, labels),
        'top' = make_groups_top(grouping, breaks, labels),
        'values' = make_groups_values(grouping, breaks, labels),
    )
    
    switch(return, 
           'plot' = {
               try({
                   # unpack the colors input
                   colors <- unpack(params$group_colors)
                   
                   # call plot
                   plot_predictions(
                       rank = rank,
                       group = group,
                       labels = labels,
                       colors = colors,
                       xlab = params$xlabel,
                       ylab = params$ylabel,
                       main = params$main
                   )
               })
           },
           'test' = {
               # unpack the compare input
               compare <- unpack(params$compare)
               
               # make an index to subset to the compared groups and remove na
               ind <- group %in% compare && !is.na(group)
               
               # call test
               tidy(
                   test_predictions(
                       rank = rank[ind],
                       group = group[ind],
                       compare = compare,
                       alternative = params$alternative
               )
                   
               )
           })
}

#' Group a vector by quantiles
#'
#' @param grouping A numeric vector
#' @param breaks A numeric vector of length two
#' @param labels A character vector
#' 
#' @details breaks is the lower and upper quantiles in that order to cut
#'  the vector into a factor. 
#'
#' @return A factor
make_groups_quantiles <- function(grouping, breaks, labels) {
    stopifnot(length(breaks) == 2)
    
    vec <- cut(
        grouping,
        breaks = quantile(grouping, c(0, breaks, 1)),
        labels = labels,
        include.lowest = TRUE
    )
    
    return(factor(vec, levels = labels))
}

#' Group a vector by top n from both directions
#'
#' @inheritParams make_groups_quantiles 
#' 
#' @details breaks is the number of entries from the lower and the upper
#' directions in that order to cut the vector into a factor.
#' 
#' @return A factor
make_groups_top <- function(grouping, breaks, labels) {
    stopifnot(length(breaks) == 2)
    
    vec <- vector(mode = 'character', length = length(grouping))
    vec[order(grouping)[1:min(abs(breaks))]] <- labels[1]
    vec[order(grouping, decreasing = TRUE)[1:max(breaks)]] <- labels[length(labels)]
    if (length(labels) > 2) {
        vec[vec == ''] <- labels[2]
    } else {
        vec[vec == ''] <- NA
    }
    return(vec)
}

#' Group a vector by values
#'
#' @inheritParams make_groups_quantiles 
#' 
#' @details breaks is the break values from the vector in order from smaller
#' to bigger values to cut the vector into a factor.
#' 
#' @return A factor
make_groups_values <- function(grouping, breaks, labels) {
    stopifnot(length(breaks) == 2)
    vec <- cut(
        grouping,
        breaks = c(min(grouping), breaks, max(grouping)),
        labels = labels,
        include.lowest = TRUE
    )
    return(factor(vec, levels = labels))
}

#' Load transcripts from TxDb object
#'
#' @param type A character string: hg19, hg38, mm10 or mm9
#' @param columns A character vector. Default GENEID and TXID
#' @param tidy A logical. Default TRUE to return a data.frame
#'
#' @return A data.frame
load_genome <- function(type, columns = c('GENEID', 'TXID'), tidy = TRUE) {
    # switch to the corresponding bioconductor package
    txdb <- switch(
        type,
        'hg19' = TxDb.Hsapiens.UCSC.hg19.knownGene,
        'hg38' = TxDb.Hsapiens.UCSC.hg38.knownGene,
        'mm10' = TxDb.Mmusculus.UCSC.mm10.knownGene,
        'mm9' = TxDb.Mmusculus.UCSC.mm9.knownGene
    )
    
    # extract transcripts
    trans <- transcripts(
        txdb,
        columns = columns
    )
    
    # tidy if true
    if (tidy) {
        trans <- as.data.frame(
            trans
        )
    }
    
    # return
    return(trans)
}

unpack <- function(string, split = ',', as = 'character') {
    s <- strsplit(string, split = split)
    s <- unlist(s)
    switch(
        as,
        'character' = return(as.character(s)),
        'numeric' = return(as.numeric(s))
    )
}