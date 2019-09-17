uploadFileInput <- function(id) {
  ns <- NS(id)

  tagList(
    fileInput(
      ns('file'),
      label = 'Upload File'
    ),
    bsTooltip(
      ns('file'),
      'Tab separated text file ',
      'top',
      options = list(container = "body")
    )
  )
}

uploadFile <- function(input, output, session, names = FALSE) {
  # do nothing
  userFile <- reactive({
    validate(
      need(input$file, message = FALSE)
    )
    input$file
  })

  if (names) {
    column_names <- reactive({
      names(read_tsv(userFile()$datapath, n_max = 0))
    })

    return(column_names)
  } else {
    # read file
    dataframe <- reactive({
      read_tsv(userFile()$datapath)
    })

    # return dataframe
    return(dataframe)
  }
}

filterTableInput <- function(id) {
  ns <- NS(id)

  tagList(
    dataTableOutput(ns('tab'))
  )
}

filterTable <- function(input, output, session, dataframe, columns) {
  if (columns == 'All') {
    ind <- names(dataframe)
  } else {
    ind <- column
  }

  renderDataTable({
    dataframe[, ind]
  })
}
