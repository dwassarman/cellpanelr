#' Help button
#'
#' @param id inputId
#' @param ns ns function from module
#'
#' @return actionButton with the given inputId and formatted to be extra small
#' with a bold question mark inside
helpButton <- function(id, ns = NULL) {
  if (is.null(ns)) {
    actionButton(id, strong("?"), class = "btn-xs")
  } else {
    actionButton(ns(id), strong("?"), class = "btn-xs")
  }
}

upload_help_message <- function() {
  showModal(modalDialog(
    easyClose = TRUE,
    size = "l",
    footer = NULL,
    tagList(
      h4("Your data should look something like this:"),
      tags$img(src = "www/upload_example.jpeg", width = "50%"),
      br(),
      br(),
      p(strong("Each row is a different cell line."), "If there are multiple
        rows with the same cell line, the rows will be averaged for analysis."),
      p(strong("One column contains cell line names."), "These names will be
        matched to cell lines in the analysis data sets. Names do not need to
        match perfectly. Matching is done using case-insensitive alpha-numeric
        characters (mcf7 = MCF7 = MCF-7)."),
      p(strong("One column contains response values."), "Response values are
        what will be correlated with other data sets in the Analysis tab. The
        file can contain more than one response column, but only one can be
        selected at a time for analysis. Response values are often a
        measurement you have collected from each cell line (e.g. fluorescence,
        IC50, etc.). All the entries in this column must be numbers. Do not
        include any units or other text outside of the header."),
      p(strong("Columns have headers."), "Otherwise the first row of data will
         be interpreted as the column headers"),
      p(strong("Data is rectangular."), "There are no extra rows above or
        below the data with things like a title or notes. Missing values
        within the data are okay and will be filtered out during analysis.")
    )
  ))
}

dataset_help_message <- function() {
  showModal(modalDialog(
    easyClose = TRUE,
    size = "l",
    footer = NULL,
    tagList(
      h4("Cell line annotations"),
      p("Information about each cell line (e.g. tissue lineage, age and sex
        of donor, and whether sample was metastatic). Plot your response data
        against these annotations to identify trends and correlations."),
      br(),
      h4("Gene expression"),
      p("mRNA levels of 19,537 protein-coding genes determined by RNAseq. Identify
        genes whose expression levels correlate with your response data."),
      br(),
      h4("Mutations"),
      p("Point mutations and indels found in gene regions for each cell line.
        All mutations and indels to the same gene are grouped together for this
        analysis. Identify genes where mutations are associated with an increase
        or decrease in your response data.")
    )
  ))
}

no_upload_error <- function() {
  showModal(modalDialog(
    easyClose = TRUE,
    footer = NULL,
    tagList(
      h4("Please upload data in the Upload tab.")
    )
  ))
}
