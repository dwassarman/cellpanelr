# resources_tab <- tagList(
#   strong("Data:"), br(),
#   a("DepMap - Broad Institute", href = "https://depmap.org/portal/"), br(),
#   a("depmap R-pkg", href = "https://github.com/UCLouvain-CBIO/depmap"), br(),
#   hr(),
#   strong("R and shiny references:"), br(),
#   a("\"R for Data Science\" by Hadley Wickham and Garrett Grolemund"), br(),
#   a("\"Mastering Shiny\" by Hadley Wickham", href = "https://mastering-shiny.org"), br(),
#   a("\"Engineering Production-Grade Shiny Apps\" by Colin Fay, S\u00E9bastien Rochette, Vincent Guyader and Cervan Girard.",
#     href = "https://engineering-shiny.org"
#   ), br(),
#   a("\"R packages\" by Hadley Wickham and Jenny Bryan", href = "https://r-pkgs.org"),
#   hr(),
#   strong("Thank you:"),
#   br("Taia Wu")
# )

# datasets_tab <- function() {
#   renderUI(
#     DT::renderDT(DT::datatable(
#       data_annotations()
#     )) %>% shinycssloaders::withSpinner()
#   )
# }

about_tab <- function() {
  tagList(
    titlePanel("About cellpanelr"),
    p(
      "cellpanelr is a Shiny app and R package for analyzing response data
      collected from panels of cell lines. It uses data sets from DepMap to
      identify biomarkers that correlate with user-provided data. Its goal is
      to make -omics level data analysis accessible and open-source for everyone."
    ),
    p(
      "It is licensed under the",
      gpl3_link()
    ),
    p("Data sets were adapted from", depmap_link(), "and are used under the", cc_by_link()),
    hr(),
    # p(
    #   "Please cite the following publication if you use cellpanelr in your work.",
    #   br(),
    #   "text, link",
    # ),
    p(
      "Source code is available at",
      enurl("https://github.com/dwassarman/cellpanelr", "https://github.com/dwassarman/cellpanelr"),
    ),
    # p(
    #   "R package is available from CRAN at",
    #   "link",
    # ),
    p(
      "For support and bug reports please contact",
      enurl("mailto:cellpanelr@gmail.com?", "cellpanelr@gmail.com")
    )
  )
}

depmap_link <- function() {
  enurl("https://depmap.org/portal/", "DepMap (Broad Institute)")
}

cc_by_link <- function() {
  enurl("https://creativecommons.org/licenses/by/4.0/", "CC BY 4.0 License")
}

gpl3_link <- function() {
  enurl("https://www.gnu.org/licenses/gpl-3.0.en.html", "GNU General Public License 3.0")
}
