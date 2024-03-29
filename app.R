library(shiny)
library(bslib)
library(bsicons)
library(eurostat)
library(DT)
library(dataset)
library(rdflib)
library(csvwr)
library(jsonlite)
library(jsonld)

# Define UI for application that draws a histogram
ui <- page_navbar(
  theme = bs_theme(preset = "bootstrap"),
  id = "nav",
  title = div(img(src="ome_logo.png", height = 40), "Eurostat statistics browser"),
  fillable = FALSE,

  sidebar = sidebar(
    accordion(
      accordion_panel(
        "Fetch API data",
        icon = bsicons::bs_icon("cloud-arrow-down"),
        textInput("dataset_id", span("Dataset ID",
                                     tooltip(
                                       bs_icon("info-circle"),
                                       "Input eurostat dataset code here",
                                       placement = "right")
                                     ), "cult_emp_sex"),
        actionButton("search_button", "Fetch data")
        ),
      accordion_panel(
        "Change download parameters",
        icon = bsicons::bs_icon("table"),
        selectInput(inputId = "time_format",
                    label = "Select time format",
                    choices = c("date", "date_last", "num", "raw"),
                    selected = "date",
                    multiple = FALSE),
        selectInput(inputId = "type",
                    label = "Select type of returned table",
                    choices = c("code", "label"),
                    selected = "code",
                    multiple = FALSE),
        selectInput(inputId = "lang",
                    label = "Select language of data labels",
                    choices = c("en", "fr", "de"),
                    selected = "en"),
        checkboxInput(inputId = "cache",
                      label = "Use caching? Default is TRUE",
                      value = TRUE),
        checkboxInput(inputId = "update_cache",
                      label = "a logical whether to do caching, default is TRUE",
                      value = TRUE),
        # checkboxInput(inputId = "stringsAsFactors",
        #               label = "Convert variables to factors, default is FALSE",
        #               value = FALSE),
        checkboxInput(inputId = "keepFlags",
                      label = "a logical whether to keep flags (e.g. 'confidential', 'provisional'), default is FALSE",
                      value = FALSE)
        )
      )
    # conditionalPanel(
    #   "input.nav === 'Format: Dataset'",
    #   icon = bsicons::bs_icon("file-earmark-code"),
    #   selectInput(inputId = "selected_metadata_standard",
    #               label = "Metadata implementation",
    #               choices = c("dataset", "datacite", "dublincore"),
    #               selected = "dataset",
    #               multiple = FALSE)
    #   )
    ),

  nav_spacer(),

  nav_panel("Table",
            card(
              card_header("Statistical data table"),
              card_body(
                DT::dataTableOutput("statistics_table_view")
              )
            ),
            card(
              card_header("Citation info"),
              card_body(
                verbatimTextOutput("data_citation")
              )
            )
          ),
  nav_panel("Format: Dataset",
            card(
              card_header("Dataset data (standard metadata): Preview"),
              card_body(
                verbatimTextOutput("format_dataset")
              ),
              card_footer(
                downloadButton('download_dataset_rds', "Download dataset as .rds file")
              )
            ),
            card(
              card_header("Datacite metadata: Preview"),
              card_body(
                verbatimTextOutput("format_dataset_datacite")
              ),
              card_footer(
                downloadButton('download_dataset_datacite_rds', "Download metadata as .txt (datacite)")
              )
            ),
            card(
              card_header("Dublincore metadata: Preview"),
              card_body(
                verbatimTextOutput("format_dataset_dublincore")
              ),
              card_footer(
                downloadButton('download_dataset_dublincore_rds', "Download metadata as .txt (dublincore)")
              )
            )
            ),
  nav_panel("Format: TTL",
            card(
              card_header("TTL data: Preview"),
              card_body(
                verbatimTextOutput("ttl_dataset")
              ),
              card_footer(
                downloadButton('download_dataset_ttl', "Download dataset as .ttl file")
              )
            )
          ),
  nav_panel("Format: RDF",
            card(
              card_header("RDF data: Preview"),
              card_body(
                verbatimTextOutput("rdf_dataset")
              ),
              card_footer(
                downloadButton('download_dataset_rdf', "Download dataset as RDF/.ttl file")
              )
            )
            ),
  nav_panel("Format: JSON-LD",
            card(
              card_header("JSON-LD data: Preview"),
              card_body(
                verbatimTextOutput("json_ld_dataset")
              ),
              card_footer(
                 downloadButton('download_dataset_json_ld', "Download dataset as JSON-LD file")
               )
            )
            ),
  nav_panel("Format: CSVW",
            card(
              card_header("CSVW JSON metadata: Preview"),
              card_body(
                verbatimTextOutput("csvw_json")
              ),
              card_footer(
                downloadButton('download_csvw_json', "Download dataset metadata as CSVW JSON")
              )
            )
            ),
  nav_spacer(),
  nav_panel("About",
            card(
              card_header("About this Shiny app"),
              card_body(
                HTML(
  "This project has received funding from the European Union’s Horizon Europe,
  research and innovation programme, under Grant Agreement No.101095295.
  <br><br>
  Any dissemination of results must indicate that it reflects only the author’s
  view and that the Commission Agency is not responsible for any use that may
  be made of the information it contains.
  <br><br>
  The Commission Agency is not responsible for any use that may be made of
  the information it contains. Neither Project Coordinator, nor any signatory
  party of OpenMusE Project Consortium Agreement, nor any person acting on
  behalf of any of them:
  <ul>
  <li>(a) makes any warranty or representation whatsoever, express or implied,</li>
  <ul>
    (i). with respect to the use of any information, apparatus, method, process,
    or similar item disclosed in this document, including merchantability and
    fitness for a particular purpose, or
  </li>
  <li>
    (ii). that such use does not infringe on or interfere with privately owned rights,
    including any party's intellectual property, or
  </li>
  <li>
    (iii). that this document is suitable to any particular user's circumstance; or
  </li>
  </ul>
  <li>(b) assumes responsibility for any damages or other liability whatsoever
  (including any consequential damages, even if Project Coordinator or any
  representative of a signatory party of the OpenMusE Project Consortium
  Agreement, has been advised of the possibility of such damages)
  resulting from your selection or use of this document or any information,
  apparatus, method, process, or similar item disclosed in this document.
  </li>
  </ul>
  <br>
  <img src='eu_funded_en.jpg'>
  <br>
  Funded by the European Union. Views and opinions expressed are however
  those of the author(s) only and do not necessarily reflect those of the
  European Union or the European Research Executive Agency.
  Neither the European Union nor the granting authority can be held
  responsible for them.
  <br>
  <h2>Source code</h2>
  Source code for this Shiny app is freely available on GitHub: "),
  a("https://github.com/pitkant/stats_shiny", href = "https://github.com/pitkant/stats_shiny", target = "_blank")
                )
              )
            ),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light"), align = "right"
  )
  )




# Define server logic required to draw a histogram
server <- function(input, output) {

  v <- reactiveValues(data = NULL)

  dataset_id_reactive <- eventReactive(input$search_button, {
    input$dataset_id
  })

  observeEvent(input$search_button, {

    withProgress(message = "Progress", value = 0, {

      n <- 8
      incProgress(1/n, detail = "Downloading data from API")

      v$result <- eurostat::get_eurostat(
        id = input$dataset_id,
        time_format = input$time_format,
        type = input$type,
        lang = input$lang,
        cache = input$cache,
        update_cache = input$update_cache,
        stringsAsFactors = FALSE,
        keepFlags = input$keepFlags)

      incProgress(1/n, detail = "Fetching citation metadata")

      v$citation <- eurostat::get_bibentry(
        code = input$dataset_id,
        lang = input$lang
      )

      incProgress(1/n, detail = "Extracting metadata")

      v$metadata <- eurostat:::extract_metadata(agency = "Eurostat", id = input$dataset_id)

      selected_lang_name <- switch(
        input$lang,
        "en" = v$metadata$Name_EN,
        "fr" = v$metadata$Name_FR,
        "de" = v$metadata$Name_DE
      )

      # Dataset object ####
      ## REFERENCE START
      ## dataset package article by Daniel Antal used as reference material when writing lines 269:277 below: https://dataset.dataobservatory.eu/articles/dataset.html
      v$output_dataset <- dataset::dataset(v$result,
                                           title = selected_lang_name,
                                           author = person(given = "Eurostat",
                                                           role = "aut"),
                                           year = as.integer(substr(v$metadata$UpdateDataTimestamp, 1, 4)),
                                           publisher = "Eurostat",
                                           identifier = v$metadata$DOI_URL,
                                           version = v$metadata$Version,
                                           resourceType = "Dataset")
      ## REFERENCE END

      # Datacite metadata print ####
      # Use dataset metadata as basis
      ## REFERENCE START
      ## dataset package article by Daniel Antal used as reference material when writing line 284 below: https://dataset.dataobservatory.eu/articles/metadata.html
      v$output_dataset_datacite <- dataset::as_datacite(v$output_dataset, "list")
      ## REFERENCE END
      # Dublincore metadata print ####
      # Use dataset metadata as basis
      ## REFERENCE START
      ## dataset package article by Daniel Antal used as reference material when writing line 290 below: https://dataset.dataobservatory.eu/articles/metadata.html
      v$output_dataset_dublincore <- dataset::as_dublincore(v$output_dataset, "list")
      ## REFERENCE END

      incProgress(1/n, detail = "Creating TTL object")

      # TTL file ####
      ## REFERENCE START
      ## dataset package article by Daniel Antal used as reference material / code lines partially adapted when writing lines 298:320 below: https://dataset.dataobservatory.eu/articles/dataset.html
      v$dataset_ttl_file <- file.path(tempdir(), paste0(input$dataset_id, ".ttl"))

      v$output_dataset_namespace_ttl <- dataset::dataset_namespace[
        dataset_namespace$prefix %in% c("owl:", "rdf:", "rdfs:", "qb:", "eg:"), ]

      v$output_dataset_ttl <- dataset::id_to_column(
        x = v$output_dataset,
        prefix = "eg:",
        ids = NULL)

      v$output_dataset_ttl <- dataset::dataset_to_triples(
        x = v$output_dataset_ttl,
        idcol = "rowid"
      )

      v$output_dataset_ttl$p <- paste0("eg:", input$dataset_id, "#", v$output_dataset_ttl$p)
      v$output_dataset_ttl$o <- dataset::xsd_convert(v$output_dataset_ttl$o)

      dataset::dataset_ttl_write(
        tdf = v$output_dataset_ttl,
        ttl_namespace = v$output_dataset_namespace_ttl,
        file_path = v$dataset_ttl_file
      )
      ## REFERENCE END

      # RDF file ####

      incProgress(1/n, detail = "Creating RDF object")

      ## REFERENCE START
      ## dataset package article by Daniel Antal used as reference material when writing lines 330:356 below: https://dataset.dataobservatory.eu/articles/RDF.html

      v$output_dataset_rdf <- as.data.frame(v$output_dataset)
      rownames(v$output_dataset_rdf) <- paste0(input$dataset_id, ":o", rownames(v$output_dataset))
      v$output_dataset_rdf <- dataset_to_triples(xsd_convert(v$output_dataset_rdf))

      v$output_dataset_namespace_rdf <- which(dataset_namespace$prefix %in% c(
        "owl:", "rdf:", "rdfs:", "qb:", "xsd:")
      )
      v$output_dataset_namespace_rdf <- rbind(
        dataset_namespace[v$output_dataset_namespace_rdf, ],
        data.frame(
          prefix = paste0(input$dataset_id, ":"),
          uri = paste0("<www.example.com/", input$dataset_id, "#>"))
      )

      v$output_dataset_rdf$p <- paste0(input$dataset_id, ":", v$output_dataset_rdf$p)
      v$output_dataset_rdf$s <- paste0(input$dataset_id, ":", v$output_dataset_rdf$s)

      v$dataset_rdf_file <- file.path(tempdir(), paste0(input$dataset_id, "_rdf.ttl"))
      v$dataset_rdf_file2 <- file.path(tempdir(), paste0(input$dataset_id, "_rdf2.ttl"))

      dataset_ttl_write(
        v$output_dataset_rdf,
        ttl_namespace = v$output_dataset_namespace_rdf,
        file_path = v$dataset_rdf_file,
        overwrite = TRUE)

      v$rdf <- rdflib::rdf_parse(v$dataset_rdf_file, format = "turtle")

      ## REFERENCE END

      sink(v$dataset_rdf_file2)
      print(v$rdf)
      sink()

      # JSON-LD ####

      incProgress(1/n, detail = "Creating JSON-LD object")

      ## REFERENCE START
      ## dataset package article by Daniel Antal used as reference material when writing lines 371:372 below: https://dataset.dataobservatory.eu/articles/RDF.html

      v$json_ld_file <- file.path(tempdir(), paste0(input$dataset_id, "jsonld.json"))
      v$json_ld <- rdflib::rdf_serialize(rdf = v$rdf, doc = v$json_ld_file, format = "jsonld")

      # CSVW JSON metadata ####

      incProgress(1/n, detail = "Creating CSVW JSON metadata")

      ## REFERENCE START
      ## csvwr package vignette by Robin Gower used as reference material when writing lines 381:393 below: https://cran.r-project.org/web/packages/csvwr/vignettes/read-write-csvw.html

      v$csvw_json_file <- file.path(tempdir(), paste0(input$dataset_id, "_metadata.json"))

      s <- csvwr::derive_table_schema(v$output_dataset)
      s$columns$titles[which(s$columns$titles == "values")] <- "OBS_VALUE"

      for (i in seq_along(s$columns$titles)){
        s$columns$titles[i] <- eurostat::label_eurostat_vars(x = s$columns$titles[i], id = input$dataset_id, lang = input$lang)
      }

      v$tb <- list(url=v$metadata$DOI_URL, tableSchema=s)
      v$m <- csvwr::create_metadata(tables=list(v$tb))
      v$json_metadata <- jsonlite::toJSON(v$m)
      v$csvw_json <- jsonlite::prettify(v$json_metadata)
      ## REFERENCE END
      sink(file = v$csvw_json_file)
      print(v$csvw_json)
      sink()


    })


  })

  # UI outputs ####

  # Dataset view
    output$statistics_table_view <- DT::renderDataTable({
      req(v$result)
      v$result
    })

    output$data_citation <- renderPrint({
      if (is.null(v$citation)){
        invisible()
      } else {
        print(v$citation)
      }
    })

    output$format_dataset <- renderPrint({
      req(v$output_dataset)
      v$output_dataset
    })

    output$download_dataset_rds <- downloadHandler(
      filename = function() {
        paste0(input$dataset_id,"_dataset.rds")
      },
      content = function(file) {
        saveRDS(v$output_dataset, file = file)
      }
    )

    output$format_dataset_datacite <- renderPrint({
      if (is.null(v)){
        return()
      }
      print(head(v$output_dataset_datacite, 10))
    })

    output$download_dataset_datacite_rds <- downloadHandler(
      filename = function() {
        paste0(input$dataset_id,"_datacite.txt")
      },
      content = function(file) {
        writeLines(paste(v$output_dataset_datacite, collapse = ", "), file)
      }
    )

    output$format_dataset_dublincore <- renderPrint({
      req(v$output_dataset_dublincore)
      print(v$output_dataset_dublincore, style = "text")
    })

    output$download_dataset_dublincore_rds <- downloadHandler(
      filename = function() {
        paste0(input$dataset_id,"_dublincore.txt")
      },
      content = function(file) {
        writeLines(paste(v$output_dataset_dublincore, collapse = ", "), file)
      }
    )

    # TTL
    output$ttl_dataset <- renderPrint({
      req(v$dataset_ttl_file)
      print(readLines(v$dataset_ttl_file, 30))
    })

    output$download_dataset_ttl <- downloadHandler(
      filename = function() {
        paste0(input$dataset_id,"_ttl.ttl")
      },
      content = function(file) {
        file.copy(v$dataset_ttl_file, file)
      }
    )

    # RDF
    output$rdf_dataset <- renderPrint({
      req(v$dataset_rdf_file2)
      readLines(v$dataset_rdf_file2, 30)
    })

    output$download_dataset_rdf <- downloadHandler(
      filename = function() {
        paste0(input$dataset_id,"_rdf.ttl")
      },
      content = function(file) {
        file.copy(v$dataset_rdf_file, file)
      }
    )

    # JSON-LD
    output$json_ld_dataset <- renderPrint({
      req(v$json_ld_file)
      readLines(v$json_ld_file, 30)
    })

    output$download_dataset_json_ld <- downloadHandler(
      filename = function() {
        paste0(input$dataset_id,".json")
      },
      content = function(file) {
        file.copy(v$json_ld, file)
      }
    )

    # CSVW JSON metadata
    output$csvw_json <- renderPrint({
      req(v$csvw_json_file)
      readLines(v$csvw_json_file, 100)
    })

    output$download_csvw_json <- downloadHandler(
      filename = function() {
        paste0(input$dataset_id,"_metadata.json")
      },
      content = function(file) {
        file.copy(v$csvw_json_file, file)
      },
      contentType = "application/json"
    )

}

# Run the application
shinyApp(ui = ui, server = server)
