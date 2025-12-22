library(shiny)
library(bslib)
library(bsicons)
library(eurostat)
library(DT)
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
  nav_panel("Format: RDF-XML",
            card(
              card_header("RDF data: Preview"),
              card_body(
                verbatimTextOutput("rdf_dataset")
              ),
              card_footer(
                downloadButton('download_dataset_rdf', "Download dataset as RDF/.xml file")
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
                downloadButton('download_dataset_ttl', "Download dataset as RDF/.ttl file")
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

      # FUNCTION FOR THIS APP ####
      # Needed for eurostat version 4.0

      # Function from development version 4.1 of eurostat
      build_api_base_uri <- function(agency) {
        agency <- tolower(agency)
        api_base_uri <- switch(
          agency,
          eurostat = "https://ec.europa.eu/eurostat/api/dissemination",
          estat = "https://ec.europa.eu/eurostat/api/dissemination",
          eurostat_comext = "https://ec.europa.eu/eurostat/api/comext/dissemination",
          comext = "https://ec.europa.eu/eurostat/api/comext/dissemination",
          eurostat_prodcom = "https://ec.europa.eu/eurostat/api/comext/dissemination",
          prodcom = "https://ec.europa.eu/eurostat/api/comext/dissemination",
          comp = "https://webgate.ec.europa.eu/comp/redisstat/api/dissemination",
          empl = "https://webgate.ec.europa.eu/empl/redisstat/api/dissemination",
          grow = "https://webgate.ec.europa.eu/grow/redisstat/api/dissemination")

        api_base_uri
      }

      # Function from development version 4.1 of eurostat
      get_sdmx_dataflow <- function(id, agency = "Eurostat", type = "list", lang = NULL) {

        api_base_uri <- build_api_base_uri(agency)

        dataflow_url <- paste0(
          api_base_uri,
          "/sdmx/2.1/dataflow/estat/",
          id)

        xml_object <- xml2::read_xml(dataflow_url)

        if (identical(type, "raw")) {
          return(xml_object)
        }

        # Define namespaces
        namespaces <- xml2::xml_ns(xml_object)

        # Extract Header information
        header <- xml2::xml_find_first(xml_object, ".//m:Header", namespaces)
        header_id <- xml2::xml_text(xml2::xml_find_first(header, ".//m:ID", namespaces))
        prepared <- substr(xml2::xml_text(xml2::xml_find_first(header, ".//m:Prepared", namespaces)),1,10)
        sender_id <- xml2::xml_attr(xml2::xml_find_first(header, ".//m:Sender", namespaces), "id")

        # Continue with dataflow and annotations extraction
        dataflow <- xml2::xml_find_first(xml_object, ".//s:Dataflow", namespaces)
        dataflow_id <- xml2::xml_attr(dataflow, "id")
        urn <- xml2::xml_attr(dataflow, "urn")
        agencyID <- xml2::xml_attr(dataflow, "agencyID")
        version <- xml2::xml_attr(dataflow, "version")
        isFinal <- xml2::xml_attr(dataflow, "isFinal")
        # Extract names in different languages
        # Extract names in different languages independently
        name_de <- xml2::xml_text(xml2::xml_find_first(dataflow, ".//c:Name[@xml:lang='de']", namespaces))
        name_en <- xml2::xml_text(xml2::xml_find_first(dataflow, ".//c:Name[@xml:lang='en']", namespaces))
        name_fr <- xml2::xml_text(xml2::xml_find_first(dataflow, ".//c:Name[@xml:lang='fr']", namespaces))
        if (!is.null(lang)) {
          name <- xml2::xml_text(xml2::xml_find_first(dataflow, sprintf(".//c:Name[@xml:lang='%s']", lang), namespaces))
        } else {
          name <- name_en
        }

        source_institutions <- list()
        doi_details <- NULL

        annotations_nodes <- xml2::xml_find_all(dataflow, ".//c:Annotation", namespaces)
        for (node in annotations_nodes) {
          title <- xml2::xml_text(xml2::xml_find_first(node, ".//c:AnnotationTitle", namespaces))
          type <- xml2::xml_text(xml2::xml_find_first(node, ".//c:AnnotationType", namespaces))
          texts_nodes <- xml2::xml_find_all(node, ".//c:AnnotationText", namespaces)


          # Assign specific annotations based on type
          if (type == "OBS_PERIOD_OVERALL_LATEST") {
            latest_period_timestamp <- title  # Directly store the latest period timestamp
          } else if (type == "OBS_PERIOD_OVERALL_OLDEST") {
            oldest_period_timestamp <- title
          } else if (type == "UPDATE_DATA") {
            update_data_timestamp <- title
          } else if(type == "SOURCE_INSTITUTIONS"){
            if (!is.null(lang)) {
              source_institutions <- xml2::xml_text(
                xml2::xml_find_all(node, sprintf(".//c:AnnotationText[@xml:lang='%s']", lang))
              )
            } else {
              source_institutions <- xml2::xml_text(
                xml2::xml_find_all(node, ".//c:AnnotationText[@xml:lang='en']")
              )
            }
          }

          if (grepl("adms:Identifier", title)) {
            title_xml <- xml2::read_xml(title)
            doi_url <- xml2::xml_attr(xml2::xml_find_first(title_xml, ".//adms:Identifier"), "rdf:about", xml2::xml_ns(title_xml))
          }
        }

        metadata <- list(
          name = name,
          name_en = name_en,
          name_de = name_de,
          name_fr = name_fr,
          doi_url = ifelse(exists("doi_url"), eval(doi_url), NA_character_),
          dataflow_id = dataflow_id,
          agency_id = agencyID,
          id = header_id,
          prepared = prepared,
          sender_id = sender_id,
          oldest_period_timestamp = oldest_period_timestamp,
          latest_period_timestamp = latest_period_timestamp,
          update_data_timestamp = update_data_timestamp,
          source_institutions = source_institutions,
          urn = urn,
          version = version,
          is_final = isFinal
        )

        return(metadata)

      }

      # EUROSTAT DATA RETRIEVAL ####

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

      # Eurostat 4.1 feature
      # v$concept_scheme <- eurostat:::get_sdmx_conceptscheme(
      #   id = input$dataset_id,
      #   lang = input$lang
      # )

      incProgress(1/n, detail = "Fetching citation metadata")

      v$citation <- eurostat::get_bibentry(
        code = input$dataset_id,
        lang = input$lang
      )

      incProgress(1/n, detail = "Extracting metadata (Eurostat dataflow)")

      v$dataflow <- get_sdmx_dataflow(agency = "Eurostat", id = input$dataset_id)

      selected_lang_name <- switch(
        input$lang,
        "en" = v$dataflow$name_en,
        "fr" = v$dataflow$name_fr,
        "de" = v$dataflow$name_de
      )

      v$output_dataset <- v$result

      incProgress(1/n, detail = "Creating RDF object")

      # RDF/XML file ####

      v$output_dataset_rdf <- suppressWarnings(
        rdflib::as_rdf(v$output_dataset)
        )

      v$dataset_rdf_file_path <- file.path(tempdir(), paste0(input$dataset_id, "_rdf.rdf"))

      rdflib::rdf_serialize(v$output_dataset_rdf,
                            doc = v$dataset_rdf_file_path,
                            format = "rdfxml")

      incProgress(1/n, detail = "Creating TTL object")

      # TTL file ####

      v$dataset_ttl_file_path <- file.path(tempdir(), paste0(input$dataset_id, "_rdf.ttl"))

      rdflib::rdf_serialize(
        v$output_dataset_rdf,
        doc = v$dataset_ttl_file_path,
        format = "turtle"
      )

      v$dataset_rdf_file_path2 <- file.path(tempdir(), paste0(input$dataset_id, "_rdf2.ttl"))
      v$rdf_ttl_viewable <- rdflib::rdf_parse(v$dataset_ttl_file_path, format = "turtle")

      sink(v$dataset_rdf_file_path2)
      print(v$rdf_ttl_viewable)
      sink()

      # JSON-LD ####

      incProgress(1/n, detail = "Creating JSON-LD object")

      v$json_ld_file_path <- file.path(tempdir(), paste0(input$dataset_id, "_jsonld.json"))
      rdflib::rdf_serialize(rdf = v$rdf_ttl_viewable,
                            doc = v$json_ld_file_path,
                            format = "jsonld")

      # CSVW JSON metadata ####

      incProgress(1/n, detail = "Creating CSVW JSON metadata")

      ## REFERENCE START (csvwr)
      ## csvwr package vignette by Robin Gower used as reference material when writing lines 381:393 below: https://cran.r-project.org/web/packages/csvwr/vignettes/read-write-csvw.html

      v$csvw_json_file_path <- file.path(tempdir(), paste0(input$dataset_id, "_metadata.json"))

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

      sink(file = v$csvw_json_file_path)
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
      req(v$dataset_ttl_file_path)
      print(readLines(v$dataset_ttl_file_path, 30))
    })

    output$download_dataset_ttl <- downloadHandler(
      filename = function() {
        paste0(input$dataset_id,"_ttl.ttl")
      },
      content = function(file) {
        file.copy(v$dataset_ttl_file_path, file)
      }
    )

    # RDF
    output$rdf_dataset <- renderPrint({
      req(v$dataset_rdf_file_path2)
      readLines(v$dataset_rdf_file_path2, 30)
    })

    output$download_dataset_rdf <- downloadHandler(
      filename = function() {
        paste0(input$dataset_id,"_rdf.ttl")
      },
      content = function(file) {
        file.copy(v$dataset_rdf_file_path, file)
      }
    )

    # JSON-LD
    output$json_ld_dataset <- renderPrint({
      req(v$json_ld_file_path)
      readLines(v$json_ld_file_path, 30)
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
      req(v$csvw_json_file_path)
      readLines(v$csvw_json_file_path, 100)
    })

    output$download_csvw_json <- downloadHandler(
      filename = function() {
        paste0(input$dataset_id,"_metadata.json")
      },
      content = function(file) {
        file.copy(v$csvw_json_file_path, file)
      },
      contentType = "application/json"
    )

}

# Run the application
shinyApp(ui = ui, server = server)
