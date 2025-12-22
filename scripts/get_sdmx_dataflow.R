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
