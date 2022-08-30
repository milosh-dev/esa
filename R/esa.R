#####################################################
#
# Andmepäring Statistikaameti andmebaasi andmetabeli info saamiseks
# Autorid: Raoul Lättemäe, Taavi Unt, Andres Võrk
#
#####################################################
#library(tidyverse)  #import dplyr
#library(rsdmx)
#library(lubridate)

#' List all available SDMX tables
#'
#' Get the list of all tables available in Statistical Office of Estonia
#'
#' @param update Boolean. If true, then the table will be recreated from internet. Otherwise it's loaded from memory
#'
#' @importFrom stringr str_split_fixed
#' @importFrom xml2 read_html
#' @importFrom rvest html_text
#' @importFrom rvest html_nodes
#' @examples
#' \dontrun{
#' ESA.list() # Lists all the tables
#' ESA.list(update = TRUE)  # Updates the list
#' }
#' @export
ESA.list <- function(update = FALSE) {
  # Save
  if(!update) {
    if(file.exists("data-raw/esa.rda")){
      #esa.tables <<-
      load("data-raw/esa.rda")
      esa.tables <<- esa.tables
      #      return(datasets)
    }
  }

  # Eestikeelsed andmed
  url <- "http://andmebaas.stat.ee/?lang=et"
  page = xml2::read_html(url)
  et=page %>%
    rvest::html_nodes(".ds") %>%
    rvest::html_text() %>%
    stringr::str_split_fixed(":", 2) %>%
    data.frame()
  colnames(et) = c("Id","Selgitus")

  # Inglisekeelsed andmed
  url <- "http://andmebaas.stat.ee/?lang=en"
  page = xml2::read_html(url)
  en=page %>%
    rvest::html_nodes(".ds") %>%
    rvest::html_text() %>%
    stringr::str_split_fixed(":", 2) %>%
    data.frame()
  colnames(en) = c("Id","Description")

  esa.tables <- dplyr::left_join(et, en, by = 'Id')

  # Salvestab andmed keskkonda
  esa.tables <<- as.data.frame(esa.tables)
  save(esa.tables, file = "data-raw/esa.rda")
  #  return(datasets)
}


#' Get Metadata
#'
#' Gets table structure information from Estonian Statistical Office
#'
#' @importFrom rsdmx readSDMX
#' @importFrom magrittr "%>%"
#'
#' @param table A string, representing the name of the table
#' @return This function creates a dataframes with list of variables and dimensions in the table
#' @examples
#' ESA.metadata("IA02")
#' @export
ESA.metadata <- function(table){
  dfstruct <- rsdmx::readSDMX(paste0("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetDataStructure/", table))
  ESA.reset()
  varnames <<- as.data.frame(dfstruct@concepts)
  varnames$Name.et <- iconv(varnames$Name.et, "UTF-8")
  labellist <- slot(dfstruct, "codelists")
  labelnames <- sapply(slot(labellist, "codelists"), function(x) slot(x, "id"))

  for(i in setdiff(labelnames, paste0("CL_", table, "_OBS_STATUS"))) {
    assign(substr(i,nchar(paste0("CL_", table, "_"))+1, nchar(i)),
           as.data.frame(slot(dfstruct, "codelists"), codelistId = i) %>%
             select(id, starts_with("label")), envir = globalenv())
  }
}

#' Clears metadata from global environment
#'
#' @return Clears metadata from global environment
#' @export
ESA.reset <- function() {
  options(warn=-1)
  rm(DIM1, DIM2, DIM3, DIM4, TIME, TIME_FORMAT, varnames, envir=globalenv())
  options(warn=0)
}

#' Creates url from table and restrictions
#'
#' @param table A string of a table name. e.g IA02
#' @param restrictions A string or vector of an optional restrictions list.
#' @param start An optional year number for start of the data
#' @param end An optional year number for end of the data
#'
#' @return A string of url for database query
#' @examples
#' ESA.url("IA02")  # Returns a whole CPI table
#' ESA.url("IA02", '1')  # Returns a CPI table for id = 1 Total
#' ESA.url("IA02", c('1+3','1+2'), 1995, 2000) # Returns CPI table for Total and Alcohol, January and February in dates 1995-2000
#' @export
ESA.url <- function(table, restrictions = NA, start = NA, end = NA) {
  # Fill query, if restrictions are defined
  # if(!is.na(restrictions))  {
  #  query <- paste(restrictions, collapse = ".")
  #  query <- paste(query, "/", sep = "")
  #} else {
  #  query <- ""
  #}

  # Fill query, if restrictions are defined
  ifelse(!is.na(restrictions),
         query <- paste(paste(restrictions, collapse = "."), "/", sep = ""),
         query <- ""
  )

  # If start time is defined
  if(!is.na(start)){
    start <- paste("startTime", start, sep ="=")
  }

  # if end time is defined
  if(!is.na(end)) {
    end <- paste("endTime", end, sep ="=")
  }

  # merge start and end
  if(!is.na(start)) {
    if(!is.na(end)) {
      time <- paste("?", start, "&", end, sep = "")
    } else {
      time <- paste("?", start, sep = "")
    }
  } else {
    if(!is.na(end)) {
      time <- paste("?", end, sep = "")
    } else {
      time <- ""
    }
  }

  # create a query
  url <- paste("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetData/", table, "/", query, "all/", time, sep = "")
  return(url)
}

#' Dependencies
#'
#' Requests data from Statistical Office of Estonia (ESA)
#'
#' @importFrom rsdmx readSDMX
#' @import dplyr
#' @importFrom methods slot
#'
#' @param table A string of a table name. e.g IA02
#' @param restrictions A string or vector of an optional restrictions list.
#' @param start An optional year number for start of the data
#' @param end An optional year number for end of the data
#'
#' @return A string of url for database query
#' @examples
#' \dontrun{
#' ESA.url("IA02")  # Returns a whole CPI table
#' ESA.url("IA02", '1')  # Returns a CPI table for id = 1 Total
#' ESA.url("IA02", c('1+3','1+2'), 1995, 2000) # Returns CPI table for Total and Alcohol, January and February in dates 1995-2000
#' }
#' @export
ESA.query <- function(table, restrictions = NA, start = NA, end = NA) {
  # Get url of the table
  url <- ESA.url(table, restrictions, start, end)

  # Fetch the data. (Use temporary storage, as this is better for longer tables)
  tf <- tempfile(tmpdir = tdir <- tempdir())
  download.file(url, tf)
  df <- readSDMX(tf, isURL = FALSE) %>% as.data.frame()

  # Drop OBS_STATUS column
  if("OBS_STATUS" %in% colnames(df)) {
    df <- subset(df, select = -c(OBS_STATUS))
  }

  # Download the labels
  dfstruct <- readSDMX(paste0("http://andmebaas.stat.ee/restsdmx/sdmx.ashx/GetDataStructure/", table))
  varnames <- as.data.frame(dfstruct@concepts)
  varnames$Name.et <- iconv(varnames$Name.et, "UTF-8")
  labellist <- slot(dfstruct, "codelists")
  labelnames <- sapply(slot(labellist, "codelists"), function(x) slot(x, "id"))

  for(i in setdiff(labelnames, paste0("CL_", table, "_OBS_STATUS"))) {
    assign(substr(i,nchar(paste0("CL_", table, "_"))+1, nchar(i)),
           as.data.frame(methods::slot(dfstruct, "codelists"), codelistId = i) %>%
             dplyr::select(id, starts_with("label")))
  }

  for(i in setdiff(names(df), c("obsTime", "obsValue"))) {
    df <- merge(df, get(i), by.x=i, by.y = "id" )
    et <- varnames %>%
      filter(id == i) %>%
      select(Name.et)

    # If label is not found
    if(is.na(et)) {
      et <- paste0(i,'label.et')
    }

    en <- varnames %>%
      filter(id == i) %>%
      select(Name.en)

    # If label is not found
    if(is.na(en)) {
      en <- paste0(i,'label.en')
    }

    names(df)[names(df) == 'label.en'] <- en # paste0(i,'label.en')
    names(df)[names(df) == 'label.et'] <- et # paste0(i,'label.et')
  }
  return(df)
}

#' Creates monthly time series from the data
#' Use as
#'
#' Dpendencies:
#' @import lubridate
#'
#' @param table dataframe of the raw data, returned from the statistical offic
#' @param year optional column name for the year data. Defaults to obsTime
#' @param month optional column name for the month data. Defaults to DIM3
#' @param name optional column name for the newly created date field. Defaults to obsDate
#' @return dataframe with the newly created time column
#' @examples
#' \dontrun{
#' ESA.monthly(data)
#' ESA.monthly(data, month = 'DIM2')
#' }
#' @export
ESA.monthly <- function(table, year = 'obsTime', month = 'DIM3', name = 'obsDate') {
  # Note that `table` and table() are needed to surpress object type is not surpressable error
  # @see https://stackoverflow.com/questions/40623749/what-is-object-of-type-closure-is-not-subsettable-error-in-shiny
  # https://files.speakerdeck.com/presentations/5a82a69085a445bc92427f2d3762a4c9/slide_1.jpg
  table[[name]] <- lubridate::ymd(
      paste(
          table[[year]],
          table[[month]],
          '15',
          sep = "-")
    )
  lubridate::day(table[[name]]) <- lubridate::days_in_month(table[[name]])
  # Rearrange columns
  table <- table[c(name, year, month, setdiff(names(table), c(name, year, month)))]
  return(table)
}

#' Creates monthly time series from the data
#' Use as
#'
#' Dpendencies:
#' @import lubridate
#'
#' @param table dataframe of the raw data, returned from the statistical offic
#' @param year optional column name for the year data. Defaults to obsTime
#' @param quarter optional column name for the quarterly data. Defaults to DIM3
#' @param name optional column name for the newly created date field. Defaults to obsDate
#' @return dataframe with the newly created time column
#' @examples
#' \dontrun{
#' ESA.quarterly(data)
#' ESA.quarterly(data, quarter = 'DIM3')
#' }
#' @export
ESA.quarterly <- function(table, year = 'obsTime', quarter = 'DIM3', name = 'obsDate') {

  quarter.n <- paste(quarter, ".m", sep="")

  table[[quarter.n]] <- sapply(table[[quarter]], switch, "I" = 3, "II" = 6, "III" = 9, "IV" = 12)
  table[[quarter.n]] <- as.numeric(table[[quarter.n]])

  table[[name]] <- lubridate::ymd(
    paste(
      table[[year]],
      table[[quarter.n]],
      '15',
      sep = "-")
  )
  lubridate::day(table[[name]]) <- lubridate::days_in_month(table[[name]])
  # Rearrange columns
  table <- table[c(name, year, quarter, setdiff(names(table), c(name, year, quarter)))]
  return(table)
}


#' Table title
#'
#' Returns title of the table
#'
#' @importFrom magrittr "%>%"
#'
#' @param table A string of a table name. e.g IA02
#' @return dataframe of official titles of the table from Statistics Estonia
#' @examples
#' ESA.titles("IA02")
#' @export
ESA.titles <- function(table){
  load(file = "/home/raoul/Dokumendid/R/packages/esa/data-raw/esa.rda")
  return(esa.tables %>% filter(Id == table))
}
