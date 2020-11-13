capitalize <- function(string) {
  paste0(substr(string, 1, 1),
         tolower(substr(string, 2, nchar(string))))
}

na_replace <- function(df) {
  df[is.na(df)] <- ""
  return(df)
}

text_between_curly_brackets <- function(string) {
  min <- min(gregexpr("\\{", string)[[1]])
  max <- max(gregexpr("\\}", string)[[1]])

  ## if there are no curly braces, keep everyting
  if( (!is.na(min) & min == -1L) | (!is.na(max) & max == -1L) ) {
    min <- 0
    ## this is nchar to remove the trailing comma
    max <- nchar(string)
  }

  content <- substring(string, min + 1, max - 1)
  return(content)
}

#' @importFrom stringr str_squish
remove_entry_newlines <- function(entry) {

  idx <- stringr::str_detect(entry, pattern = '^[[:blank:]]*[[:alpha:]]+[[:blank:]]*=[[:blank:]]*(\\{|")?')

  idx[1] <- TRUE
  idx[ length(idx) ] <- TRUE

  if(any(!idx)) {
    new_lines <- which(!idx)
    for(n in rev(new_lines)) {
      entry[n-1] <- paste(entry[n-1], str_squish(entry[n]))
      entry <- entry[-n]
    }
  }
  return(entry)
}
