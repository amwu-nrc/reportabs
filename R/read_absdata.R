#' @title Read ABS time series data.
#'
#' @name read_absdata
#'
#' @description
#' Read ABS time series data from the [amwudata](https://github.com/amwu-nrc/amwudata) package.
#'
#'
#' @param name The name, as a string, of the dataset to download.A full list of available data is available at https://github.com/amwu-nrc/amwudata
#' @param export_dir The directory in which to save downloaded data. Defaults to a temporary directory.
#'
#' @return data
#' @export
#'
read_absdata <- function(name = NULL,
                         export_dir = tempdir()) {

  base_url <- "https://github.com/amwu-nrc/amwudata/raw/master/data/"
  url <- paste0(base_url, name, ".rda")

  out_path <- file.path(export_dir, paste0(name, ".rda"))

  if (!file.exists(out_path)) {

      tryCatch(utils::download.file(url,
                                    destfile = out_path,
                                    mode = "wb"),
               error = "Download failed. Probably the requested file doesn't exist.")



  } else {
    message("Reading ", name, " file found in ", export_dir)
  }

  load(out_path)

  d <- get(name)

  return(d)
}
