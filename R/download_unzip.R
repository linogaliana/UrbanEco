#' General method for downloading and unzipping a file/directory from url
#'
#' Automatically download and unzip data from an URL. Data can be written in the disk
#' or directly read into R
#' @param url URL that will be requested
#' @param unzip If \code{TRUE}, downloaded file is unzipped, otherwise not (\code{FALSE})
#' @param extension Extension of the file if \code{method = 'r'} (read file)
#' @param dest Destination where file/directory should be put when \code{method = 'w'}.
#' Ignored otherwise
#' @param ... Other arguments to pass to control reading or writing behavior
#' @examples {
#' shpbike <- UrbanEco::download.unzip(url = "https://github.com/linogaliana/UrbanEco/blob/master/inst/stations-velib-disponibilites-en-temps-reel.zip?raw=true")
#' out <- UrbanEco::download.unzip(url = "http://vlsstats.ifsttar.fr/rawdata/RawData/RawData_OLD/data_all_Paris.jjson_2017-06-01-1496291195.gz", extension = '.json')
#' }

download.unzip <- function(url = "http://opendata.paris.fr/explore/dataset/stations-velib-disponibilites-en-temps-reel/download?format=shp",
                           unzip = TRUE, extension = '.shp', method = "r",
                           dest = getwd(), ...){

  # GETTING ARGUMENT LIST
  arglist <- as.list(match.call(expand.dots=FALSE))

  # CREATE TEMPORARY DIRECTORY
  tmpdir <- tempdir()
  temp <- tempfile()
  oldwd <- getwd()
  td <- tempdir()

  # CREATE TEMPORARY FILE
  if (stringr::str_detect(url,".gz$")){
    tf <- tempfile(tmpdir=tmpdir, fileext=".gz")
  } else{
    tf <- tempfile(tmpdir=tmpdir, fileext=".zip")
  }

  # Method download can be auto or curl
  download.method <- if (is.null(arglist$...$download.method)) "auto" else as.character(arglist$...$sep)
  download.file(url, tf, method = download.method)

  # ENSURE EXTENSION NAME IS CONSISTENT
  if (!stringr::str_detect(extension,"^.")) extension <- paste0(".",extension)

  # LIST CONTENT ZIP OR FILENAME
  if (unzip){
    if (stringr::str_detect(url,".gz$")){
      fpath <- tf
    } else{
    fname = unzip(tf, list=TRUE)$Name

    # UNZIP
    unzip(tf, files=fname, exdir=td, overwrite=TRUE)

    # fpath is the full path to the extracted file
    fpath <- file.path(td, fname)
    }
  } else{
    fpath <- tf
  }

  if (method == "w"){
    message(paste0("Writing file at ", dest,"/tmp"))
    dir.create(paste0(dest,"/tmp"), showWarnings = FALSE)
    file.copy(from=fpath, to=paste0(dest,"/tmp"),
             recursive = TRUE,
             copy.mode = TRUE)
  } else if (method == 'r'){
    message(paste0("Reading ",extension, " file without writing"))

    if (extension == ".shp"){
      shpfiles <- if (sum(stringr::str_detect(fpath,".shp"))==1)
        try(rgdal::readOGR(fpath[stringr::str_detect(fpath,".shp")])) else
        lapply(fpath[stringr::str_detect(fpath,".shp")], function(nam) try(rgdal::readOGR(nam)))
      return(shpfiles)
    } else if (extension == ".csv"){
      sep <- if (is.null(arglist$...$sep)) "," else as.character(arglist$...$sep)
      col_names <- if (is.null(arglist$...$col_names)) TRUE else arglist$...$col_names
      return(
        lapply(fpath[stringr::str_detect(fpath,".csv")], function(nam) try(readr::read_delim(nam,
                                                                                             delim = sep,
                                                                                             col_names = col_names)))
      )
    } else if (extension == ".json"){
        return(lapply(readLines(tf), jsonlite::fromJSON))
    }

  }




}
