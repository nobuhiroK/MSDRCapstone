#' Read NOAA significant earthquke data
#'
#' @importFrom readr read_delim
#' @param path  defalut path set to inst/extdata/"signif.txt"
#' @param url  set TRUE to download data from NOAA website
#'
#' @return a tibble containing 47 vaiables
#'
#'
#' @examples
#' dat <- read_signif(url = TRUE)
#'
#' @export

read_signif <- function(path = system.file("extdata", "signif.txt", package = "MSDRCapstone"), url =FALSE) {
  if (url ==  TRUE){
    signif_dat <- read_delim(url("https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"),  delim = '\t')
  }else{
    signif_dat <- read_delim(path,  delim = '\t')
  }
}


#' Clean the row signif_dat
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom lubridate make_date
#' @name %>%
#' @rdname pipe
#'
#' @description
#'     A date column created by uniting the year, month, day and converting it to the Date class
#'     LATITUDE and LONGITUDE columns converted to numeric class
#'
#' @return
#' a tibble containing 48 vaiables
#'
#'
#' @examples
#' clean_dat <- eq_clean_data()
#'
#' @export

eq_clean_data <- function(dat = read_signif()) {
  dt_clean_month_day_date <- dat %>%
    mutate(MONTH = ifelse(is.na(MONTH), 1, MONTH)) %>%
    mutate(DAY = ifelse(is.na(DAY), 1, DAY)) %>%
    mutate(DATE = make_date(YEAR, MONTH, DAY))

}



