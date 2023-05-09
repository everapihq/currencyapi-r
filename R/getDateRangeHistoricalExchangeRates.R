#' Get historical exchange rates for a specific date for a single or multiple currencies.

#' @param datetime_start (required) Datetime for the start of your requested range (format: 2021-12-31T23:59:59Z / ISO8601 Datetime)
#' @param datetime_end (required) Datetime for the end of your requested range (format: 2021-12-31T23:59:59Z / ISO8601 Datetime)
#' @param accuracy The accuracy you want to receive; Possible Values: day, hour, quarter_hour, minute; Default: day For valid time ranges see below
#' @param base_currency The base currency for the conversion (e.g., "USD")
#' @param currencies A list of comma seperated currency codes which you want to get (EUR,USD,CAD) By default all available currencies will be shown

#' @return Returns exchange rates for a given time range. Generally, we provide data going back to 1999.
#' @export
#'

get_date_range_historical_exchange_rates <- function(datetime_start, datetime_end, accuracy = NULL, base_currency = NULL, currencies = NULL) {

  # ensure necessary packages are installed
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Please install the 'httr' package to use this function.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install the 'jsonlite' package to use this function.")
  }

  # check for API key or ask for API key
  apikey <- currencyapi_api_key()

  # check if datetime_start is passed and not null
  if(is.null(datetime_start)) {
    stop("datetime_start is required and can not be null.")
  }

  # check if datetime_end is passed and not null
  if(is.null(datetime_end)) {
    stop("datetime_end is required and can not be null.")
  }

  # define the API URL
  api_url <- "https://api.currencyapi.com/v3/range"

  # generate query
  params <- list(apikey = apikey, datetime_start = datetime_start, datetime_end = datetime_end)

  # append params to query if not null
  if(!is.null(accuracy)) {
    params['accuracy'] <- accuracy
  }

  if(!is.null(base_currency)) {
    params['base_currency'] <- base_currency
  }

  if(!is.null(currencies)) {
    currencies <- gsub(" ", "", currencies)
    params['currencies'] <- currencies
  }

  # make the API request
  response <- httr::GET(api_url, query = params)

  # check if the request was successful
  data <- success_check(response)

  data <- jsonlite::fromJSON(httr::content(response, as = 'text' ,type = 'application/json', encoding="UTF-8"), flatten = TRUE)

  # return the result
  return(data)
}
