#' Convert a value to a specific range of other currencies from the Currencyapi.com API. Retrieve the latest exchange rate data or for a specific historic date.

#' @param value (required) The value you want to convert
#' @param date Date to retrieve historical rates from (format: 2021-12-31)
#' @param base_currency The base currency for the conversion (e.g., "USD")
#' @param currencies A list of comma seperated currency codes which you want to get (EUR,USD,CAD) By default all available currencies will be shown

#' @return Returns calculated values for today or any given date for all or a specific set of currencies.
#' @export


convert_exchange_rates <- function(value, date = NULL, base_currency = NULL, currencies = NULL) {

  # check for API key or ask for API key
  apikey <- currencyapi_api_key()

  # ensure necessary packages are installed
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Please install the 'httr' package to use this function.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install the 'jsonlite' package to use this function.")
  }

  # define the API URL
  api_url <- "https://api.currencyapi.com/v3/convert"

  # generate query
  params <- list(apikey = apikey, value = value)

  # append params to query if not null
  if(!is.null(date)) {
    params['date'] <- date
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
