#' Tanrabad casecount reader
#'
#' @param txt casecount json text
#' @return data frame table
#' @examples
#' library(Rbigstream)
#' host <- "http://sample.bigstream.io/"
#' storage_name <- "sample.trb"
#' token <- "token"
#' bs.connect(host, storage_name, token)
#' json <- storage.get(storage_name,flatten=FALSE)
#' df <- trb.readcasecount(json)
#' @export

trb.readcasecount <- function(txt) {
  if(!jsonlite::validate(txt)) stop("txt is not json format")
  txt %>%
    as.tbl_json %>%
    enter_object("data") %>%
    spread_values(
      disease_code = jstring("disease_code"),
      disease_name = jstring("disease_name")
    ) %>%
    enter_object("data") %>%
    gather_array %>%
    spread_values(
      province.code = jstring("province_code"),
      province.name = jstring("province_name"),
      province.namt = jstring("province_namt")
    ) %>%
    enter_object("data")%>%
    gather_array %>%
    spread_values(
      week_of_year = jnumber("week_of_year"),
      year = jnumber("year"),
      case_count = jnumber("case_count")
    )
}
