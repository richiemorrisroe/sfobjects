##TODO: this should probably return a level object
base_gm <- "https://www.stockfighter.io/gm/"
##' Start a level by name
##' See above
##' @title start_level
##' @param level 
##' @return a level object
##' @author richie
##' @export
start_level <- function(level="first_steps", ...) {
    base_gm <- "https://www.stockfighter.io/gm/levels/"
    url <- paste(base_gm, "/",level, sep="")
    res <- httr::POST(url,
                      httr::add_headers("X-Starfighter-Authorization"=apikey),
                      ...)
}
##' Change a level in some form (stop, resume etc)
##' See above. I really have only used stop. 
##' @title change_instance
##' @param level an object returned by start_level
##' @param action the action to perform (stop, resume)
##' @return a HTTP response indicating the response of the server
##' @author richie
##' @export
change_instance <- function(level, action) {
    id <- level@id
    url <- paste(base_gm, "instances/", id, "/", action, sep="")
    if(action %in% c("stop", "resume")) {
        res <- httr::POST(url=url, httr::add_headers("X-Starfighter-Authorization"=apikey))
    }
    else {
        res <- httr::GET(url, httr::add_headers("X-Starfighter-Authorization"=apikey))
    }
    res
}
##' Get some information relating to the level. This often includes information to make things easier
##'
##' See above
##' @title level_status 
##' @param level a level object
##' @param apikey an authorization code
##' @return a HTTP response indicating the results
##' @author richie
##' @export
level_status <- function(level, apikey)  {
    stopifnot(class(level)=="level") 
    instance <- level@id
    url <- paste(base_gm, "instances/", instance, sep="")
    start <- get_time()
    res <- httr::GET(url,
                     httr::add_headers(
                         "X-Starfighter-Authorization"=apikey))
    end <- get_time()
    ts <- make_timestamp(start, end)
    resp <- stockfighterr::parse_response(res)
    resp$timestamp <- ts
    resp
}
##' get the level id from a level object
##'
##' See above
##' @title get_id
##' @param response 
##' @return an integer ID which refers to the level
##' @author richie
##' @export
get_id <- function(response) {
    pr <- parse_response(response)
    id <- pr[["instanceId"]]
    id
}
