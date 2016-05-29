#tiny change
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
    level_data <- httr::content(level)
    instance_id <- level_data[["instanceId"]]
    url <- paste(base_gm, "instances/", instance_id, "/", action, sep="")
    print(instance_id)
    print(url)
    if(action %in% c("stop", "resume")) {
        res <- httr::POST(url=url, httr::add_headers("X-Starfighter-Authorization"=apikey), httr::verbose())
    }
    else {
        res <- httr::GET(url, httr::add_headers("X-Starfighter-Authorization"=apikey), httr::verbose())}
    res
}
##' Get some information relating to the level. This often includes information to make things easier
##'
##' See above
##' @title level_status 
##' @param level a HTTP response representing a level
##' @param ... Other arguments passed to GET()
##' @return a HTTP response indicating the results
##' @author richie
##' @export
level_status <- function(level, ...)  {
    if(class(level)=="level") {
        instance <- level@id
    }
    else {
        instance <- httr::content(level)[["instanceId"]]
    }
    url <- paste(base_gm, "instances/", instance, sep="")
    res <- httr::GET(url,
                     httr::add_headers(
                         "X-Starfighter-Authorization"=apikey), ...)
    
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
