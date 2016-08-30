#TODO: figure out if all classes should have a local time
setClass(Class="trades",
         slots=list(ok = "logical",
                    account = "character",
                    venues = "character",
                    tickers = "character",
                    timestamp="data.frame"),
         prototype=list(ok=NA,
                        account=NA_character_,
                        venues=NA_character_,
                        tickers=NA_character_,
                        timestamp=data.frame(start=NA, end=NA)))
##TODO: this doesn't match the signature returned by timed_ functions
##TODO: this also needs a constructor function
setClass(Class="Timed",
         slots=list(timestamp="POSIXct", res="ANY"))
##' Extract the local ts from an object inheriting from class Time
##'
##' Simples.
##' @title get_time
##' @param obj 
##' @return a timestamp in POSIXct format
##' @author richie
##' @export
get_time <- function(obj) {
    stopifnot(is.list(obj))
    stopifnot(length(obj)==2)
    ts <- obj$timestamp
    ts
}
##' Extract the non-ts component of a Timed request
##'
##' More simples
##' @title get_result
##' @param obj 
##' @return 
##' @author richie
get_result <- function(obj) {
        stopifnot(is.list(obj))
        stopifnot(length(obj)==2)
        res <- obj[[2]]
        res
}
##' A get account function for objects inheriting from the trade class
##'
##' See above
##' @title account
##' @param obj 
##' @return an account; i.e. an alphanumeric character vector of length one
##' @author richie
##' @export
account <- function(object) {
    object@account

}
setMethod("account", signature("trades"),
          def = account)

setGeneric("account", function(object) {
  standardGeneric("account")
})
##' Generic function for extracting venue from an object inheriting from trades
##'
##' See above
##' @title venue
##' @param object an object inheriting from class trades
##' @return a character string containing the venue
##' @author richie
##' @export
venue <- function(object) {
    object@venues
}
setMethod("venue", signature("trades"),
          def = venue)
setGeneric("venue", function (object) {
    standardGeneric("venue")
})

##' A ticker method for objects inheriting from trades
##'
##' Currently this will fail to be useful if there are multiple tickers on the level. This has not happened yet, but probably will soon
##' @title ticker
##' @param object an object inheriting from class trades
##' @return a length one character vector containing the stock(s) traded on the level
##' @author richie
##' @export
ticker <- function(object) {
    object@tickers
}
setMethod("ticker", signature("trades"),
          def = ticker)
setGeneric("ticker", function (object) {
    standardGeneric("ticker")
})
##TODO: can this be generalised to handle quotes also
##TODO: why do we need seperate functions for bids and asks?
##TODO: use parse_ts
##' Function to extract the current bids from an orderbook object
##'
##' See above
##' @title get_bids
##' @param object an orderbook object
##' @return a dataframe containing the bids on the orderbook
##' @author richie
##' @export
get_bids <- function(object) {
    if(dim(na.omit(object@bids))[1]==0) {
        return(data.frame(price=NA, qty=NA, isBuy=NA, ts=object@ymdhms))
    } else {
        bids <- object@bids
        ts <- object@ymdhms
        bids[,"ts"] <- rep(ts, length=nrow(bids))
        bids
    }
}
##TODO: actually use this function
##TODO: might be easier to generalise this across bids and asks
bidsummary <- function(bids) {
    bids2 <- bids %>%
        group_by(price) %>%
        summarise(qty=sum(qty, na.rm=TRUE)) %>%
        arrange(desc(price)) %>%
        mutate(cum_qty=cumsum(qty))
    print(bids2)
}
## setMethod("bids", signature("orderbook"),
##           def = bids)
##TODO: use parse_ts here
##' extract the asks component of an orderbook
##'
##' See above
##' @title get_asks
##' @param object 
##' @return a dataframe containing the current asks from the orderbook obect
##' @author richie
##' @export
get_asks <- function(object) {
    if(dim(na.omit(object@asks))[1]==0) {
        return(data.frame(price=NA, qty=NA, isBuy=NA, ts=object@ymdhms))
    } else {
        asks <- object@asks
        ts <- object@ymdhms
        asks[,"ts"] <- rep(ts, length=nrow(asks))
        asks
    }
}
## setMethod("asks", signature("orderbook"),
##           def = asks)


##TODO: should level class include level status also?
setClass(Class="level",
                  slots = list(id="integer",
                               instructions="list",
                               seconds_per_trading_day="integer"
                               ), contains="trades")
##' A function to parse a response and convert the content to an object of class level
##'
##' See above
##' @title level
##' @param response 
##' @return a level object
##' @author richie
##' @export
level <- function(response) {
    if(class(response)=="response") {
        resp <- parse_response(response)
    }
    else {
        resp <- response
    }
    if(resp$ok) {
    lev <- new("level", ok=resp$ok,
                 id=resp$instanceId,
                 account=resp$account,
                 instructions=resp$instructions,
                 tickers=resp$tickers,
                 venues=resp$venues,
                 seconds_per_trading_day=resp$secondsPerTradingDay
                 )
    return(lev)
    }
    else {
        message("Something went wrong with the network call")
    }
}
setClassUnion("List", members=c("list", NULL))
setClass("status",
                   contains = "trades",
         slots = c(flash="List",
                   done="logical",
                   state="character",
                   details="List"))
setClass("orderbook",
         slots = list(ymdhms="POSIXt",
                      bids="data.frame",
                      asks="data.frame"), contains="trades")
##TODO: decide whether or not to use these, and either remove or expand based on that decision
setMethod("==",
    signature(e1 = "orderbook", e2="orderbook"),
    function (e1, e2)  {
        if(all(is.na(e1@bids) & is.na(e2@bids)))
        if(all.equal(e1@bids, e2@bids) &
           all.equal(e1@asks, e2@asks)) TRUE else FALSE
    }
    )
##TODO: lastTrade and quoteTime should be a POSIXct object
setClass("quote",
         slots=list(bid="integer",
                    ask="integer",
                    bidSize="integer",
                    askSize="integer",
                    bidDepth="integer",
                    askDepth="integer",
                    last="integer",
                    lastSize="integer",
                    lastTrade="character",
                    quoteTime="character"),
         prototype=prototype(ok=NA,
                    venue=NA_character_,
                    symbol=NA_character_,
                    bid=NA_integer_,
                    bidSize=NA_integer_,
                    askSize=NA_integer_,
                    bidDepth=NA_integer_,
                    askDepth=NA_integer_,
                    last=NA_integer_,
                    lastSize=NA_integer_,
                    lastTrade=NA_character_,
                    quoteTime=NA_character_),
         contains="trades")
##TODO: figure out if I need these
setMethod("==",
    signature(e1 = "quote", e2 = "quote"),
    function (e1, e2) 
    {
        ifelse(e1@bid == e2@bid &
               e1@bidSize==e2@bidSize &
               e1@askSize==e2@askSize &
               e1@bidDepth==e2@bidDepth &
               e1@askDepth==e2@askDepth &
               e1@last==e2@last &
               e1@lastSize==e2@lastSize &
               e1@lastTrade==e2@lastTrade, TRUE, FALSE)
    }
    )
##TODO: use this or remove it, definitely needs a timestamp
setClass(Class="order",
         representation=list(
             direction = "character",
                    price = "integer",
                    qty = "integer",
             ordertype = "character"), contains="trades")
setClass(Class="order-response",
         representation = list (original_qty="integer",
                                id = "integer",
                                ts = "POSIXt",
                                fills = "list",
                                total_filled = "integer",
                                open = "logical"), contains="order")
##TODO: remove one of order or new_order
##' create order-response object
##'
##' returns the object
##' @title order
##' @param response 
##' @return an order-response object
##' @author richie
### @export
order <- function(response) {
    ord <- new("order-response",
               ok=response$ok,
               account=response$account,
               venues=response$venue,
               tickers=response$symbol,
               ordertype=response$ordertype,
               price=response$price,
               qty=response$qty,
               id=response$id,
               ts=response$ts,
               fills=response$fills,
               total_filled=response$totalFilled,
               open=response$open)
    return(ord)
}
setMethod("as.data.frame",
    signature(x = "order-response"),
    function (x, row.names = NULL, optional = FALSE, ...) 
    {
        res <- data.frame(ok=x@ok,
               account=x@account,
               venues=x@venues,
               tickers=x@tickers,
               ordertype=x@ordertype,
               price=x@price,
               qty=x@qty,
               id=x@id,
               ts=x@ts,
               fills=x@fills,
               total_filled=x@total_filled,
               open=x@open,
               start=x@timestamp$start,
               end=x@timestamp$en)
    }
)
setClass(Class="Position", slots = list(
                               total_sold = "integer",
                               total_bought = "integer",
                               cash = "numeric",
                               current_position = "integer",
                               net_account_value = "numeric",
                               open_orders = "list",
                               fills = "list"),
         prototype = list(
             total_sold = 0L,
             total_bought = 0L,
             cash = 0,
             current_position = 0L,
             net_account_value = 0,
             open_orders = list(),
             fills = list()),
         contains="trades")
##' Create an order-response object
##'
##' See above
##' @title new_order
##' @param response an response to place_order
##' @return an object of class order response
##' @author richie
##' @export
new_order <- function(response) {
    ## stopifnot(class(response) == "response")
    ## p <- parse_response(response)
    p <- response
    ord <- new("order-response",
               ok=p$ok,
               account = p$account,
               tickers = p$symbol,
               venues = p$venue,
               direction = p$direction,
               qty = p$qty,
               price = p$price,
               ordertype = p$orderType,
               ts = lubridate::ymd_hms(p$ts),
               id = p$id,
               fills = p$fills,
               total_filled = p$totalFilled,
               open = p$open,
               timestamp = data.frame(start=p$start, end=p$end))
    ord
}
##' create a function that returns a component of an object
##'
##' Useful for making getters It seems like this won't work in its current form. I really want to get this idea working though
##' @title get_component
##' @param x an object
##' @param component the name of the slot to extract
##' @return a function which will get that slot from that object
##' @author richie
##' @export
get_component <- function(x, component) {
    res <- function (x) x[[component]]
    res
}
##TODO: write a quote method and make this generic
##' @export
spreads.orderbook <- function(orderbook) {
    bids <- orderbook@bids
    asks <- orderbook@asks
    spread <-  min(asks$price) - max(bids$price)
    spread
}
##TODO: see if this can be removed, far too specific
##' utility function for ensuring that bid/ask dfs do not return null
##' See description
##' @title df_or_null
##' @param order a list based including either a df of bids or asks
##' @param component either bid or ask
##' @return a new dataframe containing NA if component is null, else component
##' @author richie
##' @export
df_or_null <- function(order, component) {
    if(is.null(order[[component]])) {
        order[[component]] <- data.frame(price=NA, qty=NA, isBuy=NA)
    }
    order
}
##TODO: fix time function
##TODO: handle NULL in class instatiation
##' Function that creates a new orderbook object
##' wrapper around a call to "new" that performs null removals and other checks
##' @title orderbook
##' @param order a response from the API as a named list containing all fields or NULL
##' @return an orderbook object
##' @author richie
##' @export
orderbook <- function(order) {
    ###ugh, this is horrible. reassignment to the same name?
    order <- df_or_null(order, "bids")
    order <- df_or_null(order, "asks")
    if(is.null(order$error)) {
        tsparsed <- lubridate::ymd_hms(order$ts)
    }
    if(is.null(order$error)) {
    orderbook <- with(order,
         new("orderbook",
             ok=ok,
             venues=venue,
             tickers=symbol,
             ymdhms=tsparsed,
             bids=bids,
             asks=asks,
             timestamp=timestamp))
    orderbook
    }
}
##' An as.data.frame method for class orderbook
##'
##' See above
##' @title as.data.frame.orderbook
##' @param orderbook 
##' @return a dataframe containing all fields from the orderbook
##' @author richie
##' @export
as.data.frame.orderbook <- function (x, row.names = NULL, optional = FALSE, ...) {
    ordslots <- c("ymdhms", "bids", "asks", "ok", "account", "venues", "tickers")
    ##subtract two for bid and asks, add 3 for the cols of ob@bids/@asks
    ordbids <- get_bids(x)
    ordasks <- get_asks(x)
    time <- x@ymdhms
    names <- c("time", names(ordbids))
    bidask <- rbind(ordbids, ordasks)
    times <- rep(time, nrow(bidask))
    bidasktime <- cbind(times, bidask)
    dfs <- as.data.frame(bidasktime)
    dfs
}
setMethod("as.data.frame",
          signature(x = "orderbook"),
          def=as.data.frame.orderbook)
##' Creates an object of type quote from a named list returned by parse_response
##'
##' See above
##' @title new_quote
##' @param quote 
##' @return an S4 object of type quote
##' @author richie
##' @export
new_quote <- function(quote) {
    q <- new("quote")
    q@ok <- ifelse(!is.null(quote$ok), quote$ok, q@ok)
    q@venues <- quote$venue
    q@tickers <- quote$symbol
    q@bid <- ifelse(!is.null(quote$bid), as.integer(quote$bid), 0L)
    q@ask <- ifelse(!is.null(quote$ask), as.integer(quote$ask), 0L)
    q@bidSize <- ifelse(!is.null(quote$bidSize), as.integer(quote$bidSize), 0L)
    q@askSize <- ifelse(!is.null(quote$askSize), as.integer(quote$askSize), 0L)
    q@bidDepth <- ifelse(!is.null(quote$bidDepth), as.integer(quote$bidDepth), 0L)
    q@askDepth <- ifelse(!is.null(quote$askDepth),  as.integer(quote$askDepth), 0L)
    q@last <- ifelse(!is.null(quote$last), quote$last, NA_integer_)
    q@lastSize <- ifelse(!is.null(quote$lastSize), quote$lastSize, NA_integer_)
    q@lastTrade <- ifelse(!is.null(quote$lastTrade),
                          (quote$lastTrade), NA_character_)
    q@quoteTime <- ifelse(!is.null(quote$quoteTime),
                          (quote$quoteTime), NA_character_)
    q@account <- "MISSING"
    q@timestamp <- quote$timestamp
    q
}
##' An as.data.frame method for quote
##'
##' See above
##' @title as.data.frame.quote
##' @param quote 
##' @return a vector containing the elements of the quote
##' @author richie
##' @export
as.data.frame.quote <- function (x, row.names = NULL, optional = FALSE, ...) {
    res <- data.frame(
        x@bid ,
        x@ask,
        x@bidSize ,
        x@askSize ,
        x@bidDepth ,
        x@askDepth,
        x@last,
        x@lastSize ,
        x@lastTrade ,
        x@quoteTime,
        x@ok,
        x@account,
        x@venue ,
        x@symbol
    )
    }
setMethod("as.data.frame",
    signature(x = "quote"),
    definition=as.data.frame.quote
)
##' A show method for quote objects
##'
##' See above
##' @title show.quote
##' @param quote 
##' @return called for side effects
##' @author richie
##' @export
show.quote <- function(quote) {
    print(unlist(quote))
}
##' A show method for trade objects
##'
##' See above
##' @title show.trade
##' @param trade 
##' @return called for side effects
##' @author richie
##' @export
show.trade <- function(trade) {
    print(data.frame(account=trade$account,
                     venue=trade$venue,
                     ticker=trade$symbol))
}
setGeneric("show", def=show.quote)
##' A summary method for objects of class orderbook
##'
##' See above
##' @title summary.orderbook
##' @param object 
##' @param ... 
##' @return a dataframe containing prices and cumulative_qty for either bids or asks
##' @author richie
##' @export
summary.orderbook <- function(object, type=c("bids", "asks")) {
    bids <- object@bids
    asks <- object@asks
    bids2 <- bids %>%
    dplyr::group_by(price) %>%
    dplyr::summarise(qty=sum(qty)) %>%
        dplyr::arrange(desc(price))  %>%
        dplyr::mutate(cum_qty=cumsum(qty))
    asks2 <- asks %>%
            dplyr::group_by(price) %>%
    dplyr::summarise(qty=sum(qty)) %>%
        dplyr::arrange(price)  %>%
        dplyr::mutate(cum_qty=cumsum(qty))
    list(bids=bids2, asks=asks2,
         ratio=max(bids2$cum_qty) / max(asks2$cum_qty))
    if(type=="bids")
        return(bids2)

    else {
        return(asks2)
    }
    
}
setMethod("summary",
          signature(object = "orderbook"),
          def=summary.orderbook)

##' a bids function for class orderbook
##'
##' See above
##' @title bids.orderbook
##' @param orderbook 
##' @return a dataframe containing bids 
##' @author richie
##' @export
bids.orderbook <- function(orderbook) {
    bids <- orderbook@bids
    t2 <-   bids %>% summarise(value=sum(price), qty=sum(qty), rows=n(), std=sd(price))
}
##' A print method for position objects
##'
##' See above
##' @title print.Position
##' @param x a position argument
##' @param ... further arguments 
##' @return a represenation of the position object
##' @author richie
##' @export
print.Position <- function (x, ...)
    {
        cur_pos <- x@total_bought - x@total_sold
        print(data.frame(
            sold=x@total_sold,
            bought=x@total_bought,
            position=cur_pos,
            cash=x@cash/100))
    }
setMethod("print",
    signature(x = "Position"),
    def=print.Position
    )
##' Call for level status, return object of class status
##'
##' See above
##' @title as_status
##' @param level a level object
##' @param apikey an APIKEY, for none use ""
##' @return an S4 object of class status
##' @author richie
##'@export
as_status <- function(level, apikey) {
    stat <- level_status(level, apikey)
    browser()
    statp <- parse_response(stat)
    if(is.null(statp$flash)) {
        statp$flash <- list()
    }
    stat2 <- new("status",
                 ok= statp$ok,
                 done = statp$done,
                 state = statp$state,
                 details = statp$details,
                 flash = statp$flash)
    return(stat2)
}
                 
                 
##' Calls get_orderbook, and returns an orderbook object
##'
##' wraps calling the API, converting to list and then to an object of class orderbook
##' @title as_orderbook 
##' @param venue a venue
##' @param stock a stock
##' @return an orderbook object
##' @author richie
##' @export
as_orderbook <- function(venue, stock) {
    start <- get_time()
    res <- stockfighterr::get_orderbook(venue, stock)
    end <- get_time()
    timestamp <- make_timestamp(start, end)
    resp <- stockfighterr::parse_response(res)
    resp$timestamp <- timestamp
    respo <- orderbook(resp)
}
##' get a quote and return a quote object
##'
##' wraps the get_quote, parse_response and new_quote functions
##' @title as_quote
##' @param venue venue
##' @param stock stock
##' @return a quote object for the given venue/stock
##' @author richie
##' @export
as_quote <- function(venue, stock) {
    strt <- get_time()
    q <- get_quote(venue, stock)
    end <- get_time()
    timestamp <- make_timestamp(strt, end)
    qp <- parse_response(q)
    qp$timestamp <- timestamp
    qq <- new_quote(qp)
}
                                       
##' Call get all orders and return an orderlist object
##'
##' Not entirely implemented yet
##' @title as_orderlist
##' @param level the current level
##' @param apikey X-Starfighter-Authorization
##' @param id 
##' @return an orderlist object
##' @author richie
##' @export
as_orderlist <- function(level, apikey) {
    account <- account(level)
    venue <- venue(level)
    start <- get_time()
    allord <- get_all_orders(venue, account, apikey)
    end <- get_time()
    ts <- make_timestamp(start, end)
    allordp <- parse_response(allord)
    ord <- allordp$orders
    
    if(length(ord)>0 && dim(ord)[1] > 0) {
        ord[,"start"] <- ts[1]
        ord[,"end"] <- ts[2]
        ordlist <- apply(ord, 1, new_order)
    }
    else {
        ordlist <- NULL
    }
}

