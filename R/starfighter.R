##tinychange
##' Convert factor variable to numeric
##'
##' Mostly because I dislike StringsAsFactors=FALSE
##' @title as.fnumeric
##' @param x a factor variable
##' @return a numeric variable
##' @author richie
##'@export
as.fnumeric <- function(x) {
    as.numeric(as.character(x))
}
##' Misguided parsing function
##' See above
##' @title parse_quote
##' @param quote a quote object returned from the Stockfighter API
##' @return a matrix maybe?
##' @author richie
parse_quote <- function(quote) {
    quotecontents <- sapply(quote, function(x) content(x))
    numrows <- length(quotecontents)
    numcols <- max(sapply(quotecontents, length))
    resmat <- matrix(data=NA, nrow=numrows, ncol=numcols)
    nameextractors <- tolower(names(quotecontents[[1]]))
    
    for(i in 1:length(nameextractors)) {
        fun <- get_component(component=nameextractors[i])
        browser()
        part <- fun(x=quotecontents)
        resmat[1:length(part),i] <- part
    }
    resmat
}  



##' repeat a function n times, storing the results in a list
##'
##' Should really generalise this to also allow for while loops, as it would simplify the monitor function greatly
##' @title repeat_call
##' @param times A positive integer equal to or greater than 1. The number of times the function is called.
##' @param call a function. Use of function(x) notation will be required for most functions
##' @param sleep number of seconds to sleep between iterations
##' @return a list containing the results of repeatedly evaluating call. 
##' @author richie
##' @export
repeat_call <- function(times, call, sleep=0,print=FALSE ) {
    reslist <- vector(mode="list", length=times)
    fun <- match.fun(call)
    for(i in 1:times) {
        if(print) {
            print(i)
        }
        reslist[[i]] <- fun()
        if(sleep) {
            Sys.sleep(sleep)
        }
    }
    reslist
}
##' Convert a variable of factor class to numeric
##'
##' See above
##' @title as.fnumeric
##' @param x a factor variable that represents numbers
##' @return a numeric vector
##' @author richie
as.fnumeric <- function(x) {
    as.numeric(as.character(x))
}
##TODO: check if this is used
response_to_df <- function(parsed_response) {
    parsedmat <- do.call("rbind", parsed_response)
    parsed.df <- sapply(as.data.frame(parsedmat), unlist)
    parsed.df
}
##TODO:merge this with wss shell to create a monster!!!!(or at least reduce duplication)
get_tickertape <- function(account, venue) {
    ##sadface, Curl (and thus httr) doesn't support websockets
    base_url_wss <- "wss://api.stockfighter.io/ob/api/ws/"
    url <- paste(base_url_wss, account, "/venues/", venue, "/tickertape", sep="")
    res <- httr::GET(url, add_headers("X-Starfighter-Authorization"=apikey))
}
##DONE: replace with lubridate::parse_date_time(t, "YmdhmOS") and call to set digits.secs=6
##TODO: use this function throughout code (figure out how to get Sys.time to work
##' An almost entirely unnecessary parsing function
##'
##' See above
##' @title parse_ts
##' @param order an object that has a ts type column
##' @param timestamp the column to convert (needs options(fractional.seconds=7+) to be useful
##' @return the object with the time in POSIXct with 6fractional seconds
##' @author richie
parse_ts <- function(order, timestamp) {
    myts <- withr::with_options(c(digits.secs=6),
        code=lubridate::parse_date_time(order[[timestamp]], "YmdhmOS6"), tz="UTC")

}
##TODO: change to as.data.frame method
##' Convert a list of quote objects to a dataframe
##'
##' Lists of quote objects are returned by the monitor function. 
##' @title qlist_to_df
##' @param quotelist a list of quote objects
##' @return a dataframe containing the quote data, with NA for any value that was NULL in the response
##' @author richie
##' @export
qlist_to_df <- function(quotelist) {
    stopifnot(class(quotelist[[1]])=="quote")
    matrows <- length(quotelist)
    matcols <- length(slotNames(quotelist[[1]]))
    resmat <- matrix(data=NA, nrow=matrows, ncol=matcols)
    for(i in 1:length(quotelist)) {
        resmat[i,] <- as.vector.quote(quotelist[[i]])
    }
    resmat
    colnames(resmat) <- slotNames(quotelist[[1]])
    resdf <- as.data.frame(resmat)
    ## resdf <- dplyr::mutate(resdf, ok=as.logical(ok),
    ##                 bid=as.fnumeric(bid),
    ##                 bidSize=as.fnumeric(bidSize),
    ##                 askSize=as.fnumeric(askSize),
    ##                 bidDepth=as.fnumeric(bidDepth),
    ##                 last=as.fnumeric(last),
    ##                 lastSize=as.fnumeric(lastSize),
    ##                 last_trade=lubridate::ymd_hms(lastTrade),
    ##                 quote_time=lubridate::ymd_hms(quoteTime))
    resdf
}

##TODO:merge or delete
##' An attempt at using the websocket endpoints.
##'
##' it appears that httr and curl don't support wss endpoints. I suspect that with some care, an upgrade request can be sent as per the spec (which allows for a HTTP response). This turned out to be true, I can get the server upgrade response, but I can't seem to parse it correctly (yet). This attempt was doomed, but I can call out to an external program and get the data, which is better than nothing

##' @title get_tickertape
##' @param account 
##' @param venue 
##' @param ... 
##' @return nothing, as of yet
##' @author richie
get_tickertape <- function(account, venue, ...) {
    base_ws<- "wss://api.stockfighter.io/ob/"
    url <- paste(base_url, "ws/", account, "/venues/", venue, "/executions", sep="")
    res <- httr::GET(url, add_headers(api_key=apikey,
                                      "Upgrade"="websocket",
                                      "Connection"="Upgrade",
                                      "Sec-Websocket-Version"="13",
                                      "Sec-Websocket-Key"="e23vsOnh1QGrwNI7ahsr7w==" ), ...)
    return(res)
}
##TODO: probably delete
##' Monitor a stockfighter level
##'
##' Runs a while loop around a particular level
##' should probably generalise and re-factor this
##' @title monitor
##' @param venue 
##' @param stock 
##' @param level 
##' @param name 
##' @return nothing, called for side effects
##' @author richie
##' @export
monitor <- function(venue, stock, level=level, name=name) {
    message("Stock is ", stock, " venue is ", venue, "\n")
    start_time <- Sys.time()
    ok <- TRUE
    ##this is total overkill, but hey
    ordlist <- vector(mode="list", length=2e6)
    quotelist <- vector(mode="list", length=2e5)
    i <- 1
    tryCatch({
        while(ok) {
            new_time <- Sys.time()
            cl <- parallel::makeForkCluster(nnodes=6)
            listvars <- rep(list(c(venue, stock)), 50)
            orders <-
                parallel::parLapply(cl,
                                    listvars,
                                    function (x)
                                        get_orderbook(venue=x[1], stock=x[2]))
            parallel::stopCluster(cl=cl)
            quote <- get_quote(venue=venue, stock=stock)
            ordlist[[i]] <- orders
            quotelist[[i]] <- quote
            cat("iteration at ", i, "\n")
            i <- i + 1
            ok <- content(orders[[length(orders)]])$ok
            end_time <- Sys.time()
            ## cat("time taken ", end_time-new_time, "\n")
            status <- change_instance(level, "")
            browser()
            parsed_status <- parse_response(status)
            if(parsed_status$state != "open") {
                break
            }
        }
    }, finally = {
        cat("reached finally", "\n")
        ## myorders <- get_all_orders(
        file.ord <- paste("orderlist_", args[1], "_", args[2], ".rda", sep="")
        file.quote <- paste("quotelist_", args[1], "_", args[2], ".rda", sep="")
        save(ordlist, file=file.ord)
        save(quotelist, file=file.quote)
        change_instance(level=level, "stop")
        end_time <- Sys.time()
        cat("Total time taken, ", end_time-start_time, "\n")
    })
    ordlist
}
##TODO: generalise for level-status object
level_3_stats <- function(level) {
    stat <- level_status(level=level)
    sp <- stat %>% parse_response()
    if(!is.null(sp$flash)) {
    nums <- stringr::str_extract_all(unlist(stat[["flash"]]), "\\$[0-9]+")
    cash <- nums[[1]][1]
    NAV <-  nums[[1]][2]
    stats <- c(cash=cash, NAV=NAV)
    stats
    }
    else {
        return(NA)
    }
}
##TDOD: generalise all this to_df conversions
##' Take a series of calls to get_orderbook, and return the results as a df
##'
##' See above. Will document the fields here later
##' @title parse_orderlist
##' @param orderlist a list of calls to get_orderbook 
##' @return a dataframe containing columns titled price, qty, isBuy, ok, venue, symbol, ymdhms (the timestamp) and milliseconds
##' @author richie
##' @export
parse_orderlist <- function(orderlist, parallel=TRUE, data.frame=TRUE) {
    if(!parallel) {
        odl2 <- orderlist[sapply(orderlist, function(x) !is.null(x))]
        odl3 <- odl2[sapply(odl2, function (x) class(x)!="try-error")]
        od.p <- lapply(odl3, parse_response)
        odlok <- od.p[sapply(od.p, function (x) x$ok==TRUE)]
        od.ob <- odlok %>% sapply(., orderbook)
        if(data.frame) {
        od.df <- lapply(od.ob, as.data.frame.orderbook)
        res <- do.call("rbind", od.df)
        
        return(res)
        }
        else {
            res <- od.ob
            return(res)
        }
    }
    else {
        odl2 <- orderlist[sapply(orderlist, function (x) !is.null(x))]
        od.p <- lapply(odl2, function (x) lapply(x, parse_response))
        odlok <- lapply(od.p, function (x) lapply(x, function (y) y$ok==TRUE))
        od.ob <- odlok %>% lapply(., function (x) sapply(x, function (y) orderbook(y)))
        od.df <- lapply(od.ob, function (x) lapply(x, function (y) as.data.frame.orderbook(y)))
        browser()
        res <- do.call("rbind", od.df)
    }
    res
}

##TODO: use this position, in updating state, no network access used
##' Some stuff
##'
##' Some more stuff
##' @title get_position
##' @param orders 
##' @return a dataframe containing the current position
##' @author richie
##' @export
get_position <- function(orders) {
    ord2 <- orders$orders
    if(sum(ord2$totalFilled)==0) {
        return(data.frame(direction=NA, spend2=NA, total_filled=0, ppu=0))
    }
    fills <- select(ord2, id, direction, fills, totalFilled)
    fills2 <- tidyr::unnest(fills)
    pos <- fills2 %>%
        dplyr::mutate_(spend=price*qty) %>%
        dplyr::group_by(direction) %>%
        dplyr::summarise(spend2=sum(spend),
                  total_filled=sum(totalFilled)) %>%
        dplyr::mutate(ppu=spend2/total_filled)
    pos
    
}

##TODO: remove network access keep logic
##' Currently just gets bid and ask price
##'
##' Don't think this is at the right level of abstraction
##' @title get_price_and_qty
##' @param orderbook an orderbook object
##' @return a list containing bidprice and askprice
##' @author richie
##' @export
get_price_and_qty <- function(orderbook) {
    bids.ord <- get_bids(orderbook)
    asks.ord <- get_asks(orderbook)
    bidna <- all(is.na(bids.ord))
    askna <- all(is.na(asks.ord))
    bothna <- all(bidna, askna)
    venue <- venue(orderbook)
    tick <- ticker(orderbook)
    if (bothna) {
        message("absolutely no market, monitoring again")
        bids <- get_bid(venue, tick)
        return(list(bids[1], bids[2], (bids[2] - bids [1])))
        }
    
    if(bidna) {
        spread <- 20
        message(paste("no curent bids, making market"))
        askprice <- min(asks.ord$price)
        bidprice <- ceiling(askprice - (spread / 4))
        return(list(bidprice, askprice, spread))
        }
    if(askna) {
        spread <- 20
          bidprice <- max(bids.ord$price) + 1
          askprice <- floor(bidprice + (spread / 4))
          return(list(bidprice, askprice, spread))
    }

    message("got to calculating spread")
    if(!bothna) {
    spread <- min(asks.ord$price) - max(bids.ord$price)  
    myspread <- floor(spread * 0.8)
    message(paste("spread is", myspread))
    bidprice <- ceiling(max(bids.ord$price) + (myspread / 4))
    askprice <- floor(min(asks.ord$price) - (myspread / 4))

    return(list(bidprice, askprice, spread))
    }
}

##TODO: fold into summary function
##' Honestly think I'll fold this into the summary function
##'
##' 
##' @title bid_ask_ratio
##' @param ord 
##' @return a list containing the bids, asks and the relative qty between them
##' @author richie
bid_ask_ratio <- function(ord) {
    bidsum <- summary(ord, type="bids")
    asksum <- summary(ord, type="asks")
    bid_to_ask_qty <- max(bidsum$cum_qty) / max(asksum$cum_qty)
    reslist <- list(b2a_qty=bid_to_ask_qty,
                    bids=bidsum, asks=asksum)
    reslist
    
}

##' iterate over a list of venues, returning the stocks available at each
##'
##' As above
##' @title get_all_tickers
##' @param venues a vector of venues 
##' @return a list of tickers available at each venue
##' @author richie
##'@export
get_all_tickers <- function(venues) {
    myticks <- list()
    for(i in 1:nrow(venues$venues)) {
        ven <- venues$venues$venue
        myticks[[i]] <- get_tickers(ven[i])
    }
    myticks
}
##TODO: handle how this interacts with the object structure
##' Returns f with timing added
##'
##' returns a function that will do the original work, and has an additional time component which gives the start and end times 
##' @title timed
##' @param f a function
##' @param ... other arguments to be passed to f
##' @return a function with a signature of ... which passes arguments to f
##' @author richie
##' @export
timed <- function(f, ...) {
    function(...) {
        ## dots <- as.list(substitute(list(...)))[-1L]
        ## args <- formals(f)
        ## names(dots) <- args
        start <- withr::with_options(c(digits.secs=6), lubridate::now(tzone="UTC"))
        res <- f(...)
        end <- withr::with_options(c(digits.secs=6), lubridate::now(tzone="UTC"))
        res <- list(time=data.frame(start=start, end=end), res)
    }
}
##' get a timestamp with 6 fractional seconds in specified timezone
##'
##' Use to get local time inside functions requiring network calls. This function defaults to UTC
##' @title time
##' @return a timestamp object
##' @author richie
##'@export
get_time <- function(tz="UTC") {
    instant <- withr::with_options(c(digits.secs=6), lubridate::now(tzone=tz))
}
##' Take the results of two calls to get_time, and return a dataframe containing the start and end of the interval
##'
##' Perhaps I should use the Interval class from lubridate?
##' @title make_timestamp
##' @param start the first instant
##' @param end the last instant
##' @return a data.frame containing start and end times
##' @author richie
##'@export
make_timestamp <- function(start, end) {
    res <- data.frame(start=start, end=end)
}
## TODO: does this even work still?
##' shell out to wss program to get either executions or tickertape
##'
##' Need to integrate this properly at some point
##' @title wss_shell
##' @param level the current level
##' @param type either tickertape (quotes) or executions 
##' @param location path for the wsclient executable
##' @return nothing, called for side effects
##' @author richie
##'@export
wss_shell <- function(level, type=c("tickertape", "executions"), location="./", ofile) {
    account <- account(level)
    venue <- venue(level)
    stock <- ticker(level)
    base_wss <-  "wss://api.stockfighter.io/ob/api/ws/"
    full_url <- paste0(base_wss, account, "/venues/", venue, "/", type, "/", "stocks/", stock)
    print(full_url)
    system(paste0(location, "wsclient ", full_url, "> ", ofile),  wait=FALSE)
}
##' Run information functions in separate threads to ensure timing accuracy
##'
##' This function should give a window into market state at a consistent time
##' @title state_of_market
##' @param level a level
##' @param apikey an X-Starfighter-Authorization token
##' @return a list containing an orderbook, a quote object and details of current orders on this level
##' @author richie
##'@export
##'@importFrom future '%<-%'
state_of_market <- function(level, apikey, timed=TRUE) {
    stopifnot(class(level)=="level")
    account <- account(level)
    venue <- venue(level)
    stock <- ticker(level)
    if(timed) {
        quote <- future::future(timed_quote(venue, stock))
        ord <- future::future(timed_orderbook(venue, stock))
        myorders <- future::future(timed_orderlist(level, apikey))
        status <- future::future(timed_status(level, apikey=apikey))
        } else {
            quote <- future::future(as_quote(venue, stock))
            ord <- future::future(as_orderbook(venue, stock))
            myorders <- future::future(as_orderlist(level, apikey))
            status <- future::future(level_status(level, apikey=apikey))
        }
    res <- list(orderbook=future::value(ord),
                quote=future::value(quote),
                myorders=future::value(myorders),
                status=future::value(status))
    names(res) <- c("orderbook", "quote", "myorders", "level_status")
    res
}

##TODO: use df conversion functions to handle this, index as a zoo class
##TODO: currently, this does nothing
##' Create or update an environment with market data
##'
##' This function uses global assignment and environments, so weird stuff is probably going to happen. (not right now it doesn't)
##' @title update_state
##' @param state an object returned by state_of_market
##' @return 
##' @author richie
##' @export
update_state <- function(current, previous) {
    if(is.null(previous)) {
        orderb <- purrr::map(state, "orderbook")
        ## b2a <- bid_ask_ratio(orderb)
    quote <- purrr::map(state, "quote")
    myorders <- purrr::map(state, "quote")
    status <- purrr::map(state, "level-status")
    res <- list(orderb, quote, myorders, status)
    }
}
##TODO: incorporate into update_state
##' Extract important fields from quote object
##'
##' To be used to build up market model
##' @title quote_stats
##' @param current current quote
##' @param previous previous output from quote stats.
##' @return a dataframe containing market information
##' @author richie
###@export
quote_stats <- function(current, previous) {
    if(is.null(previous)) {
        previous <- data.frame(bid = NA,ask = NA, last = NA,
                         ts = NA, start = NA, end = NA)
    }
    q <- current$quote[[2]]
    bid <- q@bid
    ask <- q@ask
    last <- q@last
    ts <- q@quoteTime
    start_time <- current$quote$time[1]
    end_time <- current$quote$time[2]
    res <- data.frame(bid = bid, ask = ask, last = last,
                      ts = ts,
                      start = start_time,
                      end = end_time)
    updatedf <- rbind(previous, res)
}
##' returns a dataframe containing all bids and asks
##'
##' Uses server time as index 
##' @title orderbook_stats
##' @param current the new orderbook
##' @param previous the previous orderbook
##' @return an update
##' @author richie
##' @export
orderbook_stats <- function(current, previous) {
    if (is.null(previous)) {
        bids <- get_bids(current[[2]])
        asks <- get_asks(current[[2]])
        orderb <- rbind(bids, asks)
    }
    else {
        bids <- get_bids(current[[2]])
        asks <- get_asks(current[[2]])
        curr <- rbind(bids, asks)
        if(class(previous)=="orderbook") {
            prevb <- get_bids(previous)
            preva <- get_asks(previous)
            prev <- rbind(prevb, preva)
        }
        else {
            prev <- previous
        }
        orderb <- rbind(prev, curr)
    }
}
##'@export
timed_orderbook <- timed(as_orderbook)
##'@export
timed_quote <- timed(as_quote)
##'@export
timed_orderlist <- timed(as_orderlist)
##'@export
timed_status <- timed(level_status)

full_orderbook <- function(orderbook) {
    for(i in seq_along(orderbook)) {
        if(i==1) {
            ord <- orderbook_stats(orderbook[i], previous=NULL)
        }
        else {
            ord <- orderbook_stats(orderbook[i], ord)
        }
        ord
    }
    ord
}
