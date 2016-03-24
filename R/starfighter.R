#tinychange
require(httr)
require(dplyr)
##' Convert factor variable to numeric
##'
##' Mostly because I dislike StringsAsFactors=FALSE
##' @title as.fnumeric
##' @param x a factor variable
##' @return a numeric variable
##' @author richie
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


## ok <- get_component(component="ok")
## symbol <- get_component(component="symbol")
## venue <- get_component(component="venue")
## bid <- get_component(component="bid")
## ask <- get_component(component="ask")
## bidsize <- get_component(component="bidSize")
## asksize <- get_component(component="askSize")
## biddepth <- get_component(component="bidDepth")
## askdepth <- get_component(component="askDepth")
## last <- get_component(component="last")
## lastsize <- get_component(component="lastSize")
## lasttrade <- get_component(component="lastTrade")
## quotetime <- get_component(component="quoteTime")

##man i really want this to work
##it doesn't though, suggesting I'm missing something subtle
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
repeat_call <- function(times, call, sleep=10) {
    reslist <- vector(mode="list", length=times)
    fun <- match.fun(call)
    for(i in 1:times) {
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
##' Get the "current" asking price for a stock
##'
##' This is overcomplicated and conflates the price to pay with 
##' @title get_price
##' @param venue 
##' @param ticker 
##' @param fudge 
##' @return a vector containing a fudged bid and a qty to trade with
##' @author richie
##' @export
get_bid <- function(venue, ticker, spread=100) {
    q <- get_quote(venue, ticker) %>% parse_response()
    bid <- q$bid
    ask <- q$ask
    last <- q$last
    print(c(bid, ask, last))
    while(all(is.null(bid) & is.null(ask) & is.null(last))) {
        browser()
        message("not even a last price")
        q2 <- get_quote(venue, ticker)
        qp <- parse_response(q2)
        bid <- qp$bid
        ask <- qp$ask
        last <- qp$last
    }
    if(is.null(bid) & is.null(ask)) {
        message("both missing")
        price <- last
        buyprice <- price - floor(spread / 4)
        sellprice <- price + floor(spread / 4)
    }
    if(is.null(bid) & !is.null(ask)) {
        message("bid missing")
        sellprice <- ask
        buyprice <- ask - floor(spread / 4)
    }
    if(is.null(ask) & !is.null(bid)) {
        message("ask missing")
        buyprice <- bid
        sellprice <- bid + floor(spread / 4)
    }
    if(!is.null(bid) & !is.null(ask)) {
        message("both present")
        spread <- ask - bid
        buyprice <- bid + floor(spread / 4)
        sellprice <- ask - floor(spread / 4)
    }
    if(is.null(buyprice) & is.null(sellprice)) {
        browser()
    }
    return(c(buyprice, sellprice))
}
##' More level specific functions!
##' See above
##' @title get_first_real_price
##' @param venue venue
##' @param stock ticker
##' @param fudge how much to increase the price by
##' @return a suggested bid and qty. 
##' @author richie
get_first_real_price <- function (venue, stock, fudge=0) {
    fudged_bid <- NA
    qty <- NA
    while(is.na(fudged_bid) | is.na(qty)) {
        orderinfo <- get_bid(content(get_quote(venue, stock)))
        fudged_bid <- orderinfo[1]
        qty <- orderinfo[2]
    }
    res <- c(fudged_bid, qty)
}
        


response_to_df <- function(parsed_response) {
    parsedmat <- do.call("rbind", parsed_response)
    parsed.df <- sapply(as.data.frame(parsedmat), unlist)
    parsed.df
}
get_tickertape <- function(account, venue) {
    ##sadface, Curl (and thus httr) doesn't support websockets
    base_url_wss <- "wss://api.stockfighter.io/ob/api/ws/"
    url <- paste(base_url_wss, account, "/venues/", venue, "/tickertape", sep="")
    res <- httr::GET(url, add_headers("X-Starfighter-Authorization"=apikey))
}

##' Calculate the spreads between bid and ask from an orderbook object
##'
##' Doesn't really work right now
##' @title spreads.orderbook
##' @param orderbook an orderbook object
##' @return a data.frame containing the prices and differences between them
##' @author richie


        

##' An almost entirely unnecessary parsing function
##'
##' See above
##' @title parse_ts
##' @param order an object that has a ts type column
##' @param timestamp the column to convert (needs options(fractional.seconds=7+) to be useful
##' @return a dataframe containing the ymdhms and fractional second components
##' @author richie
parse_ts <- function(order, timestamp) {
    myts <- lubridate::ymd_hms(as.character(order[[timestamp]]))
    split <- unlist(strsplit(as.character(order[[timestamp]]), ".", fixed=TRUE))
    millis <- stringr::str_extract(split[2], "[0-9]+")
    df <- return(data.frame(ymdhms=myts, milli=as.numeric(millis)))

}
get_component <- function(level, component) {
    levelcon <- parse_response(level)
    component <- unlist(levelcon[[component]])
    component
}




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
    matcols <- length(getSlots("quote"))
    resmat <- matrix(data=NA, nrow=matrows, ncol=matcols)
    for(i in 1:length(quotelist)) {
        resmat[i,] <- as.vector.quote(quotelist[[i]])
    }
    resmat
    colnames(resmat) <- names(getSlots("quote"))
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
##' Convert a quote object to a vector
##'
##' See above
##' @title as.vector.quote
##' @param quote an object of class quote
##' @return a vector containing the slots of the quote object
##' @author richie

##' An attempt at using the websocket endpoints.
##'
##' it appears that httr and curl don't support wss endpoints. I suspect that with some care, an upgrade request can be sent as per the spec (which allows for a HTTP response). This turned out to be true, I can get the server upgrade response, but I can't seem to parse it correctly (yet)

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
##' Monitor a stockfighter level
##'
##' Runs a while loop around a particular level
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
            ## print(orders[length(orders)])
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
##' Top level function for buying and selling according to rules
##'
##' See above
##' @title market_make
##' @param level a level object
##' @param ordertype the type of orders to place
##' @return a list containing the orders placed
##' @author richie
market_make <- function(level, ordertype="limit", qty=NULL) {
    if(level=="TEST") {
        account <- "EXB123456"
        venue <- "TESTEX"
        ticker <- "FOOBAR"
        balance <- NULL
    }
    else {
    account <- get_component(level, "account")
    venue <- get_component(level, "venues")
    ticker <- get_component(level, "tickers")
    balance <- get_component(level, "balances")
    }
    ## browser()
    buys <- NA
    sells <- NA
    prices <- c(buys, sells)
    while(any(is.na(prices))) {
    orders <- get_orderbook(venue, ticker)
    parsed <- orderbook(parse_response(orders))
    if(level!="TEST") {
    status <- level_status(level=level)
    status.p <- parse_response(status)
    if(!is.null(status.p$flash)) {
        flash <- status.p$flash
        print(flash)
    }
    }
    if(is.na(parsed@bids$price)) {
        next
    }
    buys <- ceiling(min(parsed@bids$price))
    sells <- floor(max(parsed@asks$price))
    buy_qty <- floor(min(parsed@bids$qty))
    sell_qty <- floor(min(parsed@asks$qty))
    prices <- c(buys, sells)
    qties <- c(buy_qty, sell_qty)
    cat(prices, "\n")
    }
    directions <- c("buy", "sell")
    if(is.null(qty)) {
        qty <- 1
    }
    reslist <- list()

    
    stat <- level_status(level=level)
    sp <- stat %>% parse_response()
    if(!is.null(sp$flash)) {
    nums <- stringr::str_extract_all(unlist(stat[["flash"]]), "\\$[0-9]+")
    cash <- nums[[1]][1]
    NAV <-  nums[[1]][2]
    }
}
stupid_loop <- function(ordlist) {
    reslist <- vector(mode="list", length=length(ordlist))
    for(i in 1:length(ordlist)) {
        reslist[[i]] <- as.data.frame.orderbook(ordlist[[i]])
    }
    reslist
}
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
##' Get spreads and various orderbook related stuff
##'
##' 
##' @title get_spreads
##' @param venue 
##' @param stock 
##' @return a list containing an orderbook, the spread and the minqty available 
##' @author richie
##' @export
get_spreads <- function(venue, stock, spread) {
    bids <- data.frame(price=NA, qty=NA, isBuy=NA)
    asks <- data.frame(price=NA, qty=NA, isBuy=NA)
    nulls <- -1
    spread <- NA
    while(is.na(spread)) {
        nulls <- nulls + 1
        cat("There have been ", nulls, " nulls", "\n")
        orderbook <- get_orderbook(venue=venue, stock=stock) %>%
            parse_response() %>%
            orderbook()
        bids <- orderbook@bids
        asks <- orderbook@asks
        spread <- min(asks$price, na.rm=TRUE)- max(bids$price, na.rm=TRUE)
        bidqty <- bids$qty
        askqty <- asks$qty
        qty <- min(min(bidqty), min(askqty), na.rm=TRUE)
        ob <- list(orderbook=orderbook, spread=spread, minqty=qty)
    }
    return(ob)
}
##' Some bullshit
##'
##' More bullshit
##' @title trade
##' @param orderbook an orderbook object
##' @param qty the qty to trade
##' @param prices the price to trade at
##' @return a list containing the buy and sell trades, as well as an orderbook
##' @author richie
##' @export
trade <- function(level=NULL, qty=NULL) {
    account <- account(level)
    venue <- venue(level)
    stock <- ticker(level)
    qty <- qty
    prices <- get_bid(venue, stock)
    message("buying at ", prices[1], "\n",
            "Selling at ", prices[2], "\n")
    directions <- c("buy", "sell")
    reslist <- list()
    for(i in 1:length(prices)) {
        myord<- create_order(account=account,
                           venue=venue,
                           stock=stock,
                           price=prices[i],
                           qty=qty,
                           direction=directions[i],
                           ordertype="limit")
        placed <- place_order(venue=venue, stock=stock, body=myord, apikey=apikey)
        if(httr::status_code(placed)==200) {
            reslist[[i]] <- placed %>% parse_response()
        }
            else {
                message(placed$error)
            }
    }
    names(reslist) <- directions
    return(reslist)
    }
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
        mutate(spend=price*qty) %>%
        group_by(direction) %>%
        summarise(spend2=sum(spend),
                  total_filled=sum(totalFilled)) %>%
        mutate(ppu=spend2/total_filled)
    pos
    
}

##' Make a trade of size qty/split in the form of multiple orders
##'
##' 
##' @title place_many_orders
##' @inherit create_order
##' @param account 
##' @param venue 
##' @param stock 
##' @param price 
##' @param qty 
##' @param direction 
##' @param ordertype 
##' @param split 
##' @return a list containing the responses from each call
##' @author richie
##' @export
place_many_orders <- function(account, venue, stock,
                              price, qty, direction,
                              ordertype, split) {
    minqty <- floor(qty/split)
    prices <- ifelse(rnorm(1)>0, price + 5, price -5)
    rep(prices, times=split)
    ord <- create_order(account=account,
                        venue = venue,
                        stock = stock,
                        price = price,
                        qty = minqty,
                        direction = direction,
                        ordertype=ordertype
                        )
    
    ords <- rep(list(ord), times=split)
    cl <- parallel::makeForkCluster(nnodes=split)
    res <- parallel::parLapply(cl, X=ords, fun=function (x)
        place_order(x$venue, x$stock, body=x, apikey=apikey))
    return(res)
}
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
##' Update a position object based on orders
##'
##' See above
##' @title update_position
##' @param position An object of class position
##' @return an updated object of class position
##' @author richie
##' @export
update_position <- function(position, apikey) {
    stopifnot(class(position)=="Position")
    venue <- venue(position)
    tick <- ticker(position)
    account <- account(position)
    allord <- get_all_orders(venue, account, apikey) %>%
        parse_response()
    if(length(allord$orders)>0) {
        ords <- allord$orders %>%
            group_by(direction) %>%
            summarise(filled=sum(totalFilled), price=sum(price, na.rm=TRUE))
        filled <- ords$filled
        price <- ords$price
        cash <- price[2] - price[1]
        new_pos <- position
        new_pos@total_sold <-filled[2] 
        new_pos@total_bought <- filled[1]
        new_pos@cash <- cash
        new_pos@fills <- allord$orders$fills
        open_ord <- filter(allord$orders, isTRUE(open))
        new_pos@open_orders <- open_ord
        
        return(new_pos)
    }
    return(position)
}
##' Function to sell/buy inventory to reduce holdings to zero
##'
##' Uses ioc orders to prevent bots from catching on
##' @title clear_position
##' @param level the current level
##' @param position an object of class position
##' @return a new position object
##' @author richie
##' @export
clear_position <- function(level, position, apikey, tolerance) {
    account <- account(level)
    venue <- venue(level)
    stock <- ticker(level)
    sumpos <- print(position)
    while(abs(sumpos$position) >= tolerance) {
        if(sumpos$position > 0) {
            compsell <- 0
            bids <- get_bid(venue, stock)
            pricesell <- bids[2] + 1
            
            message("clearing position at  ", pricesell, "\n")
            sell <- create_order(account, venue, stock, price=pricesell, qty=sumpos$position, direction="sell", ordertype="ioc")
            sellp <-
                place_order(venue, stock,
                            body=sell, apikey=apikey) %>%
                parse_response()
            if(sellp$totalFilled==0) {
                compsell <- compsell + 1
            }
            position <- update_position(position, apikey)
            sumpos <- print(position)
            print(position)
        }
    
        if(sumpos$position<0) {
            compbuy <- 0
            bidsdf <- get_bid(venue, stock)
            pricebuy <- bidsdf[1] + 1
            message("clearing position at  ", pricebuy, "\n")
            buy <- create_order(account, venue, stock, price=pricebuy, qty=(sumpos$position)*-1, direction="buy", ordertype="ioc")
            buyp <-
                place_order(venue, stock,
                            body=buy, apikey=apikey) %>%
                parse_response()
                if(buyp$totalFilled==0) {
                    compbuy <- compbuy + 1
                }
            position <- update_position(position, apikey)
            sumpos <- print(position)
            print(position)
        }
    }
    return(position)
}
