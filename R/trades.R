##TODO: parallelise this along with cancelling orders
##TODO: use make_order to simplify
##' Places a pair of buy and sell orders
##'
##' This is actually OK now. It works, and doesn't have too much spaghetti code.Need to use parLapply to ensure that orders are placed at the same time. Possible cause of the lack of making money
##' @title trade
##' @param level 
##' @param position 
##' @param qty the qty to trade
##' @param orderbook an orderbook object
##' @param prices the price to trade at
##' @return a list containing the buy and sell trades, as well as an orderbook
##' @author richie
##' @export
trade <- function(level, position, qty, spread) {
    account <- account(level)
    venue <- venue(level)
    stock <- ticker(level)
    qty <- qty
    prices <- get_spreads(venue, stock, position, spread)
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
##TODO: make this actually work, generalise the cluster building
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
    ## prices <- ifelse(rnorm(1)>0, price + 5, price - 5),
    rep(price, times=split)
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
##TODO: how is this different from get_price_and_qty
##TODO: remove network access from everything
##TODD: generalise all of this spread logic
##' Return a bid price and ask price derived from get_quote, regardless
##'
##' messages when it has a problem
##' @title get_price
##' @param venue 
##' @param ticker 
##' @param fudge 
##' @return a vector containing a fudged bid and a qty to trade with
##' @author richie
##' @export
get_bid <- function(venue, ticker, spread=40) {
    q <- get_quote(venue, ticker) %>% parse_response()
    bid <- q$bid
    ask <- q$ask
    last <- q$last
    print(c(bid, ask, last))
    while(all(is.null(bid) & is.null(ask) & is.null(last))) {
        message("not even a last price")
        ##TODO: at this point, make any market you like
        Sys.sleep(2)
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

    return(c(buyprice, sellprice))
}
##TODO: remove network access, rationalise spread logic
##' Get spreads and various orderbook related stuff
##'
##' This is fucking bullshit
##' @title get_spreads
##' @param venue 
##' @param stock 
##' @return a list containing an orderbook, the spread and the minqty available 
##' @author richie
##' @export
get_spreads <- function(venue, stock, position, spread) {
    ord <- as_orderbook(venue, stock)
    ## q <- as_quote(venue, stock)
    bids <- summary(ord, type="bids")
    asks <- summary(ord, type="asks")
    book <- list(bids=bids, asks=asks)
    pos <- position@current_position
    if(pos > 0) {
        bid <- median(bids$price) - 1
        ask <- min(asks$price) - 10
        res <- c(bid, ask)
    }
    if(pos < 0) {
        ask <- median(asks$price) + 1
        bid <- min(bids$price) + 10
        res <- c(bid, ask)
    }
    if(pos==0) {
        res <- get_bid(venue, stock, spread=spread)
    }
    res
}
##TODO: use this function somewhere. Need to keep track of P&L at the order level
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
##TODO: die, network access, die.
##TODO: either this or get_position is redundant
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
            summarise(filled=sum(totalFilled), price=sum((price*qty), na.rm=TRUE))
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
##TODO: this always loses money. Smaller orders are probably better for this sort of thing
##TODO: also, die, network access, die! recursive network access, at that
##' Function to sell/buy inventory to reduce holdings to zero
##'
##' Uses ioc orders to prevent bots from catching on (i don't believe that this ever worked)
##' @title clear_position
##' @param level the current level
##' @param position an object of class position
##' @return a new position object
##' @author richie
##' @export
clear_position <- function(level, position, apikey, tolerance, spread) {
    account <- account(level)
    venue <- venue(level)
    stock <- ticker(level)
    sumpos <- print(position)
    while(abs(sumpos$position) >= tolerance) {
        if(sumpos$position > 0) {
            compsell <- 0
            bids <- get_spreads(venue, stock, position, spread)
            pricesell <- bids[2] - (spread/4)
            
            message("clearing position at  ", pricesell, "\n")
            sellp <- make_order(level, price=pricesell, direction="sell", qty=sumpos$position, ordertype="ioc", apikey=apikey) %>% parse_response()
            if(sellp$totalFilled==0) {
                compsell <- compsell - (spread / 4)
            }
            position <- update_position(position, apikey)
        }
    
        if(sumpos$position<0) {
            compbuy <- 0
            bidsdf <- get_spreads(venue, stock, position, spread)
            pricebuy <- bidsdf[1] + compbuy
            message("clearing position at  ", pricebuy, "\n")
            buyp <- make_order(level, price=pricebuy, direction="buy", qty=(sumpos$position)*-1, ordertype="ioc", apikey=apikey) %>% parse_response()
                if(buyp$totalFilled==0) {
                    compbuy <- compbuy + (spread / 4)
                }
            position <- update_position(position, apikey)
            sumpos <- print(position)
            print(position)
        }
    }
    return(position)
}
##' Wrapper around create and place_ order
##'
##' Just to save typing, essentially
##' @title make_order
##' @param level a level object
##' @param price an integer price
##' @param qty an integer qty
##' @param direction either buy or sell
##' @param ordertype one of limit, market, ioc or fok
##' @param apikey your apikey
##' @return a response representing the status of an order
##' @author richie
##' @export
make_order <- function(level, price, qty, direction, ordertype, apikey) {
    account <- account(level)
    venue <- venue(level)
    stock <- ticker(level)
    creat <- create_order(account=account,
                          venue=venue,
                          stock=stock,
                          price=price,
                          qty=qty,
                          direction=direction,
                          ordertype=ordertype)
    place <- place_order(venue, stock, body=creat, apikey=apikey)
    return(place)
}
##TODO: remove sequential network access
##' Stuff
##'
##' more stuff
##' @title cancel_orders
##' @param orders 
##' @return void
##' @author richie
##' @export
cancel_orders <- function(orders, apikey) {
    cancellist <- vector(mode="list", length=nrow(orders))
    venue <- unique(orders$venue)
    stock <- unique(orders$symbol)
    for(i in 1:nrow(orders)) {
        cancellist[[i]] <- cancel_order(orders$id[i], venue=venue, stock=stock, apikey=apikey)
    }
    cancellist
}
##TODO: refactor to use cancel_order to enable threading
##' Cancel a set of open orders
##'
##' Cancel limit orders older than seconds
##' @title cancel_old_olders
##' @param orders either a response or a dataframe containing orders
##' @param seconds if order$ts>seconds, cancel
##' @param apikey authorisation
##' @return 
##' @author richie
##' @export
cancel_old_orders <- function(orders, seconds=10, apikey=apikey) {
    if(class(orders)=="response") {
        ordersp <- parse_response(orders)
        orderdf <- ordersp$orders
    }
    if(is.data.frame(orders)) {
        orderdf <- orders
    }
    if(is.list(orders)) {
        orderdf <- orders$orders
    }
    oldord <-
        filter(orderdf, open==TRUE) %>%
        filter(ts<(lubridate::ymd_hms(Sys.time()) - lubridate::seconds(x=seconds)))
    if(nrow(oldord)!=0) {
    cancels <- cancel_orders(oldord, apikey=apikey)
    cancels
    }
    else {
        cancels <- NA
    }
    cancels
}

test_parallel <- function(venue, stock) {
    cl <- parallel::makeForkCluster(nnodes = 6)
            listvars <- rep(list(c(venue, stock)), 6)
            orders <- parallel::parLapply(cl, listvars, function(x) as_orderbook(venue = x[1], 
                                                                                  stock = x[2]))
}
##TODO: remove sequential network access
update_orders <- function(orders) {
    ids <- lapply(orders, "[[", "id")
    venue <- orders[1][["venue"]]
    stock <- orders[1][["stock"]]
    status <- sapply(ids, function(x) get_order_status(x, venue, stock))
}
