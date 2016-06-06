require(stockfighterr)
require(testthat)
apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
ordbuy <- create_order(account = "EXB123456",
                                         venue = "TESTEX",
                                         stock = "FOOBAR",
                                         price = 11000,
                                         qty = 200,
                                         direction = "buy",
                       ordertype = "limit")

test_that("create order returns a list",
          expect_is(ordbuy, "list" ))
test_that("place_order returns a response", {
    apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
    ordbuy <- create_order(account = "EXB123456",
                           venue = "TESTEX",
                           stock = "FOOBAR",
                           price = 11000,
                           qty = 200,
                           direction = "buy",
                           ordertype = "limit")
placebuy <- place_order("TESTEX", "FOOBAR",
                        body = ordbuy, apikey=apikey)
    expect_is(placebuy, "response")
})


test_that("get_all_orders returns a response", {
    apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
    expect_is(get_all_orders("TESTEX", "EXB123456", apikey), "response")
})


test_that("get_order_status returns a response", {
        apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
        ordbuy <- create_order(account = "EXB123456",
                           venue = "TESTEX",
                           stock = "FOOBAR",
                           price = 11000,
                           qty = 200,
                           direction = "buy",
                           ordertype = "limit")
    placebuy <- stockfighterr::place_order("TESTEX", "FOOBAR",
                                           body = ordbuy, apikey=apikey)
        placebuy.p <- parse_response(placebuy)
          testthat::expect_is(
              stockfighterr::get_order_status(
                  placebuy.p$id,
                  "TESTEX",
                  "FOOBAR"), "response")
})


test_that("cancel order returns a response", {
        apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
        ordbuy <- create_order(account = "EXB123456",
                           venue = "TESTEX",
                           stock = "FOOBAR",
                           price = 11000,
                           qty = 200,
                           direction = "buy",
                           ordertype = "limit")
    placebuy <- place_order("TESTEX", "FOOBAR",
                            body = ordbuy, apikey=apikey)
        placebuy.p <- parse_response(placebuy)
    expect_is(cancel_order(placebuy.p$id,
                           "TESTEX",
                           "FOOBAR", apikey=apikey), "response")
})


test_that("ioc orders are always closed after response", {
        apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
    ord <- create_order("EXB123456",
                        "TESTEX",
                        "FOOBAR",
                        1000,
                        100000,
                        direction = "buy",
                        ordertype="ioc")
    placed <- place_order("TESTEX", "FOOBAR", body=ord, apikey=apikey)
    placedp <- parse_response(placed)
    expect_equal(placedp$open, FALSE)
})


test_that("parse_response returns a list from an order object", {
        apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
        ord <- create_order("EXB123456",
                        "TESTEX",
                        "FOOBAR",
                        1000,
                        100000,
                        direction = "buy",
                        ordertype="ioc")
    expect_is(parse_response(
        place_order("TESTEX", "FOOBAR",
                    body=ord, apikey=apikey)), "list")
})
test_that("a placed order returns a totalFilled numeric greater than 0", {
        apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
        ord <- create_order("EXB123456",
                        "TESTEX",
                        "FOOBAR",
                        1000,
                        100000,
                        direction = "buy",
                        ordertype="ioc")
    placed <- place_order("TESTEX", "FOOBAR", body=ord, apikey=apikey)
    placedp <- parse_response(placed)
        expect_gte(placedp[["totalFilled"]], 0)
    })

test_that("place_order returns a response", {
            apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
        ordbuy <- create_order(account = "EXB123456",
                           venue = "TESTEX",
                           stock = "FOOBAR",
                           price = 11000,
                           qty = 200,
                           direction = "buy",
                           ordertype = "limit")
    placebuy <- stockfighterr::place_order("TESTEX", "FOOBAR",
                                           body = ordbuy, apikey=apikey)
    expect_is(placebuy, "response")
})
test_that("get spreads returns a vector of length two", {
    apikey <- get_api_key("~/Dropbox/Code/Starfighter/apikey.txt")
    pos <- new("Position")
    pos@current_position <- 0L
    expect_equal(length(get_spreads("TESTEX", "FOOBAR", position=pos, spread=100)), 2)
})
