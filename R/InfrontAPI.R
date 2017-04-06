# Library depencencies
library(jsonlite)
library(httr)
library(plotly)

#symbols = c("OSS:STL", "OSS:YAR", "SSE:ABB")
#fields = c("open", "high", "low")
#from = 20170101
#to = 20170331
# Request/Response: Get.Symbols


#' @export
Get.Symbols <- function(symbols, fields, from, to) {
    if (missing(symbols)) {
        stop("You need to input a feed and market symbol of class 'character'. \n E.g. \nFEED:TICKER\n, or c(\"LSE:AAL\",\"OSS:STL\"")
    }
    if (missing(fields)) {
        fields = "last"
    }
    if (!is.character(symbols)) {
        stop("Symbols inputs must be of class and type 'character': \n E.g. \"NYSE:MSFT\" or as a vector c(\"NYSE:MSFT\",\"LSE:AAL\") ")
    }
    if (!missing(fields)) {
        if (!is.character(fields)) {
            stop("Fields inputs must be of class and type 'character': \n E.g. \"Volume\" or as a vector c(\"bid\",\"ask\") ")
        }
    }
    if (!is.integer(from) && !is.numeric(from)) {
        stop("'From date' inputs must be of class integer or numeric: \n If you are not sure about the class, conver the date as: as.integer(date) ")
    }
    if (!is.integer(to) && !is.numeric(to)) {
        stop("'From date' inputs must be of class integer or numeric: \n If you are not sure about the class, conver the date as: as.integer(date) ")
    }
    if (from > to) {
        stop("Initiation date must be bigger or equal to that last date in the dataset.")
    }

    request_payload = toJSON(list(symbols = symbols,
                         fields = fields,
                         from = unbox(from),
                         to = unbox(to)))
    post_result <- POST("http://localhost:8844/api/v1/history", body = request_payload)

    resp_2 <- content(post_result, "text", encoding="utf-8")

    json <- fromJSON(resp_2) #handle Json reponse
    fields_ <- as.character(json$fields)
    rows_ <- json$rows
    out_ <- lapply(by(rows_, rows_[, 2], identity), as.data.frame)
    out_ <- lapply(out_, "rownames<-", NULL)
    out_ <- lapply(out_, setNames, fields_)

    cat("Your dataset has been successfully imported! \n")    
    return(out_)
}

# Example of Get.Symbols
#MyStocks <- Get.Symbols(symbols = c("OSS:STL", "OSS:YAR", "SSE:ABB"), fields = c("open", "high", "low"), from = 20010101, to = 20170331)

# Get.Live : Spanshot
#symbols <- c("OSS:STL", "SSE:ABB")
#fields <- c("bid","ask","last")

#' @export
Get.Live <- function(symbols,fields) {
    if (missing(symbols)) {
        stop("You need to input a feed and market symbol of class 'character'. \n E.g. \nFEED:TICKER\n, or c(\"LSE:AAL\",\"OSS:STL\"")
    }
    if (missing(fields)) {
        fields = "last"
    }
    if (!is.character(symbols)) {
        stop("Symbols inputs must be of class and type 'character': \n E.g. \"NYSE:MSFT\" or as a vector c(\"NYSE:MSFT\",\"LSE:AAL\") ")
    }
    if (!missing(fields)) {
        if (!is.character(fields)) {
            stop("Fields inputs must be of class and type 'character': \n E.g. \"Volume\" or as a vector c(\"bid\",\"ask\") ")
        }
    }
    request_payload = toJSON(list(symbols = symbols,
                             fields = fields))
    post_result <- POST("http://localhost:8844/api/v1/snapshot", body = request_payload)
    resp_2 <- content(post_result, "text", encoding = "utf-8")

    json <- fromJSON(resp_2) #handle Json reponse
    fields_ <- as.character(json$fields)
    rows_ <- json$rows
    out_ <- lapply(by(rows_, rows_[, 2], identity), as.data.frame)
    out_ <- lapply(out_, "rownames<-", NULL)
    out_ <- lapply(out_, setNames, fields_)

    cat("Your dataset has been successfully imported! \n")
    return(out_)
}


# Get.Overview(feed="OSS") <- without the "chain"
# Get.Overview(feed="OSS",chain="somechain")

#' @export
Get.Overview <- function(feed, chain) {
    if (missing(feed)) {
    stop("Feed argument required. \n It must be a string of type 'character'. \n E.g. \"LSE\"")
    }
    if (missing(chain)) {
        request_payload = toJSON(list(feed = unbox(feed)))
        post_result <- POST("http://localhost:8844/api/v1/feed", body = request_payload)
        resp_ <- content(post_result, "text", encoding = "utf-8")
        resp_ <- fromJSON(resp_)
        cat("The Symbols available in feed ")
        cat(feed)
        cat(" are: ")
        print(resp_$rows)
        return(resp_$rows)
    }
        else {
            request_payload = toJSON(list(feed = unbox(feed), chain = unbox(chain)))
            post_result <- POST("http://localhost:8844/api/v1/feed", body = request_payload)
            resp_ <- content(post_result, "text", encoding = "utf-8")
            resp_ <- fromJSON(resp_)
            cat("The Symbols available in chain ")
            cat(chain)
            cat(" in feed ")
            cat(feed)
            cat(" are: ")
            print(resp_$rows)
            return(resp_$rows)
            
        }
}

#' @export
Infront.help <- function() {
    cat("      Infront Desktop API for R v1.0 \n ")
    cat("      Mantainer: api@infrontfinance.com \n ")
    cat("      Visit infrontfinance.com for more information")

}

#Quick.Chart("OSS:STL", from=20170101, to=20170330)
#Symbol <- "OSS:STL"
#from = 20170101
#to = 20170330

#' @export
Quick.Chart <- function(Symbol, from, to) {
    require(plotly)
    fillcolor = "#ff6666"
    hollowcolor = "#39ac73"
    linewidth = 4
    plotcolor = "#3E3E3E"
    papercolor = "#1E2022"
    fontcolor = "#B3A78C"
    startdate = as.character(from)
    sym_dat <- Get.Symbols(symbols = Symbol, fields = c("open", "high", "low", "last", "volume"), from, to)
    sym_dat <- sym_dat[[1]][-2]
    sym_dat <- sym_dat[order(sym_dat$date),] # sorted correctly

    
    prices <- data.frame(time = as.Date(sym_dat$date, "%Y%m%d"),
                       open = as.numeric(as.character(sym_dat$open)),
                       high = as.numeric(as.character(sym_dat$high)),
                       low = as.numeric(as.character(sym_dat$low)),
                       close = as.numeric(as.character(sym_dat$last)),
                       volume = as.numeric(as.character(sym_dat$volume)))

    # Create line segments for high and low prices
    plot.base <- data.frame()
    plot.hollow <- data.frame()
    plot.filled <- data.frame()

    for (i in 1:nrow(prices)) {
        x <- prices[i,]

        # For high / low
        mat <- rbind(c(x[1], x[3]),
                 c(x[1], x[4]),
                 c(NA, NA))

        plot.base <- rbind(plot.base, mat)

        # For open / close
        if (x[2] > x[5]) {
            mat <- rbind(c(x[1], x[2]),
                   c(x[1], x[5]),
                   c(NA, NA))

            plot.filled <- rbind(plot.filled, mat)
        } else {
            mat <- rbind(c(x[1], x[2]),
                   c(x[1], x[5]),
                   c(NA, NA))

            plot.hollow <- rbind(plot.hollow, mat)
        }
    }

    colnames(plot.base) <- colnames(plot.hollow) <- colnames(plot.filled) <- c("x", "y")
    plot.base$x <- as.Date(as.numeric(plot.base$x))
    plot.hollow$x <- as.Date(as.numeric(plot.hollow$x))
    plot.filled$x <- as.Date(as.numeric(plot.filled$x))

    hovertxt <- paste("Date: ", round(prices$time, 2), "<br>",
                    "High: ", round(prices$high, 2), "<br>",
                    "Low: ", round(prices$low, 2), "<br>",
                    "Open: ", round(prices$open, 2), "<br>",
                    "Close: ", round(prices$close, 2))


    # Base plot for High / Low prices
    p <- plot_ly(plot.base, x = x, y = y, mode = "lines",
               marker = list(color = '#9b9797'),
               line = list(width = 1),
               showlegend = F,
               hoverinfo = "none")

    # Trace for when open price > close price
    p <- add_trace(p, data = plot.filled, x = x, y = y, mode = "lines",
                 marker = list(color = fillcolor),
                 line = list(width = linewidth),
                 showlegend = F,
                 hoverinfo = "none")

    # Trace for when open price < close price
    p <- add_trace(p, data = plot.hollow, x = x, y = y, mode = "lines",
                 marker = list(color = hollowcolor),
                 line = list(width = linewidth),
                 showlegend = F,
                 hoverinfo = "none")

    # Trace for volume
    p <- add_trace(p, data = prices, x = prices$time, y = prices$volume,
                 marker = list(color = "#ff9933"), type="bar",
                 showlegend = F,
                 hoverinfo = "x+y",
                 yaxis = "y2")

    # Trace for hover info
    p <- add_trace(p, data = prices, x = time, y = high, opacity = 0, hoverinfo = "text",
                 text = hovertxt, showlegend = F)

    # Layout options
    p <- layout(p, xaxis = list(title = "", showgrid = F,
                              tickformat = "%b-%Y",
                              tickfont = list(color = fontcolor),
                              rangeselector = list(
                                x = 0.85, y = 0.97, bgcolor = "fontcolor",
                                buttons = list(
                                  list(
                                    count = 3,
                                    label = "3 mo",
                                    step = "month",
                                    stepmode = "backward"),
                                  list(
                                    count = 6,
                                    label = "6 mo",
                                    step = "month",
                                    stepmode = "backward"),
                                  list(
                                    count = 1,
                                    label = "1 yr",
                                    step = "year",
                                    stepmode = "backward"),
                                  list(
                                    count = 1,
                                    label = "YTD",
                                    step = "year",
                                    stepmode = "todate"),
                                  list(step = "all")))),

              yaxis = list(title = "Price", gridcolor = "#8c8c8c",
                           tickfont = list(color = fontcolor),
                           titlefont = list(color = fontcolor),
                           domain = c(0.30, 0.95)),

              yaxis2 = list(gridcolor = "#8c8c8c",
                            tickfont = list(color = fontcolor),
                            titlefont = list(color = fontcolor),
                            side = "right",
                            domain = c(0, 0.2)),

              paper_bgcolor = papercolor,
              plot_bgcolor = plotcolor,
              margin = list(r = 50, t = 50))

    return(p)
}

