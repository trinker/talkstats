ts_chatbox <- function(user = NULL, pass = NULL, 
    splitDate = TRUE, adjwidth = TRUE, clear.username = FALSE,
    clear.password = FALSE) {                           
    suppressPackageStartupMessages(require(RCurl))                        
    suppressPackageStartupMessages(require(XML))                               
    curl <- getCurlHandle()                                               
    curlSetOpt(cookiefile='cookies.txt', curl = curl)  
    data(ts_username)
    data(ts_password) 
    loc <- paste0(find.package("talkstats"), "/data")
    if ((is.null(user) & is.na(ts_username) & !exists(".ts_username", 
        envir = .GlobalEnv)) | clear.username) {
        cat("\n","Enter Talkstats Username","\n")
        ts_username <- scan(n=1,what = character(0), quiet=T)
        .ts_username <<- ts_username 
        unlink(paste0(loc, "/ts_username.rda"), recursive = TRUE, 
            force = FALSE)
        save(ts_username, file = paste0(loc, "/ts_username.rda"))
    }     
    if ((is.null(pass) & is.na(ts_password) & !exists(".ts_password", 
        envir = .GlobalEnv)) | clear.username) {
        cat("\n","Enter Talkstats Password","\n")
        ts_password <- scan(n=1,what = character(0), quiet=T)
        .ts_password <<- ts_password 
        unlink(paste0(loc, "/ts_password.rda"), recursive = TRUE, 
            force = FALSE)
        save(ts_password, file = paste0(loc, "/ts_password.rda"))
    }      
    if (exists(".ts_usernameR", envir = .GlobalEnv) & is.null(user) &
        !exists("ts_username")) {
        user <- .ts_username
    } else {
        user <- ts_username
    }   
    if (exists(".ts_password", envir = .GlobalEnv) & is.null(pass) &
        !exists("ts_password")) {
        pass <- .ts_password
    } else {
        pass <- ts_password
    }   
    x <- postForm("http://www.talkstats.com/login.php",                   
        do="login",                                                       
        vb_login_username=user,                                           
        vb_login_password=pass,                                           
        cookieuser='1',                                                   
        curl = curl)                                                                                                                  
    tmp <- getForm("http://www.talkstats.com/mgc_cb_evo.php",             
        do = "save_archives",                                             
        channel_id="0",                                                   
        format = "csv",                                                   
        curl = curl) 
    txt <- textConnection(tmp)                                                     
    w <- read.csv(txt)                                   
    w[, "Chats"] <- as.character(w[, "Chats"])                                                                                                  
    tsdate <- function(date){                                             
        u <- as.character(date)                                           
        v <- sapply(strsplit(u, "/", fixed=TRUE), function(x) x[[1]])     
        v <- as.numeric(v)                                                
        y <- substr(Sys.time(), 1, 4)                                     
        if (v[1] > v[length(v)]) {                                        
            as.POSIXct(paste0(y, "/",  u, ":00"),                         
                format = c("%Y/%m/%d %H:%M:%S"))                          
        } else {                                                          
            y <- ifelse(v > 6, as.character(as.numeric(y)-1), y)          
            as.POSIXct(paste0(y, "/",  u, ":00"),                         
                format = c("%Y/%m/%d %H:%M:%S"))                          
        }                                                                 
    }                                                                     
    w$Date <- as.POSIXct(tsdate(w[, 1]))                                              
    names(w) <- c("date", "person", "dialogue")                                                                                                     
    removepatterns <- c("\\[youtube[^\\[]*\\[/youtube\\]",                
        "\\[URL[^\\[]*\\[/URL\\]", "\\[YOUTUBE[^\\[]*\\[/YOUTUBE\\]",     
        "\\[url[^\\[]*\\[/url\\]", "\\[tex\\][^[]*\\[/tex\\]",             
        "\\[img[^\\[]*\\[/img\\]",                                        
        "\\[MATH[^\\[]*\\[/MATH\\]", "\\[TEX\\][^[]*\\[/TEX\\]",           
        "\\[CODE[^\\[]*\\[/CODE\\]", "\\[QUOTE[^\\[]*\\[/QUOTE\\]",       
        "\\[HTML[^\\[]*\\[/HTML\\]", "\\[PHP[^\\[]*\\[/PHP\\]",           
        "\\[SPOILER[^\\[]*\\[/SPOILER\\]", "\\[code[^\\[]*\\[/code\\]",   
        "\\[quote[^\\[]*\\[/quote\\]", "\\[html[^\\[]*\\[/html\\]",       
        "\\[php[^\\[]*\\[/php\\]", "\\[spoiler[^\\[]*\\[/spoiler\\]",     
        "\\[color[^\\[]*\\]", "\\[/color\\]",                             
        "\\[SIZE[^\\[]*\\]", "\\[/SIZE\\]",                               
        "\\[size[^\\[]*\\]", "\\[/size\\]",                               
        "\\[COLOR[^\\[]*\\]", "\\[/COLOR\\]",                             
        "\\[font[^\\[]*\\]", "\\[/font\\]", "\\[u[^\\[]*\\]", "\\[/u\\]", 
        "\\[FONT[^\\[]*\\]", "\\[/FONT\\]", "\\[U[^\\[]*\\]", "\\[/U\\]", 
        "\\[b[^\\[]*\\]", "\\[/b\\]", "\\[i[^\\[]*\\]", "\\[/i\\]",       
        "\\[B[^\\[]*\\]", "\\[/B\\]", "\\[I[^\\[]*\\]", "\\[/I\\]")                                                                                                                                                         
    removestuff <- function(x){                                           
        for(i in seq(removepatterns)){                                    
            x <- gsub(removepatterns[i], "", x)                           
        }                                                                 
        return(as.character(x))                                           
    }                   
    clean <- function(text) {
        gsub("\\s+", " ", gsub("\r|\n|\t", " ", text))
    }    
    reducer <- function(x) gsub("\\s+", " ", x)  
    Trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    blank2NA <-
    function(dataframe, missing=NA){
        cn <- which(sapply(dataframe, is.character))
        FUN <- function(da) {
            if (is.character(da)) da <- Trim(da)
            da[da==""] <- missing
            names(da) <- NULL
            return(da)
        }
        DF <- data.frame(apply(dataframe, 2, FUN), check.names=FALSE)
        sapply(cn, function(i) {DF[, i] <<- as.character(DF[, i])})
        return(DF)
    }
    colsplit2df <-
    function(dataframe, splitcol = 1, new.names=NULL, sep=".", 
             orig.keep=FALSE, ...){
        if (!is.data.frame(dataframe)){
            stop("Please supply a data.frame to colsplit2df")
        }
        if (is.numeric(dataframe[, splitcol])) {
            stop("splitcol can not be numeric")
        }
        X <- data.frame(do.call(rbind, strsplit(as.vector(
            dataframe[, splitcol]), split = sep, fixed=TRUE)))
        z <- if (!is.numeric(splitcol)) {
            match(splitcol, names(dataframe)) 
        } else {
            splitcol
        }
        if (!is.null(new.names)) {
            colnames(X) <- new.names
        }
        if (z!=1 & ncol(dataframe) > z) {
            w <- cbind(dataframe[, 1:(z-1), drop=FALSE], X, 
                dataframe[, (z + 1):ncol(dataframe), drop=FALSE])
        } else {
            if (z!=1 & ncol(dataframe) == z) {
                w <- cbind(dataframe[, 1:(z-1), drop=FALSE], X)
            } else {
                if (z==1 & ncol(dataframe) > z) {
                    w <- cbind(X, dataframe[, (z + 1):ncol(dataframe), drop=FALSE])
                } else {
                    w <- X
                }
            }
        }
        if (is.null(new.names) &"&" %in% unlist(strsplit(names(dataframe[, 
            splitcol, drop=FALSE]), split=NULL))) {
            nams <- unlist(strsplit(names(dataframe[, 
                splitcol, drop=FALSE]), split="&"))
            colnames(w)[1:length(nams)] <- nams
        }
        if(orig.keep) {
            w <- cbind(dataframe[, splitcol, drop=FALSE], w)
        }
        return(w)
    }
                                                        
    removetags <- Vectorize(removestuff, USE.NAMES = FALSE)               
    w$dialogue <- removetags(clean(w$dialogue))                           
    w$dialogue <- reducer(Trim(w$dialogue))                               
    w <- blank2NA(w)  
    if (splitDate) {
        w <- colsplit2df(w, sep= " ", new.names=c("date", "time"))
    } else {
        w$date <- as.POSIXct(as.character(w$date))  
    }
    close(txt) 
    if (adjwidth) {
        b4 <- options()$width; options(width=1000)
        print(w)
        options(width=b4)                                               
        invisible(w) 
    } else {   
        return(w)                                                         
    }
}
