rsChromeDriver = function (port = 4567L, browser = "chrome", version="latest",verbose = TRUE, check = TRUE, ...)
{
    browser <- match.arg(browser)
    if (identical(browser, "internet explorer") && !identical(.Platform[["OS.type"]],
        "windows")) {
        stop("Internet Explorer is only available on Windows.")
    }
    selServ <- wdman::chrome(port = port, verbose = verbose,
        version = version, check = check)
    remDr <- remoteDriver(browserName = browser, port = port,...)
    count <- 0L
    while (inherits(res <- tryCatch({
        remDr$getStatus()
    }, error = function(e) {
        e
    }), "error")) {
        Sys.sleep(1)
        count <- count + 1L
        if (count > 5L) {
            warning("Could not determine server status.")
            break
        }
    }
    remDr$open(silent = !verbose)
    csEnv <- new.env()
    csEnv[["server"]] <- selServ
    csEnv[["client"]] <- remDr
    clean <- function(e) {
        chk <- suppressMessages(tryCatch({
            e[["client"]]$close()
        }, error = function(e) e))
        e[["server"]]$stop()
    }
    reg.finalizer(csEnv, clean)
    class(csEnv) <- c("rsClientServer", class(csEnv))
    return(csEnv)
}

wait.until.downloaded = function(cmd, download.dir=get.chrome.download.dir(), timeout.sec=30, ignore.ext=c("tmp","crdownload")) {
  cmd = substitute(cmd)
  restore.point("wait.until.downloaded")
  prev.files = list.files(download.dir)
  eval(cmd)

  start.time = as.integer(Sys.time())
  new.files = NULL
  cat("\nWait until file is downloaded...")
  while(as.integer(Sys.time())-start.time<=timeout.sec) {

    files = list.files(download.dir)
    new.files = setdiff(files, prev.files)
    new.files = new.files[!tools::file_ext(new.files) %in% ignore.ext]
    if (length(new.files)>0) break
  }
  return(file.path(download.dir, new.files))
}



# Default Chrome download dir
get.chrome.download.dir = function() {
  sys = Sys.info()[["sysname"]]
  if (sys=="Linux") {
    file.path("home", Sys.info()[["user"]], "Downloads")
  } else if (sys=="Windows") {
    file.path("C:/", "Users", Sys.info()[["user"]], "Downloads")
  } else {
    file.path("", "Users", Sys.info()[["user"]], "Downloads")
  }
}


oecd.dropdown.elements = function(div.sel) {
  restore.point("oecd.dropdown.elements")
  sel = paste0(div.sel, " > div > ul > li > a")
  fields = remDr$findElements(using="css", sel)

  li = lapply(fields, function(field) {
    c(code=field$getElementAttribute("data-value")[[1]], label=field$getElementAttribute("innerHTML")[[1]])
  })
  df = unique(do.call(rbind,li))
  labels = df[,2]
  names(labels) = df[,1]
  labels
}

getInnerHTML = function(sel, verbose=TRUE) {
  restore.point("getInnerHTML")
  webElem <- remDr$findElement(using = 'css', sel)
  res = webElem$getElementAttribute("innerHTML")[[1]]
  cat("\n",res)
  res
}




patientFindElement = function(css, wait=0.1, max=4, return.all=FALSE) {
  restore.point("patientFindElement")
  cat("\nTry to find ", css)
  start = as.numeric(Sys.time())
  end = start + max
  while(TRUE) {
    els = remDr$findElements(using="css",css)
    if (length(els)>0) return(els[[1]])
    if (end < as.numeric(Sys.time())) {
      stop(paste0("Timeout: could not find ", css))
    }
    Sys.sleep(wait)
  }
}

# some helper functions
getField = function(css, field="value", unlist=TRUE,...) {
  webElem = patientFindElement(css,...)
  res = webElem$getElementAttribute(field)
  if (unlist) return(unlist(res))
  res
}
sendKeys = function(css, text,...) {
  webElem = patientFindElement(css,...)
  if (!is.list(text)) text = list(text)
  webElem$sendKeysToElement(text)
}
clickElement=function(css,...) {
  scroll.to.element(css)
  webElem = patientFindElement(css,...)
  restore.point("clickElement")
  webElem$clickElement()
}

trySeveral = function(cmd, timeout.sec=30,wait.sec=5, times=NULL) {
  cmd = substitute(cmd)
  restore.point("trySeveral")
  if (!is.null(times)) {
    for (i in 1:times) {
      res = try(eval(cmd),silent = TRUE)
      if (!is(res,"try-error")) {
        return(res)
      } else {
        cat("\nfailed ", deparse(cmd))
      }
    }
  } else {
    start.time = as.integer(Sys.time())
    while(as.integer(Sys.time())-start.time<=timeout.sec) {
      res = try(eval(cmd),silent = TRUE)
      if (!is(res,"try-error")) {
        return(res)
      } else {
        cat("\nfailed ", deparse(cmd))
      }
      Sys.sleep(wait.sec)
    }

  }

  res
}

scroll.to.element = function(sel) {
  remDr$executeScript(paste0('$("', sel,'")[0].scrollIntoView( false );'), args = list())
}
