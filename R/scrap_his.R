scrap.ulm.vorlesungsverzeichnis = function() {
  binman::rm_platform("phantomjs")
  wdman::selenium(retcommand = TRUE)

  library(RSelenium)
  library(restorepoint)
  library(dplyr)
  library(digest)
  library(stringtools)

  rD = rsChromeDriver(check=FALSE)
  remDr <- rD$client

  url = "https://campusonline.uni-ulm.de/qislsf/rds?state=wtree&search=1&trex=step&root120172=20006|20360|19862&P.vx=lang"

  cat("\nNavigate to ", url,"...")
  remDr$navigate(url)


  sel.tmp = "#wrapper > div.divcontent > div.content > table:nth-child({{num+7}}) > tbody > tr > td:nth-child(2) > table > tbody > tr > td:nth-child(3) > a"

  num = 1
  sel = replace.whiskers(sel.tmp, values=nlist(num))

  clickElement(sel)


  # Another list
  sel.tmp = "#wrapper > div.divcontent > div.content > table:nth-child({{num+8}}) > tbody > tr > td:nth-child(2) > table > tbody > tr > td:nth-child(3) > a"

  num = 1
  sel = replace.whiskers(sel.tmp, values=nlist(num))

  clickElement(sel)



  cat("\nExtract title and description...")
  # Find title
  title = getInnerHTML(".indicator-head.line-top.line-bottom h1")


  # Find variable description
  descr = getInnerHTML(".more-section p")
  descr = c(descr, paste0("<br>Source: OECD (<a href='", url,"' target='_blank'>",url,"</a>)"))

  # Try to click on yearly frequency




  dest.dir = file.path(getwd(),"oecd")
  dest.dir = file.path(getwd(),"oecd_descr")


  urls = str.trim(readLines("urls.txt"))
  ignore = str.starts.with(urls,"#") | nchar(urls)==0
  urls = urls[!ignore]

  surls = readLines("success_urls.txt")

  urls = setdiff(urls, surls)
  for (url in urls) {
    res = try(save.oecd.indicator(url, dest.dir, dest.descr.dir))
    if (!is(res,"try-error")) {
      write(url,"success_urls.txt", append=TRUE)
    } else {
      write(url,"failed_urls.txt", append=TRUE)
    }
  }

}
