examples.write.full.log = function() {
  main.dir = "C:/libraries/stuko/ulm"
  setwd(main.dir)
  db.dir = file.path(main.dir, "db")
  db = get.stukodb(db.dir)

}

update.log.sems = function() {
  main.dir = "C:/libraries/stuko/ulm"
  setwd(main.dir)
  db.dir = file.path(main.dir, "db")
  db = get.stukodb(db.dir)


  logs = dbGet(db,"log")

  logtxt = logs$logtext


  str = trimws(str.right.of(logtxt, "SoSe"))
  str = str.left.of(str,":")
  str = str.left.of(str," ")
  str = str.left.of(str,")")
  str = str.left.of(str,"\n")

  sose = as.numeric(str)
  logtxt[which(is.na(sose))[1:10]]

  str = trimws(str.right.of(logtxt, "WiSe"))
  str = str.left.of(str,":")
  str = str.left.of(str," ")
  str = str.left.of(str,")")
  str = str.left.of(str,"\n")
  str = str.left.of(str,"/")

  wise = as.numeric(str)
  logtxt[which(is.na(sose))[1:10]]

  na.zero = function(x) {
    x[is.na(x)] = 0
    x
  }
  sem = na.zero(sose)*10 + na.zero(wise*10+5)
  sem[!is.na(wise) & !is.na(sose)] = 0
  table(sem)

  str[1:20]


  logs$semester=sem

  dbWithTransaction(db, {
    dbDelete(db, "log", params=NULL)
    dbInsert(db,"log", logs)
  })

  head(sose)

  yaml.dir = system.file("yaml", package = "stuko")
  sets = rmdtools::read.yaml(file.path(yaml.dir,"sets.yaml"))
  sems = sets$semester
  semnames = names(sems)

}


write.full.log = function(log.file = "log.txt", db= get.stukodb(),...) {
  restore.point("update.log.ui")
  log = dbGet(db,"log") %>%
    arrange(logtime)

  txt = paste0(strftime(log$logtime,"%Y-%m-%d %H:%M"), " von ", log$userid, "\n\n", log$logtext, collapse="\n\n-------------------------------\n\n")
  rmdtools::writeUtf8(txt, log.file)
}

make.log.text = function(app=getApp(), days=as.numeric(getInputValue("logDaysInput")), as.df=FALSE) {
  restore.point("make.log.text")
  if (isTRUE(is.na(days)) | length(days)==0)
    days = 200
  start.date = Sys.Date()
  start.date = start.date - days
  glob = app$glob


  start.time = as.numeric(as.POSIXct(paste0(start.date, " 00:00")))
  sql = paste0("select * from log where logtime >= ", start.time)
  glob$log = dbGet(glob$db,"log", sql=sql) %>%
    arrange(desc(logtime))


  if (as.df) {
    return(glob$log)
  }

  if (NROW(glob$log)==0) {
    return(paste0("Keine Eintraege innerhalb der letzten ", days, " Tage."))
  }

  txt = paste0(strftime(glob$log$logtime,"%Y-%m-%d %H:%M"), " von ", glob$log$userid, "\n\n", glob$log$logtext, collapse="\n\n-------------------------------\n\n")
  txt
}

make.log.ui.handlers = function() {
  buttonHandler("showLogBtn", update.log.ui)
  setDownloadHandler("downloadLogBtn",
   filename=function(...) "stuko_log.txt",
   content = function(file, ...) {
     withProgress({
       txt = sep.lines(make.log.text())
       writeLines(txt, file)
     },
     message="Die Logdatei wird erstellt. Dies dauert einen Moment..."
     )
   }
  )

}

make.log.ui = function() {
  ui = tagList(
    br(),
    shiny::textInput("logDaysInput",label = "Bis wie viele Tage zurueck?",value = 100),
    downloadButton("downloadLogBtn", "Logdatei downloaden"),
    actionButton("showLogBtn","Logdatei anzeigen"),
    br(),
    div(id="logUIDiv")
  )
  make.log.ui.handlers()
  ui
}

update.log.ui = function(app=getApp(), glob=app$glob,...) {
  restore.point("update.log.ui")

  txt = paste0("<pre>",make.log.text(app),"<pre>")

  setInnerHTML("logUIDiv",txt)
  #setUI("logUI", tags$pre(txt))
  return()
}


write.stuko.log = function(logtext,logtype, logtime=Sys.time(), userid=first.non.null(app$userid,"unknown"),semester=0, kursmodulid=NA, db=get.stukodb(), app=getApp()) {
  restore.point("write.stuko.log")
  log = list(logtime=logtime, userid=userid,logtype=logtype,semester=semester, kursmodulid=kursmodulid,logtext=logtext)
  dbInsert(db,"log", log)
}



