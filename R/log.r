examples.write.full.log = function() {
  main.dir = "D:/libraries/stuko/ulm"
  setwd(main.dir)
  db.dir = file.path(main.dir, "db")
  db = get.stukodb(db.dir)
}

write.full.log = function(log.file = "log.txt", db= get.stukodb(),...) {
  restore.point("update.log.ui")
  log = dbGet(db,"log") %>%
    arrange(logtime)

  txt = paste0(strftime(log$logtime,"%Y-%m-%d %H:%M"), " von ", log$userid, "\n\n", log$logtext, collapse="\n\n-------------------------------\n\n")
  rmdtools::writeUtf8(txt, log.file)
}
