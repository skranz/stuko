fose.ui = function(..., app=getApp(), glob=app$glob) {
  cat("\nfose.ui")

  restore.point("fose.ui")

  pe = filter(glob$person, personid %in% app$admin.for)
  choices = pe$personid
  names(choices) = pe$kurzname


  ui = tagList(
    h3("Forschungssemester"),
    p("Nicht vergessen alle Aenderungen zu speichern!"),
    uiOutput("foseUI")
  )
  update.fose.ui()
  ui

}

update.fose.ui = function(app=getApp(), glob=app$glob, db=glob$db) {
  cat("\nupdate.fose.ui")

  restore.point("update.fose.ui")

  if (is.null(glob[["fose"]])) {
    glob$fose = dbGet(glob$db, "fose")
  }
  glob$fose = glob$fose %>% arrange(-semester)
  tab.ui = tableform.ui(form=glob$forms$fose, data=glob$fose,use.delete.btn = TRUE)

  ui = tagList(
    tab.ui,
    simpleButton("addfoseBtn","Neues Forschungssemester"),
    simpleButton("savefoseBtn", "Alle Aenderungen Speichern", form.sel="#bossSelect, .tableform-fose-input"),
    uiOutput("savefoseAlert")
  )
  setUI("foseUI", ui)

  buttonHandler("addfoseBtn", function(...) {
    restore.point("addfoseBtn")
    form.html.table.add.row(form=glob$forms$fose,data=data.frame(personid=glob$person$personid[1],semester=app$sem,comment=""), use.delete.btn=TRUE)
  })

  buttonHandler("savefoseBtn", save.fose.click)

}

save.fose.click = function(formValues, ..., app=getApp(), glob=app$glob, db=glob$db) {
  fose = extract.tableform.formValues(formValues, form=glob$forms$fose)
  old.fose = glob$fose
  restore.point("save.fose.click")

  log = paste0("Forschungssemester modifiziert:")
  diff = tables.diff(fose, old.fose)
  if (NROW(diff$added)>0) {
    log = paste0(log,paste0("\n  Neu:      ", diff$added$personid, " ",semester_name(as.integer(diff$added$semester))," ",diff$added$comment,  collapse=""))
  }
  if (NROW(diff$removed)>0) {
    log = paste0(log,paste0("\n  Entfernt: ", diff$removed$personid,  " ",semester_name(as.integer(diff$removed$semester))," ",diff$removed$comment, collapse=""))
  }


  dbWithTransaction(db, {
    dbDelete(db, "fose", list())
    dbInsert(db, "fose", fose)
    write.stuko.log(log, "mod_fose")
  })
  glob$fose = dbGet(db, "fose")

  html = paste0("Die Aenderungen bei den Forschungssemestern wurden mit folgendem Logeintrag gespeichert.","<pre>\n", log,"</pre>")
  timedMessage("savefoseAlert",html=html,millis = 10000)

  invisible(fose)
}
