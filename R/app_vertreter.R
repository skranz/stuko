vertreter.ui = function(..., app=getApp(), glob=app$glob) {
  restore.point("vertreter.ui")

  pe = filter(glob$person, personid %in% app$admin.for)
  choices = pe$personid
  names(choices) = pe$kurzname


  ui = tagList(
    selectInput("bossSelect","Vertreter fuer folgenden Koordinator definieren:", choices),
    uiOutput("vertreterUI")
  )

  selectChangeHandler("bossSelect",function(value, ...) {
    restore.point("bossSelectChange")
    bossid = value
    update.vertreter.ui(bossid)
  })

  bossid = choices[1]
  update.vertreter.ui(bossid)
  ui

}

update.vertreter.ui = function(bossid=NULL, app=getApp(), glob=app$glob, db=glob$db) {
  restore.point("update.vertreter.ui")

  .bossid = bossid
  ver = filter(glob$vertreter, bossid==.bossid)

  tab.ui = tableform.ui(form=glob$forms$vertreter, data=select(ver,-bossid),use.delete.btn = TRUE)

  ui = tagList(
    tab.ui,
    simpleButton("addVertreterBtn","Neuer Vertreter"),
    simpleButton("saveVertreterBtn", "Alle Aenderungen Speichern", form.sel="#bossSelect, .tableform-vertreter-input"),
    uiOutput("saveVertreterAlert")
  )
  setUI("vertreterUI", ui)


  buttonHandler("addVertreterBtn", function(...) {
    restore.point("addVertreterBtn")
    form.html.table.add.row(form=glob$forms$vertreter, data=data_frame(email="",admin=FALSE), use.delete.btn=TRUE)
  })

  buttonHandler("saveVertreterBtn", save.vertreter.click)

}

save.vertreter.click = function(formValues, ..., app=getApp(), glob=app$glob, db=glob$db) {
  restore.point("save.vertreter.click")
  ver = extract.tableform.formValues(formValues, form=glob$forms$vertreter)
  bossid = .bossid = formValues$bossSelect

  ver$bossid = bossid
  ver$admin = as.logical(ver$admin)
  ver = select(ver, bossid, email, admin)

  over = filter(glob$vertreter, bossid == .bossid)

  log = paste0("Vertreter von ", bossid, " modifiziert:")
  diff = tables.diff(ver, over)
  if (NROW(diff$added)>0) {
    log = paste0(log,paste0("\n  Neu:      ", diff$added$email, " (admin=",diff$added$admin,")", collapse=""))
  }
  if (NROW(diff$removed)>0) {
    log = paste0(log,paste0("\n  Entfernt: ", diff$removed$email, " (admin=",diff$removed$admin,")", collapse=""))
  }


  dbWithTransaction(db, {
    dbDelete(db, "vertreter", list(bossid=bossid))
    dbInsert(db, "vertreter", ver)
    write.stuko.log(log, "mod_vertreter")
  })

  glob$vertreter = dbGet(db, "vertreter")

  html = paste0("Die Aenderungen bei den Vertretern wurden mit folgendem Logeintrag gespeichert.","<pre>\n", log,"</pre>")
  timedMessage("saveVertreterAlert",html=html,millis = 10000)

  invisible(ver)
}
