
update.module.ui = function(app=getApp(), glob=app$glob,...) {
  restore.point("update.module.ui")
  sd = get.sem.data()


  df = make.module.datatable.df(sd)

  dt = datatable(df,selection = 'none',escape=-1,rownames = FALSE, filter=list(position="top", clear=FALSE, plain=TRUE),
    class="display compact",
#    style="bootstrap",
    autoHideNavigation = TRUE, extensions = c('FixedColumns','ColReorder','Select',"Buttons"),options = list(
    lengthMenu = c(10, 25, 50, 1000),
    dom = 'Blfrtip',
    buttons = c('copy','excel','csv'),
    select = FALSE,
    columnDefs = list(list(width="15em", targets=c(1)),list(width="3em", targets=3)),
    autoWidth=TRUE,
    scrollX=TRUE,colReorder = TRUE,fixedColumns = list(leftColumns = 2)))

  shinyEvents::setDataTable("moduleTable", dt,server=TRUE)

  classEventHandler("editModulBtn",event = "click",function(data=NULL, ...) {
    restore.point("editModulBtn")
    show.edit.modul(get.sem.data()$module[data$row,])
    cat("\neditModul clicked...")
  })
}



make.module.datatable.df = function(sd, app=getApp(), glob=app$glob) {
  restore.point("make.module.table.df")

  rows = seq_len(NROW(sd$module))
  btns= paste0(
    simpleButtonVector(id=paste0("editModulBtn_",rows),icon=icon(name = "pencil"), size="sm",extra.class = "editModulBtn",extra.head=paste0('data-row="',rows,'"')),
    simpleButtonVector(id=paste0("delModulBtn_",rows),icon=icon(name = "trash-o"), size="sm", extra.class="delModulBtn", extra.head=paste0('data-row="',rows,'"'))
  )


  df = transmute(sd$module,Aktion=btns, Modul=titel, BaMa=bama,Studiengang=studiengang, Zuordnung=zuordnung, Schwerpunkte=schwerpunkt, Extern=ifelse(extern,"extern","intern"), ECTS=as.integer(ects),
#   Kurse = num_kurse,
    Prüfung=pruefungsform, Dozent=dozent, Kurs=kurs, 'Anz. Kurs'=num_kurs, 'Modifiziert am'=as.Date(modify_time), 'Modifiziert durch'=modify_user)

  df
}

show.edit.modul = function(modul,..., app=getApp(), glob=app$glob) {
  restore.point("show.edit.modul")

  app$modul = modul
  form = glob$forms$modul
  sd = get.sem.data(modul$semester)

  widgets = lapply(names(form$fields), function(name) {
      fieldInput(name=name,form=form, value = modul[[name]], lang="de",sets = glob$sets)
  })


  form.sel = paste0(paste0("#modul_",names(form$fields)), collapse=", ")

  ui = tagList(
    h3("Modul bearbeiten"),
    fluidRow(column(width = 12, widgets[1])),
    layout.widgets.as.fluid.grid(widgets[-1], 3),
    simpleButton("saveModulBtn","Modul Speichern",form.sel = form.sel),
    uiOutput("saveModulAlert")
  )

  buttonHandler("saveModulBtn", function(...) {
    save.modul.click(...)
  })

  setUI("editModulUI",ui)
  evalJS("$('html, body').animate({scrollTop: $('#addModulBtn').offset().top + 'px'}, 'fast');")
}

save.modul.click = function(modul = app$modul, formValues,..., app=getApp(), glob=app$glob) {
  restore.point("save.modul.click")
  cat("\nsave.modul.click")

  modulid = modul$modulid
  semester = modul$semester

  # Extract values
  kuv = extract.form.formValues(formValues, form=glob$forms$modul)

  nmo = modul
  nmo[names(kuv)] = kuv

  sd = get.sem.data(modul$semester)
  okupe = filter(sd$kupe,semester==modul$semester, modulid==modul$modulid)
  okumo = filter(sd$kumo,semester==modul$semester, modulid==modul$modulid)

  modify_time = Sys.time()
  diff.log = modul.diff.log(nmo=nmo,nmope=nmope, nmomo=nmomo, omo=modul,okupe=okupe, okumo=okumo, module=sd$module)

  # Keine Modifikationen
  if (is.null(diff.log)) {
    timedMessage("saveModulAlert",paste0("Sie haben noch keine Modifikationen am Modul vorgenommen."))
    return()
  }

  update.db.modul(nmo, nmope, nmomo, modify_time=modify_time, log=diff.log)

  app$modul = nmo

  sd = get.sem.data(update = TRUE)

  # Update table
  #proxy = dataTableProxy("moduleTable")
  #df = make.module.datatable.df(sd)
  #replaceData(proxy,df,resetPaging=FALSE)
  #update.module.ui()

  html = paste0("Die Änderungen im Modul," ,nmo$modulname," wurden mit folgender Logdatei gespeichert.","<pre>\n", diff.log,"</pre>")
  timedMessage("saveModulAlert",html=html,millis = 10000)
}

update.db.modul = function(modul, mozu, most,mosp, db=get.stukodb(),modify_user = app$userid, modify_time=Sys.time(), log=NULL, write_log = !is.null(log)) {
  restore.point("update.db.modul")

  modulid = modul$modulid
  semester = modul$semester

  modul$modify_user = modify_user
  modul$modify_time = modify_time

  if (!is.list(log) & write_log) {
    log = list(logtime=modify_time, userid=modify_user,logtype="modul", logtext=log)
  }

  res = dbWithTransaction(db,{
    dbDelete(db,"modul",nlist(modulid, semester))
    dbDelete(db,"modulzuordnung",nlist(modulid, semester))
    dbDelete(db,"modulschwerpunkt",nlist(modulid, semester))
    dbDelete(db,"modulstudiengang",nlist(modulid, semester))

    dbInsert(db,"modul",modul)
    if (NROW(mozu)>0)
      dbInsert(db,"modulzuordnung", mozu)
    if (NROW(mosp)>0)
      dbInsert(db,"modulschwerpunkt", mosp)
    if (NROW(most)>0)
      dbInsert(db,"modulstudiengang", most)

    if (write_log)
      dbInsert(db,"log", log)

  })
  return(!is(res,"try-error"))

}

modul.diff.log = function(nmo,nmope,nmomo,omo,okupe,okumo,modify_user = app$userid, modify_time=Sys.time(), module=NULL) {
  restore.point("modul.diff.log")

  change = FALSE
  log = paste0("Modifiziere Modul ", omo$modulname, " (", omo$modulid, ")")

  # modul
  cols = colnames(omo)
  diff.cols = setdiff(
    cols[is.true(nmo != omo)],
    c("modify_time","modify_user")
  )

  if (length(diff.cols)>0) {
    change = TRUE
    log = paste0(log,
      "\n\nModifikationen an 'modul':",
      paste0("\n  ",diff.cols,": '", omo[diff.cols], "' zu '", nmo[diff.cols],"'", collapse="")
    )
  }

  # Modulperson
  okupe$personid[is.na(okupe$personid)] = ""
  diff = tables.diff(select(nmope, -semester, -modulid), select(okupe,-semester, -modulid, -name))
  if (NROW(diff$added)>0 | NROW(diff$removed)>0) {
    change = TRUE
    added = do.call(paste,c(sep=", ",as.list(diff$added)))
    removed = do.call(paste,c(sep=", ",as.list(diff$removed)))
    log = paste0(log,
      "\n\nModifikationen an 'modulperson':",
      if (NROW(diff$added)>0)
        paste0("\n  Neu:", paste0("\n   - ", added, collapse="")),
      if (NROW(diff$removed)>0)
        paste0("\n  Entfernt oder Ueberschrieben:", paste0("\n   - ", removed, collapse=""))
    )
  }

  # modulmodul
  diff = tables.diff(select(nmomo,modulid), select(okumo,modulid))
  if (NROW(diff$added)>0 | NROW(diff$removed)>0) {
    change = TRUE
    log = paste0(log,
      "\n\nModifikationen an 'modulmodul':",
      if (NROW(diff$added)>0) {
        str = diff$added$modulid
        if (!is.null(module)) str = unique(filter(module, modulid == str)[["label"]])
        paste0("\n  Neu:", paste0("\n   - ", str, collapse=""))
      },
      if (NROW(diff$removed)>0) {
        str = diff$removed$modulid
        if (!is.null(module)) str = unique(filter(module, modulid == str)[["label"]])
        paste0("\n  Neu:", paste0("\n   - ", str, collapse=""))
      }
    )
  }

  if (!change)
    return(NULL)


  cat(log)

  log
}
