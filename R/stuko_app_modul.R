
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

  most = filter(sd$most,semester==modul$semester, modulid==modul$modulid)
  mosp = filter(sd$mosp,semester==modul$semester, modulid==modul$modulid)
  mozu = filter(sd$mozu,semester==modul$semester, modulid==modul$modulid)

  widgets = lapply(names(form$fields), function(name) {
    if (name == "studiengang") {
      #restore.point("kdfdhfkdh")
      value = most$studiengang
    } else if (name == "zuordnung") {
      value = mozu$zuordnung
    } else if (name == "schwerpunkt") {
      value = mosp$schwerpunkt
    } else {
      value = modul[[name]]
    }

    fieldInput(name=name,form=form, value = value, lang="de",sets = glob$sets)
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

save.modul.click = function(modul = app$modul, formValues,..., app=getApp(), glob=app$glob, sd = get.sem.data(modul$semester)) {
  restore.point("save.modul.click")
  cat("\nsave.modul.click")

  modulid = modul$modulid
  semester = modul$semester

  # Extract values
  mov = extract.form.formValues(formValues, form=glob$forms$modul)

  nmo = modul[colnames(sd$mo)]
  fields = intersect(names(mov), names(nmo))
  nmo[fields] = mov[fields]

  nmost = if (NROW(mov$studiengang)>0)
    fast_df(modulid=modulid,semester=semester, studiengang=unlist(mov$studiengang))

  nmosp = if (NROW(mov$schwerpunkt)>0)
    fast_df(modulid=modulid,semester=semester, schwerpunkt=unlist(mov$schwerpunkt))

  nmozu = if (NROW(mov$zuordnung)>0)
    fast_df(modulid=modulid,semester=semester, zuordnung=unlist(mov$zuordnung))


  omo = filter(sd$mo,semester==modul$semester, modulid==modul$modulid)
  omost = filter(sd$most,semester==modul$semester, modulid==modul$modulid)

  omosp = filter(sd$mosp,semester==modul$semester, modulid==modul$modulid)

  omozu = filter(sd$mozu,semester==modul$semester, modulid==modul$modulid)

  modify_time = Sys.time()
  diff.log = modul.diff.log(nmo=nmo,nmost=nmost, nmosp=nmosp,nmozu=nmozu, omo=omo,omost=omost, omosp=omosp,omozu=omozu, module=sd$module)

  # Keine Modifikationen
  if (is.null(diff.log)) {
    timedMessage("saveModulAlert",paste0("Sie haben noch keine Modifikationen am Modul vorgenommen."))
    return()
  }

  update.db.modul(nmo, nmost, nmosp, nmozu, modify_time=modify_time, log=diff.log)

  modul[colnames(nmo)] = nmo
  app$modul = modul

  sd = get.sem.data(update = TRUE)

  html = paste0("Die Änderungen im Modul," ,nmo$modulname," wurden mit folgender Logdatei gespeichert.","<pre>\n", diff.log,"</pre>")
  timedMessage("saveModulAlert",html=html,millis = 10000)
}

update.db.modul = function(mo, most,mosp,mozu, db=get.stukodb(),modify_user = app$userid, modify_time=Sys.time(), log=NULL, write_log = !is.null(log)) {
  restore.point("update.db.modul")

  modulid = modul$modulid
  semester = modul$semester

  mo$modify_user = modify_user
  mo$modify_time = modify_time

  if (!is.list(log) & write_log) {
    log = list(logtime=modify_time, userid=modify_user,logtype="modul", logtext=log)
  }

  res = dbWithTransaction(db,{
    dbDelete(db,"modul",nlist(modulid, semester))
    dbDelete(db,"modulzuordnung",nlist(modulid, semester))
    dbDelete(db,"modulschwerpunkt",nlist(modulid, semester))
    dbDelete(db,"modulstudiengang",nlist(modulid, semester))

    dbInsert(db,"modul",mo)
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

modul.diff.log = function(nmo,nmost,nmosp,nmozu,omo,omost,omosp,omozu,modify_user = app$userid, modify_time=Sys.time(), module=NULL) {
  restore.point("modul.diff.log")

  change = FALSE
  log = paste0("Modifiziere Modul ", omo$titel, " (", omo$modulid, ", ", semester_name(omo$semester), ")\n")

  # modul
  cols = colnames(omo)
  diff.cols = setdiff(
    cols[is.true(nmo != omo)],
    c("modify_time","modify_user")
  )

  if (length(diff.cols)>0) {
    change = TRUE
    log = paste0(log,
      paste0("\n  ",diff.cols,": '", omo[diff.cols], "' zu '", nmo[diff.cols],"'", collapse="")
    )
  }

  # modulstudiengang
  added = unique(setdiff(nmost$studiengang, omost$studiengang))
  if (NROW(added)>0) {
    change = TRUE
    log = paste0(log,"\nNeue Studiengaenge: ",paste0(added, collapse=", "))
  }
  removed = unique(setdiff(omost$studiengang, nmost$studiengang))
  if (NROW(removed)>0) {
    change = TRUE
    log = paste0(log,"\nEntfernte Studiengaenge: ",paste0(removed, collapse=", "))
  }

  # modulschwerpunkt
  added = unique(setdiff(nmosp$schwerpunkt, omosp$schwerpunkt))
  if (NROW(added)>0) {
    change = TRUE
    log = paste0(log,"\nNeue Schwerpunkte: ",paste0(added, collapse=", "))
  }
  removed = unique(setdiff(omosp$schwerpunkt, nmosp$schwerpunkt))
  if (NROW(removed)>0) {
    change = TRUE
    log = paste0(log,"\nEntfernte Schwerpunkte: ",paste0(removed, collapse=", "))
  }

  # modulzuordnung
  added = unique(setdiff(nmozu$zuordnung, omozu$zuordnung))
  if (NROW(added)>0) {
    change = TRUE
    log = paste0(log,"\nNeue Zuordnungen: ",paste0(added, collapse=", "))
  }
  removed = unique(setdiff(omozu$zuordnung, nmozu$zuordnung))
  if (NROW(removed)>0) {
    change = TRUE
    log = paste0(log,"\nEntfernte Zuordnungen: ",paste0(removed, collapse=", "))
  }

  if (!change)
    return(NULL)

  cat(log)

  log
}
