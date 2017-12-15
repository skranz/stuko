module.ui = function(..., app=getApp(), glob=app$glob) {
  ui= tagList(
    dataTableOutput("moduleTable"),
    simpleButton("refreshModuleBtn","",icon = icon("refresh")),
    simpleButton("addModulBtn","Neues Modul anlegen"),
    simpleButton("delModuleBtn","Markierte Module löschen",form.sel = ".modulCheck"),
    uiOutput("editModulUI")
  )

  buttonHandler("addModulBtn",new.modul.click)
  buttonHandler("delModuleBtn",delete.module.click)
  ui

}

update.module.ui = function(app=getApp(), glob=app$glob,...) {
  restore.point("update.module.ui")
  sd = get.sem.data()


  df = make.module.datatable.df(sd)
  if (is.null(df)) {
    shinyEvents::setDataTable("moduleTable", NULL)
    return()
  }

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
    module = get.sem.data()$module
    modul = filter(module, modulid==data$modulid)
    app$new.modul = FALSE

    show.edit.modul(modul)
    cat("\neditModul clicked...")
  })

}



make.module.datatable.df = function(sd, app=getApp(), glob=app$glob) {
  restore.point("make.module.table.df")

  if (NROW(sd$module)==0) return(NULL)


  ids = sd$module$modulid
  btns= paste0(
    checkBoxInputVector(paste0("modulCheck_",ids), value=FALSE, extra.class="modulCheck", extra.head=paste0('data-kursids="',ids,'" style="padding-right: 3px"')),
    simpleButtonVector(id=paste0("editModulBtn_",ids),icon=icon(name = "pencil"), size="sm", extra.class="editModulBtn", extra.head=paste0('data-modulid="',ids,'"'))
  )


  df = transmute(sd$module,Aktion=btns, Modul=titel, BaMa=bama,Studiengang=studiengang, Zuordnung=zuordnung, Schwerpunkte=schwerpunkt, Extern=ifelse(extern,"extern","intern"), ECTS=as.integer(ects),
#   Kurse = num_kurse,
    Pruefung=pruefungsform, Dozent=dozent, Kurs=kurs, 'Anz. Kurs'=num_kurs, 'Modifiziert am'=as.Date(modify_time), 'Modifiziert durch'=modify_user)

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
    uiOutput("saveModulAlert"),
    simpleButton("saveModulBtn","Modul Speichern",form.sel = form.sel)
  )

  buttonHandler("saveModulBtn", function(...) {
    save.modul.click(...)
  })

  setUI("editModulUI",ui)
  evalJS("$('html, body').animate({scrollTop: $('#addModulBtn').offset().top + 'px'}, 'fast');")
}


delete.module.click = function(formValues, ..., app=getApp()) {
  restore.point("delete.module.click")
  semester = app$sem
  module = get.sem.data()$module
  ids = get.selected.modulid(formValues)
  module = filter(module, modulid %in% ids)


  buttonHandler("cancelModulDelBtn",function(...) removeModal())
  buttonHandler("confirmModulDelBtn", function(...) {
    restore.point("kjslfdhfk")
    logtext= paste0("Entferne aus ", semester_name(semester), " die Module:\n", paste0("  -",module$titel, collapse="\n"))

    db = app$glob$db
    dbWithTransaction(db,{
      for (id in module$modulid) {
        dbDelete(db,"modul",list(semester=semester, modulid=id))
        dbDelete(db,"modulstudiengang",list(semester=semester, modulid=id))
        dbDelete(db,"modulschwerpunkt",list(semester=semester, modulid=id))
        dbDelete(db,"modulzuordnung",list(semester=semester, modulid=id))
        dbDelete(db,"kursmodul",list(semester=semester, modulid=id))
      }

      write.stuko.log(logtext, logtype="del_modul")
    })
    setUI("editModulUI","")
    sd = get.sem.data(update=TRUE)
    update.module.ui()
    removeModal()
  })

  showModal(modalDialog(easyClose=TRUE,fade=FALSE,
    title="Modul wirklich entfernen?",
    p(paste0("Sie Sie sicher, dass Sie fuer das ", semester_name(semester), " die Module ",paste0(module$titel,collapse=",")," mit allen Verknuepfungen entfernen wollen?")),
    footer = tagList(simpleButton("confirmModulDelBtn","Ja, entferne Module."), simpleButton("cancelModulDelBtn","Abbruch"))
  ))

}

new.modul.click = function(..., app=getApp()) {
  restore.point("new.modul.click")
  modul = list(modulid="", semester=app$sem,modify_time = Sys.time(), modify_user = app$userid, code="", extern=FALSE, titel="", ects=0, pruefungsform="k")
  app$new.modul = TRUE
  show.edit.modul(modul)
}


save.modul.click = function(modul = app$modul, formValues,..., sd = get.sem.data(modul$semester)) {
  app=getApp()
  glob=app$glob
  modify_user = app$userid

  restore.point("save.modul.click")
  cat("\nsave.modul.click")

  semester = modul$semester

  # Extract values
  mov = extract.form.formValues(formValues, form=glob$forms$modul)



  nmo = modul[colnames(sd$mo)]
  fields = intersect(names(mov), names(nmo))
  nmo[fields] = mov[fields]

  # Hat sich modulid geaendert
  if (!is.true(nmo$modulid == modul$modulid) & !is.true(app$new.modul)) {
    html = paste0("Sie haben die Modul-ID geaendert von ", modul$modulid , " zu ", nmo$modulid,". Eine Aenderung der Modul-ID ist aber nicht moeglich.")
    timedMessage("saveModulAlert",html=html,millis = 10000)
    return()
  } else if (is.true(app$new.modul)) {
    res = is.new.modulid.valid(nmo$modulid)
    if (!res$ok) {
      timedMessage("saveModulAlert", msg=res$msg, millis=10000)
      return()
    }
  }



  nmost = if (NROW(mov$studiengang)>0)
    fast_df(modulid=nmo$modulid,semester=semester, studiengang=unlist(mov$studiengang))

  nmosp = if (NROW(mov$schwerpunkt)>0)
    fast_df(modulid=nmo$modulid,semester=semester, schwerpunkt=unlist(mov$schwerpunkt))

  nmozu = if (NROW(mov$zuordnung)>0)
    fast_df(modulid=nmo$modulid,semester=semester, zuordnung=unlist(mov$zuordnung))


  omo = filter(sd$mo,semester==modul$semester, modulid==modul$modulid)
  omost = filter(sd$most,semester==modul$semester, modulid==modul$modulid)

  omosp = filter(sd$mosp,semester==modul$semester, modulid==modul$modulid)

  omozu = filter(sd$mozu,semester==modul$semester, modulid==modul$modulid)

  modify_time = Sys.time()

  if (!app$new.modul) {
    diff.log = modul.diff.log(nmo=nmo,nmost=nmost, nmosp=nmosp,nmozu=nmozu, omo=omo,omost=omost, omosp=omosp,omozu=omozu, module=sd$module)

    # Keine Modifikationen
    if (is.null(diff.log)) {
      timedMessage("saveModulAlert",paste0("Sie haben noch keine Modifikationen am Modul vorgenommen."))
      return()
    }
  } else {
    diff.log = paste0("Neues Modul im ", semester_name(nmo$semester), " erstellt: ", nmo$titel, " (", nmo$modulid,")")
  }

  update.db.modul(nmo, nmost, nmosp, nmozu,modify_user=modify_user, modify_time=modify_time, log=diff.log)

  modul[names(nmo)] = nmo
  modul$modify_user = modify_user
  app$modul = as_data_frame(modul)
  app$new.modul = FALSE

  sd = get.sem.data(update = TRUE)

  html = paste0("Die Aenderungen im Modul," ,nmo$titel," wurden mit folgendem Logeintrag gespeichert.","<pre>\n", diff.log,"</pre>")
  timedMessage("saveModulAlert",html=html,millis = 10000)
}

update.db.modul = function(mo, most,mosp,mozu, db=get.stukodb(),modify_user, modify_time=Sys.time(), log=NULL, write_log = !is.null(log)) {
  restore.point("update.db.modul")

  modulid = mo$modulid
  semester = mo$semester

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
    cols[!is.true(nmo == omo)],
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



is.new.modulid.valid = function(modulid, module=get.sem.data()$mo) {
  if (modulid %in% module$modulid) {
    row = match(module$modulid)
    return(list(ok=FALSE, msg=paste0("Die Modul-ID ", modulid, " wird bereits vom exitierenden Modul ", module$titel[row], " genutzt.")))
  }
  if (!is.valid.id(modulid)) {
    return(list(ok=FALSE, msg=paste0("Die angegebene Modul-ID ", modulid, " hat keinen validen Syntax. Starten Sie mit einem Buchstaben und nutzen Sie nur Buchstaben, Zahlen und Unterstrich.")))
  }

  list(ok=TRUE)
}

examples.fast.edit = function() {
  restore.point.options(display.restore.point=TRUE)
  stuko.dir = "D:/libraries/stuko/ulm"
  setwd(stuko.dir)
  sapp = stukoApp(stuko.dir, sem=170)
  glob = sapp$glob
  app = eventsApp()
  app$glob = glob
  app$sem = sapp$sem
  sd = get.sem.data()

  mo = sd$mo
  mo = sd$mozu %>% group_by(modulid,semester) %>%
    summarize(zuordnung=list(zuordnung)) %>%
    right_join(mo, by=c("modulid","semester"))

  mo = sd$mosp %>% group_by(modulid,semester) %>%
    summarize(schwerpunkt=list(schwerpunkt)) %>%
    right_join(mo, by=c("modulid","semester"))

  mo = sd$most %>% group_by(modulid,semester) %>%
    summarize(studiengang=list(studiengang)) %>%
    right_join(mo, by=c("modulid","semester"))

  mo = ungroup(mo)

  saveRDS(mo,"D:/libraries/stuko/mo_fast_edit.Rds")


  df = transmute(mo, Modul=titel,
    Zuordnungen=multiSelectizeInputVector("zuordnung", value=zuordnung,choices=glob$sets$zuordnung),
    Studiengaenge=multiSelectizeInputVector("studiengang", value=studiengang,choices=glob$sets$studiengang),
    Schwerpunkte=multiSelectizeInputVector("schwerpunkt", value=schwerpunkt,choices=glob$sets$schwerpunkt),
  )

  cat(df$Zuordnungen[[1]])
  #viewApp(app)
  app$ui = fluidPage(
    selectizeHeaders(),
    tags$style("#modul-form-table td,#modul-form-table th {padding-right: 1em}"),
    p("Tabelle:"),
    HTML(form.html.table(df,id="modul-form-table",nowrap=FALSE)),
    tags$script(HTML('$("#modul-form-table select").selectize();'))
  )

  appInitHandler(function(...) {
    #evalJS('$("#modul-form-table select").selectize();')
  })

  viewApp(app,launch.browser = TRUE)
}

# Work in Progress

set.module.table.edit.ui = function(...,app=getApp()) {
  restore.point("set.module.table.edit.ui")
  glob = app$glob

  app = eventsApp()

  form = as.environment(glob$forms$modul_table_edit)
  sd = get.sem.data()
  data = select(sd$mo, titel, extern, zuordnung, studiengang)

  form$data = form$org.data = data

  ui = tableform.ui(form=form, auto.filter = TRUE, use.checkbox = TRUE, paginate = TRUE)

  tableformFilterHandler("module_table", form=form, function(value,filter, fdata,...) {
    restore.point("filter.change")
    tableform.update.body(form=form, data=fdata)
    cat("\nfilter changed", sample.int(1000,1))
  })
}


get.selected.modulid = function(formValues) {
  na = names(formValues)
  na = na[str.starts.with(na,"modulCheck_")]
  vals = unlist(formValues[na])
  na = str.right.of(na,"modulCheck_")
  na[vals]
}
