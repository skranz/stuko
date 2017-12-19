kurse.ui = function(..., app=getApp(), glob=app$glob) {

  kfilter = app$all.koord$personid
  names(kfilter) = paste0("Von ",app$all.koord$vorname, " ",app$all.koord$nachname, " koordinierte Kurse.")

  if (app$stuko) {
    kfilter = c("Alle Kurse"="all", kfilter)
  } else {
    kfilter(kfilter, "Alle Kurse")
  }
  app$kfilter = kfilter[1]

  ui = tagList(
    if (length(kfilter)>1) {
      tagList(HTML("Kursauswahl: "),simpleSelect("kfilter","",choices = kfilter))
    },
    div(id="kurseTableDiv",
      dataTableOutput("kurseTable")
    ),
    actionButton("refreshKurseBtn","",icon = icon("refresh")),
    simpleButton("addKursBtn","Neuen Kurs anlegen"),
    simpleButton("deactivateKurseBtn","Markierte Kurse (de-)aktivieren",form.sel = ".kursCheck"),
    simpleButton("copyKurseBtn","Markierte Kurse in anderes Semester kopieren"),
    simpleButton("delKurseBtn","Markierte Kurse entfernen",form.sel = ".kursCheck"),
    uiOutput("editKursUI"),
    br(),
    slimCollapsePanel("Hintergrundinformationen",HTML(glob$info$kurse))
  )

  buttonHandler("addKursBtn",new.kurs.click)
  buttonHandler("deactivateKurseBtn",deactivate.kurse.click)
  buttonHandler("delKurseBtn",delete.kurse.click)
  buttonHandler("copyKurseBtn", function(...) {
    showModal(copy.kurse.ui())
  })


  selectChangeHandler("kfilter", function(value,...) {
    app$kfilter = value
    update.kurse.ui()
  })

  ui
}


update.kurse.ui = function(app=getApp(), glob=app$glob,...) {
  restore.point("update.kurse.ui")
  sd = get.sem.data()

  #tab = html.table(sd$kurse)

  glob$sets$kursform

  df = make.kurse.datatable.df(sd)
  if (is.null(df)) {
    #setHtmlHide("kurseTableDiv")
    shinyEvents::setDataTable("kurseTable", NULL)
    return()
  } else {
    #setHtmlShow("kurseTableDiv")
  }

  dt = datatable(df,selection = 'none',escape=-1,rownames = FALSE, filter=list(position="top", clear=FALSE, plain=FALSE),
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

  #dt = dt %>% formatStyle("Aktiv", target="row",backgroundColor = "#ee0000")

  shinyEvents::setDataTable("kurseTable", dt,server=TRUE)

  classEventHandler("editKursBtn",event = "click",function(data=NULL, ...) {
    restore.point("editKursBtn")
    kurse = get.sem.data()$kurse
    kurs = filter(kurse, kursid==data$kursid)
    app$new.kurs = FALSE
    show.edit.kurs(kurs)
    cat("\neditKurs clicked...")
  })

}

make.kurse.datatable.df = function(sd, app=getApp(), glob=app$glob, kfilter = first.non.null(app$kfilter,"all")) {
  restore.point("make.kurse.table.df")


  kurse = sd$kurse

  # Filter Kurse fuer spezifierten Koordinator
  if (kfilter != "all") {
    kurse = filter(kurse, has.substr(koid, kfilter))
  }

  if (NROW(kurse)==0) return(NULL)



  kursids = kurse$kursid
  btns= paste0(
    checkBoxInputVector(paste0("kursCheck_",kursids), value=FALSE, extra.class="kursCheck", extra.head=paste0('data-kursids="',kursids,'" style="padding-right: 3px"')),

    simpleButtonVector(paste0("editKursBtn_",kursids),icon=icon(name = "pencil"), size="sm",extra.class = "editKursBtn",extra.head=paste0('data-kursid="',kursids,'"'))
    #simpleButtonVector(id=paste0("delKursBtn_",kursids),icon=icon(name = "trash-o"), size="sm", extra.class="delKursBtn", extra.head=paste0('data-kursids="',kursids,'"'))
  )


  df = transmute(kurse,Aktion=btns, Kurs=kursname, Dozent=dozent,Aktiv=ifelse(aktiv,"Ja","Nein"),SWS=sws_kurs+sws_uebung,BaMa=bama, Zuordnung=zuordnung, Schwerpunkte=schwerpunkt, Kursform=to.label(kurse$kursform, glob$sets$kursform), Sprache=sprache, Extern=ifelse(extern,"extern","intern"), ECTS=as.integer(ects), Koordinator=koordinator,Lehrauftrag=lehrauftrag, Module = num_modul, Turnus=turnus,  'Pruefung'=pruefungsform, Codesharing=ifelse(nchar(codeshare)>0,"Ja",""), 'Modifiziert am'=as.Date(modify_time), 'Modifiziert durch'=modify_user)

  df
}

show.edit.kurs = function(kurs,..., app=getApp(), glob=app$glob) {
  restore.point("show.edit.kurs")

  app$kurs = kurs
  form = glob$forms$kurs
  sd = get.sem.data(kurs$semester)

  widgets = lapply(names(form$fields), function(name) {
      fieldInput(name=name,form=form, value = kurs[[name]], lang="de",sets = glob$sets)
  })



  kp = filter(sd$kupe,kursid==kurs$kursid)
  kp.ui = tableform.ui(form=glob$forms$kursperson, data=select(kp,-semester, -kursid, -name), use.delete.btn = TRUE)

  #view.ui(kp.ui)


  mymodul = sd$kumo %>% filter(kursid == kurs$kursid, semester==kurs$semester)
  mymodul = mymodul$modulid

  mods = sd$module$modulid
  names(mods) = sd$module$label

  modSel = selectInput("kurseditModul", "Module", choices=mods, selected=mymodul, multiple=TRUE, width="80em")

  form.sel = paste0(c(paste0("#kurseditModul"),paste0("#kurs_",names(form$fields)),".tableform-kursperson-input"), collapse=", ")

  nw = length(widgets)
  ui = tagList(
    h3("Kurs bearbeiten"),
    fluidRow(column(width = 12, widgets[1])),
    modSel,
    helpText("Um Module zu bearbeiten / zu erstellen, gehen Sie bitte zum Reiter Module. Jedem Kurs sollte mind. ein Modul zugeordnet sein."),
    h4("Mitarbeiter"),
    kp.ui,
    simpleButton("addKurspersonBtn","Neuer Mitarbeiter."),
    helpText("Wenn kein Koordinator eingetragen wird, verwaltet der Studiendekan den Kurs direkt. Die Spalte SWS ist nur fuer Lehrbeauftragte relevant."),
    layout.widgets.as.fluid.grid(widgets[c(-1,-nw)], 3),
    fluidRow(column(width = 12, widgets[nw])),
    uiOutput("saveKursAlert"),
    simpleButton("saveKursBtn","Speichern",form.sel = form.sel)
  )

  buttonHandler("addKurspersonBtn", function(...) {
    restore.point("addKurspersonBtn")
    form.html.table.add.row(form=glob$forms$kursperson, data=data_frame(personid=NA, nachname="", vorname="", rolle="", lehrauftrag="-", dozent_sws=0), use.delete.btn = TRUE)
  })

  buttonHandler("saveKursBtn", function(...) {
    save.kurs.click(...)
  })





  setUI("editKursUI",ui)
  evalJS("$('html, body').animate({scrollTop: $('#addKursBtn').offset().top + 'px'}, 'fast');")
}


get.selected.kursid = function(formValues) {
  na = names(formValues)
  na = na[str.starts.with(na,"kursCheck_")]
  vals = unlist(formValues[na])
  na = str.right.of(na,"kursCheck_")
  na[vals]
}

deactivate.kurse.click = function(formValues, ..., app=getApp()) {
  restore.point("deactivate.kurse.click")
  semester = app$sem
  kurse = get.sem.data()$kurse
  ids = get.selected.kursid(formValues)

  dbWithTransaction(app$glob$db, {
    for (id in ids) {
      # Toggle aktiv
      kurs = filter(kurse, kursid==id)
      aktiv = kurs$aktiv[[1]]

      dbUpdate(app$glob$db,"kurs",vals = list(aktiv=!aktiv),where=list(semester=semester, kursid=id))
    }
  })

  sd = get.sem.data(update=TRUE)
  update.kurse.ui()

}


delete.kurse.click = function(formValues, ..., app=getApp()) {
  restore.point("delete.kurs.click")
  semester = app$sem
  kurse = get.sem.data()$kurse
  ids = get.selected.kursid(formValues)
  kurse = filter(kurse, kursid %in% ids)

  buttonHandler("cancelKursDelBtn",function(...) removeModal())
  buttonHandler("confirmKursDelBtn", function(...) {
    logtext= paste0("Entferne aus ", semester_name(semester), " die Kurse:\n", paste0("  -",kurse$kursname, collapse="\n"))

    db = app$glob$db
    dbWithTransaction(db,{
      for (id in kurse$kursid) {
        dbDelete(db,"kurs",list(semester=semester, kursid=id))
        dbDelete(db,"kursperson",list(semester=semester, kursid=id))
        dbDelete(db,"kursmodul",list(semester=semester, kursid=id))
      }

      write.stuko.log(logtext, logtype="del_kurs")
    })
    setUI("editKursUI","")
    sd = get.sem.data(update=TRUE)
    update.kurse.ui()
    removeModal()
  })

  showModal(modalDialog(easyClose=TRUE,fade=FALSE,
    title="Kurse wirklich entfernen?",
    p(paste0("Sie Sie sicher, dass Sie fuer das ", semester_name(semester), " die Kurse '", paste0(kurse$kursname, collapse=", "),"' mit allen Verknuepfungen entfernen wollen?")),
    footer = tagList(simpleButton("confirmKursDelBtn","Ja, entferne Kurs."), simpleButton("cancelKursDelBtn","Abbruch"))
  ))

}

new.kurs.click = function(..., app=getApp()) {
  restore.point("new.kurs.click")
  kurs = list(kursid="", semester=app$sem, aktiv=TRUE, turnus=2, kursform="vu", zeitform="w", sws_kurs=0, sws_uebung=0, sprache="de")
  app$new.kurs = TRUE
  show.edit.kurs(kurs)
}

save.kurs.click = function(kurs = app$kurs, formValues,..., app=getApp(), glob=app$glob, check.kursid=TRUE) {
  restore.point("save.kurs.click")
  cat("\nsave.kurs.click")

  semester = kurs$semester

  # Extract values
  kuv = extract.form.formValues(formValues, form=glob$forms$kurs)

  nku = kurs
  nku[names(kuv)] = kuv

  # Hat sich kursid geaendert
  if (!is.true(nku$kursid == kurs$kursid) & !is.true(app$new.kurs)) {
    html = paste0("Sie haben die Kurs-ID geaendert von ", kurs$kursid , " zu ", nku$kursid,". Eine Aenderung der Kurs-ID ist aber nicht moeglich.")
    timedMessage("saveKursAlert",html=html,millis = 10000)
    return()
  } else if (is.true(app$new.kurs)) {
    res = is.new.kursid.valid(nku$kursid)
    if (!res$ok) {
      timedMessage("saveKursAlert", msg=res$msg, millis=10000)
      return()
    }
  }


  kumov = unlist(formValues$kurseditModul)

  nkumo = if (NROW(kumov)>0) fast_df(kursid=nku$kursid, semester=semester, modulid=kumov)


  kupev = extract.tableform.formValues(formValues, form=glob$forms$kursperson)

  nkupe = if(NROW(kupev)>0) kupev %>% mutate(semester=semester, kursid=nku$kursid)

  # set vorname and nachname automatic
  # for persons with personid
  nkupe = set.personid.names(nkupe)


  sd = get.sem.data(kurs$semester)
  okupe = filter(sd$kupe,semester==kurs$semester, kursid==kurs$kursid)
  okumo = filter(sd$kumo,semester==kurs$semester, kursid==kurs$kursid)

  modify_time = Sys.time()

  if (!app$new.kurs) {
    diff.log = kurs.diff.log(nku=nku,nkupe=nkupe, nkumo=nkumo, oku=kurs,okupe=okupe, okumo=okumo, module=sd$module)



    # Keine Modifikationen
    if (is.null(diff.log)) {
      timedMessage("saveKursAlert",paste0("Sie haben noch keine Modifikationen am Kurs vorgenommen."))
      return()
    }
  } else {
    diff.log = paste0("Neuer Kurs im ", semester_name(nku$semester), " erstellt:\n  -", nku$kursname, " (", nku$kursid,")")
  }

  update.db.kurs(nku, nkupe, nkumo,modify_user = app$userid, modify_time=modify_time, log=diff.log)

  app$kurs = as_data_frame(nku)
  app$new.kurs = FALSE

  sd = get.sem.data(update = TRUE)

  # Update table
  #proxy = dataTableProxy("kurseTable")
  #df = make.kurse.datatable.df(sd)
  #replaceData(proxy,df,resetPaging=FALSE)
  #update.kurse.ui()

  html = paste0("Die Aenderungen im Kurs '" ,nku$kursname,"' wurden mit folgendem Logeintrag gespeichert.","<pre>\n", diff.log,"</pre>")
  timedMessage("saveKursAlert",html=html,millis = 10000)
}

update.db.kurs = function(kurs, kursperson, kursmodul, db=get.stukodb(),modify_user = app$userid, modify_time=Sys.time(), log=NULL, write_log = !is.null(log)) {
  restore.point("update.db.kurs")

  kursid = kurs$kursid
  semester = kurs$semester

  kurs$modify_user = modify_user
  kurs$modify_time = modify_time

  if (!is.list(log) & write_log) {
    log = list(logtime=modify_time, userid=modify_user,logtype="kurs", logtext=log)
  }

  res = dbWithTransaction(db,{
    dbDelete(db,"kurs",nlist(kursid, semester))
    dbDelete(db,"kursperson",nlist(kursid, semester))
    dbDelete(db,"kursmodul",nlist(kursid, semester))

    dbInsert(db,"kurs",kurs)
    if (NROW(kursperson)>0)
      dbInsert(db,"kursperson", kursperson)
    if (NROW(kursmodul)>0)
      dbInsert(db,"kursmodul", kursmodul)

    if (write_log)
      dbInsert(db,"log", log)

  })
  return(!is(res,"try-error"))

}

kurs.diff.log = function(nku,nkupe,nkumo,oku,okupe,okumo,modify_user = app$userid, modify_time=Sys.time(), module=NULL) {
  restore.point("kurs.diff.log")

  change = FALSE
  log = paste0("Kurs ", oku$kursname, " (", oku$kursid, ") im ", semester_name(oku$semester), " modifiziert:")

  # kurs
  cols = names(oku)
  diff.cols = setdiff(
    cols[is.true(as_data_frame(nku) != as_data_frame(oku))],
    c("modify_time","modify_user")
  )

  if (length(diff.cols)>0) {
    change = TRUE
    log = paste0(log,
      paste0("\n  ",diff.cols,": '", oku[diff.cols], "' zu '", nku[diff.cols],"'", collapse="")
    )
  }

  # Kursperson
  okupe$personid[is.na(okupe$personid)] = ""
  diff = tables.diff(select(nkupe, -semester, -kursid), select(okupe,-semester, -kursid, -name))
  if (NROW(diff$added)>0 | NROW(diff$removed)>0) {
    change = TRUE
    added = do.call(paste,c(sep=", ",as.list(diff$added)))
    removed = do.call(paste,c(sep=", ",as.list(diff$removed)))
    log = paste0(log,
      if (NROW(diff$added)>0)
        paste0("\nNeue Person:", paste0("\n   - ", added, collapse="")),
      if (NROW(diff$removed)>0)
        paste0("\nEntfernte oder Ueberschriebene Person:", paste0("\n   - ", removed, collapse=""))
    )
  }

  # kursmodul
  diff = tables.diff(select(nkumo,modulid), select(okumo,modulid))
  if (NROW(diff$added)>0 | NROW(diff$removed)>0) {
    change = TRUE
    log = paste0(log,
      if (NROW(diff$added)>0) {
        str = diff$added$modulid
        if (!is.null(module)) str = unique(filter(module, modulid == str)[["label"]])
        paste0("\nNeu zugeordnetes Modul:", paste0("\n   - ", str, collapse=""))
      },
      if (NROW(diff$removed)>0) {
        str = diff$removed$modulid
        if (!is.null(module)) str = unique(filter(module, modulid == str)[["label"]])
        paste0("\nEntferntes Modul:", paste0("\n   - ", str, collapse=""))
      }
    )
  }

  if (!change)
    return(NULL)


  cat(log)

  log
}


is.new.kursid.valid = function(kursid, kurse=get.sem.data()$ku) {
  if (kursid %in% kurse$kursid) {
    row = match(kurse$kursid)
    return(list(ok=FALSE, msg=paste0("Die Kurs-ID ", kursid, " wird bereits vom exitierenden Kurs ", kurse$kursname[row], " genutzt.")))
  }
  if (!is.valid.id(kursid)) {
    return(list(ok=FALSE, msg=paste0("Die angegebene Kurs-ID ", kursid, " hat keinen Validen Syntax. Starten Sie mit einem Buchstaben und nutzen Sie nur Buchstaben, Zahlen und Unterstrich.")))
  }

  list(ok=TRUE)
}

is.valid.id = function(id) {
  nid = make.names(gsub(".","_",id, fixed=TRUE))
  id == nid
}



copy.kurse.ui = function(tosem = first.non.null(app$tosem, app$sem+10),..., app=getApp(), glob=app$glob) {
  restore.point("copy.kurse.ui")

  ui = modalDialog(size = "l", footer=NULL,
    tagList(
      if (app$stuko & app$admin)
        p("Hinweis: Um als StuKo-Administrator alle Turnusgemaessen Kurse in ein neues Semester zu kopieren, nutzen Sie die entsprechende Option im Reiter 'Admin'"),

      selectInput("tosemCK","Kurse und zugehoerige Module im folgendes Semester kopieren", choices = glob$sets$semester, selected=tosem),
      checkboxInput("tosemOverwriteCK",label="Bereits im neuen Semester angelegte Kurse und Module ueberschreiben"),
      hr(),
      simpleButton("tosemOkCKBtn", "Markierte Kurse und Module kopieren",form.sel = ".kursCheck, #tosemCK, #tosemOverwriteCK"), simpleButton("cancelModalBtn","Abbruch")
    )
  )

  buttonHandler("cancelModalBtn",function(...) removeModal())
  buttonHandler("tosemOkCKBtn",function(formValues,...) {
    restore.point("tosemOkCKBtn")
    tosem = as.integer(formValues$tosemCK)
    overwrite = as.logical(formValues$tosemOverwriteCK)

    kursids = get.selected.kursid(formValues)
    cat("Copy kurses to", tosem,"...")
    copy.selected.kurse(tosem=tosem,overwrite = overwrite, kursids=kursids)
    removeModal()
  })
  ui
}


copy.selected.kurse = function(kursids, tosem, overwrite=FALSE, ...,fromsem = getApp()$sem, db=get.stukodb()) {
  restore.point("copy.selected.kurse")

  sd = get.sem.data(fromsem)

  ku = filter(sd$kurs, kursid %in% kursids)

  tosd = get.sem.data(tosem)

  if (!overwrite) {
    ku = filter(ku, ! kursid %in% tosd$kurs$kursid)
  }
  if (NROW(ku)==0) return(NULL)

  ku = arrange(ku, desc(aktiv))
  dupl = duplicated(ku$kursid)
  ku = ku[!dupl, ]

  kumo = filter(sd$kumo, kursid %in% ku$kursid)
  kupe = filter(sd$kupe, kursid %in% ku$kursid)
  mo   = filter(sd$mo, modulid %in% kumo$modulid)
  most = filter(sd$most, modulid %in% kumo$modulid)
  mosp = filter(sd$mosp, modulid %in% kumo$modulid)
  mozu = filter(sd$mozu, modulid %in% kumo$modulid)


  log = paste0("Kopiere ausgewählte Kurse und Module von ", semester_name(fromsem), " nach ", semester_name(tosem),"\n\n",
    NROW(ku), " Kurse:\n",
    paste0("  - ", ku$kursname, collapse="\n"),
    "\n\n", NROW(mo), " Module:\n",
    paste0("  - ", mo$titel, collapse="\n")
  )

  if (NROW(ku)>0)   ku$semester   = tosem
  if (NROW(kumo)>0) kumo$semester = tosem
  if (NROW(kupe)>0) kupe$semester = tosem
  if (NROW(mo)>0)   mo$semester   = tosem
  if (NROW(most)>0) most$semester = tosem
  if (NROW(mosp)>0) mosp$semester = tosem
  if (NROW(mozu)>0) mozu$semester = tosem

  ku$zukunft_sem = tosem + 5*ku$turnus
  ku$zukunft_sem2 = tosem + 5*(ku$turnus*2)


  dbWithTransaction(db, {
    if (overwrite) {
      for (kursid in ku$kursid) {
        dbDelete(db,"kurs", list(semester=tosem, kursid=kursid))
        dbDelete(db,"kursperson", list(semester=tosem, kursid=kursid))
        dbDelete(db,"kursmodul", list(semester=tosem, kursid=kursid))
      }
      for (kursid in ku$kursid) {
        dbDelete(db,"modul", list(semester=tosem, modulid=modulid))
        dbDelete(db,"modulzuordnung", list(semester=tosem, modulid=modulid))
        dbDelete(db,"modulschwerpunkt", list(semester=tosem, modulid=modulid))
        dbDelete(db,"modulstudiengang", list(semester=tosem, modulid=modulid))
      }
    }
    dbInsert(db,"kurs",ku)
    dbInsert(db,"kursperson",kupe)
    dbInsert(db,"kursmodul",kumo)
    dbInsert(db,"modul",mo)
    dbInsert(db,"modulzuordnung",mozu)
    dbInsert(db,"modulschwerpunkt",mosp)
    dbInsert(db,"modulstudiengang",most)

    write.stuko.log(log,"kopiere")

  })
  sd = get.sem.data(tosem, update = TRUE)
}


