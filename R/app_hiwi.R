hiwis.ui = function(..., app=getApp(), glob=app$glob) {
  restore.point("hiwis.ui")

  kfilter = app$all.koord$personid
  names(kfilter) = paste0(app$all.koord$vorname, " ",app$all.koord$nachname)
  app$hiwi.koord = kfilter[1]
  ui = tagList(
    if (length(kfilter)>1) {
      tagList(HTML("Koordinator auswaehlen: "),simpleSelect("hiwikoord","",choices = kfilter))
    },
    #h4("Antraege fuer Hilfskraefte:"),
    div(id="hiwisTableDiv",
      dataTableOutput("hiwisTable")
    ),
    simpleButton("addHiwiBtn","Neuen Hilfskraftantrag erstellen"),
    uiOutput("editHiwiUI"),
    br(),
    slimCollapsePanel("Hintergrundinformationen",HTML(glob$info$hiwis))
  )

  buttonHandler("addHiwiBtn",new.hiwi.click)

  selectChangeHandler("kfilter", function(value,...) {
    app$hiwi.koord = value
    update.hiwis.ui()
  })

  ui
}


update.hiwis.ui = function(app=getApp(), glob=app$glob,...) {
  restore.point("update.hiwis.ui")
  sd = get.sem.data()

  is.stuko = filter(glob$person, personid == app$hiwi.koord)$stuko

  if (is.stuko) {
    app$hiwis = dbGet(glob$db, "hiwi", list(semester=app$sem))
  } else {
    app$hiwis = dbGet(glob$db, "hiwi", list(personid=app$hiwi.koord, semester=app$sem))
  }


  df = make.hiwis.datatable.df(sd)
  if (is.null(df)) {
    #setHtmlHide("hiwisTableDiv")
    shinyEvents::setDataTable("hiwisTable", NULL)
    return()
  } else {
    #setHtmlShow("hiwisTableDiv")
  }

  dt = datatable(df,selection = 'none',escape=-1,rownames = FALSE, filter=list(position="top", clear=FALSE, plain=FALSE),
    class="display compact",
#    style="bootstrap",
    autoHideNavigation = TRUE, extensions = c("Buttons"),
    options = list(
      lengthMenu = c(10, 25, 50, 1000),
      dom = 'Blfrtip',
      buttons = c('copy','excel','csv'),
      select = FALSE,
      columnDefs = list(list(width="3em", targets=c(0))),
      autoWidth=TRUE
    )
  )

  shinyEvents::setDataTable("hiwisTable", dt,server=TRUE)

  classEventHandler("editHiwiBtn",event = "click",function(data=NULL, ...) {
    restore.point("editHiwiBtn")
    hiwis = app$hiwis
    hiwi = filter(hiwis, antragid==data$antragid)
    app$new.hiwi = FALSE
    show.edit.hiwi(hiwi, is.stuko=is.stuko)
    cat("\neditHiwi clicked...")
  })

}

make.hiwis.datatable.df = function(sd, hiwis=app$hiwis, app=getApp(), glob=app$glob, kfilter = first.non.null(app$kfilter,"all")) {
  restore.point("make.hiwis.table.df")

  if (NROW(hiwis)==0) return(NULL)
  ids = hiwis$antragid
  btns= paste0(
    simpleButtonVector(paste0("editHiwiBtn_",ids),icon=icon(name = "pencil"), size="sm",extra.class = "editHiwiBtn",extra.head=paste0('data-hiwiid="',ids,'"'))
  )


  df = transmute(hiwis,Aktion=btns, Status=to.label(antragstatus))

  df
}

show.edit.hiwi = function(hiwi, is.stuko = FALSE, new.hiwi = isTRUE(app$new.hiwi), ..., app=getApp(), glob=app$glob) {
  restore.point("show.edit.hiwi")

  app$hiwi = hiwi
  form = glob$forms$hiwi
  sd = get.sem.data(hiwi$semester)

  widgets = lapply(names(form$fields), function(name) {
      fieldInput(name=name,form=form, value = hiwi[[name]], lang="de",sets = glob$sets)
  })
  nw = length(widgets)
  ui = tagList(
    h3("Hilfskraftantrag bearbeiten"),
    layout.widgets.as.fluid.grid(widgets, 2),
    uiOutput("saveHiwiAlert"),
    simpleButton("saveHiwiBtn","Antrag Speichern",form.sel = form.sel)
  )
  buttonHandler("saveHiwiBtn", function(...) {
    save.hiwi.click(...)
  })





  setUI("editHiwiUI",ui)
  evalJS("$('html, body').animate({scrollTop: $('#addHiwiBtn').offset().top + 'px'}, 'fast');")
}


get.selected.hiwiid = function(formValues) {
  na = names(formValues)
  na = na[str.starts.with(na,"hiwiCheck_")]
  vals = unlist(formValues[na])
  na = str.right.of(na,"hiwiCheck_")
  na[vals]
}

deactivate.hiwis.click = function(formValues, ..., app=getApp()) {
  restore.point("deactivate.hiwis.click")
  semester = app$sem
  hiwis = get.sem.data()$hiwis
  ids = get.selected.hiwiid(formValues)

  dbWithTransaction(app$glob$db, {
    for (id in ids) {
      # Toggle aktiv
      hiwi = filter(hiwis, hiwiid==id)
      aktiv = hiwi$aktiv[[1]]

      dbUpdate(app$glob$db,"hiwi",vals = list(aktiv=!aktiv),where=list(semester=semester, hiwiid=id))
    }
  })

  sd = get.sem.data(update=TRUE)
  update.hiwis.ui()

}


delete.hiwis.click = function(formValues, ..., app=getApp()) {
  restore.point("delete.hiwi.click")
  semester = app$sem
  hiwis = get.sem.data()$hiwis
  ids = get.selected.hiwiid(formValues)
  hiwis = filter(hiwis, hiwiid %in% ids)

  buttonHandler("cancelHiwiDelBtn",function(...) removeModal())
  buttonHandler("confirmHiwiDelBtn", function(...) {
    logtext= paste0("Entferne aus ", semester_name(semester), " die hiwis:\n", paste0("  -",hiwis$hiwiname, collapse="\n"))

    db = app$glob$db
    dbWithTransaction(db,{
      for (id in hiwis$hiwiid) {
        dbDelete(db,"hiwi",list(semester=semester, hiwiid=id))
        dbDelete(db,"hiwiperson",list(semester=semester, hiwiid=id))
        dbDelete(db,"hiwimodul",list(semester=semester, hiwiid=id))
      }

      write.stuko.log(logtext, logtype="del_hiwi")
    })
    setUI("editHiwiUI","")
    sd = get.sem.data(update=TRUE)
    update.hiwis.ui()
    removeModal()
  })

  showModal(modalDialog(easyClose=TRUE,fade=FALSE,
    title="hiwis wirklich entfernen?",
    p(paste0("Sie Sie sicher, dass Sie fuer das ", semester_name(semester), " die hiwis '", paste0(hiwis$hiwiname, collapse=", "),"' mit allen Verknuepfungen entfernen wollen?")),
    footer = tagList(simpleButton("confirmHiwiDelBtn","Ja, entferne Hiwi."), simpleButton("cancelHiwiDelBtn","Abbruch"))
  ))

}

new.hiwi.click = function(..., app=getApp()) {
  restore.point("new.hiwi.click")
  hiwi = list(hiwiid="", semester=app$sem, aktiv=TRUE, turnus=2, hiwiform="vu", zeitform="w", sws_hiwi=0, sws_uebung=0, sprache="de")
  app$new.hiwi = TRUE
  show.edit.hiwi(hiwi)
}

save.hiwi.click = function(hiwi = app$hiwi, formValues,..., app=getApp(), glob=app$glob, check.hiwiid=TRUE) {
  restore.point("save.hiwi.click")
  cat("\nsave.hiwi.click")

  semester = hiwi$semester

  # Extract values
  kuv = extract.form.formValues(formValues, form=glob$forms$hiwi)

  nku = hiwi
  nku[names(kuv)] = kuv

  # Hat sich hiwiid geaendert
  if (!is.true(nku$hiwiid == hiwi$hiwiid) & !is.true(app$new.hiwi)) {
    html = paste0("Sie haben die Hiwi-ID geaendert von ", hiwi$hiwiid , " zu ", nku$hiwiid,". Eine Aenderung der Hiwi-ID ist aber nicht moeglich.")
    timedMessage("saveHiwiAlert",html=html,millis = 10000)
    return()
  } else if (is.true(app$new.hiwi)) {
    res = is.new.hiwiid.valid(nku$hiwiid)
    if (!res$ok) {
      timedMessage("saveHiwiAlert", msg=res$msg, millis=10000)
      return()
    }
  }


  kumov = unlist(formValues$hiwisditModul)

  nkumo = if (NROW(kumov)>0) fast_df(hiwiid=nku$hiwiid, semester=semester, modulid=kumov)


  kupev = extract.tableform.formValues(formValues, form=glob$forms$hiwiperson)

  nkupe = if(NROW(kupev)>0) kupev %>% mutate(semester=semester, hiwiid=nku$hiwiid)

  # set vorname and nachname automatic
  # for persons with personid
  nkupe = set.personid.names(nkupe)


  sd = get.sem.data(hiwi$semester)
  okupe = filter(sd$kupe,semester==hiwi$semester, hiwiid==hiwi$hiwiid)
  okumo = filter(sd$kumo,semester==hiwi$semester, hiwiid==hiwi$hiwiid)

  modify_time = Sys.time()

  if (!app$new.hiwi) {
    diff.log = hiwi.diff.log(nku=nku,nkupe=nkupe, nkumo=nkumo, oku=hiwi,okupe=okupe, okumo=okumo, module=sd$module)



    # Keine Modifikationen
    if (is.null(diff.log)) {
      timedMessage("saveHiwiAlert",paste0("Sie haben noch keine Modifikationen am Hiwi vorgenommen."))
      return()
    }
  } else {
    diff.log = paste0("Neuer Hiwi im ", semester_name(nku$semester), " erstellt:\n  -", nku$hiwiname, " (", nku$hiwiid,")")
  }

  update.db.hiwi(nku, nkupe, nkumo,modify_user = app$userid, modify_time=modify_time, log=diff.log)

  app$hiwi = as_data_frame(nku)
  app$new.hiwi = FALSE

  sd = get.sem.data(update = TRUE)

  # Update table
  #proxy = dataTableProxy("hiwisTable")
  #df = make.hiwis.datatable.df(sd)
  #replaceData(proxy,df,resetPaging=FALSE)
  #update.hiwis.ui()

  html = paste0("Die Aenderungen im Hiwi '" ,nku$hiwiname,"' wurden mit folgendem Logeintrag gespeichert.","<pre>\n", diff.log,"</pre>")
  timedMessage("saveHiwiAlert",html=html,millis = 10000)
}

update.db.hiwi = function(hiwi, hiwiperson, hiwimodul, db=get.stukodb(),modify_user = app$userid, modify_time=Sys.time(), log=NULL, write_log = !is.null(log)) {
  restore.point("update.db.hiwi")

  hiwiid = hiwi$hiwiid
  semester = hiwi$semester

  hiwi$modify_user = modify_user
  hiwi$modify_time = modify_time

  if (!is.list(log) & write_log) {
    log = list(logtime=modify_time, userid=modify_user,logtype="hiwi", logtext=log)
  }

  res = dbWithTransaction(db,{
    dbDelete(db,"hiwi",nlist(hiwiid, semester))
    dbDelete(db,"hiwiperson",nlist(hiwiid, semester))
    dbDelete(db,"hiwimodul",nlist(hiwiid, semester))

    dbInsert(db,"hiwi",hiwi)
    if (NROW(hiwiperson)>0)
      dbInsert(db,"hiwiperson", hiwiperson)
    if (NROW(hiwimodul)>0)
      dbInsert(db,"hiwimodul", hiwimodul)

    if (write_log)
      dbInsert(db,"log", log)

  })
  return(!is(res,"try-error"))

}

hiwi.diff.log = function(nku,nkupe,nkumo,oku,okupe,okumo,modify_user = app$userid, modify_time=Sys.time(), module=NULL) {
  restore.point("hiwi.diff.log")

  change = FALSE
  log = paste0("Hiwi ", oku$hiwiname, " (", oku$hiwiid, ") im ", semester_name(oku$semester), " modifiziert:")

  # hiwi
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

  # Hiwiperson
  okupe$personid[is.na(okupe$personid)] = ""
  diff = tables.diff(select(nkupe, -semester, -hiwiid), select(okupe,-semester, -hiwiid, -name))
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

  # hiwimodul
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


is.new.hiwiid.valid = function(hiwiid, hiwis=get.sem.data()$ku) {
  if (hiwiid %in% hiwis$hiwiid) {
    row = match(hiwis$hiwiid)
    return(list(ok=FALSE, msg=paste0("Die Hiwi-ID ", hiwiid, " wird bereits vom exitierenden Hiwi ", hiwis$hiwiname[row], " genutzt.")))
  }
  if (!is.valid.id(hiwiid)) {
    return(list(ok=FALSE, msg=paste0("Die angegebene Hiwi-ID ", hiwiid, " hat keinen Validen Syntax. Starten Sie mit einem Buchstaben und nutzen Sie nur Buchstaben, Zahlen und Unterstrich.")))
  }

  list(ok=TRUE)
}

is.valid.id = function(id) {
  nid = make.names(gsub(".","_",id, fixed=TRUE))
  id == nid
}


