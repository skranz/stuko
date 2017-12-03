examples.stuko.app = function() {
  restore.point.options(display.restore.point=TRUE)
  stuko.dir = "D:/libraries/stuko/ulm"
  setwd(stuko.dir)
  app = stukoApp(stuko.dir, sem=170)
  #viewApp(app)

  viewApp(app,launch.browser = TRUE)
}

stukoApp = function(stuko.dir = getwd(),sem = default_semester(),...) {
  restore.point("stukoApp")
  app = eventsApp()
  glob = app$glob

  glob$stuko.dir = stuko.dir
  glob$db.dir = file.path(stuko.dir, "db")
  glob$db = get.stukodb(glob$db.dir)

  glob$tpl.dir = file.path(stuko.dir, "report_tpl")

  glob$sem.dat = list()

  glob$yaml.dir = system.file("yaml", package = "stuko")
  glob$sets = rmdtools::read.yaml(file.path(glob$yaml.dir,"sets.yaml"))

  glob$person = dbGet(glob$db,"person")
  person = glob$person$personid
  names(person) = paste0(glob$person$nachname,", ",toupper(substring(glob$person$vorname,1,1)),".")
  person = c("", person)
  glob$sets[["person"]] = person


  forms = c("kurs")
  glob$forms = list()
  for (form in forms) {
    form.file = paste0(glob$yaml.dir,"/", form, ".yaml")
    #yaml = rmdtools::read.yaml(form.file,check.by.row = TRUE)
    glob$forms[[form]] = load.and.init.form(form.file, prefix=paste0(form,"_"))
  }
  form = "kursperson"
  fields = rmdtools::read.yaml(paste0(glob$yaml.dir,"/", form, ".yaml"))$fields
  glob$forms[["kursperson"]] = tableform(id="kursperson", fields=fields, lang="de",sets=glob$sets)

  glob$info = list()
  glob$rmd.dir = system.file("rmd", package = "stuko")
  glob$info$kurse = markdown_html(readUtf8(file.path(glob$rmd.dir,"kurse_info.Rmd")))



  app$sem =  sem

  app$ui = fluidPage(
    selectizeHeaders(),
    tags$head(tags$style(HTML(
    '
.table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
    background-color: #eeeeee;
    color: black;
}'
    ))),
    htmlDependency("font-awesome", "4.7.0", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css"),
    tags$head(tags$style(form.table.default.css())),
    tags$style(HTML(
      ".vector-input-container, .vector-input-container .form-control {margin-bottom: 0px;}")),
    p("StuKo-App"),
    uiOutput("mainUI")
  )

  lomo = loginModule(container.id = "mainUI", init.userid = "test@uni-ulm.de",need.password = FALSE,need.userid = FALSE, login.fun = stuko.login.fun)

  appInitHandler(function(...) {
    restore.point("stukoLoginDispatch")
    initLoginDispatch(lomo)
  })
  app

}

stuko.login.fun = function(userid,...) {
  restore.point("stuko.login.fun")
  app$userid = userid
  ui = stuko.ui()
  setUI("mainUI",ui)
  update.stuko.ui()

}

stuko.ui = function(..., app=getApp(), glob=app$glob) {
  restore.point("stuko.ui")

  ui = tagList(
    selectInput("semInput","Semester",choices = glob$sets$semester, selected=app$sem),
    uiOutput("stukoAlert"),
    tabsetPanel(
      tabPanel("Kurse",
        dataTableOutput("kurseTable"),
        simpleButton("refreshKurseBtn","",icon = icon("refresh")),
        simpleButton("addKursBtn","Neuen Kurs anlegen"),
        uiOutput("editKursUI"),
        br(),
        slimCollapsePanel("Hintergrundinformationen",HTML(glob$info$kurse))

      ),
      tabPanel("Module",
        dataTableOutput("moduleTable"),
        simpleButton("refreshModuleBtn","",icon = icon("refresh")),
        simpleButton("addModulBtn","Neues Modul anlegen"),
        uiOutput("editModulUI")
      ),
      tabPanel("Reports",
        helpText("Drücken Sie auf den entsprechenden Knopf um den Report zu erzeugen und als Word-Dateien herunterzuladen."),
        downloadButton("repLPBtn", "Lehrangebot"),
        helpText("Eine formale Darstellung des Lehrprogramms, wie es in der StuKo und Fakultätsrat beschlossen wird."),
        downloadButton("repDiagBtn", "Diagnostik des Lehrangebots"),
        helpText("Eine Diagnostik des Lehrprogramms. Vor allem gedacht um noch offene Baustellen in den Daten zu entdecken bevor das Lehrprogramm offiziell beschlossen wird.")
        #simpleButton("repLBBtn","Lehrbeauftragte"),
        #simpleButton("repKoordBtn","Vorlesungen nach Koordinatoren sortiert.")
      ),
      tabPanel("Log",
        simpleButton("refreshLogBtn","",icon = icon("refresh")),
        #dataTableOutput("logTable")
        uiOutput("logUI")
      )
    )
  )
  selectChangeHandler("semInput", function(value,...) {
    app$sem = value
    update.kurse.ui()
    update.module.ui()
  })

  buttonHandler("refreshKurseBtn", update.kurse.ui)
  buttonHandler("refreshModuleBtn", update.module.ui)
  buttonHandler("refreshLogBtn", update.log.ui)

  setDownloadHandler("repLPBtn",
    filename=function(app = getApp())
      paste0("Lehrangebot_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      restore.point("jsfhshfzgfzzfhvn")
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        lehrangebot.report(semester=app$sem, db=app$glob$db, tpl.dir=app$glob$tpl.dir, out.file=file)
      )
    }
  )

  setDownloadHandler("repDiagBtn",
    filename=function(app = getApp())
      paste0("Lehrangebot_Diagnostik_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        lehrangebot.diagnostik.report(semester=app$sem, db=app$glob$db, tpl.dir=app$glob$tpl.dir, out.file=file)
      )
    }
  )


  ui
}


update.stuko.ui = function(app=getApp()) {
  restore.point("update.stuko.ui")
  update.kurse.ui()
  update.module.ui()
  update.log.ui()
}

update.log.ui = function(app=getApp(), glob=app$glob,...) {
  restore.point("update.kurse.ui")

  glob$log = dbGet(glob$db, "log") %>%
    arrange(desc(logtime))

  txt = paste0(strftime(glob$log$logtime,"%Y-%m-%d %H:%M"), " von ", glob$log$userid, "\n\n", glob$log$logtext, collapse="\n\n-------------------------------\n\n")

  setUI("logUI", tags$pre(txt))
  return()

  df = transmute(glob$log,
    "Datum" =logtime,"Nutzer"=userid,"Eintrag" = paste0("<pre>", logtext,"</pre>")
  )

  dt = datatable(df,selection = 'none',
    #escape=-3,
    rownames = FALSE, filter=list(position="top", clear=FALSE, plain=TRUE),
    class="compact",
    style="bootstrap",
    autoHideNavigation = TRUE, extensions = c('Select',"Buttons"),options = list(
    lengthMenu = c(10, 25, 50, 1000),
    dom = 'Blfrtip',
    buttons = c('copy','excel','csv'),
    select = FALSE,
    #columnDefs = list(list(width="15em", targets=c(1)),list(width="3em", targets=3)),
    autoWidth=TRUE,
    scrollX=TRUE))

  shinyEvents::setDataTable("logTable", dt,server=TRUE)

}

update.kurse.ui = function(app=getApp(), glob=app$glob,...) {
  restore.point("update.kurse.ui")
  sd = get.sem.data()

  #tab = html.table(sd$kurse)

  glob$sets$kursform

  df = make.kurse.datatable.df(sd)

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

  shinyEvents::setDataTable("kurseTable", dt,server=TRUE)

  classEventHandler("editKursBtn",event = "click",function(data=NULL, ...) {
    restore.point("editKursBtn")
    show.edit.kurs(get.sem.data()$kurse[data$row,])
    cat("\neditKurs clicked...")
  })
}

make.kurse.datatable.df = function(sd, app=getApp(), glob=app$glob) {
  restore.point("make.kurse.table.df")

  rows = seq_len(NROW(sd$kurse))
  btns= paste0(
    simpleButtonVector(id=paste0("editKursBtn_",rows),icon=icon(name = "pencil"), size="sm",extra.class = "editKursBtn",extra.head=paste0('data-row="',rows,'"')),
    simpleButtonVector(id=paste0("delKursBtn_",rows),icon=icon(name = "trash-o"), size="sm", extra.class="delKursBtn", extra.head=paste0('data-row="',rows,'"'))
  )


  df = transmute(sd$kurse,Aktion=btns, Kurs=kursname, Dozent=dozent,SWS=sws_kurs+sws_uebung,BaMa=bama, Zuordnung=zuordnung, Schwerpunkte=schwerpunkt, Kursform=to.label(sd$kurse$kursform, glob$sets$kursform), Sprache=sprache, Extern=ifelse(extern,"extern","intern"), ECTS=as.integer(ects), Koordinator=koordinator,Lehrauftrag=lehrauftrag, Module = num_modul, Turnus=turnus, Aktiv=ifelse(aktiv,"Ja","Nein"), Prüfung=pruefungsform, Codesharing=ifelse(nchar(codeshare)>0,"Ja",""), 'Modifiziert am'=modify_time, 'Modifiziert durch'=modify_user)

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
  kp.ui = tableform.ui(form=glob$forms$kursperson, data=select(kp,-semester, -kursid, -name))

  #view.ui(kp.ui)


  mymodul = sd$kumo %>% filter(kursid == kurs$kursid, semester==kurs$semester)
  mymodul = mymodul$modulid

  mods = sd$module$modulid
  names(mods) = sd$module$label

  modSel = selectInput("kurseditModul", "Module", choices=mods, selected=mymodul, multiple=TRUE, width="80em")

  form.sel = paste0(c(paste0("#kurseditModul"),paste0("#kurs_",names(form$fields)),".tableform-kursperson-input"), collapse=", ")

  ui = tagList(
    h3("Kurs bearbeiten"),
    fluidRow(column(width = 12, widgets[1])),
    modSel,
    helpText("Um Module zu bearbeiten / zu erstellen, gehen Sie bitte zum Reiter Module. Jedem Kurs sollte mind. ein Modul zugeordnet sein."),
    h4("Mitarbeiter"),
    kp.ui,
    simpleButton("addKurspersonBtn","Neuer Mitarbeiter."),
    helpText("Wenn kein Koordinator eingetragen wird, verwaltet der Studiendekan den Kurs direkt. Die Spalte SWS ist nur für Lehrbeauftragte relevant."),
    layout.widgets.as.fluid.grid(widgets[-1], 3),
    simpleButton("saveKursBtn","Speichern",form.sel = form.sel),
    uiOutput("saveKursAlert")
  )

  buttonHandler("addKurspersonBtn", function(...) {
    restore.point("addKurspersonBtn")
    form.html.table.add.row(form=glob$forms$kursperson, data=data_frame(personid=NA, nachname="", vorname="", rolle="", lehrauftrag="-", dozent_sws=0))
  })

  buttonHandler("saveKursBtn", function(...) {
    save.kurs.click(...)
  })





  setUI("editKursUI",ui)
  evalJS("$('html, body').animate({scrollTop: $('#addKursBtn').offset().top + 'px'}, 'fast');")
}

save.kurs.click = function(kurs = app$kurs, formValues,..., app=getApp(), glob=app$glob) {
  restore.point("save.kurs.click")
  cat("\nsave.kurs.click")

  kursid = kurs$kursid
  semester = kurs$semester

  # Extract values
  kuv = extract.form.formValues(formValues, form=glob$forms$kurs)

  nku = kurs
  nku[names(kuv)] = kuv

  kumov = unlist(formValues$kurseditModul)

  nkumo = if (NROW(kumov)>0) fast_df(kursid=kursid, semester=semester, modulid=kumov)


  kupev = extract.tableform.formValues(formValues, form=glob$forms$kursperson)

  nkupe = if(NROW(kupev)>0) kupev %>% mutate(semester=semester, kursid=kursid)

  # set vorname and nachname automatic
  # for persons with personid
  nkupe = set.personid.names(nkupe)


  sd = get.sem.data(kurs$semester)
  okupe = filter(sd$kupe,semester==kurs$semester, kursid==kurs$kursid)
  okumo = filter(sd$kumo,semester==kurs$semester, kursid==kurs$kursid)

  modify_time = Sys.time()
  diff.log = kurs.diff.log(nku=nku,nkupe=nkupe, nkumo=nkumo, oku=kurs,okupe=okupe, okumo=okumo, module=sd$module)

  # Keine Modifikationen
  if (is.null(diff.log)) {
    timedMessage("saveKursAlert",paste0("Sie haben noch keine Modifikationen am Kurs vorgenommen."))
    return()
  }

  update.db.kurs(nku, nkupe, nkumo, modify_time=modify_time, log=diff.log)

  app$kurs = nku

  sd = get.sem.data(update = TRUE)

  # Update table
  #proxy = dataTableProxy("kurseTable")
  #df = make.kurse.datatable.df(sd)
  #replaceData(proxy,df,resetPaging=FALSE)
  #update.kurse.ui()

  html = paste0("Die Änderungen im Kurs," ,nku$kursname," wurden mit folgender Logdatei gespeichert.","<pre>\n", diff.log,"</pre>")
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
  log = paste0("Modifiziere Kurs ", oku$kursname, " (", oku$kursid, ")")

  # kurs
  cols = colnames(oku)
  diff.cols = setdiff(
    cols[is.true(nku != oku)],
    c("modify_time","modify_user")
  )

  if (length(diff.cols)>0) {
    change = TRUE
    log = paste0(log,
      "\n\nModifikationen an 'kurs':",
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
      "\n\nModifikationen an 'kursperson':",
      if (NROW(diff$added)>0)
        paste0("\n  Neu:", paste0("\n   - ", added, collapse="")),
      if (NROW(diff$removed)>0)
        paste0("\n  Entfernt oder Ueberschrieben:", paste0("\n   - ", removed, collapse=""))
    )
  }

  # kursmodul
  diff = tables.diff(select(nkumo,modulid), select(okumo,modulid))
  if (NROW(diff$added)>0 | NROW(diff$removed)>0) {
    change = TRUE
    log = paste0(log,
      "\n\nModifikationen an 'kursmodul':",
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

layout.widgets.as.fluid.grid = function(widgets, ncol=2, byrow=TRUE, width=floor(12/ncol)) {
  restore.point("layout.widgets.as.fluid.grid")


  n = length(widgets)
  nrow = ceiling(n / ncol)
  fr = vector("list",nrow)
  start = 1
  row = 1
  for (row in seq_len(nrow)) {
    cols = start:(start+ncol-1)
    cols = cols[cols<=n]
    rf = lapply(widgets[cols], column, width=width)
    fr[[row]] = do.call(fluidRow, rf)
    start = start+ncol
  }
  tagList(fr)
}

to.label = function(val, keys, labels = names(keys)) {
  restore.point("to.label")
  if (is.null(keys)) return(val)
  ind = match(val, unlist(keys))
  labels[ind]
}

set.personid.names = function(kupe, person=app$glob$person, app=getApp()) {
  restore.point("set.personid.names")
  rows = match(kupe$personid, person$personid)
  use = !is.na(rows)
  kupe$vorname[use] = person$vorname[rows[use]]
  kupe$nachname[use] = person$nachname[rows[use]]
  kupe



}



