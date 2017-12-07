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


  forms = c("kurs","modul")
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
        helpText("Eine Diagnostik des Lehrprogramms. Vor allem gedacht um noch offene Baustellen in den Daten zu entdecken bevor das Lehrprogramm offiziell beschlossen wird."),
        downloadButton("repLBBtn","Lehrbeauftragte"),
        helpText("Eine Liste der Lehrbeauftragten mit Kurs, Koordinator und Vergütung.")
        #simpleButton("repKoordBtn","Vorlesungen nach Koordinatoren sortiert.")
      ),
      tabPanel("Admin",
        simpleButton("makeSnapshotBtn", "Sicherheitskopie der Datenbank erstellen")
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

  setDownloadHandler("repLBBtn",
    filename=function(app = getApp())
      paste0("Lehrbeauftragte_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        lehrauftrag.report(semester=app$sem, db=app$glob$db, tpl.dir=app$glob$tpl.dir, out.file=file, sets=glob$sets)
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
  restore.point("update.log.ui")

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

make.snapshot.click = function() {
  restore.point("make.snapshot.click")


  modalDialog(easyClose = TRUE,
    title="Sicherheitskopie erstellen",
    textArea("snapshotDescr","Beschreibung der Sicherheitskopie (optional)"),
    footer =
  )
}
