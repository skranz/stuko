examples.stuko.app = function() {
  restore.point.options(display.restore.point=TRUE)
  stuko.dir = "D:/libraries/stuko/ulm"
  setwd(stuko.dir)
  app = stukoApp(stuko.dir, sem=185, init.userid = "sebastian.kranz@uni-ulm.de",need.password = FALSE,need.userid = FALSE, semdb.dir = "D:/libraries/stuko/semdb")
  #viewApp(app)

  viewApp(app,launch.browser = TRUE)
}

stukoApp = function(stuko.dir = getwd(),sem = default_semester(),use.jobs=FALSE, semdb.dir = NULL, ...) {
  restore.point("stukoApp")
  app = eventsApp()
  glob = app$glob

  glob$use.jobs = use.jobs
  glob$stuko.dir = stuko.dir
  glob$db.dir = file.path(stuko.dir, "db")
  glob$db = get.stukodb(glob$db.dir)

  glob$semdb.dir = semdb.dir

  glob$tpl.dir = file.path(stuko.dir, "report_tpl")
  glob$snapshot.dir = file.path(stuko.dir, "snapshots")

  glob$sem.dat = list()

  glob$yaml.dir = system.file("yaml", package = "stuko")
  glob$sets = rmdtools::read.yaml(file.path(glob$yaml.dir,"sets.yaml"))
  glob$strings = rmdtools::read.yaml(file.path(glob$yaml.dir,"strings.yaml"))

  if (use.jobs) {
    start.date = Sys.Date()
    month(start.date) = month(start.date)-16
    start.time = as.numeric(as.POSIXct(start.date))
    sql = paste0('select * from job where jobstate in ("o","b","w") OR givetime >= ', start.time)
    glob$jobs = dbGet(glob$db,table="job", sql = sql,null.as.na = FALSE)
    glob$jobs$frist = frist.days(glob$jobs$wishdate)

  }


  person = dbGet(glob$db,"person")
  person$vorname[is.na(person$vorname)] = ""
  person$kurzname = paste0(person$nachname, ifelse(nchar(person$vorname)>0, paste0(", ", substring(person$vorname,1,1),"."), ""))
  glob$person = person
  glob$vertreter = dbGet(glob$db,"vertreter")

  person = glob$person$personid
  names(person) = paste0(glob$person$nachname, ifelse(nchar(glob$person$vorname)>0, paste0(", ", substring(glob$person$vorname,1,1),"."), ""))

  person = c("", person)
  glob$sets[["person"]] = person

  glob$snapshots =dbGet(glob$db, "snapshot")

  forms = c("kurs","modul")
  glob$forms = list()
  for (form in forms) {
    form.file = paste0(glob$yaml.dir,"/", form, ".yaml")
    #yaml = rmdtools::read.yaml(form.file,check.by.row = TRUE)
    glob$forms[[form]] = load.and.init.form(form.file, prefix=paste0(form,"_"))
  }

  forms = c("kursperson","modul_table_edit", "vertreter")
  for (form in forms) {
    fields = rmdtools::read.yaml(paste0(glob$yaml.dir,"/", form, ".yaml"))$fields
    glob$forms[[form]] = tableform(id=form, fields=fields, lang="de",sets=glob$sets)
  }



  glob$info = list()
  glob$rmd.dir = system.file("rmd", package = "stuko")
  glob$info$kurse = markdown_html(readUtf8(file.path(glob$rmd.dir,"kurse_info.Rmd")))



  app$sem =  sem

  app$ui = fluidPage(
    selectizeHeaders(),
    jqueryLayoutHeader(),
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
    uiOutput("mainUI")
  )

  lomo = loginModule(container.id = "mainUI", ..., login.fun = stuko.login.fun)

  appInitHandler(function(...) {
    restore.point("stukoLoginDispatch")
    initLoginDispatch(lomo)
  })
  app

}

stuko.login.fun = function(userid,..., app=getApp()) {
  restore.point("stuko.login.fun")
  glob = app$glob


  if (!userid %in% c(glob$vertreter$email, glob$person$email)) {
    msg = paste0("Der Nutzer ", userid, " ist nicht in der Liste der Zugangsberechtigten eingetragen.")
    setUI("mainUI", HTML(msg))
    setAppIsAuthenticated(FALSE)
    return()
  }
  app$userid = userid

  app$all.bossid = c(
    filter(glob$vertreter,email==app$userid)$bossid,
    filter(glob$person,email==app$userid)$personid
  )
  # Alle Koordinatoren die der Nutzer vertritt
  app$all.koord = filter(glob$person, personid %in% app$all.bossid, koordinator)

  app$admin = any(
    filter(glob$vertreter,email==app$userid)$admin,
    filter(glob$person,email==app$userid)$admin
  )

  app$admin.for = c(
    filter(glob$vertreter,email==app$userid, admin)$bossid,
    filter(glob$person,email==app$userid)$personid
  )
  app$stuko = any(filter(glob$person, personid %in% app$all.bossid)$stuko)

  if (glob$use.jobs) {
    app$rjobs = filter(glob$jobs, receiverid %in% app$all.bossid)
    app$gjobs = filter(glob$jobs, giverid %in% app$all.bossid)
    cat("\nNum app$gjobs = ", NROW(app$gjobs),"\n" )

  }

  ui = stuko.ui()
  setUI("mainUI",ui)
  update.stuko.ui()

}

stuko.ui = function(..., userid=app$userid, app=getApp(), glob=app$glob) {
  restore.point("stuko.ui")

  tabs = remove.null(list(
    if (glob$use.jobs) tabPanel("Jobs", jobs.ui()),
    if (glob$use.jobs & app$stuko) tabPanel("Erteilte Jobs", gjobs.ui()),
    tabPanel("Kurse", kurse.ui()),
    tabPanel("Module", module.ui()),
    tabPanel("Reports", reports.ui()),
    if (app$admin & app$stuko) tabPanel("Admin", admin.ui()),
    if (app$stuko ) tabPanel("Log",
      HTML("Eintraege der letzten 8 Monate"),
      actionButton("refreshLogBtn","",icon = icon("refresh")),
      #dataTableOutput("logTable")
      uiOutput("logUI")
    ),
    if (length(app$admin.for)>0) tabPanel("Vertreter", vertreter.ui())
  ))

  ui = tagList(
    fluidRow(
      column(width = 3,p("WiWi Ulm Kursverwaltung")),
      column(width = 4,tagList(HTML("Semester: "),simpleSelect("semInput","",choices = glob$sets$semester, selected=app$sem))),
      column(width = 3,p(paste0("Nutzer: ", userid)))
    ),
    uiOutput("stukoAlert"),
    do.call(tabsetPanel, tabs)
  )
  selectChangeHandler("semInput", function(value,...) {
    app$sem = as.integer(value)
    update.kurse.ui()
    update.module.ui()
  })

  buttonHandler("refreshKurseBtn", update.kurse.ui)
  buttonHandler("refreshModuleBtn", update.module.ui)
  buttonHandler("refreshLogBtn", update.log.ui)


  ui
}


update.stuko.ui = function(app=getApp()) {
  restore.point("update.stuko.ui")
  if (app$glob$use.jobs) {
    update.jobs.ui()
    update.gjobs.ui()
  }
  update.kurse.ui()
  update.module.ui()
  update.log.ui()
  update.admin.ui()

}

update.log.ui = function(app=getApp(), glob=app$glob,...) {
  restore.point("update.log.ui")

  start.date = Sys.Date()
  month(start.date) = month(start.date)-8


  sql = paste0("select * from log where logtime >= ", start.date)
  glob$log = dbGet(glob$db,"log", sql=sql) %>%
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
