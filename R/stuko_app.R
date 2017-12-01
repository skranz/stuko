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

  app$sem =  sem

  app$ui = fluidPage(
    selectizeHeaders(),
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
  ui = stuko.ui()
  setUI("mainUI",ui)
  update.stuko.ui()

}

stuko.ui = function(..., app=getApp(), glob=app$glob) {
  restore.point("stuko.ui")

  ui = tagList(
    selectInput("semInput","Semester",choices = glob$sets$semester, selected=glob$sem),

    tabsetPanel(
      tabPanel("Kurse",
        dataTableOutput("kurseTable"),
        simpleButton("addKursBtn","Neuen Kurs anlegen"),
        uiOutput("editKursUI")
      ),
      tabPanel("Module", uiOutput("moduleUI")),
      tabPanel("Reports", uiOutput("reportsUI"))
    )
  )
  ui
}


update.stuko.ui = function(app=getApp()) {
  restore.point("update.stuko.ui")
  update.kurse.ui()
}

update.kurse.ui = function(app=getApp(), glob=app$glob) {
  restore.point("update.kurse.ui")
  sd = get.sem.data()

  #tab = html.table(sd$kurse)

  glob$sets$kursform

  rows = seq_len(NROW(sd$kurse))
  btns= paste0(
    simpleButtonVector(id=paste0("editKursBtn_",rows),icon=icon(name = "pencil"), size="sm",extra.class = "editKursBtn",extra.head=paste0('data-row="',rows,'"')),
    simpleButtonVector(id=paste0("delKursBtn_",rows),icon=icon(name = "trash-o"), size="sm", extra.class="delKursBtn", extra.head=paste0('data-row="',rows,'"'))
  )

  bama = paste0(ifelse(sd$kurse$ba,"BA",""), " ", ifelse(sd$kurse$ma,"MA",""))

  df = transmute(sd$kurse,Aktion=btns, Kurs=kursname, Dozent=dozent,SWS=sws_kurs+sws_uebung,BaMa=bama, Zuordnung=zuordnung, Schwerpunkte=sp, Kursform=to.label(sd$kurse$kursform, glob$sets$kursform), Sprache=sprache, Turnus=turnus, Aktiv=ifelse(aktiv,"Ja","Nein"))

  df$Zuordnung = gsub("NUF WP 35 LP, NUF WP 42 LP, NUF WP 49 LP", "NUF WP", fixed=TRUE, df$Zuordnung)


  dt = datatable(df,escape=-1,rownames = FALSE, filter=list(position="top", clear=FALSE, plain=TRUE), style="bootstrap", autoHideNavigation = TRUE, extensions = c('FixedColumns','ColReorder','Select'),options = list(
    select = list(style="api"),
    columnDefs = list(list(width="20em", targets=1),list(width="3em", targets=3)),
    autoWidth=TRUE,
    scrollX=TRUE,colReorder = TRUE,fixedColumns = list(leftColumns = 2)))

  shinyEvents::setDataTable("kurseTable", dt)

  classEventHandler("editKursBtn",event = "click",function(data=NULL, ...) {
    restore.point("editKursBtn")
    show.edit.kurs(get.sem.data()$kurse[data$row,])
    cat("\neditKurs clicked...")
  })
}

show.edit.kurs = function(kurs,..., app=getApp(), glob=app$glob) {
  restore.point("show.edit.kurs")

  form = glob$forms$kurs
  widgets = lapply(names(form$fields), function(name) {
      fieldInput(name=name,form=form, value = kurs[[name]], lang="de",sets = glob$sets)
  })

  kp = dbGet(glob$db,"kursperson",list(semester=kurs$semester, kursid=kurs$kursid))
  kp.ui = tableform.ui(form=glob$forms$kursperson, data=select(kp,-semester, -kursid))

  #view.ui(kp.ui)

  sd = get.sem.data(kurs$semester)

  mymodul = sd$kursmodul %>% filter(kursid == kurs$kursid, semester==kurs$semester)
  mymodul = mymodule$modulid
  #%>%left_join(sd$modul, by=c("modulid","semester"))

  module = sd$modul$modulid
  names(module) = paste0(sd$modul$titel,", ",sd$modul$ects, " ECTS,",to.label(sd$modul$pruefungsform,glob$sets$pruefungsform),", ", ifelse(sd$modul$extern,"extern","intern"))

  modSel = selectInput("kurseditModul", "Module", choices=module, selected=mymodule, multiple=TRUE, width="60em")


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
    simpleButton("saveKurs","Speichern")
  )

  buttonHandler("addKurspersonBtn", function(...) {
    restore.point("addKurspersonBtn")
    form.html.table.add.row(form=glob$forms$kursperson, data=data_frame(personid=NA, nachname="", vorname="", rolle="", lehrauftrag="-", dozent_sws=0))
  })






  setUI("editKursUI",ui)
  evalJS("$('html, body').animate({scrollTop: $('#addKursBtn').offset().top + 'px'}, 'fast');")
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
  ind = match(val, unlist(keys))
  labels[ind]
}
