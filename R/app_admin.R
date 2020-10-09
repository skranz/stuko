admin.ui = function(...) {
  ui = tagList(
    uiOutput("snapshotInfoUI"),
    simpleButton("makeSnapshotBtn", "Sicherheitskopie der Datenbank erstellen"),
    hr(),
    simpleButton("copyAllKurseBtn", "Kurse und Module fuer ein neues Semester erstellen (kopiere Kurse die gemaess Turnus im neuen Semester stattfinden).")
  )
  buttonHandler("makeSnapshotBtn", make.snapshot.click)
  buttonHandler("copyAllKurseBtn", function(...) {
    showModal(copy.all.kurse.ui())
  })
  ui
}


update.admin.ui = function() {
  update.snapshot.info()
}

copy.all.kurse.ui = function(tosem = first.non.null(app$tosem, app$sem+10),..., app=getApp(), glob=app$glob) {
  restore.point("copy.all.kurse.ui")

  ui = modalDialog(size = "l", footer=NULL,
    tagList(
      selectInput("tosemInput","Turnusgemaesse Kurse und Module im folgenden Semester erstellen", choices = glob$sets$semester, selected=tosem),
      #helpText("Kopiere Kurse aus vorherigen Semestern, die gemaess Turnus im obigen Semester stattfinden."),
      checkboxInput("tosemOverwrite",label="Bereits im neuen Semester angelegte Kurse und Module ueberschreiben"),
      checkboxInput("tosemJustAktiv",label="Nur aktive Kurse aus vergangenen Semestern uebertragen",value = TRUE),
      uiOutput("tosemInfo"),
      hr(),
      simpleButton("tosemOkBtn", "Kurse und Module anlegen",form.ids = c("tosemInput","tosemOverwrite","tosemJustAktiv")), simpleButton("cancelModalBtn","Abbruch")
    )
  )
  selectChangeHandler("tosemInput", function(value,...) {
    update.copy.all.kurse.ui(as.integer(value))
  })

  buttonHandler("cancelModalBtn",function(...) removeModal())
  buttonHandler("tosemOkBtn",function(formValues,...) {
    restore.point("tosemOkBtn")
    tosem = as.integer(formValues$tosemInput)
    overwrite = as.logical(formValues$tosemOverwrite)
    just.aktiv = as.logical(formValues$tosemJustAktiv)

    cat("Copy all kurses to", tosem,"...")
    copy.all.kurse(tosem=tosem,overwrite = overwrite, just.aktiv=just.aktiv)
    removeModal()
  })

  update.copy.all.kurse.ui(tosem)
  ui
}

get.turnus.kurse.tosem = function(tosem, lags=1:5, db=get.stukodb()) {
  restore.point("get.turnus.kurse.tosem")
  li = lapply(lags, function(lag) {
    ku = dbGet(db, "kurs", list(semester=tosem-lag*5, turnus=lag),empty.as.null = TRUE)
  })
  res = bind_rows(li)
  res
  #dupl = duplicated(res$kursid)
  #res = res[!dupl,]
  #res
}

update.copy.all.kurse.ui = function(tosem, ..., app=getApp(), db=get.stukodb()) {
  restore.point("update.copy.all.kurse.ui")

  exku = dbGet(db, "kurs", list(semester=tosem))


  newku = get.turnus.kurse.tosem(tosem, db=db)


  akku = filter(newku, aktiv==TRUE)
  dupl = duplicated(akku$kursid)
  akku = akku[!dupl,]

  dupl = duplicated(newku$kursid)
  newku = newku[!dupl,]

  num.inter = length(intersect(newku$kursid, exku$kursid))
  num.inter.akku = length(intersect(akku$kursid, exku$kursid))

  html = paste0("- Fuer das Semester ", semester_name(tosem), " sind bereits ", NROW(exku), " Kurse eingetragen.<br>- Aus vorherigen Semestern wuerden turnusgemaess noch ", NROW(akku)-num.inter.akku, " aktive Kurse hinzukommen. Wenn auch nicht-aktive Kurse übernommen werden kämen noch ", NROW(newku)-num.inter," Kurse hinzu.<br>- Wenn bereits angelegte Kurse im ", semester_name(tosem), " ueberschrieben werden sollen, werden ", num.inter.akku, " Kurse ueberschrieben, bzw. ", num.inter, " Kurse, wenn auch nicht-aktive Kurse uebernommen werden.")
  setUI("tosemInfo", HTML(html))

}

copy.all.kurse = function(tosem, overwrite=FALSE, just.aktiv = FALSE, ..., db=get.stukodb()) {
  restore.point("copy.all.kurse")

  newku = get.turnus.kurse.tosem(tosem, db=db)
  tosd = get.sem.data(tosem)
  if (just.aktiv) {
    newku = filter(newku, aktiv==TRUE)
  }
  if (!overwrite) {
    newku = filter(newku, ! kursid %in% tosd$kurs$kursid)
  }


  if (NROW(newku)==0) return()

  newku = arrange(newku, desc(semester))
  dupl = duplicated(newku$kursid)
  newku = newku[!dupl, ]

  # Set Seminartermine empty for new semester
  newku$seminar_termine = ""

  sems = unique(newku$semester)

  sems = sems[sems < tosem]

  ku = kupe= kumo=mo=most=mosp=mozu=NULL
  for (sem in sems) {
    sd = get.sem.data(sem, cache=FALSE)
    semku = filter(newku, semester==sem)
    ku   = rbind(ku,semku)
    semkumo = filter(sd$kumo, kursid %in% semku$kursid)
    kumo = rbind(kumo,semkumo)
    kupe = rbind(kupe,filter(sd$kupe, kursid %in% semku$kursid))
    semmo = filter(sd$mo, modulid %in% semkumo$modulid)
    mo   = rbind(mo,semmo)
    most = rbind(most,filter(sd$most, modulid %in% semmo$modulid))
    mosp = rbind(mosp,filter(sd$mosp, modulid %in% semmo$modulid))
    mozu = rbind(mozu,filter(sd$mozu, modulid %in% semmo$modulid))
  }

  log = paste0("Automatische Uebertragung von Kursen und Modulen in das ", semester_name(tosem),"\n\n",
    NROW(ku), " Kurse:\n",
    paste0("  - ", ku$kursname, " (", semester_name(ku$semester),")", collapse="\n"),
    "\n\n", NROW(mo), " Module:\n",
    paste0("  - ", mo$titel, " (", semester_name(mo$semester),")", collapse="\n")
  )

  if (NROW(ku)>0)   ku$semester   = tosem
  if (NROW(kumo)>0) kumo$semester = tosem
  if (NROW(kupe)>0) kupe$semester = tosem
  if (NROW(mo)>0)   mo$semester   = tosem
  if (NROW(most)>0) most$semester = tosem
  if (NROW(mosp)>0) mosp$semester = tosem
  if (NROW(mozu)>0) mozu$semester = tosem


  #stop()

  dbWithTransaction(db, {
    if (overwrite) {
      for (kursid in ku$kursid) {
        dbDelete(db,"kurs", list(semester=tosem, kursid=kursid))
        dbDelete(db,"kursperson", list(semester=tosem, kursid=kursid))
        dbDelete(db,"kursmodul", list(semester=tosem, kursid=kursid))
      }
      for (modulid in mo$modulid) {
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

    write.stuko.log(log,"uebertragung",semester=tosem)

  })
  sd = get.sem.data(tosem, update = TRUE)

}

update.snapshot.info = function(app=getApp(), glob=app$glob) {
  restore.point("update.snapshot.info")

  snapshots = glob$snapshots
  if (NROW(snapshots)>0) {
    txt = paste0("Letzte Sicherheitskopie am ", format(max(snapshots$time), "%d.%m.%Y %H:%M"))
  } else {
    txt = "Bislang noch keine Sicherheitskopie erstellt."
  }
  setUI("snapshotInfoUI", tags$p(txt))

}

make.snapshot.click = function(..., app=getApp(), glob=app$glob, db=glob$db, type="manual") {
  restore.point("make.snapshot.click")

  buttonHandler("confirmSnapshotBtn", function(formValues,...) {
    restore.point("confirmSnapshotBtn")
    descr = formValues$snapshotDescr
    time = Sys.time()
    id = paste0("s_",format(time,"%Y-%m-%d_%H%M%S"))
    file = paste0(glob$snapshot.dir, "/", id, ".sqlite")
    dbWithTransaction(db,{
      dbInsert(db, "snapshot", list(snapshotid = id, time=time, userid=app$userid, descr=descr, type=type))

      file.copy(file.path(glob$db.dir,"stukodb.sqlite"), file)
    })
    glob$snapshots =dbGet(glob$db, "snapshot")
    update.snapshot.info()
    removeModal()
  })
  buttonHandler("cancelSnapshotBtn",function(...) removeModal())

  showModal(modalDialog(easyClose = TRUE,
    title="Sicherheitskopie der Datenbank erstellen",
    textAreaInput("snapshotDescr","Beschreibung der Sicherheitskopie (optional)",""),
    footer = tagList(simpleButton("confirmSnapshotBtn","Ok", form.ids = "snapshotDescr"), simpleButton("cancelSnapshotBtn","Abbruch"))
  ))
}

# Bugfix functions: remove unwanted duplicates

remove.stuko.table.duplicates = function() {
  restore.point.options(display.restore.point=!TRUE)
  setwd("D:/libraries/stuko/ulm/db")


  tables = c("modul","kurs","modulstudiengang","modulschwerpunkt","modulzuordnung","kursmodul","kursperson")

  for (table in tables) {
    remove.duplicates.from.table(table)
  }
}


remove.duplicates.from.table = function(table, db = get.stukodb()) {
  dat = dbGet(db, table)
  dupl = duplicated(dat)
  if (sum(dupl)>0) {
    dat = dat[!dupl,]

    dbWithTransaction(db,{
      dbDelete(db,table, params=list())
      dbInsert(db, table, dat)
    })
    cat("\n", sum(dupl), " duplicated rows from table ", table, " removed.")
  }

}

