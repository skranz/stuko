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
      checkboxInput("tosemJustAktiv",label="Nur aktive Kurse aus vergangenen Semestern uebertragen"),
      uiOutput("tosemInfo"),
      hr(),
      simpleButton("tosemOkBtn", "Kurse und Module anlegen",form.ids = c("tosemInput","tosemOverwrite")), simpleButton("cancelModalBtn","Abbruch")
    )
  )
  selectChangeHandler("tosemInput", function(value,...) {
    update.copy.all.kurse.ui(as.integer(value))
  })

  buttonHandler("cancelModalBtn",function(...) removeModal())
  buttonHandler("tosemOkBtn",function(formValues,...) {
    restore.objects("tosemOkBtn")
    tosem = formValues$tosemInput
    cat("copy all kurses to ", tosem)
    removeModal()
  })

  update.copy.all.kurse.ui(tosem)
  ui
}

update.copy.all.kurse.ui = function(tosem, ..., app=getApp(), db=get.stukodb()) {
  restore.point("update.copy.all.kurse.ui")

  newku = dbGet(db, "kurs", list(zukunft_sem=tosem))
  exku = dbGet(db, "kurs", list(semester=tosem))

  akku = filter(newku, aktiv==TRUE)

  num.inter = length(intersect(newku$kursid, exku$kursid))
  num.inter.akku = length(intersect(akku$kursid, exku$kursid))

  html = paste0("- Fuer das Semester ", semester_name(tosem), " sind bereits ", NROW(exku), " Kurse eingetragen.<br>- Aus vorherigen Semestern wuerden turnusgemaess noch ", NROW(newku)-num.inter, " Kurse hinzukommen. Hiervon sind ", NROW(akku), " Kurse aktiv.<br>- Wenn bereits angelegte Kurse im ", semester_name(tosem), " ueberschrieben werden sollen, werden ausserdem noch ", num.inter, " Kurse ueberschrieben, bzw. ", num.inter.akku, " Kurse, wenn nur aktive Kurse uebernommen werden.")
  setUI("tosemInfo", HTML(html))

}

update.all.kurse = function(tosem, overwrite=FALSE, just.aktiv = FALSE, ..., db=get.stukodb()) {
  restore.point("update.copy.all.kurse.ui")

  newku = dbGet(db, "kurs", list(zukunft_sem=tosem))

  tosd = get.sem.data(tosem)


  if (just.aktiv) {
    newku = filter(newku, aktiv==TRUE)
  }
  if (!overwrite) {
    newku = filter(newku, ! kursid %in% tosd$kurs$kursid)
  }
  sems = unique(newku$semester)

  sems = sems[sems < tosem]

  ku = kupe= kumo=NULL
  for (sem in sems) {
    sd = get.sem.data(sem, cache=FALSE)
    ku = filter(newku, semester==sem)
    kumo = filter(sd$kumo, kursid %in% ku$kursid)
    kupe = filter(sd$kupe, kursid %in% ku$kursid)


    kumo = dbGet("kursmodul", list(semester=sem))
  }


}

update.snapshot.info = function(app=getApp(), glob=app$glob) {
  snapshots = glob$snapshots
  if (NROW(snapshots)>0) {
    txt = paste0("Letzte Sicherheitskopie am ", format(max(snapshots$time), "%d.%m.%Y %H:%M"))
  } else {
    txt = "Bislang noch keine Sicherheitskopie erstellt."
  }
  setUI("snapshotInfoUI", p(txt))

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
