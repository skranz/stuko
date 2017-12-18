reports.ui = function(...) {
  ui = tagList(
    helpText("Druecken Sie auf den entsprechenden Knopf um den Report zu erzeugen und als Word-Dateien herunterzuladen."),
    downloadButton("repLPBtn", "Lehrangebot"),
    helpText("Eine formale Darstellung des Lehrprogramms, wie es in der StuKo und Fakultaetsrat beschlossen wird."),
    downloadButton("repDiagBtn", "Diagnostik des Lehrangebots"),
    helpText("Eine Diagnostik des Lehrprogramms. Vor allem gedacht um noch offene Baustellen in den Daten zu entdecken bevor das Lehrprogramm offiziell beschlossen wird."),
    downloadButton("repLBBtn","Lehrbeauftragte"),
    helpText("Eine Liste der Lehrbeauftragten mit Kurs, Koordinator und Verguetung.")
    #simpleButton("repKoordBtn","Vorlesungen nach Koordinatoren sortiert.")
  )

    setDownloadHandler("repLPBtn",
    filename=function(app = getApp())
      paste0("Lehrangebot_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      restore.point("jsfhshfzgfzzfhvn")
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        lehrangebot.report(semester=app$sem, db=app$glob$db, tpl.dir=app$glob$tpl.dir, out.file=file, strings=glob$strings)
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
