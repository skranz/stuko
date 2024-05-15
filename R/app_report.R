reports.ui = function(..., app=getApp(), glob=app$glob) {
  ui = tagList(
    helpText("Druecken Sie auf den entsprechenden Knopf um den Report zu erzeugen und als Word-Dateien herunterzuladen."),

    downloadButton("repLPBtn", "Lehrangebot"),
    helpText("Eine formale Darstellung des Lehrprogramms, wie es in der StuKo und Fakultaetsrat beschlossen wird."),



    downloadButton("repDiagBtn", "Diagnostik des Lehrangebots"),
    helpText("Eine Diagnostik des Lehrprogramms. Vor allem gedacht um noch offene Baustellen in den Daten zu entdecken bevor das Lehrprogramm offiziell beschlossen wird."),

    downloadButton("repLaspBtn", "Lehrangebot nach Schwerpunkten"),
    helpText("Lehrangebot nach Schwerpunkten und vergleich mit 2 vorherigen Semerstern."),

    downloadButton("repWinfLABtn", "Lehrangebot Wirtschaftsinformatik"),
    helpText("Wirtschaftsinformatik Lehrangebot"),


    downloadButton("repPlanBtn", "4-Semestersplanung nach Profilen"),
    helpText("Geplantes Lehrangebot fuer 4 Semester nach Profilen."),

    downloadButton("repKernbereichBtn", "4-Semesterplanung Kernbereiche / AQMT"),
    helpText("Geplantes Lehrangebot MSc: Kernbereiche und AQMT fuer 4 Semester."),


    downloadButton("repLBBtn","Lehrbeauftragte"),
    helpText("Eine Liste der Lehrbeauftragten mit Kurs, Koordinator und Verguetung."),

    downloadButton("repPruefungBtn","Pruefungsliste"),
    helpText("Eine Liste der Pruefungen mit Pruefungsnummern zur zentralen Klausurplanung."),

    downloadButton("repEvalBtn","Evaluierungswuensche"),
    helpText("Eine Liste der Evaluierungswuensche (z.B. nur Vorlesung oder Vorlesung und Uebung) aller Kurse."),

    downloadButton("repSeminarterminBtn","Seminare: Anmelde- und Pruefungstermine"),
    helpText("Eine Liste der Anmelde- und Pruefungstermine der Seminare."),

    downloadButton("repFoseBtn","Forschungssemester"),
    helpText("Planung der Forschungssemester fuer naechste 6 Jahre"),

    downloadButton("repPlanEnBtn", "Plan englische Kurse"),
    helpText("Geplantes englischsprachiges Lehrangebot fuer 4 Semester, inkl. Kernbereiche und AQMT im Master."),

    downloadButton("repPlanProfileEnBtn", "Plan englische Kurse nach Profilen"),
    helpText("Geplantes englischsprachiges Lehrangebot fuer 4 Semester nach Schwerpunkten."),


    if (!is.null(app$glob$semdb.dir)) {
      tagList(
        downloadButton("repSeminarsBtn","Seminare im Lehrangebot und Matchingsoftware"),
        helpText("Vergleicht (per Fuzzy-Stringmatching) die Seminare, die im Lehrangebot eingetragen sind, mit den Seminaren, die in der Seminarsoftware freigeschaltet sind.")
      )
    },
    p()
    #simpleButton("repKoordBtn","Vorlesungen nach Koordinatoren sortiert.")
  )

    setDownloadHandler("repLPBtn",
    filename=function(app = getApp())
      paste0("Lehrangebot_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      restore.point("repLPBtn Downloadhandler")
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        lehrangebot.report(semester=app$sem, db=app$glob$db, tpl.dir=app$glob$tpl.dir, out.file=file, strings=glob$strings)
      )
    }
  )

  setDownloadHandler("repWinfLABtn",
    filename=function(app = getApp())
      paste0("WiInf_Lehrangebot_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      restore.point("repWinfLABtn Downloadhandler")
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        winf.lehrangebot.report(semester=app$sem, db=app$glob$db, tpl.dir=app$glob$tpl.dir, out.file=file, strings=glob$strings)
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

  setDownloadHandler("repPlanBtn",
    filename=function(app = getApp())
      paste0("Planung_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        planung.schwerpunkt.report(semester=app$sem, db=app$glob$db, out.file=file, sets=glob$sets)
      )
    }
  )

  setDownloadHandler("repKernbereichBtn",
    filename=function(app = getApp())
      paste0("Plan_Kern_AQMT_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        kernbereich.report(semester=app$sem, db=app$glob$db, out.file=file, sets=glob$sets)
      )
    }
  )

  setDownloadHandler("repPlanEnBtn",
    filename=function(app = getApp())
      paste0("Plan_English_Courses_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        kernbereich.report.engl(semester=app$sem, db=app$glob$db, out.file=file, sets=glob$sets)
      )
    }
  )
  setDownloadHandler("repPlanProfileEnBtn",
    filename=function(app = getApp())
      paste0("Plan_Profiles_English_Courses_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        planung.profile.report.engl(semester=app$sem, db=app$glob$db, out.file=file, sets=glob$sets)
      )
    }
  )

  setDownloadHandler("repFoseBtn",
    filename=function(app = getApp())
      paste0("Forschungssemester.docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        fose.report(start.semester=app$sem,end.semester=app$sem+6*10, db=app$glob$db, out.file=file, sets=glob$sets, glob=app$glob)
      )
    }
  )


  setDownloadHandler("repLaspBtn",
    filename=function(app = getApp())
      paste0("lehrangebot_schwerpunkt_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        lehrangebot.schwerpunkt.report(semester=app$sem, db=app$glob$db, out.file=file, sets=glob$sets)
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

  setDownloadHandler("repPruefungBtn",
    filename=function(app = getApp())
      paste0("Pruefungen_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        pruefung.report(semester=app$sem, db=app$glob$db, out.file=file, sets=glob$sets)
      )
    }
  )

  setDownloadHandler("repSeminarterminBtn",
    filename=function(app = getApp())
      paste0("Seminartermine_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        seminartermine.report(semester=app$sem, db=app$glob$db, out.file=file, sets=glob$sets)
      )
    }
  )

  setDownloadHandler("repEvalBtn",
    filename=function(app = getApp())
      paste0("Evaluationswunsch_",semester_name(app$sem),".docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        evaluierung.report(semester=app$sem, db=app$glob$db, out.file=file, sets=glob$sets)
      )
    }
  )


  setDownloadHandler("repSeminarsBtn",
    filename=function(app = getApp())
      paste0("Seminarvergleich.docx"),
    content = function(file, ...) {
      app=getApp()
      withProgress(message="Der Report wird erstellt. Dies dauert eine Weile...",
        seminar.report(semester=app$sem, db=app$glob$db, semdb.dir=app$glob$semdb.dir, out.file=file)
      )
    }
  )


  ui

}
