# Diagnosen zum Lehrangebot

examples.seminartermine.report = function() {
  setwd("D:/libraries/stuko/")
  db = get.stukodb("D:/libraries/stuko/ulm/db")

  semester = 185
  seminartermine.report(semester, db)

}

seminartermine.report = function(semester, db = get.stukodb(), out.dir = getwd(), out.file = paste0(out.dir,"/seminartermine.docx"), sets=getApp()$glob$sets) {
  restore.point("seminartermine.report")

  sem_label = semester_name(semester, kurz=FALSE)
  date_label = format(Sys.time(),"%d.%m.%Y %H:%M")

  sd = get.sem.data(semester)
  ku = sd$kurse

  kumo = sd$kumo
  kumo = left_join(kumo, sd$mo, by=c("semester","modulid"))
  mopr = kumo %>%
    group_by(kursid) %>%
    summarize(modulpruef = paste0("[",code,"] ", pruefungsnr, collapse="\n\n"))

  ku = inner_join(ku, select(mopr,kursid,modulpruef),by="kursid")

  ku = filter(ku,
    is.true(extern==FALSE),
    kursform == "se"
  )

  ku = mutate(ku, Kurs = paste0(kursname, " [", bama,"]")) %>%
    arrange(dozent,bama, kursname)

  tpl.file = system.file("report_tpl/seminartermin_tpl.docx",package="stuko")
  doc = read_docx(tpl.file)



  add.text= function(old, add, sep=", ") {
    if (length(old)==0) return(old)
    add.rows = nchar(old)>0
    if (length(add)==1) {
      old[add.rows] = paste0(old[add.rows],sep,add)
      old[!add.rows] = add
    } else {
      old[add.rows] = paste0(old[add.rows],sep,add[add.rows])
      old[!add.rows] = add[!add.rows]
    }

    old
  }

  # Change bookmarks
  doc = doc %>%
    body_replace_at("sem_label",paste0(sem_label," ")) %>%
    body_replace_at("date_label", date_label)

  df = transmute(ku,Seminar=Kurs,Dozent=dozent,`[Modul]  Pruefungen`=modulpruef,`Anmeldungs- und Pruefungstemin`=seminar_termine)

  if (NROW(df)>0) {
    doc = doc %>%
      body_add_table(df, style="Plain Table 1")
  } else {
    doc = doc %>%
      body_add_par("--- Kein Eintrag ---")
  }

  print(doc, target = out.file)

  invisible(doc)
}



