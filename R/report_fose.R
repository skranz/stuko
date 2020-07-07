# Diagnosen zum Lehrangebot

examples.fose.report = function() {
  setwd("D:/libraries/stuko/")
  db = get.stukodb("D:/libraries/stuko/ulm/db")

  start.semester = 200
  end.semester = 270
  fose.report(start.semester,end.semester, db)

}

prepare.org.fose = function(org.fose, start.semester, end.semester, max.back=10,sets=getApp()$glob$sets) {
  org.fose$in_db = TRUE
  fose.last = org.fose %>%
    group_by(personid) %>%
    summarize(semester = max(semester))
  fose.next1 = fose.last %>%
    mutate(
      semester = semester+5*9,
      comment = "(nach Turnus)",
      in_db = FALSE
    )
  fose.next2 = fose.next1 %>%
    mutate(
      semester = semester+5*9
    )


  fose = bind_rows(org.fose, fose.next1, fose.next2) %>%
    arrange(semester, personid)

  fose = fose %>%
    group_by(personid) %>%
    mutate(gewartet = c("?",diff(semester)/5)) %>%
    ungroup()

  fose = fose %>%
    filter(semester >=start.semester, semester <= end.semester) %>%
    filter(personid %in% sets$prof)
  fose
}

fose.report = function(start.semester,end.semester, db = get.stukodb(), out.dir = getwd(), out.file = paste0(out.dir,"/forschungssemester.docx"), sets=glob$sets, glob=getApp()$glob) {
  restore.point("fose.report")

  date_label = format(Sys.time(),"%d.%m.%Y %H:%M")

  org.fose = dbGet(db, "fose")

  fose = prepare.org.fose(org.fose, start.semester, end.semester, sets=sets)

  sql = paste0("
  SELECT kursperson.personid, kursperson.semester, kursperson.kursid, kurs.kursname, kurs.aktiv
  FROM kursperson INNER JOIN kurs
  ON kurs.semester=kursperson.semester AND
     kurs.kursid = kursperson.kursid
WHERE kursperson.semester >= ", as.integer(start.semester) , " AND kursperson.semester <= ", as.integer(end.semester), " AND kursperson.personid IS NOT NULL AND (rolle == 'do' OR rolle=='dk') AND kurs.aktiv==1")

  kupe = dbGet(db,sql=sql, convert=FALSE)

  kupe = semi_join(kupe, fose,by=c("semester","personid"))

  kurse = kupe %>%
    group_by(personid, semester) %>%
    summarize(
      kurse = paste0(kursname, collapse="; ")
    )

  fose = left_join(fose, kurse, by = c("personid","semester"))
  rows = is.na(fose$kurse)
  fose$kurse[rows] = ""

  tpl.file = system.file("report_tpl/fose_tpl.docx",package="stuko")
  doc = read_docx(tpl.file)

  # Change bookmarks
  doc = doc %>%
    body_replace_at("date_label", date_label)


  fose %>% arrange(semester, personid)
  semesters = seq(start.semester, end.semester, by=5)

  personen = glob$person$kurzname
  names(personen) = glob$person$personid
  sem = semesters[1]
  for (sem in semesters) {
    doc = doc %>%
      body_add_par(semester_name(sem), style = "heading 1")
    tab = filter(fose, semester==sem)
    if (NROW(tab)>0) {
      tab = tab %>%
        transmute(
          Dozent = personen[personid],
          Gewartet = gewartet,
          Kommentar = comment,
          Aktive_Kurse_in_Datenbank = kurse
        )
      doc = doc %>%
        body_add_table(tab, style="Plain Table 1")

    } else {
      doc = doc %>%
        body_add_par("Kein Eintrag")
    }
  }
  print(doc, target = out.file)
  invisible(doc)
}



