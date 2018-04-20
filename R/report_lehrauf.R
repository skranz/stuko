examples.lehrauftrag.report = function() {
  setwd("D:/libraries/stuko")
  db = get.stukodb()
  semester = 185
  lehrauftrag.report(semester, db)

}

lehrauftrag.report = function(semester, db = get.stukodb(), tpl.dir = getwd(), out.dir = getwd(), out.file = paste0(out.dir,"/lehrauftrag_", semester,".docx"), sets = rmdtools::read.yaml(system.file("yaml/sets.yaml", package = "stuko"))
) {
  restore.point("lehrauftrag.report")

  sem_label = semester_name(semester, kurz=FALSE)
  date_label = lang_datum(Sys.Date())


  sd = get.sem.data(sem=semester)
  lb = sd$kupe %>% filter(lehrauftrag != '-') %>%
    filter(lehrauftrag != "hp") # keine Honorarprofs

  lbku = inner_join(lb, select(sd$kurse,kursid, semester, kursname, koordinator), by=c("kursid","semester")) %>%
    arrange(koordinator, nachname)

  tab = transmute(lbku,
    Lehrbeauftragter = paste0(nachname, ", ", vorname),
    Kurs =kursname,
    Koordinator = koordinator,
    #'Kompensation' = to.label(lehrauftrag,sets$lehrauftrag),
    'LVS' = dozent_sws,
    la = str.left.of(lehrauftrag,"_pb"),
    Prueft = ifelse(la != lehrauftrag,"X","")
  )

  tpl.file = system.file("report_tpl/lehrauftrag_tpl.docx",package="stuko")

  doc = read_docx(tpl.file)

  # Change bookmarks
  doc = doc %>%
    body_replace_at("sem_label",paste0(sem_label," ")) %>%
    body_replace_at("date_label", date_label)


  las = c("ov","rk","vg","rk_vg")


  for (.la in las) {
    label = to.label(.la,sets$lehrauftrag)
    df = filter(tab, la==.la) %>% select(-la)
    doc = doc %>%
      body_add_par(label,style = "heading 1")
    if (NROW(df)>0) {
      doc = doc %>%
        body_add_table(df,style="Plain Table 1")
    } else {
      doc = doc %>%
        body_add_par("---")
    }
  }

  #ft = regulartable(tab)
  #doc = doc %>%
  #  body_add_table(tab)

  print(doc, target = out.file)
  invisible(doc)
}
