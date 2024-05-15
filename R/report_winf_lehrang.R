examples.winf.lehrangebot.report = function() {
  setwd("C:/libraries/stuko")
  db = get.stukodb("C:/libraries/stuko/ulm/db")
  semester = 245
  tpl.dir = system.file("report_tpl",package="stuko")
  winf.lehrangebot.report(semester, db, tpl.dir=tpl.dir)

}

winf.lehrangebot.report = function(semester, db = get.stukodb(), tpl.dir = getwd(), out.dir = getwd(), out.file = paste0(out.dir,"/winf_lehrangebot_", semester,".docx"), strings=list("Uebung"="Uebung")) {
  restore.point("winf.lehrangebot.report")

  sem_label = semester_name(semester, kurz=FALSE)
  date_label = lang_datum(Sys.Date())

  sd = get.sem.data(semester)
  kurse = sd$kurse %>%
    filter(aktiv==TRUE) %>%
    arrange(kursname) %>%
    mutate(zu = tolower(zuordnung), sp="")

  tpl.file = file.path(tpl.dir,"winf_lehrangebot_tpl.docx")

  doc = read_docx(tpl.file)
  #ds = docx_summary(doc)

  # Change bookmarks
  doc = doc %>%
    body_replace_at("sem_label",paste0(sem_label," ")) %>%
    body_replace_at("date_label", date_label)


  add.ft = function(dat, title="Vorlesung") {
    if (NROW(dat)==0) return(doc)
    ft = kurse.winf.lehrangebot.word.table(dat, strings=strings, title=title)
    doc %>%
      body_add_flextable(ft,align="left", pos = "after")
  }

  doc = doc %>%
    body_add_par("Pflichtkurse", style = "heading 1")
  dat  = filter(kurse, has.substr(zu, "wi pflicht"))
  doc = add.ft(dat, title="Kurs (Pflicht)")


  doc = doc %>%
    body_add_par("Aufbaubereich Wirtschaftswissenschaften", style = "heading 1")
  dat  = filter(kurse, has.substr(zu, "wi aufbau wiwi"))
  doc = add.ft(dat, title="Vorlesung (Aufbau WiWi)")

  doc = doc %>%
    body_add_par("Aufbaubereich Informatik", style = "heading 1")
  dat  = filter(kurse, has.substr(zu, "wi aufbau inf"))
  doc = add.ft(dat, title="Vorlesung (Aufbau Inf)")

  doc = doc %>%
    body_add_par("Analytics Wirtschaftswissenschaften", style = "heading 1")
  dat  = filter(kurse, has.substr(zu, "wi analytics wiwi"))
  doc = add.ft(dat, title="Vorlesung (Analytics WiWi)")

  doc = doc %>%
    body_add_par("Analytics Informatik", style = "heading 1")
  dat  = filter(kurse, has.substr(zu, "wi analytics inf"))
  doc = add.ft(dat, title="Vorlesung (Analytics Inf)")

  doc = doc %>%
    body_add_par("Digital Business Wirtschaftswissenschaften", style = "heading 1")
  dat  = filter(kurse, has.substr(zu, "wi digbu wiwi"))
  doc = add.ft(dat, title="Vorlesung (Digital WiWi)")

  doc = doc %>%
    body_add_par("Digital Business Informatik", style = "heading 1")
  dat  = filter(kurse, has.substr(zu, "wi digbu inf"))
  doc = add.ft(dat, title="Vorlesung (Digital Inf)")

  doc = doc %>%
    body_add_par("Wahlpflichtkurse", style = "heading 1")
  dat  = filter(kurse, has.substr(zu, "wi wp"))
  doc = add.ft(dat, title="Vorlesung (WP WiInf)")


  doc = doc %>%
    body_add_par("Aufbaubereich Informatik", style = "heading 1")
  dat  = filter(kurse, has.substr(zu, "wi aufbau inf"))
  doc = add.ft(dat, title="Vorlesung")




  print(doc, target = out.file)
  invisible(doc)
}


kurse.winf.lehrangebot.word.table = function(dat,  strings, title="Vorlesung") {
  restore.point("kurse.winf.lehrangebot.word.table")
  df = adapt.kurse.for.lehrangebot(dat, show.sp=FALSE, strings=strings)
  colnames(df)[1] = title

  ft <- regulartable(data = df) %>% autofit() %>%
    bold(part = "header") %>% bold(j=3) %>%
    fontsize(size=12,part="all") %>%
    align( align = "left", part = "all" ) %>%
    align(align="center",j = 2, part="all") %>%
    border(border=fp_border(style="none"), part="body") %>%
    border(border=fp_border(style="none"), border.bottom = fp_border(style="solid", width=1), part="header") %>%
    padding( padding.top = 2, padding.bottom=0, part = "body" ) #%>%
    #rotate(align = "top", part = "body")

  ft = ft %>% width(1,3.5) %>% width(2,1) %>% width(3,3)

  ft
}

