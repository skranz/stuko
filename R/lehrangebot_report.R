examples.lehrangebot.report = function() {
  sem_label = "Sommersemester 2018"
  date_label = format(Sys.Date(),"%d.%m.%Y")

  setwd("D:/libraries/stuko")
  db = get.stukodb()

  semester = 160
  copy.modules.to.semester(175, 160)

  kurse = dbGet(db,"kurs",params = list(semester=semester), schemas=stukodb.schemas())

  kupe = dbGet(db,"kursperson",params = list(semester=semester), schemas=stukodb.schemas())

  kumo = dbGet(db,"kursmodul", params=list(semester=semester))
  most = dbGet(db, "modulstudiengang", params=list(semester=semester))

  mozu = dbGet(db, "modulzuordnung", params=list(semester=semester))
  mosp = dbGet(db, "modulschwerpunkt", params=list(semester=semester))

  kuzu = inner_join(kumo, mozu, by=c("semester","modulid"))
  kust = inner_join(kumo, most, by=c("semester","modulid"))

  wp.ids = kuzu$kursid[str.starts.with(kuzu$zuordnung, "WP")]
  ba.ids = kust$kursid[kust$studiengang=="WiWi_BA"]
  ma.ids = kust$kursid[kust$studiengang=="WiWi_MA"]
  nuf.ids = kust$kursid[kust$studiengang=="WiWi_NUF"]
  nuf.wp.ids = kuzu$kursid[str.starts.with(kuzu$zuordnung, "NUF WP")]

  kurse$ba_wp = kurse$kursid %in% intersect(ba.ids, wp.ids)
  kurse$ba_pflicht = kurse$kursid %in% setdiff(ba.ids, wp.ids)

  kurse$ma_wp = kurse$kursid %in% intersect(ma.ids, wp.ids)

  kurse$nuf_wp = kurse$kursid %in% setdiff(nuf.ids, nuf.pflicht.ids)
  kurse$nuf_pflicht = kurse$kursid %in% nuf.pflicht.ids

  # Schwerpunkt via paste und dann per left_join an kurse matchen
  kusp = inner_join(kumo, mosp, by=c("semester","modulid"))



  tpl.file = "lehrangebot_tpl.docx"
  library(officer)
  library(flextable)

  doc = read_docx(tpl.file)
  #ds = docx_summary(doc)

  # Change bookmarks
  doc = doc %>%
    body_replace_at("sem_label",paste0(sem_label," ")) %>%
    body_replace_at("date_label", date_label)


  df = adapt.kurse.for.table(kurse[1:15,])

  ft <- regulartable(data = df) %>%
    #theme_vanilla() %>%
    bold(part = "header") %>% bold(j=3) %>%
    fontsize(size=12,part="all") %>%
    align( align = "left", part = "all" ) %>%
    align(align="center",j = 2, part="all") %>%
    border(border=fp_border(style="none"), part="body") %>%
    border(border=fp_border(style="none"), border.bottom = fp_border(style="solid", width=1), part="header") %>%
    autofit() %>% width(1,4) %>% width(2,1) %>% width(3,3)

  doc = doc %>%
    cursor_bookmark("wiwi_ba_pflicht") %>%
    body_add_flextable(ft,align="left", pos = "on")
  print(doc, target = "lehrangebot.docx")
}

cursor_bookmark_or_stay = function(x,id) {
  res = try(cursor_bookmark(x,id),silent = TRUE)
  if (is(res,"try-error")) {
    return(x)
  }
  res
}

adapt.kurse.for.table = function(df) {
  li = lapply(1:NROW(df), function(row) {
    res = adapt.kurs.for.table(df[row,])
  })
  bind_rows(li)
}

adapt.kurs.for.table = function(kurs) {
  restore.point("adapt.kurs.for.table")
  doz = filter(kupe, kursid==kurs$kursid)

  kurs$dozenten = paste0(unique(paste0(substring(doz$vorname,1,1), ". ", doz$nachname)), collapse=", ")
  kurs$Vorlesung = paste0(kurs$kursname,"\n")
  res = select(kurs, Vorlesung, SWS=sws_kurs, Dozent=dozenten)
  res = rbind(res, data.frame(Vorlesung=" ",SWS="", Dozent=""))


}
