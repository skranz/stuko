examples.lehrangebot.report = function() {
  setwd("D:/libraries/stuko")
  db = get.stukodb("D:/libraries/stuko/ulm/db")
  semester = 185
  lehrangebot.report(semester, db)

}

lehrangebot.report = function(semester, db = get.stukodb(), tpl.dir = getwd(), out.dir = getwd(), out.file = paste0(out.dir,"/lehrangebot_", semester,".docx"), strings=list("Uebung"="Uebung")) {
  restore.point("lehrangebot.report")

  sem_label = semester_name(semester, kurz=FALSE)
  date_label = lang_datum(Sys.Date())

  kurse = load.kurse.for.lehrangebot(semester=semester, db=db)
  if (is.null(kurse)) return(NULL)

  kurse = filter(kurse, aktiv==TRUE)

  tpl.file = file.path(tpl.dir,"lehrangebot_tpl.docx")

  doc = read_docx(tpl.file)
  #ds = docx_summary(doc)

  # Change bookmarks
  doc = doc %>%
    body_replace_at("sem_label",paste0(sem_label," ")) %>%
    body_replace_at("date_label", date_label)


  add.ft = function(key, dat, show.sp=FALSE) {
    if (NROW(dat)==0) return(doc)
    ft = kurse.lehrangebot.word.table(dat, show.sp, strings=strings)
    doc %>%
      cursor_bookmark(key) %>%
      body_add_flextable(ft,align="left", pos = "on")
  }

  kurse$sp[is.na(kurse$sp)] = ""
  vorl = filter(kurse, kursform %in% c("vu","v")) %>% arrange(kursname)

  key = "wiwi_ba_pflicht"
  dat  = filter(vorl,ba_pflicht)
  doc = add.ft(key,dat, show.sp=FALSE)

  key = "wiwi_ba_wp"
  dat  = filter(vorl,ba_wp & !bama_wp)
  doc = add.ft(key,dat, show.sp=TRUE)


  key = "wiwi_bama_wp"
  dat  = filter(vorl,bama_wp)
  doc = add.ft(key,dat, show.sp=TRUE)

  key = "wiwi_ma_wp"
  dat  = filter(vorl,ma_wp & !bama_wp)
  doc = add.ft(key,dat, show.sp=TRUE)

  key = "nuf_pflicht"
  dat  = filter(vorl,nuf_pflicht)
  doc = add.ft(key,dat, show.sp=FALSE)

  key = "nuf_wp"
  dat  = filter(vorl,nuf_wp)
  doc = add.ft(key,dat, show.sp=FALSE)

  key = "sem_ba"
  dat  = filter(kurse,kursform=="se", ba) %>% arrange(kursname)
  doc = add.ft(key,dat, show.sp=FALSE)

  key = "sem_ma"
  dat  = filter(kurse,kursform=="se", ma) %>% arrange(kursname)
  doc = add.ft(key,dat, show.sp=FALSE)

  print(doc, target = out.file)
  invisible(doc)
}

cursor_bookmark_or_stay = function(x,id) {
  res = try(cursor_bookmark(x,id),silent = TRUE)
  if (is(res,"try-error")) {
    return(x)
  }
  res
}

kurse.lehrangebot.word.table = function(dat, show.sp=FALSE, strings) {
  restore.point("kurse.lehrangebot.word.table")
  df = adapt.kurse.for.lehrangebot(dat, show.sp=show.sp, strings=strings)

  ft <- regulartable(data = df) %>% autofit() %>%
    bold(part = "header") %>% bold(j=3) %>%
    fontsize(size=12,part="all") %>%
    align( align = "left", part = "all" ) %>%
    align(align="center",j = 2, part="all") %>%
    border(border=fp_border(style="none"), part="body") %>%
    border(border=fp_border(style="none"), border.bottom = fp_border(style="solid", width=1), part="header") %>%
    padding( padding.top = 2, padding.bottom=0, part = "body" ) %>%
    rotate(align = "top", part = "body")


  if (show.sp) {
    ft = ft %>% bold(j=4) %>%
      width(1,2.75) %>% width(2,0.7) %>% width(3,2) %>% width(4,1.5)
  } else {
    ft = ft %>% width(1,3.5) %>% width(2,1) %>% width(3,3)
  }

  ft
}


adapt.kurse.for.lehrangebot = function(df, show.sp=FALSE, strings) {
  li = lapply(1:NROW(df), function(row) {
    res = adapt.kurs.for.lehrangebot(df[row,], show.sp=show.sp,strings)
  })
  bind_rows(li)
}

adapt.kurs.for.lehrangebot = function(kurs, show.sp=FALSE, strings) {
  restore.point("adapt.kurs.for.lehrangebot")
  kurs$Vorlesung = kurs$kursname

  res = select(kurs, Vorlesung, SWS=sws_kurs, Dozent=dozent, Schwerpunkt=sp)
  has_u = kurs$kursform %in% "vu"
  if (has_u) {
    if (is.na(kurs$ul) | is.true(kurs$ul==""))
      kurs$ul = "NN"
    res_u = transmute(kurs, Vorlesung=paste0("  - ", strings$Uebung), SWS=sws_uebung, Dozent=ul, Schwerpunkt="")
    res = rbind(res,res_u)
  }

  res = rbind(res, data.frame(Vorlesung="",SWS="", Dozent="", Schwerpunkt=""))

  if (!show.sp) res = res[,1:3]
  res

}

load.kurse.for.lehrangebot = function(semester, db=get.stukodb(), remove.duplicated = TRUE, add.zuordnung=FALSE) {
  restore.point("load.kurse.for.lehrangebot")

  kurse = dbGet(db,"kurs",params = list(semester=semester), schemas=stukodb.schemas())

  #if(NROW(kurse)==0) return(NULL)

  kupe = dbGet(db,"kursperson",params = list(semester=semester), schemas=stukodb.schemas())

  kumo = dbGet(db,"kursmodul", params=list(semester=semester))
  most = dbGet(db, "modulstudiengang", params=list(semester=semester))

  mozu = dbGet(db, "modulzuordnung", params=list(semester=semester))
  mosp = dbGet(db, "modulschwerpunkt", params=list(semester=semester))

  kuzu = inner_join(kumo, mozu, by=c("semester","modulid"))
  kust = inner_join(kumo, most, by=c("semester","modulid"))

  wp.ids = kuzu$kursid[str.starts.with(kuzu$zuordnung, "WP")]
  #ba.pflicht.ids = kuzu$kursid[str.starts.with(kuzu$zuordnung, "WP")]
  ba.ids = kust$kursid[kust$studiengang=="WiWi_BA"]
  ma.ids = kust$kursid[kust$studiengang=="WiWi_MA"]
  nuf.ids = kust$kursid[kust$studiengang=="NUF_MA"]
  nuf.pflicht.ids = kuzu$kursid[str.starts.with(kuzu$zuordnung, "NUF Pflicht")]

  kurse$ba = kurse$kursid %in% ba.ids
  kurse$ma = kurse$kursid %in% c(ma.ids, nuf.ids)


  kurse$ba_wp = kurse$kursid %in% intersect(ba.ids, wp.ids)
  kurse$ba_pflicht = kurse$kursid %in% setdiff(ba.ids, wp.ids)

  kurse$ma_wp = kurse$kursid %in% intersect(ma.ids, wp.ids)

  kurse$nuf_wp = kurse$kursid %in% setdiff(nuf.ids, nuf.pflicht.ids)
  kurse$nuf_pflicht = kurse$kursid %in% nuf.pflicht.ids
  kurse$bama_wp = kurse$ba_wp & kurse$ma_wp


  # Schwerpunkt via paste und dann per left_join an kurse matchen
  kusp = inner_join(kumo, mosp, by=c("semester","modulid"))
  kusp = left_join(kusp,select(kurse, kursid, bama_wp), by="kursid") %>%
    mutate(sp_bama=substring(schwerpunkt,1,2))

  # Schwerpunkte von reinen Bachelor oder reinen Mastervorlesungen
  kusp1 = kusp %>%
    filter(!bama_wp) %>%
    group_by(kursid) %>%
    summarize(sp = paste0(unique(substring(schwerpunkt,4)), collapse=", ")) %>%
    select(kursid, sp)

  if (NROW(kusp)>0) {
    # Schwerpunkte von Kursen fuer Master und Bachelor
    kusp2 = kusp %>%
      filter(bama_wp) %>%
      group_by(kursid,sp_bama) %>%
      summarize(sp = paste0(unique(substring(schwerpunkt,4)), collapse=", ")) %>%
      ungroup() %>%
      tidyr::spread(key=sp_bama, value=sp) %>%
      mutate(sp = paste0("BA:", BA,"\nMA:",MA)) %>%
      select(kursid, sp)
  } else {
    kusp2 = NULL
  }

  kusp_sum = rbind(kusp1, kusp2)

  kurse = left_join(kurse, kusp_sum, by="kursid")

  if (NROW(kupe)>0) {
    # Dozenten
    kudo = kupe %>%
      filter(rolle %in% c("do","dk","ul")) %>%
      mutate(rolle = ifelse(rolle=="ul", "ul", "do")) %>%
      group_by(kursid, rolle) %>%
      summarize(dozent = paste0(unique(paste0(substring(vorname,1,1), ". ", nachname)), collapse=", ")) %>%
      ungroup() %>%
      tidyr::spread(key=rolle, value=dozent)
    if (!has.col(kudo,"do")) kudo$do = ""
    if (!has.col(kudo,"ul")) kudo$ul = ""
  } else {
    kudo = data_frame(kursid=character(0),do = character(0), ul=character(0))
  }
  kudo = select(kudo, kursid=kursid, dozent=do, ul=ul)
  kurse = left_join(kurse, kudo, by="kursid")

  kumo.ids = unique(kumo$kursid)
  kurse$has.modul = kurse$kursid %in% kumo.ids

  kurse$sp[is.na(kurse$sp)] = ""
  kurse$dozent[is.na(kurse$dozent)] = "NN"
  kurse$dozent[nchar(kurse$dozent)==0] = "NN"
  kurse$dozent[str.trim(kurse$dozent)=="."] = "NN"



  if (add.zuordnung) {
    kurse = left_join(kurse, kuzu, by=c("kursid","semester")) %>%
      group_by(kursid, semester) %>%
      mutate(zuordnung=paste0(sort(zuordnung), collapse=", ")) %>%
      ungroup()
  }
  kurse = kurse[!duplicated(kurse),]

  #kuzu

  kurse
}

