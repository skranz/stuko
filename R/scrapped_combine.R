examples.scrapped.to.db.csv = function() {
  setwd("D:/libraries/stuko/")
  semester = 175
  scrapped.to.db.csv(semester=semester)
}

scrapped.to.db.csv = function(semester, dir = setwd()) {
  schema.file = system.file("schema/stukodb.yaml",package = "stuko")
  schemas = dbmisc::load.and.init.schemas(schema.file)

  cat("\nErstelle Personen...")
  scrapped.person.to.db.csv()
  cat("\nErstelle Modulhandbuchbeschreibungen...")
  mhb = scrapped.mhb.to.db.csv(schemas)
  cat("\nErstelle Module...")
  scrapped.modul.to.db.csv(semester, schemas)
  cat("\nErstelle Kurse...")
  scrapped.kurs.to.db.csv()

}


scrapped.kurs.to.db.csv = function() {
  restore.point("scrapped.kurs.to.db.csv")

  dat = read_csv("vvz_wiwi.csv")
  dat = rename(dat, kursname=kurs)

  dat$sprache = cld2::detect_language(dat$kursname)
  dat$sprache[is.na(dat$sprache)] = "-"

  dat$kursid = gsub(".","_",dat$vnum,fixed=TRUE)
  unique(dat$semester)
  year = substring(dat$semester,5,6)
  dat$semester = as.integer(paste0(year, ifelse(str.starts.with(dat$semester, "sose"),"0","5")))

  dat = mutate(dat,
    aktiv = TRUE,
    sws_kurs = sws,
    sws_uebung = 0
  )

  #cat(paste0('"',unique(dat$art),'" = ""', collapse=",\n"))
  dat$kursform = recode(dat$art,
    "Vorlesung/ Übung" = "vu",
    "Vorlesung" = "v",
    "Seminar" = "se",
    "Kolloquium" = "kol",
    "Begleitseminar" = "bse",
    "Übung" = "u",
    "Tutorium" = "tut",
    "Übung/Seminar" = "se"
  )
  dat$kursform = str.trim(dat$kursform)
  rows = str.starts.with(dat$kursform,"Vorlesung/")
  dat$kursform[rows] = "vu"
  rows = str.ends.with(dat$kursform,"bung")
  dat$kursform[rows] = "u"
  rows = str.ends.with(dat$kursform,"Seminar")
  dat$kursform[rows] = "se"



  dat$zeitform = recode(dat$kursform,
    "vu" = "w",
    "v" = "w",
    "se" = "b",
    "kol" = "-",
    "bse" = "-",
    "u" = "w",
    "tut" = "w"
  )




  # Default Turnus: Jedes zweite Semester
  dat$turnus = 2
  dat$zukunft_sem = add.semester(dat$semester, dat$turnus)
  dat$zukunft_sem2 = add.semester(dat$semester, dat$turnus*2)
  dat$codeshare = ""


  dat = dat[!duplicated(dat),]
  # Merge Vorlesung / Übung
  adapt.uebung = function(df) {
    if (NROW(df)<=1) return(df)
    u.rows = which(df$kursform == "u")
    v.rows = which(df$kursform %in% c("v","vu"))
    if (length(u.rows)==0 & length(v.rows)==0) {
      return(df)
    }
    if (length(u.rows)==0) {
      return(df[v.rows,])
    }
    v.row = v.rows[1]
    u.row = u.rows[1]
    df$kursform[v.row] = "vu"
    df$sws_uebung[v.row] = df$sws_kurs[u.row]
    df$dozenten[v.row] = paste0(df$dozenten[v.row],";",df$dozenten[u.row])

    df[-u.rows,]
  }

  dat = dat %>% group_by(kursname, semester) %>%
    do(adapt.uebung(.)) %>%
    ungroup()

  kurs = select(dat,kursid, semester, aktiv, vnum, kursname, sws_kurs, sws_uebung, kursform, zeitform, sprache, turnus, zukunft_sem, zukunft_sem2,codeshare)



  kurs = kurs[!duplicated(kurs),]

  write_csv(kurs, "kurs_db.csv")


  # Kurspersonen
  d = select(dat, kursid, semester,kursform, dozenten) %>%
    mutate(dozent = strsplit(dozenten,";",fixed=TRUE)) %>%
    tidyr::unnest(dozent)

  d = d[!duplicated(d),]

  d = mutate(d,
    nachname = str.trim(str.left.of(d$dozent,",")),
    vorname = str.trim(str.right.of(d$dozent,",")),
    lehrauftrag = "",
    dozent_sws = 0
  )

  pers = read_csv("person_db.csv")
  nn = amatch.with.rel.dist(d$nachname, pers$nachname, method="osa")
  vn = amatch.with.rel.dist(d$vorname, pers$vorname, method="osa")

  is.match = nn$dist <= 0.25 & vn$dist <= 0.25
  d$m.nachname = nn$key
  d$m.vorname = vn$key
  d = left_join(d, select(pers, personid, m.vorname=vorname, m.nachname=nachname), by=c("m.vorname","m.nachname")) %>%
    select(-m.vorname, -m.nachname)
  d$personid[!is.match | is.na(d$personid)] = ""

  rows = has.substr(d$nachname,"Klier") & has.substr(d$vorname,"Mat")
  d$personid[rows] = "klier_mathias"
  d$nachname[rows] = "Klier"
  d$vorname[rows] = "Mathias"

  d = group_by(d, kursid, semester) %>%
    mutate(rolle = ifelse(1:n()==1, "do","ul")) %>%
    ungroup()
  d$rolle[d$personid != ""] = "dk"
  d$rolle[d$kursform == "u"] = "ul"

  kursperson = select(d, kursid, semester, personid, vorname, nachname, rolle, lehrauftrag, dozent_sws)

  write_csv(kursperson, "kursperson_db.csv")

  # Module und Kursmatching
  mod = read_csv("modul_db.csv")

  mk = amatch.with.rel.dist(kurs$kursname, mod$titel)
  ignore = mk$dist > 0.3
  code = mod$code[match(mk$key, mod$titel)]


  km = select(kurs, kursid, semester)
  km$code = code
  km = km[!ignore,]
  km = left_join(km, select(mod, code, modulid), by="code") %>%
    select(kursid, semester, modulid) %>%
    unique

  write_csv(km, "kursmodul_db.csv")

}

add.semester = function(semester, add) {
  semester + add*5
}

scrapped.person.to.db.csv = function() {
  dat = read_csv("ansprechpartner.csv")
  dat$email = tolower(paste0(dat$vorname,".",dat$nachname,"@uni-ulm.de"))
  dat$vorname[is.na(dat$vorname)] =""
  dat$email[nchar(dat$vorname)==0] =""

  pvn = str.left.of(dat$vorname,"-")
  pnn = str.left.of(dat$nachname,"-")
  pvn[is.na(pvn)] = ""
  pnn[is.na(pnn)] = ""
  center = ifelse(nchar(pvn)>0,"_","")

  dat$personid = replace.umlaute(tolower(paste0(pnn,center,pvn)))



  dat$koordinator = TRUE
  dat$admin = FALSE
  admins = c("Kranz","Rieber","Gebhardt","Studiendekan WiWi")
  rows = dat$nachname %in% admins
  dat$stuko[rows] = dat$admin[rows] = TRUE

  dat = select(dat, personid, email, vorname, nachname, titel, koordinator, admin)

  dupl = duplicated(dat[,"personid"])
  dat = dat[!dupl,]

  write_csv(dat, "person_db.csv")

}

scrapped.modul.to.db.csv = function(semester,schemas=stukodb.schemas()) {
  restore.point("scrapped.modul.to.db.csv")

  mo = read_csv("modules_with_extern.csv",col_types = cols(
    .default = col_character(),
    extern = col_integer(),
    code = col_character(),
    ects = col_integer()
  ))
  mo$code = ifelse(nchar(mo$code)>5,substring(mo$code, 6), mo$code)

  mo$semester = semester

  # Prüfungsform
  n = NROW(mo)
  pf = rep("-",n)
  rows = has.substr(tolower(mo$bewertungsmethode), "schriftlich") | has.substr(tolower(mo$bewertungsmethode), "klausur")
  pf[rows] = "k"
  rows.m = has.substr(tolower(mo$bewertungsmethode), "mündlich")
  pf[rows.m & rows] = "km"
  pf[rows.m & !rows] = "m"
  rows = has.substr(tolower(mo$bewertungsmethode), "seminar") | has.substr(tolower(mo$titel), "seminar") | has.substr(tolower(mo$zuordnung), "seminar")
  pf[rows] = "se"

  mo$pruefungsform=pf


  # Combine Zuordnung for equivalent modules
  mo = mo %>% group_by(code, titel, ects, pruefungsform) %>%
    mutate(all_zu = paste0(zuordnung, collapse=","), all_sg=paste0(handbuch, collapse=",")) %>%
    ungroup()

  # Create modulid and remove duplicated modules
  mof = filter(mo, !duplicated(select(mo, code, titel, ects, pruefungsform)))

  # Create modulid
  # set default modulid
  mof = mof %>%
    arrange(code, handbuch) %>%
    group_by(code) %>%
    mutate(modulid = paste0(code,"_",1:n())) %>%
    ungroup()

  # Modul-Zuordnungen Tabelle
  zu_table = read_csv("zuordnung.csv")

  mof$zul = lapply(strsplit(mof$all_zu,",", fixed=TRUE), function(zus) {
    restore.point("hsjfhkfh")
    zu = unique(str.trim(zus))
    rows = match(zu, zu_table$mhb)
    as.character(na.omit(zu_table$kurz[rows]))
  })

  mozu = select(mof,modulid, semester, zuordnung = zul) %>% tidyr::unnest(zuordnung)
  write_csv(mozu,"modulzuordnung_db.csv")

  # Modul-Studiengang Tabelle
  mof$sgl = lapply(strsplit(mof$all_sg,",", fixed=TRUE), function(sg) {
    zu = sort(unique(str.trim(sg)))
  })

  mosg = select(mof,modulid, semester, studiengang = sgl) %>% tidyr::unnest(studiengang)
  write_csv(mosg,"modulstudiengang_db.csv")


  # modulschwerpunkt
  sp_table = read.csv("schwerpunkte.csv",stringsAsFactors = FALSE)

  mof$spl = lapply(1:NROW(mof), function(row) {
    restore.point("hsjfhkfh")
    sp = unique(str.trim(strsplit(mof$all_zu[row],",", fixed=TRUE)[[1]]))
    sp = sp[str.starts.with(sp, "Schwerpunkt")]

    sg = unique(mosg$studiengang[mosg$modulid %in% mof$modulid[row]])
    rows = match(sp, sp_table$mhb)
    rows = rows[!is.na(rows)]
    res = NULL
    if ("WiWi_BA" %in% sg) {
      res = c(res, sp_table$ba[rows])
    }
    if ("WiWi_MA" %in% sg) {
      res = c(res, sp_table$ma[rows])
    }
    res
  })
  mosp = select(mof,modulid, semester, schwerpunkt = spl) %>% tidyr::unnest(schwerpunkt) %>% filter(nchar(schwerpunkt)>0)
  write_csv(mosp,"modulschwerpunkt_db.csv")

  # write modules
  mod = select(mof, modulid, semester, code, extern, titel, ects, pruefungsform)
  write_csv(mod,"modul_db.csv")
}



scrapped.mhb.to.db.csv = function(schemas) {
  restore.point("scrapped.to.mhb.db.csv")

  mo = read_csv("modules_with_extern.csv",col_types = cols(
    .default = col_character(),
    extern = col_integer(),
    code = col_character(),
    ects = col_integer()
  ))
  mo$code = ifelse(nchar(mo$code)>5,substring(mo$code, 6), mo$code)


  mhb = as_data_frame(dbmisc::convert.r.to.db(filter(mo, extern==0), schema=schemas$mhb))

  # remove duplicated module descriptions
  dupl = duplicated(select(mhb,-handbuch,-code))
  mhb = mhb[!dupl,]

  # set default modulid
  mhb = mhb %>%
    arrange(code, handbuch) %>%
    group_by(code) %>%
    mutate(modulid = paste0(code,"_",1:n())) %>%
    ungroup()

  write_csv(mhb,"mhb_db.csv")
  invisible(mhb)

}

scrapped.combine = function() {
  setwd("D:/libraries/stuko/")
  library(readr)

  mo = read_csv("modules.csv")
  vv = read_csv("vvz_wiwi.csv")
  vv = group_by(vv, kurs) %>%
    mutate(
      sem = paste0(unique(semester), collapse=", "),
      dozenten=paste0(unique(dozenten), collapse=", ")
    ) %>%
    ungroup()

  vv = vv[!duplicated(vv$kurs),]
  unique(vv$kurs)

  vk = unique(vv$kurs)
  mk = unique(mo$Titel)

  df = make.amatch.table(vk,mk, names=c("kurs","modul")) %>%
    arrange(desc(dist_lcs))

  df = left_join(df, select(vv,kurs, sem, dozenten), by="kurs" )

  write_csv(df, "kurs_modul_match.csv")
}

make.amatch.table = function(v1, v2, methods=c("lcs","qgram", "osa"), maxDist=Inf,names=c("v1","v2"),...) {
  restore.point("make.amatch.table")

  n = length(methods)
  m.li = lapply(methods, function(method) {
    amatch(v1,v2, method=method, maxDist = maxDist)
  })
  v2.li = lapply(1:n, function(i) {
    v2[m.li[[i]] ]
  })

  d.li = lapply(1:n, function(i) {
    stringdist(v1, v2.li[[i]], method=methods[i], maxDist=maxDist)
  })
  names(v2.li)=paste0(names[2],"_",methods)
  names(d.li)=paste0("dist_", methods)

  df = cbind(data_frame(x=v1),as_data_frame(v2.li), as_data_frame(d.li) )
  colnames(df)[1] = names[1]

  df
}
