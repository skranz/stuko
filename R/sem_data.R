
get.sem.data = function(sem=app$sem, update=FALSE, app=getApp(), glob=app$glob) {
  restore.point("get.sem.data")
  sem.key = as.character(sem)
  if (!update & !is.null(glob$sem.dat[[sem.key]])) {
    return(glob$sem.dat[[sem.key]])
  }

  semester = sem
  db = get.stukodb()
  # Load main tables
  ku = dbGet(db,"kurs",nlist(semester))
  kupe = dbGet(db,"kursperson",nlist(semester))

  kumo = dbGet(db,"kursmodul",nlist(semester))

  mo = dbGet(db, "modul", nlist(semester))
  most = dbGet(db, "modulstudiengang", params=list(semester=semester))
  mozu = dbGet(db, "modulzuordnung", params=list(semester=semester))
  mosp = dbGet(db, "modulschwerpunkt", params=list(semester=semester))

  # Init modul table with detailed information


  # Add Studiengang
  module = mo %>%
    left_join(most, by=c("modulid","semester")) %>%
    group_by(modulid, semester) %>%
    mutate(studiengang = paste0(sort(unique(studiengang)), collapse=", ")) %>%
    dplyr::distinct(modulid, semester, .keep_all=TRUE) %>%
    mutate(bama = str.trim(paste0(
      ifelse(has.substr(studiengang,"BA"),"BA","")," ",
      ifelse(has.substr(studiengang,"MA"),"MA","")
    )))

  # Add zuordnung
  module = module %>%
    left_join(mozu, by=c("modulid","semester")) %>%
    group_by(modulid, semester) %>%
    mutate(zuordnung = paste0(sort(unique(zuordnung)), collapse=", ")) %>%
    dplyr::distinct(modulid, semester, .keep_all=TRUE)

  module$zuordnung = gsub("NUF WP 35 LP, NUF WP 42 LP, NUF WP 49 LP", "NUF WP", fixed=TRUE, module$zuordnung)

  # Add schwerpunkt
  module = module %>%
    left_join(mosp, by=c("modulid","semester")) %>%
    group_by(modulid, semester) %>%
    mutate(schwerpunkt = paste0(sort(unique(schwerpunkt)), collapse=", ")) %>%
    dplyr::distinct(modulid, semester, .keep_all=TRUE) %>%
    ungroup()

  module = module %>%
    change.df.na() %>%
    mutate(label = paste0(titel,", ",ects, " ECTS,", bama,", ", ifelse(extern,"extern,",""), zuordnung, ",", schwerpunkt, ", ", to.label(pruefungsform,glob$sets$pruefungsform))) %>%
    ungroup() %>%
    arrange(titel)


  ## Kurse


  # Dozenten und Koordinator
  kupe = kupe %>% mutate(name = paste0(substring(vorname,1,1), ". ", nachname)) %>% replace_na(list(lehrauftrag="-"))


  kudo = kupe %>% filter(rolle %in% c("do","dk"))
  kuko = kupe %>% filter(rolle %in% c("dk","ko"))
  kuul = kupe %>% filter(rolle %in% c("ul"))

  kurse = ku %>%
    left_join(select(kudo, kursid,semester,name), by=c("kursid","semester")) %>%
    group_by(kursid, semester) %>%
    mutate(dozent = paste0(sort(unique(name)), collapse=", ")) %>%
    select(-name) %>%
    dplyr::distinct(kursid, semester, .keep_all=TRUE)

  kurse = kurse %>%
    left_join(select(kuko, kursid,semester,name,koid=personid), by=c("kursid","semester")) %>%
    group_by(kursid, semester) %>%
    mutate(koordinator = paste0(sort(unique(name)), collapse=", "), koid = paste0(sort(unique(koid)), collapse=", ")) %>%
    select(-name) %>%
    dplyr::distinct(kursid, semester, .keep_all=TRUE)

  kurse = kurse %>%
    left_join(select(kuul, kursid,semester,name), by=c("kursid","semester")) %>%
    group_by(kursid, semester) %>%
    mutate(ul = paste0(sort(unique(name)), collapse=", ")) %>%
    select(-name) %>%
    dplyr::distinct(kursid, semester, .keep_all=TRUE)

  kurse = kurse %>%
    left_join(select(kupe, kursid,semester,lehrauftrag), by=c("kursid","semester")) %>%
    group_by(kursid, semester) %>%
    mutate(lehrauftrag = paste0(sort(unique(lehrauftrag)), collapse=", ")) %>%
    dplyr::distinct(kursid, semester, .keep_all=TRUE)


  # Add modulinfo to kurse
  kumos = left_join(select(module,-modify_time,-modify_user), kumo, by=c("modulid","semester"))


  kurse = kurse %>% ungroup() %>%
    left_join(kumos, by=c("kursid","semester")) %>%
    change.df.na() %>%
    group_by(kursid, semester) %>%
    mutate(
      num_modul = sum(is.true(nchar(modulid)>0)),
      titel = paste0(unique(titel), collapse=", "),
      extern = first(extern),
      studiengang = paste0(unique(studiengang), collapse=", "),
      ects = paste0(unique(ects), collapse=", "),
      schwerpunkt = paste0(unique(schwerpunkt), collapse=", "),
      bama = paste0(unique(bama), collapse=", "),
      pruefungsform = paste0(unique(pruefungsform), collapse=", "),
      zuordnung = paste0(unique(zuordnung), collapse=", "),
    ) %>%
    dplyr::distinct(kursid, semester, .keep_all=TRUE) %>%
    ungroup() %>%
    arrange(kursname)

  # Add modulinfo to kurse
  mokus = left_join(select(kurse,kursid, semester, kursname, dozent), kumo, by=c("kursid","semester"))

  module = module %>% ungroup() %>%
    left_join(mokus, by=c("modulid","semester")) %>%
    change.df.na() %>%
    group_by(modulid, semester) %>%
    mutate(
      num_kurs = sum(is.true(nchar(kursid)>0)),
      kurs = paste0(unique(kursname), collapse="; "),
      dozent = paste0(unique(dozent), collapse=", ")
    ) %>%
    dplyr::distinct(modulid, semester, .keep_all=TRUE) %>%
    ungroup() %>%
    arrange(titel)


  sd = nlist(
    module,
    kurse,
    ku, kupe, kumo,
    mo, most,mosp, mozu
  )
  if (!is.null(glob))
    glob$sem.dat[[sem.key]] = sd
  sd
}

change.df.na = function(df, char.value="", num.value=NA, logical.value=NA, else.value = "") {
  restore.point("change.df.na")
  if (NROW(df)==0) return(df)

  li = lapply(df, function(vals) {
    na.rows = is.na(vals)
    if (is.character(vals)) {
      vals[na.rows] = char.value
    } else if (is.numeric(vals)) {
      vals[na.rows] = num.value
    } else if (is.logical(vals)) {
      vals[na.rows] = logical.value
    } else if (is.POSIXct(vals) | is.Date(vals)) {
    } else {
      vals[na.rows] = else.value
    }
    vals
  })
  if (is(df,"tbl_df")) {
    as_data_frame(li)
  } else if (is.data.frame(df)) {
    as.data.frame(li)
  } else {
    as(li, class(df))
  }

}
