# Export all Kurse to a flat CSV file
export.kurse = function(sem, out="csv") {
  stuko.dir = "C:/libraries/stuko/ulm"
  setwd(stuko.dir)
  db.dir = file.path(stuko.dir, "db")
  sem = 215

  db = get.stukodb(db.dir)


  semester = sem
  # Load main tables
  ku = dbGet(db,"kurs",nlist(semester, aktiv=TRUE))
  kursids = ku$kursid

  kupe = dbGet(db,"kursperson",nlist(semester)) %>%
    filter(kursid %in% kursids)


  kumo = dbGet(db,"kursmodul",nlist(semester)) %>%
    filter(kursid %in% kursids)

  mo = dbGet(db, "modul", nlist(semester)) %>%
    filter(modulid %in% unique(kumo$modulid))


  most = dbGet(db, "modulstudiengang", params=list(semester=semester))
  mozu = dbGet(db, "modulzuordnung", params=list(semester=semester))
  mosp = dbGet(db, "modulschwerpunkt", params=list(semester=semester))

  # Dozenten
  kupe = filter(kupe, rolle %in% c("do","dk"))

  kust = left_join(kumo, most, by=c("modulid","semester"))
  kuzu = left_join(kumo, mozu, by=c("modulid","semester"))
  kusp = left_join(kumo, mosp, by=c("modulid","semester"))

  kust$is.ba = has.substr(kust$studiengang,"BA")
  kubama = kust %>%
    group_by(kursid) %>%
    summarize(
      ba = any(studiengang == "WiWi_BA"),
      ma = any(studiengang == "WiWi_MA"),
      nuf = any(studiengang=="NUF_MA"),
      wiwi.bama = ifelse(ba,ifelse(ma, "BAMA","BA"),"MA"),
    ) %>%
    ungroup()

  ku = left_join(ku, kubama, by="kursid")
  # Init modul table with detailed information

  kuextern = kumo %>%
    left_join(mo, by ="modulid") %>%
    group_by(kursid) %>%
    summarize(
      extern = any(!extern),
      ects = median(ects),
      num.modul = n()
    )

  ku = left_join(ku, kuextern, by="kursid")

  ku.info = nlist(ku, kupe, kuzu, kusp)

  saveRDS(ku.info,"stuko_kurse.Rds")
}
