# Functions for one time adaptions
#

check.duplicate.module = function() {
  setwd("C:/libraries/stuko/")
  db = get.stukodb("C:/libraries/stuko/ulm/db")


  mo = dbGet(db,"modul")
  mozu = dbGet(db, "modulzuordnung")
  mosp = dbGet(db, "modulschwerpunkt")
  most = dbGet(db, "modulstudiengang")



  mo = mo %>%
    group_by(modulid, semester) %>%
    arrange(modify_time) %>%
    mutate(keep.modul = 1:n() == n()) %>%
    filter(keep.modul)

  mozu = unique(mozu)
  most = unique(most)
  mosp = unique(mosp)

  dbWithTransaction(db,{
    dbDelete(db, "modul", list())
    dbDelete(db, "modulschwerpunkt", list())
    dbDelete(db, "modulstudiengang", list())
    dbDelete(db, "modulzuordnung", list())

    dbInsert(db, "modul", mo)
    dbInsert(db, "modulschwerpunkt", mosp)
    dbInsert(db, "modulstudiengang", most)
    dbInsert(db, "modulzuordnung", mozu)
  })

  mozu %>%
    group_by(modulid, semester, zuordnung) %>%
    summarize(
      count = n()
    ) %>%
    filter(count > 1)


  mo %>%
    group_by(modulid, semester, titel) %>%
    summarize(
      count = n()
    ) %>%
    filter(count > 1)

  ku = dbGet(db,"kurs")
  ku = ku %>%
    group_by(kursid, semester) %>%
    arrange(modify_time) %>%
    mutate(keep.kurs = 1:n() == n()) %>%
    filter(keep.kurs)


}


temp.update.kurs = function() {
  #dbmisc::dbCreateSQLiteFromSchema(system.file("schema/stukodb.yaml", package="stuko"),db.name = "stukodb.sqlite",db.dir = "D:/libraries/stuko/ulm/db",update = TRUE)


  setwd("D:/libraries/stuko/")
  db = get.stukodb("D:/libraries/stuko/ulm/db")
  dbUpdate(db,"kurs",vals=list(offen="o", findet_statt="s"))
  dbUpdate(db,"kurs",vals=list(offen="g"), where = list(kursform="se"))

  sems = c(170,175,180,185,190,195)

  sem = 185
  for (sem in sems) {
    #restore.point("idfjdfh")
    sd = get.sem.data(sem)
    kurse = sd$kurse
    if (NROW(kurse)==0) next

    for (i in seq_len(NROW(kurse))) {
      kursid = kurse$kursid[i]
      pf = kurse$pruefungsform[i]
      if (pf == "") pf = "-"
      dbUpdate(db,"kurs",vals=list(pruefung=pf), where = list(kursid=kursid, semester=sem))
    }
  }

  sem = 180
  ku = dbGet(db, "kurs")

}
