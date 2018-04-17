# Functions for one time adaptions
#


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
