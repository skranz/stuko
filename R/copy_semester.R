examples.copy.semester = function() {
  setwd("D:/libraries/stuko/")
  #create.stukodb()
  db = get.stukodb("D:/libraries/stuko/")
  #fill.stukodb.from.csv(db=db)
  copy.kurse.to.semester(db, source.sem=170, dest.sem=180)
  copy.kurse.to.semester(db, source.sem=160, dest.sem=180)
  delete.semester.kurse(db, 155)
  delete.semester.kurse(db, 160)
  delete.semester.kurse(db, 165)
  delete.semester.kurse(db, 170)
  delete.semester.kurse(db, 175)

}

copy.kurse.to.semester = function(db = get.stukodb(),source.sem, dest.sem, overwrite=FALSE) {
  restore.point("copy.kurse.to.semester")

  #stop()
  ku = dbGet(db,"kurs", list(semester=source.sem))
  kumo = dbGet(db,"kursmodul", list(semester=source.sem))
  kupe = dbGet(db,"kursperson", list(semester=source.sem))

  if (!overwrite) {
    eku = dbGet(db,"kurs", list(semester=dest.sem))
    eids = eku$kursid
    ku = filter(ku, !kursid %in% eids)
    kumo = filter(kumo, !kursid %in% eids)
    kupe = filter(kupe, !kursid %in% eids)
  }
  ku$semester = dest.sem
  kupe$semester = dest.sem
  kumo$semester = dest.sem


  dbWithTransaction(db,{
    dbInsert(db,"kurs", ku)
    dbInsert(db,"kursperson", kupe)
    dbInsert(db,"kursmodul", kumo)
  })

}

delete.semester.kurse = function(db=get.stukodb(), semester) {
  dbWithTransaction(db,{
    dbDelete(db,"kurs", list(semester=semester))
    dbDelete(db,"kursperson", list(semester=semester))
    dbDelete(db,"kursmodul", list(semester=semester))
  })
}