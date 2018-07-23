examples.import.db = function() {
  setwd("D:/libraries/stuko/")


  create.stukodb("D:/libraries/stuko/ulm/db")
  db = get.stukodb()
  #fill.stukodb.from.csv(db=db)
  #copy.modules.to.semester(175, 160)
  #copy.modules.to.semester(175, 170)

  #setwd("D:/libraries/stuko/ulm/db")
  #db = get.stukodb()
  #person = dbGet(db,"person")
  #write_csv(person,"D:/libraries/stuko/person_db.csv")
  #
  copy.modules.to.semester(src.sem = 180, dest.sem = 185)
}

get.stukodb = function(db.dir=getwd(), db.name="stukodb.sqlite", app=getApp(), schemas=stukodb.schemas()) {
  restore.point("get.stukodb")
  db = app$glob$db

  if (is.null(db)) db = getOption("stuko.db.connection")

  if (!is.null(db)) {
    if (!dbIsValid(db)) db = NULL
  }

  if (is.null(db)) {
    db = dbConnect(RSQLite::SQLite(),file.path(db.dir, db.name))
    db = set.db.schemas(db, schemas)
    options(stuko.db.connection = db)
  }
  db
}

stukodb.schemas = function() {
  schema.file = system.file("schema/stukodb.yaml",package = "stuko")
  schemas = dbmisc::load.and.init.schemas(schema.file)
  schemas
}

create.stukodb = function(db.dir=getwd(), db.name="stukodb.sqlite") {
  schema.file = system.file("schema/stukodb.yaml",package = "stuko")
  schemas = dbmisc::load.and.init.schemas(schema.file)
  dbmisc::dbCreateSQLiteFromSchema(schema.file,db.name = db.name, db.dir=db.dir,update = TRUE)
  #write.stuko.log("Empty stukodb created","new")
}

fill.stukodb.from.csv = function(db=get.stukodb(), schemas=stukodb.schemas(), csv.dir=getwd(), modify_user="_from_csv",modify_time=Sys.time()) {
  restore.point("fill.stukodb.from.csv")
  tables = names(schemas)

  table = "modul"
  for (table in tables) {
    cat("\n\n**********************************\nLoad and insert", table)
    file = paste0(table,"_db.csv")
    if (file.exists(file)) {
      dbDelete(db,table,params = NULL)
      dat = read_csv(file)
      dat$modify_user = modify_user; dat$modify_time=modify_time
      dbmisc::dbInsert(db, table, dat, schemas=schemas)
    }
  }
  write.stuko.log("Import all tables from csv","csv_all", db=db)
}

copy.modules.to.semester = function(src.sem, dest.sem, db=get.stukodb(), schemas = stukodb.schemas(), delete.old = TRUE, modify_user = paste0("_from_sem_",src.sem), modify_time=Sys.time()) {
  restore.point("copy.modules.to.semester")
  tables = c("modul","modulzuordnung","modulschwerpunkt","modulstudiengang")
  for (table in tables) {
    cat("\nCopy ", table, " from ", src.sem , " to ", dest.sem)
    dat = dbGet(db,table, list(semester = src.sem), schemas=schemas)
    dat$semester = dest.sem
    if (delete.old)
      dbDelete(db, table, list(semester = dest.sem))
    dat$modify_user = modify_user
    dat$modify_time = Sys.time()
    try(dbmisc::dbInsert(db, table,dat, schemas=schemas))
  }
  write.stuko.log(paste0("Copy modules from semester ", src.sem ," to ", dest.sem),"csv_all", db=db)
}


write.stuko.log = function(logtext,logtype, logtime=Sys.time(), userid=first.non.null(app$userid,"unknown"),db=get.stukodb(), app=getApp()) {
  restore.point("write.stuko.log")
  log = list(logtime=logtime, userid=userid,logtype=logtype, logtext=logtext)
  dbInsert(db,"log", log)
}

delete.stuko.duplicates = function(db = get.stukodb()) {
  ku = dbGet(db,"kurs")
  dupl = duplicated(select(ku,kursid, semester))
  ku = ku[!dupl,]

  kupe = dbGet(db,"kursperson")
  dupl = duplicated(kupe)
  kupe = kupe[!dupl,]

  kumo = dbGet(db,"kursmodul")
  dupl = duplicated(kumo)
  kumo = kumo[!dupl,]


  dbWithTransaction(db,{
    dbDelete(db,"kurs", list())
    dbDelete(db,"kursperson", list())
    dbDelete(db,"kursmodul", list())

    dbInsert(db,"kurs", ku)
    dbInsert(db,"kursperson", kupe)
    dbInsert(db,"kursmodul", kumo)

  })

  #mo = dbGet(db,"modul")
  #dupl = duplicated(select(mo,modulid, semester))
  #mo = mo[!dupl,]

}

examples.fill.stukodb.kurse.from.csv = function() {
  fill.stukodb.kurse.from.csv(src.sem=165, dest.sem=185, csv.dir = "D:/libraries/stuko")
}

fill.stukodb.kurse.from.csv = function(db=get.stukodb(), src.sem=165, dest.sem=185, schemas=stukodb.schemas(), csv.dir=getwd(), modify_user="_from_csv",modify_time=Sys.time()) {
  restore.point("fill.stukodb.kurse.from.csv")
  tables = c("kurs","kursperson","kursmodul")

  table = tables[1]
  for (table in tables) {
    cat("\n\n**********************************\nLoad and insert", table)
    file = file.path(csv.dir,paste0(table,"_db.csv"))
    if (file.exists(file)) {
      dat = read_csv(file)
      dat = filter(dat,semester %in% src.sem)
      dat$semester = dest.sem
      dat$modify_user = modify_user; dat$modify_time=modify_time
      #dbDelete(db,table,params = NULL)
      dbmisc::dbInsert(db, table, dat, schemas=schemas)
    }
  }
  write.stuko.log("Import kurs tables from csv","csv_all", db=db)
}
