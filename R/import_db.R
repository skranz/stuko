examples.import.db = function() {
  setwd("D:/libraries/stuko/")
  #create.stukodb()
  db = dbConnect(RSQLite::SQLite(),"stukodb.sqlite")
  fill.stukodb.from.csv(db=db)
}

get.stukodb = function(db.dir=getwd(), db.name="stukodb.sqlite") {
  db = getOption("stuko.db.connection")

  if (!is.null(db)) {
    if (!dbIsValid(db)) db = NULL
  }

  if (is.null(db)) {
    db = dbConnect(RSQLite::SQLite(),file.path(db.dir, db.name))
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
  dbmisc::dbCreateSQLiteFromSchema(schema.file,db.name = db.name, db.dir=db.dir)

}

fill.stukodb.from.csv = function(db=get.stukodb(), schemas=stukodb.schemas(), csv.dir=getwd()) {
  restore.point("fill.stukodb.from.csv")
  tables = names(schemas)

  table = "modul"
  for (table in tables) {
    cat("\n\n**********************************\nLoad and insert", table)
    file = paste0(table,"_db.csv")
    if (file.exists(file)) {
      dbDelete(db,table,params = NULL)
      dat = read_csv(file)
      dbmisc::dbInsert(db, table, dat, schemas=schemas)
    }
  }

}

copy.modules.to.semester = function(src.sem, dest.sem, db=get.stukodb(), schemas = stukodb.schemas(), delete.old = TRUE) {
  restore.point("copy.modules.to.semester")
  tables = c("modul","modulzuordnung","modulschwerpunkt","modulstudiengang")
  for (table in tables) {
    cat("\nCopy ", table, " from ", src.sem , " to ", dest.sem)
    dat = dbGet(db,table, list(semester = src.sem), schemas=schemas)
    dat$semester = dest.sem
    if (delete.old)
      dbDelete(db, table, list(semester = dest.sem))
    try(dbmisc::dbInsert(db, table,dat, schemas=schemas))
  }
}
