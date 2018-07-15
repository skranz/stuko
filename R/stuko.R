stuko.examples = function() {


  main.dir = "D:/libraries/stuko/ulm"
  schema.dir = file.path(find.package("stuko"),"inst","schema")
  db.dir = file.path(main.dir, "db")

  dbmisc::dbCreateSQLiteFromSchema(schema.file = "stukodb.yaml",schema.dir = schema.dir, db.dir=db.dir, update=TRUE)
}
