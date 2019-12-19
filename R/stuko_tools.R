# Default current semester
default_semester = function(date = Sys.Date()) {
  ye = as.integer(format(date, "%y"))
  month = lubridate::month(date)

  # Next summer term
  if (month >= 11) {
    ye = ye+1
    sem = 0
  } else if (month <= 4) {
    sem = 0
  # Winter term
  } else {
    sem = 5
  }
  ye*10+sem
}

monatsname = function(monat) {
  na = c("Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember")
  na[as.integer(monat)]
}

lang_datum = function(date=Sys.Date()) {
  mon = format(date, "%m")
  jahr = format(date, "%Y")
  tag = format(date, "%d")

  paste0(tag,". ", monatsname(mon), " ", jahr)
}

semester_name = function(semester, kurz=TRUE) {
  sose = semester %% 10 == 0
  jahr = substring(semester, 1,2)

  if (kurz & sose) {
    sem = "SoSe"
  } else if (kurz) {
    sem = "WiSe"
    jahr = paste0(jahr, "/", as.integer(jahr)+1)
    return(paste0(sem,jahr))
  } else if (!kurz & sose) {
    sem = "Sommersemester"
    jahr = paste0("20",jahr)
  } else {
    sem = "Wintersemester"
    jahr = paste0("20",jahr,"/",as.integer(jahr)+1)
  }
  paste0(sem," ", jahr)

}


knapp_semester_name = function(semester) {
  sose = semester %% 10 == 0
  jahr = substring(semester, 1,2)

  if (sose) {
    sem = "SS"
  } else {
    sem = "WS"
    jahr = paste0(jahr, "_", as.integer(jahr)+1)
    return(paste0(sem,jahr))
  }
  paste0(sem, jahr)
}



tables.diff = function(df1, df2, by=colnames(df1)) {
  restore.point("tables.diff")
  if (is.null(df1) & is.null(df2)) {
    return(list(same=NULL, added=NULL, removed=NULL))
  } else if (is.null(df1)) {
    return(list(same=NULL, added=NULL, removed=df2))
  } else if (is.null(df2)) {
    return(list(same=NULL, added=df1, removed=NULL))
  }
  key1 = do.call(paste, df1[by])
  key2 = do.call(paste, df2[by])
  same = df1[key1 %in% intersect(key1,key2),, drop=FALSE]
  removed = df2[!key2 %in% key1,,drop=FALSE]
  added = df1[!key1 %in% key2,,drop=FALSE]
  nlist(same, added, removed)
}

