refine.scraped.modules = function() {
  setwd("D:/libraries/stuko")

  mo = read_csv("modules_with_extern.csv", col_types=cols(code=col_character()))

  sp = read_csv("sprache.csv")
  mo$sprache_org = mo$sprache
  mo$sprache = match.to.table(mo$sprache, sp)


  # Find Module whose Modulbeschreibung differs
  # for different subjects
  differ = function(x) n_distinct(x) > 1

  mo$code_kurz = ifelse(nchar(mo$code)>5,substring(mo$code, 6), mo$code)
  mos = mo %>% group_by(code_kurz) %>%
    summarise_all(differ) %>%
    select(-code, -id, -handbuch)

  mo.mat = as.matrix(mos[,-1])
  any.differ = rowSums(mo.mat)>1
  dmo = mos[any.differ,]

  write_csv()
  mo


  if (FALSE) {
    sprache = unique(mo$sprache)
    df = unify.words.table(sprache, c("-", "Deutsch","Englisch", "Deutsch oder English"), file="sprache.csv")
  }



}

make.module.with.extern.template = function() {
  setwd("D:/libraries/stuko")
  mo = readr::read_csv("module.csv", col_types=cols(code=col_character()))

  mo = arrange(mo,zuordnung, modulkoordinator)
  mo$extern = 0
  mo = select(mo, titel,zuordnung, extern, everything())
  write_excel_csv(mo,"modules_with_extern.csv")

}

examples.scrap_all.modulhandbuch = function() {
  setwd("D:/libraries/stuko/module")
  dat = scrap_all_modulhandbuch()
  readr::write_csv(dat, "D:/libraries/stuko/module.csv")

}

scrap_all_modulhandbuch = function(dir=getwd()) {
  setwd(dir)

  pdf.files = list.files(dir, glob2rx("*.pdf"))

  li = lapply(pdf.files, function(file) {
    basename = tools::file_path_sans_ext(file)
    scrap_modulhandbuch(basename)
  })
  bind_rows(li)
}

scrap_modulhandbuch = function(basename,overwrite.txt = FALSE) {
  restore.point("scrap_modulhandbuch")
  library(pdftools)

  handbuch = str.between(basename,"-","-")
  is.nuf = str.starts.with(handbuch,"NUF")

  cat("\nscrap ", basename,"...")
  if (!file.exists(paste0(basename,".txt")) | overwrite.txt) {
    library(pdftools)
    txt = pdf_text(paste0(basename,".pdf"))
    writeUtf8(merge.lines(txt), paste0(basename,".txt"))
  }
  txt = readUtf8(paste0(basename,".txt"))

  if (!is.nuf) {
    ignore =
      str.starts.with(txt, "Bachelor of Science Wirtschaftswissenschaften Druckdatum:") |
      str.starts.with(txt, "Master of Science Wirtschaftswissenschaften")
  } else {
    ignore =
      str.starts.with(txt,"Master of Science Nachhaltige") |
      (str.starts.with(lag(txt),"Master of Science Nachhaltige") &
      str.starts.with(txt,"Unternehmensführung"))
  }


  txt = txt[!ignore]
  #line.dist = if (is.nuf) {1} else {2}
  line.dist = 1
  start.lines = which(str.starts.with(txt, "Modul zugeordnet zu"))-line.dist
  end.lines = c(start.lines[-1]-1,length(txt))

  txt = scraped.modules.normalize.title.lines(txt, start.lines,line.dist=line.dist)

  mod.num = 2
  li = lapply(seq_along(start.lines), function(mod.num) {
    scrap.module.info(txt[start.lines[mod.num]:end.lines[mod.num]], line.dist=line.dist)
  })

  li[[1]]
  names = sapply(li, function(el) el$Title)
  names(li) = names

  df = bind_rows(li)

  df = clean.scraped.module.info.df(df)

  df$handbuch = handbuch
  colnames(df) = tolower(colnames(df))

  readr::write_csv(df, paste0(basename,".csv"))
  invisible(df)
}

scraped.modules.normalize.title.lines = function(txt, start.lines, line.dist=2) {
  restore.point("scraped.modules.normalize.title.lines")

  # Deal with titles over three lines
  tripple = str.trim(txt[start.lines-line.dist]) != "" & str.trim(txt[start.lines-2*line.dist]) != ""

  rows = start.lines[tripple]
  txt[rows] = paste0(str.trim(txt[rows-2*line.dist])," ",str.trim(txt[rows-line.dist])," ", str.trim(txt[rows]))
  txt[rows-line.dist] = ""
  txt[rows-2*line.dist] = ""

  # Deal with titles over two lines
  double = str.trim(txt[start.lines-line.dist]) != ""

  rows = start.lines[double]
  txt[rows] = paste0(str.trim(txt[rows-line.dist])," ", str.trim(txt[rows]))
  txt[rows-line.dist] = ""

  txt
}

clean.scraped.module.info.df = function(df) {
  restore.point("clean.scraped.module.info.df")

  df$id = paste0(df$Code, ": ", df$Titel)

  # Verify that only "Zuordnung" is duplicated
  if (FALSE) {
    not.dupl = function(d) {
      if (NROW(d)==1) return(data_frame(cols=""))
      ind = sapply(d, function(vals) {
        any(duplicated(vals))
      })
      data_frame(cols=paste0(colnames(d)[!ind],collapse=", "))
    }
    nd = dat %>% group_by(id) %>%
      do(not.dupl(.)) %>%
      filter(cols != "")
    which(nd != "Zur")
    unique(nd$cols)

  }


  # Remove rows that only differ by Zuordnung
  # but store all Zuordnungen
  # comma separated
  mypaste = function(vec) {
    paste(unique(vec), collapse=", ")
  }

  dat = df %>%
    arrange(Zuordnung) %>%
    group_by(id) %>%
    mutate(Zuordnung = mypaste(Zuordnung)) %>%
    ungroup()
  dat = dat[!duplicated(dat),]


  if ("Studiengänge" %in% colnames(df)) {
    df$Studiengang = paste0(df$Studiengang," ", df[["Studiengänge"]])
    df = df[,setdiff(colnames(df,"Studiengänge"))]
  }

  # Rename columns
  #cat(paste0('"',colnames(dat),'"', collapse=", "))
  colnames(dat) = c(
    "Titel", "Zuordnung", "Code", "ECTS", "SWS", "Sprache", "Dauer", "Turnus", "Modulkoordinator", "Dozent", "Studiengang", "Vorkenntnisse", "Lernergebnisse", "Inhalt", "Literatur", "Lehrform", "Arbeitsaufwand", "Bewertungsmethode", "Notenbildung", "Grundlage_Fuer", "id"
  )

  dat = dat %>% arrange(Modulkoordinator, Dozent, Titel)

  dat
}

scrap.module.info = function(str, line.dist=2) {
  restore.point("scrap.module.info")

  Titel = str[1]

  if (Titel=="Bachelor") stop()

  row = 1 + line.dist
  Zuordnung = str.right.of(str[row],"Modul zugeordnet zu ")

  str = str[-(1:row)]
  str = str[str.trim(str)!=""]

  row = which(str.starts.with(str,"Einordnung in die"))
  str[row] = paste0("Studiengang",str.right.of(str[row],"Einordnung in die"))
  str[row+1] = str.right.of(str[row+1],"Studiengänge")
  str[row+2] = str.right.of(str[row+2],"Studiengänge",not.found = str[row+2])

  row = which(str.starts.with(str,"Lehr- und"))
  str[row] = paste0("Lehrform",str.right.of(str[row],"Lehr- und"))
  str[row+1] = str.right.of(str[row+1],"Lernformen")
  #str[row+2] = str.right.of(str[row+2],"Lernformen")


  key.rows = which(!str.starts.with(str, " ") & !str.starts.with(str,"\t") & !str.trim(str) == "")

  str[key.rows]
  keys = str.left.of(str[key.rows],"  ")
  row = str.starts.with(keys,"Bewertungsmethode")
  keys[row] = "Bewertungsmethode"


  start.lines = key.rows
  end.lines = c(start.lines[-1]-1, length(str))

  li = lapply(seq_along(key.rows), function(i) {
    s = merge.lines(str.trim(str[start.lines[i]:end.lines[i]]))
    s = str.trim(str.right.of(s, keys[i]))
    s
  })
  names(li) = keys

  if ("Studieng?nge" %in% keys)
    stop("Have Studieng?nge")

  restore.point("sfhdfhidjfojdif")
  li = c(nlist(Titel, Zuordnung), li)
  li
}

make.seminar.kurse = function(semester) {
  setwd("D:/libraries/stuko")

  db = get.stukodb()

  semester = 185
  mod = read_csv("modules_with_extern.csv", col_types=cols(code=col_character()))
  mod = filter(mod, zuordnung=="Seminare")

  mo = read_csv("modul_db.csv")
  pe = read_csv("person_db.csv")

  ku = NULL
  kupe = NULL
  kumo = NULL
  modify_time = Sys.time()
  modify_user = "auto"
  row = 1
  for (row in seq_len(NROW(mod))) {
    m = mod[row,]
    this.code = ifelse(nchar(m$code)>5,substring(m$code, 6), m$code)


    k = list(
      kursid = paste0("sem_", this.code),
      semester = semester,
      modify_time = modify_time,
      modify_user = modify_user,
      aktiv = FALSE,
      vnum = "",
      kursname = m$titel,
      sws_kurs = 2,
      sws_uebung = 0,
      kursform = "se",
      zeitform = "b",
      sprache = cld2::detect_language(m$titel),
      turnus = 2,
      zukunft_sem = semester + 10,
      zukunft_sem2 = semester + 20,
      codeshare = "",
      kommentar = ""
    )
    ku = rbind(ku, as_data_frame(k))

    pe.ind = pe$nachname %in% m$modulkoordinator
    if (sum(pe.ind)>0) {
      pe.ind = which(pe.ind)[1]
      pers = pe[pe.ind]
      kp = list(
        kursid = k$kursid,
        semester = semester,
        person = pers$personid,
        nachname = pers$nachname,
        vorname = pers$vorname,
        rolle = "dk",
        lehrauftrag = "-",
        dozent_sws = 0
      )
      kupe = rbind(kupe, as_data_frame(kp))
    }

    this.mo = filter(mo, code == this.code)
    km = list(
      kursid = k$kursid,
      semester = semester,
      modulid = this.mo$modulid[[1]]
    )

    kumo = rbind(kumo, as_data_frame(km))
  }
  ku$sprache[is.na(ku$sprache)] = "-"

  DBI::dbWithTransaction(db,{
    dbInsert(db, "kurs",ku)
    dbInsert(db, "kursperson",kupe)
    dbInsert(db, "kursmodul",kumo)

  })

}
