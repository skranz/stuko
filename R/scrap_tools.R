amatch.with.rel.dist = function(words, keys,  method="qgram") {
  restore.point("amatch.with.rel.dist")
  df = unify.words.table(words, keys, method)
  nc = pmax(nchar(df$word), nchar(df$key))
  df$dist = df$dist / nc
  df

}

unify.words.table = function(words, keys, method="qgram", file=NULL) {
  restore.point("unify.words.table")

  library(stringdist)
  ind = amatch(words, keys, maxDist = Inf, method=method)
  key = keys[ind]
  df = data_frame(word = words,key=key, dist=stringdist(words,key,method=method))

  if (!is.null(file)) {
    readr::write_csv(df, file)
  }
  df
}

match.to.table = function(x, tab, val.col=1, key.col=2) {
  val.ind = match(x, tab[[val.col]])
  tab[[key.col]][val.ind]
}


differing.df.group.rows = function(df, group_var="code", ignore.cols=c()) {
  restore.point("differeing.df.groups")
  # Find Module whose Modulbeschreibung differs
  # for different subjects
  differ = function(x) n_distinct(x) > 1

  ddf = df %>%
    group_by_(group_var) %>%
    summarise_all(differ)

  fields = unique(c(group_var, ignore.cols))

  ddf = ddf[,setdiff(colnames(ddf,fields))]
  differ.mat = as.matrix(ddf)
  differ = rowSums(differ.mat>0)
  which(differ)

}


differing.cols = function(df, collapse=NULL) {
  differs = sapply(df, function(vals) {
    dplyr::n_distinct(vals) != length(vals)
  })
  if (is.null(collapse)) {
    names(df)[differs]
  } else {
    paste0(names(df)[differs], collapse=",")
  }
}

string.approx.pair.match = function(v1,v2, method="qgram", return.index=FALSE,...) {
  if (length(v1)==0 | length(v2)==0) {
    if (return.index) return(list(i1=seq_along(v1), i2=seq_along(v2)))
    return(list(v1=v1, v2=v2))

  }

  old1 = 1:length(v1)
  old2 = 1:length(v2)
  new1 = NULL
  new2 = NULL

  mat = stringdistmatrix(v1,v2,method = method)
  while(NROW(mat)*NCOL(mat)>0) {
    cl = arrayInd(which.min(mat), dim(mat))
    new1 = c(new1,old1[cl[1]])
    new2 = c(new2,old2[cl[2]])
    old1 = old1[-cl[1]]
    old2 = old2[-cl[2]]

    mat = mat[-cl[1],-cl[2], drop=FALSE]
  }
  if (return.index) {
    return(list(i1=c(new1, old1), i2=c(new2, old2)))
  }

  res1 = v1[c(new1, old1)]
  res2 = v2[c(new2, old2)]
  list(v1=res1, v2=res2)
}
