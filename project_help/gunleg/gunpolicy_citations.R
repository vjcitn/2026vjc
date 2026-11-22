# Linking in-table literature citations (e.g. t181's "Lott and Mustard (1997)") to the
# report's own reference list, and exporting bibliographic data as BibTeX / CSL-JSON.
#
# Facts about this PDF that the code relies on (checked against the file):
#  * There is no master bibliography: each chapter ends with "Chapter N References"
#    (Chapter 18, concealed-carry, is on PDF pages 426-434; CHAPTER 19 starts on 435).
#  * Entries are RAND style, NOT author-year:
#      Last, First, and First Last, "Title," Journal, Vol. 26, No. 1, January 1997.
#      Last, First, Book Title: Subtitle, 2nd ed., Publisher, 2000.
#    Same-author/same-year disambiguation letters appear in the list itself
#    ("... January 1998a.", "... University of Chicago Press, 1998b."), so
#    "Lott (1998a)" in a table maps to exactly one entry.
#  * No entry carries a DOI.  DOIs can be looked up from Crossref (section 6, needs network).
#  * There is no hanging indent: continuation lines start flush left, so entry boundaries
#    are recovered from punctuation (an entry ends with a period; the next starts with a capital).
#
# install.packages(c("pdftools", "jsonlite"))   # httr2 only for section 6
library(pdftools)

pdf_file <- "41c0d002-scienceOfGunPolicy.pdf"          # adjust path

# t181 comes from gunpolicy_tables_pdftools.R; otherwise read the CSV saved from it
if (!exists("t181")) t181 <- read.csv("table_18_1.csv", stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# 1. Pull one chapter's reference list out of the PDF as clean lines
# ---------------------------------------------------------------------------
chapter_ref_pages <- function(chapter) {
  txt <- pdf_text(pdf_file)
  start <- which(vapply(txt, function(p) any(grepl(sprintf("^\\s*Chapter %d References\\s*$", chapter),
                                                  strsplit(p, "\n")[[1]])), NA, USE.NAMES = FALSE))[1]
  end <- which(vapply(txt, function(p) any(grepl(sprintf("^\\s*CHAPTER %d\\s*$", chapter + 1),
                                                strsplit(p, "\n")[[1]])), NA, USE.NAMES = FALSE))
  end <- end[end > start][1]
  c(start = start, end = end)
}

ref_lines <- function(chapter, chapter_title) {
  pg <- chapter_ref_pages(chapter)
  txt <- pdf_text(pdf_file)[pg["start"]:pg["end"]]
  out <- character(0); started <- FALSE
  for (page in txt) for (l in trimws(strsplit(page, "\n")[[1]])) {
    if (!nzchar(l)) next
    if (grepl(sprintf("^Chapter %d References$", chapter), l)) { started <- TRUE; out <- character(0); next }
    if (!started) next
    if (grepl(sprintf("^CHAPTER %d$", chapter + 1), l)) return(out)       # next chapter begins
    if (grepl("^The Science of Gun Policy", l) || l == chapter_title ||    # running heads
        grepl("^[0-9]+$", l)) next                                          # page numbers
    out <- c(out, l)
  }
  out
}

# An entry is complete when its last line ends with "." (but not an initial like "D.") or is a URL
split_entries <- function(lines) {
  ents <- list(); cur <- character(0)
  for (l in lines) {
    prev   <- if (length(cur)) cur[length(cur)] else ""
    closed <- (endsWith(prev, ".") && !grepl("(^|\\s)[A-Z]\\.$", prev)) || grepl("^https?://\\S+$", prev)
    if (length(cur) && closed && grepl("^[A-Z]", l)) { ents[[length(ents) + 1]] <- cur; cur <- character(0) }
    cur <- c(cur, l)
  }
  if (length(cur)) ents[[length(ents) + 1]] <- cur
  vapply(ents, function(e) {                       # re-join; undo end-of-line hyphenation
    s <- e[1]
    for (l in e[-1]) s <- if (endsWith(s, "-")) paste0(s, l) else paste(s, l)
    s
  }, "")
}

# ---------------------------------------------------------------------------
# 2. Parse one entry into fields
# ---------------------------------------------------------------------------
SUFFIX <- "(Jr\\.|Sr\\.|II|III|IV)"
MONTHS <- "January|February|March|April|May|June|July|August|September|October|November|December"

# "Lott, John R., Jr., and D. B. Mustard"  ->  family / given / suffix, one row per author
parse_names <- function(seg) {
  parts <- trimws(strsplit(gsub("\\s+", " ", sub(",\\s*$", "", trimws(seg))), ",")[[1]])
  parts <- parts[nzchar(parts)]
  if (length(parts) == 1) return(data.frame(family = parts, given = "", suffix = "", stringsAsFactors = FALSE))
  fam <- parts[1]; giv <- parts[2]; suf <- ""; i <- 3
  if (i <= length(parts) && grepl(paste0("^", SUFFIX, "$"), parts[i])) { suf <- parts[i]; i <- i + 1 }
  while (i <= length(parts)) {                      # later authors are "First Last [Suffix]"
    p <- sub("^and\\s+", "", parts[i]); i <- i + 1; s <- ""
    if (i <= length(parts) && grepl(paste0("^", SUFFIX, "$"), parts[i])) { s <- parts[i]; i <- i + 1 }
    m <- regmatches(p, regexec(paste0("^(.*)\\s+", SUFFIX, "$"), p))[[1]]
    if (length(m)) { p <- m[2]; s <- m[3] }
    tok <- strsplit(p, " ")[[1]]
    fam <- c(fam, tok[length(tok)]); giv <- c(giv, paste(tok[-length(tok)], collapse = " ")); suf <- c(suf, s)
  }
  data.frame(family = fam, given = giv, suffix = suf, stringsAsFactors = FALSE)
}

# Unquoted titles (books, reports): the title runs through the first comma-part containing ":"
# (the subtitle), else is the first part.  Remaining parts: edition, publisher/institution, series.
split_title <- function(rest) {
  rest  <- sub("\\s*(As of |https?://).*$", "", rest)
  parts <- trimws(strsplit(rest, ",")[[1]])
  j <- which(grepl(":", parts))[1]; if (is.na(j)) j <- 1
  title <- paste(parts[seq_len(j)], collapse = ", ")
  after <- if (j < length(parts)) parts[(j + 1):length(parts)] else character(0)
  ed    <- after[grepl("^[0-9]+(st|nd|rd|th) ed\\.?$", after)]
  after <- after[!after %in% ed & !grepl("^(19|20)[0-9]{2}[a-z]?\\.?$", after)]
  after <- sub("\\.$", "", after)
  k <- which(grepl("^(Inc|Ltd|LLC)$", after)); for (i in rev(k)) {            # "Regnery Publishing, Inc."
    after[i - 1] <- paste(after[i - 1], after[i], sep = ", "); after <- after[-i] }
  list(title = title, edition = sub("\\.$", "", ed[1]), after = after)
}

parse_entry <- function(e) {
  r <- list(raw = e, type = "misc", title = NA, container = NA, volume = NA, number = NA,
            month = NA, publisher = NA, institution = NA, edition = NA, note = NA, url = NA,
            doi = NA, year = "", suffix = "")
  r$url <- regmatches(e, regexpr("https?://\\S+", e)); if (!length(r$url)) r$url <- NA
  asof  <- regmatches(e, regexpr("As of [^:]+", e));   if (length(asof)) r$note <- asof

  q <- regmatches(e, regexec("^(.*?),?\\s+“(.+?)(?:,”|”,?|\\.”)\\s*(.*)$", e, perl = TRUE))[[1]]
  if (length(q)) {                                  # --- quoted title: article / chapter / webpage
    auth <- q[2]; r$title <- sub(",$", "", q[3]); rest <- q[4]
    r$authors <- if (grepl(",", auth)) parse_names(auth) else
      data.frame(family = auth, given = "", suffix = "", stringsAsFactors = FALSE)
    if (grepl("^in\\s", rest)) {                    # chapter in an edited volume
      r$type <- "incollection"
      st <- split_title(sub("^in .*?eds?\\.,\\s*", "", rest))
      r$container <- st$title; r$publisher <- st$after[1]
    } else {
      mj <- regmatches(rest, regexec(paste0("^(.*?),\\s*(?:Vol\\.|No\\.|(?:", MONTHS, ")\\b|(?:19|20)[0-9]{2})"),
                                     rest, perl = TRUE))[[1]]
      if (length(mj) && !grepl("webpage|working paper|report", mj[2], ignore.case = TRUE)) {
        r$type <- "article"; r$container <- mj[2]
      }
    }
  } else {                                          # --- unquoted: book / report
    parts <- trimws(strsplit(e, ",")[[1]])
    k <- which(startsWith(parts, "and "))[1]
    if (!is.na(k)) {
      auth <- paste(parts[1:k], collapse = ", "); rest <- paste(parts[-(1:k)], collapse = ", ")
      r$authors <- parse_names(auth)
    } else if (length(parts) > 2 && grepl("^[A-Z][a-z]+( [A-Z]\\.?| [A-Z][a-z]+)*$", parts[2]) &&
               length(strsplit(parts[2], " ")[[1]]) <= 3 && !grepl("Press|Council|Corporation", parts[1])) {
      n <- if (grepl(paste0("^", SUFFIX, "$"), parts[3])) 3 else 2
      auth <- paste(parts[1:n], collapse = ", "); rest <- paste(parts[-(1:n)], collapse = ", ")
      r$authors <- parse_names(auth)
    } else {                                        # corporate author, e.g. "National Research Council"
      auth <- parts[1]; rest <- paste(parts[-1], collapse = ", ")
      r$authors <- data.frame(family = auth, given = "", suffix = "", stringsAsFactors = FALSE)
    }
    st <- split_title(rest); r$title <- st$title; r$edition <- st$edition
    if (grepl("Press|Publishing|Publishers", rest)) { r$type <- "book"; r$publisher <- st$after[1] }
    else if (grepl("[Ww]orking [Pp]aper|Institute|Center|Corporation|Law School|University", rest)) {
      r$type <- "techreport"; r$institution <- st$after[1]
      extra <- st$after[-1]; extra <- extra[!grepl(paste0("^(", MONTHS, ")"), extra)]
      if (length(extra)) r$note <- paste(c(r$note[!is.na(r$note)], extra), collapse = "; ")
    }
  }
  yr_src <- sub("\\s*(As of |https?://).*$", "", rest)
  ym <- regmatches(yr_src, gregexpr("(?:19|20)[0-9]{2}[a-z]?\\b", yr_src, perl = TRUE))[[1]]
  if (length(ym)) { y <- ym[length(ym)]; r$year <- substr(y, 1, 4); r$suffix <- substring(y, 5) }
  mv <- regmatches(e, regexec("Vol\\.\\s*([^,]+)", e))[[1]]; if (length(mv)) r$volume <- trimws(mv[2])
  mn <- regmatches(e, regexec("No\\.\\s*([^,]+)", e))[[1]];  if (length(mn) && r$type == "article") r$number <- trimws(mn[2])
  mm <- regmatches(rest, regexpr(MONTHS, rest));            if (r$type == "article" && length(mm)) r$month <- mm
  r
}

# Citation key = surname(s) + year + disambiguation letter: LottMustard1997, Lott1998a, MoodyEtAl2014
ref_key <- function(r) {
  fam <- gsub("[^A-Za-z]", "", r$authors$family)
  base <- if (length(fam) <= 2) paste(fam, collapse = "") else paste0(fam[1], "EtAl")
  paste0(base, r$year, r$suffix)
}

# ---------------------------------------------------------------------------
# 3. Build the reference table for Chapter 18
# ---------------------------------------------------------------------------
ch18_lines <- ref_lines(18, "Concealed-Carry Laws")
ch18_refs  <- lapply(split_entries(ch18_lines), parse_entry)
keys <- make.unique(vapply(ch18_refs, ref_key, ""), sep = "_")
for (i in seq_along(ch18_refs)) ch18_refs[[i]]$key <- keys[i]
length(ch18_refs)                                  # ~135 entries

refs_df <- function(refs) do.call(rbind, lapply(refs, function(r) data.frame(
  key = r$key, type = r$type, year = r$year, title = r$title, container = r$container,
  volume = r$volume, number = r$number, publisher = r$publisher, institution = r$institution,
  url = r$url, doi = r$doi, first_author = r$authors$family[1], n_authors = nrow(r$authors),
  raw = r$raw, stringsAsFactors = FALSE)))
refs <- refs_df(ch18_refs)
head(refs[c("key", "type", "title", "container")])

# ---------------------------------------------------------------------------
# 4. In-text citation  ->  key.  Handles "A (1999)", "A and B (1999)", "A, B, and C (1999)",
#    "A et al. (2020)", and lists such as "Lott (1998a, 1998b, 2000); Lott and Whitley (2003)"
# ---------------------------------------------------------------------------
cite_keys <- function(s) {
  s <- gsub("\\)\\s+[a-f](?=$|;)", ")", trimws(s), perl = TRUE)     # footnote letter after ")"
  unlist(lapply(strsplit(s, ";\\s*")[[1]], function(piece) {
    m <- regmatches(piece, regexec("^(.*?)\\s*\\(([^)]*)\\)", piece, perl = TRUE))[[1]]
    if (!length(m)) return(NA_character_)
    who <- m[2]; yrs <- trimws(strsplit(m[3], ",")[[1]])
    surname <- function(n) gsub("[^A-Za-z]", "", tail(strsplit(trimws(n), " ")[[1]], 1))
    base <- if (grepl(" et al", who)) paste0(surname(sub(" et al.*$", "", who)), "EtAl") else {
      nm <- strsplit(who, ",\\s*and\\s+|\\s+and\\s+|,\\s*", perl = TRUE)[[1]]; nm <- nm[nzchar(nm)]
      if (length(nm) <= 2) paste(vapply(nm, surname, ""), collapse = "") else paste0(surname(nm[1]), "EtAl")
    }
    paste0(base, yrs)
  }))
}

# One row per (table row, cited work): the general form, for tables with several citations per cell
resolve_citations <- function(strings, refs) {
  out <- do.call(rbind, lapply(seq_along(strings), function(i) {
    k <- cite_keys(strings[i])
    data.frame(row = i, cited_as = strings[i], cite_key = k, stringsAsFactors = FALSE)
  }))
  j <- match(out$cite_key, refs$key)
  out$bib_key <- refs$key[j]; out$reference <- refs$raw[j]; out$doi <- refs$doi[j]; out$url <- refs$url[j]
  out
}

# One-citation-per-row version for t181
link_citations <- function(tab, col = "study", refs) {
  tab$cite_key <- vapply(tab[[col]], function(s) cite_keys(s)[1], "")
  j <- match(tab$cite_key, refs$key)
  tab$bib_key   <- refs$key[j]
  tab$reference <- refs$raw[j]
  tab$doi       <- refs$doi[j]
  tab$url       <- refs$url[j]
  tab
}
t181_linked <- link_citations(t181, "study", refs)
t181_linked[c("study", "bib_key")]
stopifnot(!anyNA(t181_linked$bib_key))             # all 22 studies resolve to a reference-list entry

# ---------------------------------------------------------------------------
# 5. Export: BibTeX and CSL-JSON
#    BibTeX name format: "Last, Suffix, First and Last, First ...".
#    Double braces around titles protect capitalisation.
# ---------------------------------------------------------------------------
bib_names <- function(a) paste(ifelse(nzchar(a$suffix),
                                      sprintf("%s, %s, %s", a$family, a$suffix, a$given),
                                      ifelse(nzchar(a$given), sprintf("%s, %s", a$family, a$given),
                                             sprintf("{%s}", a$family))), collapse = " and ")
bib_esc <- function(x) gsub("([&%$#_])", "\\\\\\1", x)

to_bibtex <- function(refs) {
  one <- function(r) {
    f <- switch(r$type,
      article      = c(journal = r$container, volume = r$volume, number = r$number, month = r$month),
      book         = c(publisher = r$publisher, edition = r$edition),
      techreport   = c(institution = r$institution),
      incollection = c(booktitle = r$container, publisher = r$publisher),
      misc         = c())
    f <- c(author = bib_names(r$authors), title = paste0("{", r$title, "}"), f,
           year = r$year, url = r$url, doi = r$doi, note = r$note)
    f <- f[!is.na(f) & nzchar(f)]
    paste0("@", r$type, "{", r$key, ",\n",
           paste(sprintf("  %s = {%s}", names(f),
                         ifelse(names(f) %in% c("url", "doi"), f, bib_esc(f))), collapse = ",\n"), "\n}")
  }
  paste(vapply(refs, one, ""), collapse = "\n\n")
}

csl_type <- c(article = "article-journal", book = "book", techreport = "report",
              incollection = "chapter", misc = "webpage")
to_csl_json <- function(refs) {
  library(jsonlite)
  items <- lapply(refs, function(r) {
    it <- list(id = r$key, type = unname(csl_type[r$type]), title = r$title,
               author = lapply(seq_len(nrow(r$authors)), function(i) {
                 a <- r$authors[i, ]
                 if (nzchar(a$given)) Filter(nzchar, list(family = a$family, given = a$given, suffix = a$suffix))
                 else list(literal = a$family) }),
               issued = list(`date-parts` = list(list(as.integer(r$year)))),
               `container-title` = r$container, volume = r$volume, issue = r$number,
               publisher = if (!is.na(r$publisher)) r$publisher else r$institution,
               URL = r$url, DOI = r$doi, note = r$note)
    Filter(function(x) !(length(x) == 1 && is.na(x)), it)
  })
  toJSON(items, auto_unbox = TRUE, pretty = TRUE)
}

cited <- ch18_refs[vapply(ch18_refs, function(r) r$key %in% t181_linked$bib_key, NA)]
cat(to_bibtex(cited[1:3]), "\n")                   # look at a few entries first
# writeLines(to_bibtex(cited),       "t181_cited.bib")
# writeLines(to_bibtex(ch18_refs),   "chapter18_all.bib")
# writeLines(to_csl_json(cited),     "t181_cited.json")   # Zotero / pandoc --citeproc

# ---------------------------------------------------------------------------
# 6. Optional: look up DOIs at Crossref (needs internet; NOT run here -- the sandbox this was
#    written in has no outbound access, so check results by eye before trusting them).
#    A candidate is accepted only if the year is within one year and the title is >= 85% similar.
# ---------------------------------------------------------------------------
norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
title_sim <- function(a, b) { a <- norm(a); b <- norm(b); 1 - adist(a, b)[1, 1] / max(nchar(a), nchar(b)) }

crossref_doi <- function(r, mailto, min_sim = 0.85) {
  if (!r$type %in% c("article", "book", "incollection")) return(NA_character_)
  library(httr2)
  q <- paste(r$title, r$authors$family[1], r$year)
  resp <- request("https://api.crossref.org/works") |>
    req_url_query(`query.bibliographic` = q, rows = 3, mailto = mailto,
                  select = "DOI,title,issued") |>
    req_retry(max_tries = 3) |> req_perform() |> resp_body_json()
  for (it in resp$message$items) {
    t <- if (length(it$title)) it$title[[1]] else ""
    y <- tryCatch(it$issued$`date-parts`[[1]][[1]], error = function(e) NA)
    if (!is.na(y) && abs(y - as.integer(r$year)) <= 1 && title_sim(t, r$title) >= min_sim) return(it$DOI)
  }
  NA_character_
}
resolve_dois <- function(refs, mailto, cache = "doi_cache.rds") {
  have <- if (file.exists(cache)) readRDS(cache) else list()
  for (r in refs) if (is.null(have[[r$key]])) { have[[r$key]] <- crossref_doi(r, mailto); Sys.sleep(0.2) }
  saveRDS(have, cache); have
}
# dois <- resolve_dois(cited, mailto = "you@example.org")
# for (i in seq_along(ch18_refs)) if (!is.null(dois[[ch18_refs[[i]]$key]])) ch18_refs[[i]]$doi <- dois[[ch18_refs[[i]]$key]]
# refs <- refs_df(ch18_refs); t181_linked <- link_citations(t181, "study", refs)

# ---------------------------------------------------------------------------
# 7. Hyperlinked HTML view of t181: each study links to its DOI when known, otherwise to the
#    entry in a reference list at the bottom of the page.
# ---------------------------------------------------------------------------
h <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x)))
linked_html <- function(tab, refs) {
  href <- ifelse(!is.na(tab$doi), paste0("https://doi.org/", tab$doi), paste0("#", tab$bib_key))
  rows <- sprintf("<tr><td><a href=\"%s\">%s</a></td><td>%s</td></tr>", href, h(tab$study), h(tab$significant_effect))
  used <- refs[refs$key %in% tab$bib_key, ]
  c("<table border=1 cellpadding=4><tr><th>Study</th><th>Significant effect</th></tr>", rows, "</table>",
    "<h3>References</h3><ol>",
    sprintf("<li id=\"%s\">%s%s</li>", used$key, h(used$raw),
            ifelse(!is.na(used$doi), sprintf(" <a href=\"https://doi.org/%s\">doi</a>", used$doi), "")),
    "</ol>")
}
# writeLines(linked_html(t181_linked, refs), "t181_linked.html")
