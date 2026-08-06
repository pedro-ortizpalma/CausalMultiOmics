# =============================================================================
# dev/build_guide.R
# Builds docs/CausalMultiOmics-guide.html
# =============================================================================
#
# The reference half of the guide is generated from the roxygen comments that
# are already in the source, so it cannot drift out of step with the code. The
# teaching half lives in dev/guide-prose.html and is written by hand.
#
# Run from the package root:  Rscript dev/build_guide.R
# =============================================================================

# -----------------------------------------------------------------------------
# Parsing
# -----------------------------------------------------------------------------

escape_html <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

#' Turn the Rd markup inside roxygen comments into HTML
#'
#' Runs after escape_html(), which leaves backslashes and braces alone, so the
#' markup is still intact by the time it gets here. Without this the reader
#' sees things like "each element of \\code{assays}" verbatim.
render_rd <- function(x) {

  x <- escape_html(x)

  # \code{\link{foo}} collapses to a plain code span.
  x <- gsub("\\\\code\\{\\\\link\\{([^}]*)\\}\\}", "<code>\\1</code>", x)
  x <- gsub("\\\\link\\{([^}]*)\\}", "<code>\\1</code>", x)
  x <- gsub("\\\\code\\{([^}]*)\\}", "<code>\\1</code>", x)

  x <- gsub("\\\\emph\\{([^}]*)\\}", "<i>\\1</i>", x)
  x <- gsub("\\\\strong\\{([^}]*)\\}", "<b>\\1</b>", x)
  x <- gsub("\\\\dQuote\\{([^}]*)\\}", "&ldquo;\\1&rdquo;", x)
  x <- gsub("\\\\sQuote\\{([^}]*)\\}", "&lsquo;\\1&rsquo;", x)

  # Anything left over is a tag this renderer does not know; drop the wrapper
  # rather than show the reader a backslash.
  x <- gsub("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1", x)

  x

}

#' Roxygen block sitting above a function
#'
#' The package style leaves a blank line between the block and the function it
#' documents, so the walk back has to step over blank lines before it starts
#' collecting. Stopping at the first blank line was what made an earlier
#' version of this parser report most of the package as undocumented.
roxygen_above <- function(lines, start) {

  j <- start - 1L

  while (j >= 1 && !nzchar(trimws(lines[j]))) j <- j - 1L

  block <- character(0)

  while (j >= 1 && grepl("^\\s*#'", lines[j])) {
    block <- c(sub("^\\s*#'\\s?", "", lines[j]), block)
    j <- j - 1L
  }

  block

}

parse_roxygen <- function(block) {

  out <- list(title = "", description = character(0), params = list(),
              value = "", internal = FALSE, exported = FALSE, keywords = "")

  if (length(block) == 0) return(out)

  tag_lines <- grepl("^@", block)

  out$internal <- any(grepl("^@keywords\\s+internal", block))
  out$exported <- any(grepl("^@export", block))

  # Free text before the first tag: first paragraph is the title.
  first_tag <- if (any(tag_lines)) which(tag_lines)[1] else length(block) + 1L
  free <- block[seq_len(first_tag - 1L)]
  free <- free[!grepl("^\\s*$", free) | TRUE]

  if (length(free) > 0) {

    breaks <- which(!nzchar(trimws(free)))
    end_title <- if (length(breaks) > 0) breaks[1] - 1L else length(free)

    out$title <- paste(trimws(free[seq_len(max(end_title, 1))]), collapse = " ")

    if (length(free) > end_title) {
      rest <- free[(end_title + 1L):length(free)]
      rest <- rest[nzchar(trimws(rest))]
      out$description <- rest
    }

  }

  # Tags: fold continuation lines into the tag they belong to.
  if (any(tag_lines)) {

    idx <- which(tag_lines)

    for (k in seq_along(idx)) {

      from <- idx[k]
      to <- if (k < length(idx)) idx[k + 1L] - 1L else length(block)

      text <- paste(trimws(block[from:to]), collapse = " ")

      if (grepl("^@param\\s", text)) {

        rest <- sub("^@param\\s+", "", text)
        name <- sub("\\s.*$", "", rest)
        desc <- sub("^\\S+\\s*", "", rest)
        out$params[[name]] <- desc

      } else if (grepl("^@return\\s", text)) {

        out$value <- sub("^@return\\s+", "", text)

      }

    }

  }

  out

}

#' What the package actually exports
#'
#' Read from NAMESPACE rather than guessed from the name. A leading dot is a
#' convention, not a rule, and several constructors have ordinary names while
#' being internal.
exported_names <- function() {

  ns <- readLines("NAMESPACE", warn = FALSE)

  direct <- sub("^export\\((.*)\\)$", "\\1", ns[grepl("^export\\(", ns)])

  methods <- sub("^S3method\\(([^,]+),\\s*(.*)\\)$", "\\1.\\2",
                 ns[grepl("^S3method\\(", ns)])

  list(direct = direct, methods = methods)

}

inventory <- function() {

  exports <- exported_names()

  rows <- list()

  for (path in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) {

    lines <- readLines(path, warn = FALSE)
    exprs <- parse(path, keep.source = TRUE)
    refs <- utils::getSrcref(exprs)

    for (i in seq_along(exprs)) {

      e <- exprs[[i]]

      if (!is.call(e)) next
      if (!(as.character(e[[1]])[1] %in% c("<-", "="))) next
      if (!is.call(e[[3]]) || as.character(e[[3]][[1]])[1] != "function") next

      name <- as.character(e[[2]])
      start <- as.integer(refs[[i]])[1]

      doc <- parse_roxygen(roxygen_above(lines, start))

      args <- names(formals(eval(e[[3]])))
      defaults <- vapply(formals(eval(e[[3]])), function(d) {
        if (missing(d) || identical(d, quote(expr = ))) "" else
          paste(deparse(d), collapse = "")
      }, character(1))

      signature <- paste0(
        name, "(",
        paste0(args, ifelse(nzchar(defaults), paste0(" = ", defaults), ""),
               collapse = ", "),
        ")"
      )

      rows[[length(rows) + 1L]] <- list(
        file = basename(path), name = name, line = start,
        signature = signature, args = args, doc = doc,
        public = name %in% exports$direct,
        method = name %in% exports$methods
      )

    }

  }

  rows

}

# -----------------------------------------------------------------------------
# Rendering
# -----------------------------------------------------------------------------

file_notes <- c(
  "CausalMultiOmics-package.R" = "The package front page. No code, only the documentation you see when you type ?CausalMultiOmics.",
  "classes.R" = "Defines the shape of every object the library produces. No calculations here at all: each function just builds an empty box with the right labelled drawers, so that a half-finished analysis is still a well-formed object.",
  "data_loading.R" = "Getting your data in. load_data() checks that the blocks are usable, that every sample is labelled, and assembles them into one container. It is the only place that touches your raw input.",
  "validation.R" = "The inspection engine. Works out what kind of numbers each block holds, measures its quality from every angle, runs a contest between ways of reshaping it, and writes the cleaning plan. It never modifies anything.",
  "preprocessing.R" = "The cleaning engine. First half is the catalogue of methods, each one a fit/apply pair so the decision can be stored and reused; second half executes a plan and records every step.",
  "analysis.R" = "The evidence engine. Nine different statistical methods each report the relationships they find, and an integrator merges them into one scored map, keeping track of how much each relationship is allowed to claim.",
  "methods.R" = "Everything to do with showing results: how each object prints and summarises, plus the two HTML report builders."
)

render_reference <- function(rows) {

  by_file <- split(rows, vapply(rows, function(r) r$file, character(1)))

  order_files <- intersect(names(file_notes), names(by_file))
  order_files <- c(order_files, setdiff(names(by_file), order_files))

  parts <- character(0)

  for (fname in order_files) {

    group <- by_file[[fname]]
    group <- group[order(vapply(group, function(r) r$line, numeric(1)))]

    cards <- vapply(group, function(r) {

      params <- if (length(r$doc$params) > 0) {
        paste0(
          "<dl class='params'>",
          paste0("<dt>", escape_html(names(r$doc$params)), "</dt><dd>",
                 render_rd(unlist(r$doc$params)), "</dd>", collapse = ""),
          "</dl>"
        )
      } else if (length(r$args) > 0) {
        paste0("<p class='muted'>Arguments: <code>",
               escape_html(paste(r$args, collapse = ", ")), "</code></p>")
      } else ""

      badge <- if (r$public) "<span class='tag pub'>public</span>"
      else if (r$method) "<span class='tag meth'>method</span>"
      else "<span class='tag int'>internal</span>"

      searchable <- tolower(paste(r$name, r$doc$title,
                                  paste(r$doc$description, collapse = " ")))

      paste0(
        "<div class='fn' data-public='", if (r$public || r$method) "1" else "0",
        "' data-search='", escape_html(searchable), "'>",
        "<div class='fn-head'><code class='fn-name'>", escape_html(r$name),
        "</code>", badge,
        "<span class='fn-loc'>", escape_html(r$file), ":", r$line, "</span></div>",
        if (nzchar(r$doc$title))
          paste0("<p class='fn-title'>", render_rd(r$doc$title), "</p>") else
            "<p class='fn-title muted'>(no description)</p>",
        if (length(r$doc$description) > 0)
          paste0("<p class='fn-desc'>",
                 render_rd(paste(r$doc$description, collapse = " ")),
                 "</p>") else "",
        "<pre class='sig'><code>", escape_html(r$signature), "</code></pre>",
        params,
        if (nzchar(r$doc$value))
          paste0("<p class='fn-ret'><b>Returns:</b> ",
                 render_rd(r$doc$value), "</p>") else "",
        "</div>"
      )

    }, character(1))

    parts <- c(parts, paste0(
      "<div class='fileblock' data-file='", escape_html(fname), "'>",
      "<h3>", escape_html(fname),
      " <span class='muted'>", length(group), " functions</span></h3>",
      if (!is.na(file_notes[fname]))
        paste0("<p class='filenote'>", escape_html(file_notes[fname]), "</p>") else "",
      paste(cards, collapse = ""),
      "</div>"
    ))

  }

  paste(parts, collapse = "")

}

render_filemap <- function(rows) {

  counts <- table(vapply(rows, function(r) r$file, character(1)))

  order_files <- intersect(names(file_notes), names(counts))
  order_files <- c(order_files, setdiff(names(counts), order_files))

  paste0(vapply(order_files, function(fname) {

    lines <- length(readLines(file.path("R", fname), warn = FALSE))

    paste0(
      "<div class='filecard'><h4>", escape_html(fname), "</h4>",
      "<p class='filestats'>", counts[[fname]], " functions &middot; ",
      lines, " lines</p>",
      "<p>", escape_html(if (is.na(file_notes[fname])) "" else file_notes[fname]),
      "</p></div>"
    )

  }, character(1)), collapse = "")

}

# -----------------------------------------------------------------------------
# Assembly
# -----------------------------------------------------------------------------

rows <- inventory()

prose <- paste(readLines("dev/guide-prose.html", warn = FALSE), collapse = "\n")

prose <- sub("<div class='filegrid' id='filegrid'></div>",
             paste0("<div class='filegrid'>", render_filemap(rows), "</div>"),
             prose, fixed = TRUE)

prose <- sub('<div class="filegrid" id="filegrid"></div>',
             paste0("<div class='filegrid'>", render_filemap(rows), "</div>"),
             prose, fixed = TRUE)

prose <- sub('<div id="reference-body"></div>',
             render_reference(rows), prose, fixed = TRUE)

n_public <- sum(vapply(rows, function(r) r$public, logical(1)))
n_method <- sum(vapply(rows, function(r) r$method, logical(1)))

style <- "
<style>
:root{--bg:#f6f7f9;--fg:#1c1f23;--muted:#6b7280;--line:#e2e5ea;--panel:#fff;
--accent:#2f6fed;--good:#1f9d55;--warn:#d97706;--bad:#dc2626;--code:#f3f4f6;}
@media (prefers-color-scheme: dark){
:root{--bg:#15171a;--fg:#e6e8eb;--muted:#9aa1ab;--line:#2b2f36;--panel:#1c1f24;
--code:#22262c;}}
*{box-sizing:border-box}
body{margin:0;font:16px/1.65 -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;
background:var(--bg);color:var(--fg)}
header{background:var(--panel);border-bottom:1px solid var(--line);padding:22px 28px;
position:sticky;top:0;z-index:20}
header h1{margin:0;font-size:21px}
header .sub{color:var(--muted);font-size:13px;margin-top:4px}
nav{display:flex;flex-wrap:wrap;gap:6px;margin-top:14px}
nav button{background:transparent;border:1px solid var(--line);border-radius:999px;
padding:6px 14px;cursor:pointer;font-size:13px;color:var(--muted)}
nav button:hover{border-color:var(--accent);color:var(--accent)}
nav button.active{background:var(--accent);border-color:var(--accent);color:#fff}
main{max-width:960px;margin:0 auto;padding:26px 28px 90px}
section{display:none}
section.active{display:block}
h2{font-size:25px;margin:8px 0 18px}
h3{font-size:18px;margin:28px 0 10px}
h4{font-size:15px;margin:0 0 6px}
p{max-width:74ch}
.lead{font-size:17px;color:var(--fg)}
.muted{color:var(--muted);font-size:13px}
code,pre{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace}
pre{background:var(--code);border:1px solid var(--line);border-radius:10px;
padding:14px 16px;overflow-x:auto;font-size:13.5px;line-height:1.55}
pre.flow{font-size:12.5px;line-height:1.35}
.code-inline,p code,dd code,li code{background:var(--code);border-radius:4px;
padding:1px 6px;font-size:13.5px}
.callout{background:var(--panel);border:1px solid var(--line);border-left:5px solid var(--warn);
border-radius:8px;padding:14px 18px;margin:18px 0;max-width:80ch}
.callout.danger{border-left-color:var(--bad)}
.callout h4{margin:0 0 8px}
.callout p{margin:0 0 8px}.callout p:last-child{margin:0}
.tip{background:var(--panel);border:1px solid var(--line);border-left:5px solid var(--good);
border-radius:8px;padding:12px 16px;margin:14px 0;font-size:14.5px;max-width:80ch}
.steps{display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:12px;margin:18px 0}
.step{background:var(--panel);border:1px solid var(--line);border-radius:10px;padding:16px}
.step p{font-size:14px;color:var(--muted);margin:6px 0 0}
.stepnum{display:inline-flex;width:26px;height:26px;border-radius:50%;background:var(--accent);
color:#fff;align-items:center;justify-content:center;font-size:13px;font-weight:700;margin-right:8px}
.objgrid,.filegrid{display:grid;grid-template-columns:repeat(auto-fit,minmax(290px,1fr));gap:14px;margin:16px 0}
.objcard,.filecard{background:var(--panel);border:1px solid var(--line);border-radius:10px;padding:16px 18px}
.objcard dl,.params{margin:8px 0 0}
.objcard dt,.params dt{font-weight:600;font-size:13.5px;margin-top:7px}
.objcard dd,.params dd{margin:1px 0 0;color:var(--muted);font-size:13.5px}
.filecard h4{font-family:ui-monospace,monospace}
.filestats{color:var(--muted);font-size:12.5px;margin:0 0 8px}
.filecard p{font-size:14px;margin:0}
table.plain{border-collapse:collapse;margin:14px 0;max-width:80ch}
table.plain th{text-align:left;padding:10px 14px 10px 0;vertical-align:top;width:130px;
border-bottom:1px solid var(--line)}
table.plain td{padding:10px 0;border-bottom:1px solid var(--line)}
.idgrid{display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:12px;margin:16px 0}
.idcard{background:var(--panel);border:1px solid var(--line);border-radius:10px;padding:14px 16px;
border-top:4px solid var(--muted)}
.idcard.weak{border-top-color:var(--muted)}
.idcard.medium{border-top-color:var(--accent)}
.idcard.strong{border-top-color:var(--good)}
.idcard b{font-family:ui-monospace,monospace;font-size:14px}
.idcard p{font-size:13.5px;color:var(--muted);margin:6px 0 0}
.errcard{background:var(--panel);border:1px solid var(--line);border-radius:10px;
padding:14px 18px;margin-bottom:12px}
.errcard>code{display:block;color:var(--bad);font-size:13.5px;margin-bottom:8px;font-weight:600}
.errcard p{font-size:14.5px;margin:5px 0}
.filterbar{display:flex;flex-wrap:wrap;align-items:center;gap:14px;margin:16px 0;
position:sticky;top:150px;background:var(--bg);padding:8px 0;z-index:5}
.filterbar input[type=text]{flex:1;min-width:220px;max-width:380px;padding:9px 12px;
border:1px solid var(--line);border-radius:8px;font-size:14px;background:var(--panel);color:var(--fg)}
.filterbar label{font-size:13.5px;color:var(--muted)}
.fileblock{margin-bottom:30px}
.fileblock h3{font-family:ui-monospace,monospace;border-bottom:2px solid var(--line);padding-bottom:6px}
.filenote{font-size:14px;color:var(--muted);margin:0 0 14px}
.fn{background:var(--panel);border:1px solid var(--line);border-radius:9px;
padding:12px 16px;margin-bottom:9px}
.fn-head{display:flex;flex-wrap:wrap;align-items:center;gap:9px}
.fn-name{font-size:14.5px;font-weight:700}
.fn-loc{color:var(--muted);font-size:12px;margin-left:auto}
.tag{font-size:11px;border-radius:999px;padding:2px 9px;font-weight:600}
.tag.pub{background:#e6f5ec;color:#14663a}
.tag.meth{background:#eef2fb;color:#25417f}
.tag.int{background:var(--code);color:var(--muted)}
.fn-title{margin:7px 0 0;font-size:14.5px}
.fn-desc{margin:5px 0 0;font-size:13.5px;color:var(--muted)}
.fn-ret{margin:7px 0 0;font-size:13.5px;color:var(--muted)}
pre.sig{margin:9px 0 0;padding:8px 12px;font-size:12.5px}
.glossary dt{font-weight:600;margin-top:12px}
.glossary dd{margin:2px 0 0;color:var(--muted);max-width:74ch}
footer{color:var(--muted);font-size:12.5px;border-top:1px solid var(--line);
padding:16px 28px;text-align:center}
</style>"

script <- "
<script>
function show(id,btn){
  document.querySelectorAll('main section').forEach(function(s){s.classList.remove('active')});
  var t=document.getElementById(id); if(t)t.classList.add('active');
  document.querySelectorAll('nav button').forEach(function(b){b.classList.remove('active')});
  if(btn)btn.classList.add('active');
  window.scrollTo({top:0,behavior:'smooth'});
}
function filterFns(){
  var q=(document.getElementById('fnFilter').value||'').toLowerCase();
  var pub=document.getElementById('publicOnly').checked;
  var shown=0,total=0;
  document.querySelectorAll('.fileblock').forEach(function(block){
    var any=false;
    block.querySelectorAll('.fn').forEach(function(fn){
      total++;
      var hit=(fn.dataset.search||'').indexOf(q)>-1;
      if(pub && fn.dataset.public!=='1') hit=false;
      fn.style.display=hit?'':'none';
      if(hit){any=true;shown++;}
    });
    block.style.display=any?'':'none';
  });
  document.getElementById('fnCount').textContent=shown+' of '+total+' functions';
}
document.addEventListener('DOMContentLoaded',function(){
  if(document.getElementById('fnFilter'))filterFns();
});
</script>"

labels <- c(start = "Start here", setup = "Setting up", tutorial = "Tutorial",
            workflow = "How it fits", objects = "The objects",
            scores = "Reading the scores", files = "The files",
            reference = "Function reference", errors = "When it breaks",
            glossary = "Glossary")

nav <- paste0(
  vapply(seq_along(labels), function(i) {
    paste0("<button class='", if (i == 1L) "active" else "",
           "' onclick=\"show('", names(labels)[i], "',this)\">",
           labels[[i]], "</button>")
  }, character(1)),
  collapse = ""
)

html <- paste0(
  "<!DOCTYPE html>\n<html lang='en'><head><meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width,initial-scale=1'>",
  "<title>CausalMultiOmics &mdash; complete guide</title>",
  style, "</head><body>",
  "<header><h1>CausalMultiOmics</h1>",
  "<div class='sub'>Complete guide &middot; ", length(rows), " functions (",
  n_public, " public, ", n_method, " display methods) &middot; generated ",
  format(Sys.Date()), "</div>",
  "<nav>", nav, "</nav></header>",
  "<main>", prose, "</main>",
  "<footer>Generated from the package source by dev/build_guide.R. ",
  "Self-contained: no internet connection needed.</footer>",
  script, "</body></html>"
)

dir.create("docs", showWarnings = FALSE)

writeLines(html, "docs/CausalMultiOmics-guide.html", useBytes = TRUE)

cat("functions documented :", length(rows), "\n")
cat("  public             :", n_public, "\n")
cat("  display methods    :", n_method, "\n")
cat("  internal           :", length(rows) - n_public - n_method, "\n")
untitled <- Filter(function(r) !nzchar(r$doc$title), rows)

cat("without a title      :", length(untitled), "\n")

if (length(untitled) > 0) {

  by_file <- table(vapply(untitled, function(r) r$file, character(1)))

  for (f in names(by_file)) cat("    ", f, ":", by_file[[f]], "\n")

  # Naming them is the difference between a count you can act on and one you
  # rediscover by grepping every file each time it moves.
  cat("    ", paste(vapply(untitled, function(r) r$name, character(1)),
                    collapse = ", "), "\n")

}
cat("output               : docs/CausalMultiOmics-guide.html (",
    round(file.size("docs/CausalMultiOmics-guide.html") / 1024, 1), "KB )\n")
