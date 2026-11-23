#' Build an SVG flow diagram from node and edge tables
#'
#' @description Skeleton diagram builder used to document how data sources,
#' functions and intermediate objects relate. Layout is specified by the caller
#' (see `dev/make_pipeline_svg.R` for the CVhelp specification).
#'
#' @param nodes data frame with columns `id`, `type` (one of "src", "fn",
#'   "obj", "out"), `x`, `y`, `w`, `h`, `title`, `lines` (text lines separated
#'   by `|`) and optionally `dashed` (logical).
#' @param edges data frame with columns `from`, `to` (node ids) and optionally
#'   `from_y`, `to_y` (absolute y of the arrow ends on the side of the node).
#'   Nodes in the same column are joined bottom-to-top; others right-to-left.
#' @param headers optional data frame with columns `x`, `y`, `label`.
#' @param groups optional data frame with columns `x`, `y`, `w`, `h` for dashed
#'   grouping boxes.
#' @param title,footnote optional text for the top and bottom of the diagram.
#' @param file optional path; if given, the SVG is written there.
#' @param width,height SVG size in pixels.
#'
#' @returns SVG text, invisibly.
#' @export
build_pipeline_svg <- function(nodes, edges, headers = NULL, groups = NULL,
                               title = NULL, footnote = NULL, file = NULL,
                               width = 1500, height = 800) {

  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  txt <- function(x, y, label, class, anchor = "middle") {
    sprintf('  <text x="%s" y="%s" text-anchor="%s" class="%s">%s</text>',
            x, y, anchor, class, esc(label))
  }

  if (is.null(nodes$dashed)) nodes$dashed <- FALSE
  nodes$dashed[is.na(nodes$dashed)] <- FALSE

  css <- c(
    ".src{fill:#dbeafe;stroke:#2563eb;stroke-width:1.5;}",
    ".fn{fill:#dcfce7;stroke:#16a34a;stroke-width:1.5;}",
    ".obj{fill:#fef3c7;stroke:#d97706;stroke-width:1.5;}",
    ".out{fill:#e5e7eb;stroke:#4b5563;stroke-width:2;}",
    ".grp{fill:none;stroke:#999;stroke-width:1.5;stroke-dasharray:6 4;}",
    ".ln{stroke:#555;stroke-width:1.5;fill:none;marker-end:url(#arr);}",
    ".t{fill:#111;}", ".tb{fill:#111;font-weight:bold;}",
    ".ts{fill:#444;font-size:11px;}",
    ".hd{fill:#111;font-weight:bold;font-size:15px;}",
    ".ti{fill:#111;font-weight:bold;font-size:20px;}")

  out <- c(
    sprintf(paste0('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %s %s" ',
                   'width="%s" height="%s" font-family="Segoe UI, Arial, sans-serif" font-size="13">'),
            width, height, width, height),
    '  <defs>',
    paste0('    <marker id="arr" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" ',
           'markerHeight="7" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="#555"/></marker>'),
    paste0('    <style>', paste(css, collapse = " "), '</style>'),
    '  </defs>')

  if (!is.null(title)) out <- c(out, txt(width / 2, 26, title, "ti"))

  if (!is.null(headers)) {
    out <- c(out, unlist(Map(function(x, y, l) txt(x, y, l, "hd"),
                             headers$x, headers$y, headers$label)))
  }
  if (!is.null(groups)) {
    out <- c(out, sprintf('  <rect x="%s" y="%s" width="%s" height="%s" rx="8" class="grp"/>',
                          groups$x, groups$y, groups$w, groups$h))
  }

  # nodes
  for (i in seq_len(nrow(nodes))) {
    n <- nodes[i, ]
    cx <- n$x + n$w / 2
    lns <- strsplit(n$lines, "|", fixed = TRUE)[[1]]
    out <- c(
      out,
      sprintf('  <rect x="%s" y="%s" width="%s" height="%s" rx="6" class="%s"%s/>',
              n$x, n$y, n$w, n$h, n$type,
              if (n$dashed) ' stroke-dasharray="5 3"' else ""),
      txt(cx, n$y + 25, n$title, "tb"),
      unlist(lapply(seq_along(lns), function(j) txt(cx, n$y + 43 + 15 * (j - 1), lns[j], "ts"))))
  }

  # edges
  if (is.null(edges$from_y)) edges$from_y <- NA_real_
  if (is.null(edges$to_y)) edges$to_y <- NA_real_
  for (i in seq_len(nrow(edges))) {
    e <- edges[i, ]
    a <- nodes[nodes$id == e$from, ]
    b <- nodes[nodes$id == e$to, ]
    if (abs((a$x + a$w / 2) - (b$x + b$w / 2)) < 1) {
      d <- sprintf("M%s,%s V%s", a$x + a$w / 2, a$y + a$h, b$y - 2)
    } else {
      y1 <- if (is.na(e$from_y)) a$y + a$h / 2 else e$from_y
      y2 <- if (is.na(e$to_y)) b$y + b$h / 2 else e$to_y
      x1 <- a$x + a$w
      x2 <- b$x - 2
      d <- if (y1 == y2) {
        sprintf("M%s,%s H%s", x1, y1, x2)
      } else {
        xm <- x1 + (x2 - x1) / 2
        sprintf("M%s,%s C%s,%s %s,%s %s,%s", x1, y1, xm, y1, xm, y2, x2, y2)
      }
    }
    out <- c(out, sprintf('  <path class="ln" d="%s"/>', d))
  }

  # legend, only for node types in use
  leg <- c(src = "External source", fn = "Function", obj = "Intermediate object", out = "Output")
  leg <- leg[names(leg) %in% nodes$type]
  ly <- height - 70
  lx <- 20
  for (k in names(leg)) {
    out <- c(out,
             sprintf('  <rect x="%s" y="%s" width="18" height="14" class="%s"/>', lx, ly, k),
             txt(lx + 26, ly + 12, leg[[k]], "t", "start"))
    lx <- lx + 40 + 7 * nchar(leg[[k]])
  }
  if (any(nodes$dashed)) {
    out <- c(out,
             sprintf('  <rect x="%s" y="%s" width="18" height="14" class="src" stroke-dasharray="3 2"/>', lx, ly),
             txt(lx + 26, ly + 12, "Hard-coded (no download)", "t", "start"))
  }
  if (!is.null(footnote)) out <- c(out, txt(20, height - 25, footnote, "ts", "start"))

  svg <- paste(c(out, "</svg>"), collapse = "\n")
  if (!is.null(file)) writeLines(svg, file)
  invisible(svg)
}
