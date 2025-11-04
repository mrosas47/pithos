#' Density-based clustering for sf POINT or data.frame (+ concave hulls)
#'
#' @description
#' Runs DBSCAN on either an sf POINT layer (using geometry coordinates)
#' or a plain data.frame (using provided or inferred coordinate columns).
#' Returns the fitted `dbscan` object, the original data with an added
#' cluster label column, and an `sf` MULTIPOLYGON layer of concave hulls
#' per cluster (excluding noise `0` and `NA`).
#'
#' @param x An `sf` object with POINT geometries, or a `data.frame`.
#' @param eps Neighbourhood radius (same units as input coords).
#' @param min_pts Minimum number of points to form a dense region.
#' @param cols When `x` is a data.frame, a character vector of length 2
#'   naming the coordinate columns (e.g., `c('x','y')`). If `NULL`, the
#'   function will try common names, then fall back to the first two
#'   numeric columns.
#' @param cluster_col Name for the output cluster label column.
#' @param na_ok Logical. If `T`, rows with missing coordinates are kept in
#'   the output with `NA` cluster labels; only complete rows are used to
#'   fit DBSCAN. If `F`, the function errors on missing coordinates.
#' @param concavity Ratio for `st_concave_hull()` (0–1, lower → more concave).
#' @param buffer_m Numeric buffer to apply to hulls after validity fixes.
#'   Units are those of the layer CRS; use metres only if the CRS units are metres.
#' @param layer Optional scalar to add a `layer` column to the hulls, mirroring
#'   the example snippet.
#' @param crs When `x` is a data.frame, optional CRS (e.g., 4326 or a `crs`
#'   object) to assign to the created points before hull computation.
#' @param ... Additional arguments passed to `dbscan::dbscan()`.
#'
#' @details
#' For sf inputs, only POINT geometries are supported. Noise points are
#' labelled `0` as in `dbscan`. Hulls are computed per cluster id > 0
#' over the subset of rows with complete coordinates used in the fit.
#' If the input is a data.frame, hulls are still produced by internally
#' creating an sf POINT layer from the coordinate columns.
#'
#' @return A list with elements:
#' \item{result}{The fitted `dbscan` object.}
#' \item{data}{The original object with an added `cluster_col` column.}
#' \item{hulls}{An `sf` layer of concave hulls with columns `cluster`, optional `layer`,
#' and geometry column named `geom`. `NULL` if no clusters were found.}
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dbscan)
#'
#' df <- data.frame(x = rnorm(300), y = rnorm(300))
#' out <- pndr.dbscan_cluster(df, eps = 0.25, min_pts = 8, concavity = 0.5, buffer_m = 0, crs = 3857)
#' plot(st_geometry(out$hulls))
#'
#' pts <- st_as_sf(df, coords = c('x','y'), crs = 4326)
#' out_sf <- pndr.dbscan_cluster(pts, eps = 0.01, min_pts = 5, concavity = 0.4)
#' table(out_sf$data$cluster)
#' }
#'
#' @export
#' @importFrom dbscan dbscan
#' @importFrom sf st_coordinates st_geometry_type st_as_sf st_crs st_is_longlat st_combine st_make_valid st_buffer st_sfc st_sf
#' @importFrom sf st_concave_hull
pndr.dbscan <- function(x, eps, min_pts = 5, cols = NULL, cluster_col = 'cluster', na_ok = T, concavity = 0.5, buffer_m = 50, layer = NULL, crs = NULL, ...) {
  if (missing(eps)) stop('eps must be provided')
  if (!is.numeric(eps) || length(eps) != 1 || !is.finite(eps)) stop('eps must be a finite numeric scalar')
  if (!is.numeric(min_pts) || length(min_pts) != 1 || min_pts < 1) stop('min_pts must be a positive integer')
  if (!is.numeric(concavity) || concavity <= 0 || concavity > 1) stop('concavity must be in (0,1]')
  if (!is.numeric(buffer_m) || length(buffer_m) != 1) stop('buffer_m must be numeric')

  is_sf <- inherits(x, 'sf')

  if (is_sf) {
    gt <- unique(st_geometry_type(x, by_geometry = T))
    if (length(gt) != 1 || !(as.character(gt) %in% c('POINT'))) stop('sf input must have POINT geometries only')
    coords_mat <- st_coordinates(x)
    if (ncol(coords_mat) < 2) stop('sf geometry must provide at least X and Y coordinates')
    coords <- coords_mat[, 1:2, drop = F]
    pts_sf <- x
  } else {
    if (!is.data.frame(x)) stop('x must be an sf object or a data.frame')
    if (!is.null(cols)) {
      if (length(cols) != 2) stop("cols must be length 2, e.g., c('x','y')")
      if (!all(cols %in% names(x))) stop('specified cols not found in data.frame')
      coords <- as.matrix(x[, cols])
      pts_sf <- st_as_sf(x[, cols], coords = cols)
    } else {
      preferred_x <- c('x','lon','lng','long','longitude')
      preferred_y <- c('y','lat','latitude')
      x_col <- preferred_x[preferred_x %in% names(x)][1]
      y_col <- preferred_y[preferred_y %in% names(x)][1]
      if (!is.na(x_col) && !is.na(y_col)) {
        coords <- as.matrix(x[, c(x_col, y_col)])
        pts_sf <- st_as_sf(x[, c(x_col, y_col)], coords = c(x_col, y_col))
      } else {
        num_idx <- which(vapply(x, is.numeric, T))
        if (length(num_idx) < 2) stop('cannot infer coordinate columns: provide `cols` or include two numeric columns')
        coords <- as.matrix(x[, num_idx[1:2]])
        pts_sf <- st_as_sf(x[, num_idx[1:2]], coords = names(x)[num_idx[1:2]])
      }
    }
    if (!is.null(crs)) st_crs(pts_sf) <- crs
    if (!is.numeric(coords)) stop('coordinate columns must be numeric')
  }

  if (!na_ok && any(!stats::complete.cases(coords))) stop('missing coordinates found; set na_ok = T to keep and label as NA')
  keep <- stats::complete.cases(coords)
  n <- nrow(coords)

  fit <- if (any(keep)) {
    dbscan(coords[keep, , drop = F], eps = eps, minPts = min_pts, ...)
  } else {
    dbscan(matrix(numeric(0), ncol = 2), eps = eps, minPts = min_pts, ...)
  }

  labels <- rep(NA_integer_, n)
  if (any(keep)) labels[keep] <- fit$cluster

  if (cluster_col %in% names(x)) warning("overwriting existing column '", cluster_col, "'")
  x[[cluster_col]] <- labels

  hulls <- NULL
  if (any(keep) && any(labels[keep] > 0, na.rm = T)) {
    pts_sf[[cluster_col]] <- labels
    pts_used <- pts_sf[keep, , drop = F]
    clu_vals <- sort(unique(labels[keep]))
    clu_vals <- clu_vals[clu_vals > 0]
    geoms <- vector('list', length(clu_vals))
    for (i in seq_along(clu_vals)) {
      idx <- which(pts_used[[cluster_col]] == clu_vals[i])
      if (length(idx) == 1) {
        g <- st_concave_hull(st_combine(pts_used[idx, , drop = F]), ratio = concavity)
      } else if (length(idx) > 1) {
        g <- st_concave_hull(st_combine(pts_used[idx, , drop = F]), ratio = concavity)
      } else {
        g <- NULL
      }
      if (!is.null(g)) {
        g <- st_make_valid(g)
        if (buffer_m != 0) {
          if (st_is_longlat(pts_used)) warning('buffer_m applied on geographic CRS; units are degrees, consider projecting.')
          g <- st_make_valid(st_buffer(g, buffer_m))
        }
      }
      geoms[[i]] <- g
    }
    keep_geom <- !vapply(geoms, is.null, T)
    if (any(keep_geom)) {
      df <- data.frame(cluster = clu_vals[keep_geom])
      if (!is.null(layer)) df$layer <- layer
      sfc <- st_sfc(geoms[keep_geom], crs = st_crs(pts_sf))
      hulls <- st_sf(df, geometry = sfc, sf_column_name = 'geom')
      if (!is.null(layer)) hulls <- hulls[, c('layer','cluster','geom')]
      else hulls <- hulls[, c('cluster','geom')]
    }
  }

  list(
    result = fit,
    data = x,
    hulls = hulls
  )
}
