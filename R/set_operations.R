#' @title Set operations on number line objects
#' @name set_operations
#' @description Perform set operations on a pair of \code{[\link{number_line}]}s.
#'
#' @param x \code{[\link{number_line}]}
#' @param y \code{[\link{number_line}]}
#' @aliases set_operations
#' @return \code{[\link{number_line}]}; \code{list}
#'
#' @details
#'
#' \bold{\code{union_number_lines()}} - Combined the range of \code{x} and that of \code{y}
#'
#' \bold{\code{intersect_number_line()}} - Subset of \code{x} that overlaps with \code{y} and vice versa
#'
#' \bold{\code{subtract_number_lines()}} - Subset of \code{x} that does not overlap with \code{y} and vice versa.
#'
#' The \code{direction} of the returned \code{[\link{number_line}]} will be that of the widest one (\code{x} or \code{y}).
#' If \code{x} and \code{y} have the same length, it'll be an \code{"increasing"} direction.
#'
#' If \code{x} and \code{y} do not overlap, \code{NA} (\code{"NA ?? NA"}) is returned.
#'
#' @seealso
#' \code{\link{number_line}};  \code{\link{overlaps}}
#'
#' @examples
#' nl_1 <- c(number_line(1, 5), number_line(1, 5), number_line(5, 9))
#' nl_2 <- c(number_line(1, 2), number_line(2, 3), number_line(0, 6))
#'
#' # Union
#' nl_1; nl_2; union_number_lines(nl_1, nl_2)
#'
#' @export
union_number_lines <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_match_ref_len(y, "x", c(1, length(y)), "y")
  if(err != F) stop(err, call. = F)

  if(length(y) == 1) y <- rep(y, length(x))
  x <- c(x[1], x); y <- c(x[1], y)
  o <- c(x[1], as.number_line(rep(NA_real_, length(x)-1)))

  lg <- overlap(x, y)
  lg[is.na(lg)] <- F
  lg <- which(lg)

  o[lg] <- number_line(
    start_point(number_line(start_point(x[lg]), start_point(y[lg]))),
    end_point(number_line(end_point(x[lg]), end_point(y[lg])))
  )

  dir <- ifelse(abs(x@.Data[lg]) > abs(y@.Data[lg]), x@.Data[lg]/abs(x@.Data[lg]), y@.Data[lg]/abs(y@.Data[lg]))
  o[lg] <- reverse_number_line(o[lg], direction= ifelse(dir == 1 & !is.na(dir), "decreasing", "increasing"))

  return(o[-1])
}

#' @rdname set_operations
#' @examples
#'
#' nl_3 <- number_line(as.Date(c("01/01/2020", "03/01/2020","09/01/2020"), "%d/%m/%Y"),
#'                     as.Date(c("09/01/2020", "09/01/2020","25/12/2020"), "%d/%m/%Y"))
#'
#' nl_4 <- number_line(as.Date(c("04/01/2020","01/01/2020","01/01/2020"), "%d/%m/%Y"),
#'                     as.Date(c("05/01/2020","05/01/2020","03/01/2020"), "%d/%m/%Y"))
#'
#' # Intersect
#' nl_3; nl_4; intersect_number_lines(nl_3, nl_4)
#' @export
intersect_number_lines <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_match_ref_len(y, "x", c(1, length(y)), "y")
  if(err != F) stop(err, call. = F)

  if(length(y) == 1) y <- rep(y, length(x))

  x <- c(x[1], x); y <- c(x[1], y)
  o <- c(x[1], as.number_line(rep(NA_real_, length(x)-1)))

  lg <- overlap(x, y)
  lg[is.na(lg)] <- F
  lg <- which(lg)

  slg <- overlaps(as.number_line(start_point(x[lg])), y[lg])
  slg[is.na(slg)] <- F
  cnd <- lg[which(slg)]
  o[cnd] <- number_line(l = start_point(x[cnd]), r = right_point(o[cnd]))

  slg <- overlaps(as.number_line(start_point(y[lg])), x[lg])
  slg[is.na(slg)] <- F
  cnd <- lg[which(slg)]
  o[cnd] <- number_line(l = start_point(y[cnd]), r = right_point(o[cnd]))

  slg <- overlaps(as.number_line(end_point(x[lg])), y[lg])
  slg[is.na(slg)] <- F
  cnd <- lg[which(slg)]
  o[cnd] <- number_line(l = end_point(x[cnd]), r = left_point(o[cnd]))

  slg <- overlaps(as.number_line(end_point(y[lg])), x[lg])
  slg[is.na(slg)] <- F
  cnd <- lg[which(slg)]
  o[cnd] <- number_line(l = end_point(y[cnd]), r = left_point(o[cnd]))

  dir <- ifelse(abs(x@.Data[lg]) > abs(y@.Data[lg]), x@.Data[lg]/abs(x@.Data[lg]), y@.Data[lg]/abs(y@.Data[lg]))
  o[lg] <- reverse_number_line(o[lg], direction= ifelse(dir == 1, "decreasing", "increasing"))
  return(o[-1])
}

#' @rdname set_operations
#' @examples
#'
#' # Subtract
#' nl_3; nl_4; subtract_number_lines(nl_3, nl_4)
#'
#' @export
subtract_number_lines <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_match_ref_len(y, "x", c(1, length(y)), "y")
  if(err != F) stop(err, call. = F)

  if(length(y) == 1) y <- rep(y, length(x))

  x <- c(x[1], x); y <- c(x[1], y)
  o2 <- o1 <- c(x[1], as.number_line(rep(NA_real_, length(x)-1)))

  lg <- overlap(x, y)
  lg[is.na(lg)] <- F
  lg <- which(lg)

  slg <- overlaps(as.number_line(start_point(x[lg])), y[lg])
  slg[is.na(slg)] <- F
  cnd <- lg[which(!slg)]
  o1[cnd] <- number_line(l = start_point(x[cnd]), r = start_point(y[cnd]))

  slg <- overlaps(as.number_line(start_point(y[lg])), x[lg])
  slg[is.na(slg)] <- F
  cnd <- lg[which(!slg)]
  o1[cnd] <- number_line(l = start_point(y[cnd]), r = start_point(x[cnd]))

  slg <- overlaps(as.number_line(end_point(x[lg])), y[lg])
  slg[is.na(slg)] <- F
  cnd <- lg[which(!slg)]
  o2[cnd] <- number_line(l= end_point(y[cnd]), r = end_point(x[cnd]))

  slg <- overlaps(as.number_line(end_point(y[lg])), x[lg])
  slg[is.na(slg)] <- F
  cnd <- lg[which(!slg)]
  o2[cnd] <- number_line(l = end_point(x[cnd]), r = end_point(y[cnd]))

  list(
    n1 = o1[-1],
    n2 = o2[-1]
  )
}

