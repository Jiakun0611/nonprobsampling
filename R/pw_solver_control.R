#' Control Solver Settings for Pseudo-Weight Estimation
#'
#' @description
#' \code{pw_solver_control()} creates a solver control object used by
#' \code{\link{est_pw}} to manage numerical settings for pseudo-weight
#' estimation.
#'
#' @details
#' The control object stores solver settings used by pseudo-weight estimation
#' stage. It is passed to \code{\link{est_pw}} through the \code{control}
#' argument.
#'
#' Currently, only \code{solver = "nleqslv"} is supported. The arguments
#' \code{method}, \code{global}, \code{xscalm}, \code{ftol}, \code{xtol}, and
#' \code{maxit} correspond to options used by \code{nleqslv::nleqslv()}.
#' They are collected internally and passed to \code{nleqslv::nleqslv()} by the
#' pseudo-weight estimation stages.
#'
#' The argument \code{ftol} is the function-value convergence tolerance. It
#' controls convergence based on the size of the estimating function. The
#' argument \code{xtol} is the parameter-step convergence tolerance. It controls
#' convergence based on changes in the parameter vector. The argument
#' \code{maxit} controls the maximum number of solver iterations.
#'
#' Additional, less commonly used \code{nleqslv} control options can be supplied
#' through \code{nleqslv_control}. To avoid ambiguity, do not supply
#' \code{ftol}, \code{xtol}, \code{maxit}, or \code{trace} inside
#' \code{nleqslv_control}; use the main arguments instead.
#'
#' @param solver Character string specifying the numerical solver used for
#'   solving the estimating equations. Currently, only \code{"nleqslv"} is
#'   supported. Default is \code{"nleqslv"}.
#'
#' @param maxit Positive finite numeric value passed to
#'   \code{nleqslv::nleqslv()} as the maximum number of solver iterations. The
#'   value is converted to an integer before being stored in the returned
#'   control object. Default is \code{150} when a global strategy is specified
#'   (i.e., \code{global != "none"}), and \code{20} when no global strategy is
#'   used (\code{global = "none"}), matching \code{nleqslv}'s own defaults.
#'   Since the default global strategy is \code{"dbldog"}, the effective default
#'   is \code{150} unless \code{global = "none"} is explicitly specified.
#'
#' @param trace Logical. If \code{TRUE}, tracing or solver progress information
#'   may be requested from the underlying numerical routine when supported.
#'   Default is \code{FALSE}.
#'
#' @param method Character string specifying the numerical method passed to
#'   \code{nleqslv::nleqslv()}. Supported values are \code{"Newton"} and
#'   \code{"Broyden"}. Default is \code{"Newton"}.
#'
#' @param global Character string specifying the global strategy passed to
#'   \code{nleqslv::nleqslv()}. Supported values are \code{"dbldog"},
#'   \code{"cline"}, \code{"pwldog"}, \code{"qline"}, \code{"gline"},
#'   \code{"hook"}, and \code{"none"}. Default is \code{"dbldog"}.
#'
#' @param xscalm Character string specifying the scaling method passed to
#'   \code{nleqslv::nleqslv()}. Supported values are \code{"fixed"} and
#'   \code{"auto"}. Default is \code{"fixed"}.
#'
#' @param ftol Positive finite numeric value passed to
#'   \code{nleqslv::nleqslv()} as the function-value convergence tolerance. This
#'   controls convergence based on the size of the estimating function. Default
#'   is \code{1e-8}.
#'
#' @param xtol Positive finite numeric value passed to
#'   \code{nleqslv::nleqslv()} as the parameter-step convergence tolerance. This
#'   controls convergence based on changes in the parameter vector. Default is
#'   \code{1e-8}.
#'
#' @param nleqslv_control A list of additional control options
#'   passed to \code{nleqslv::nleqslv()}. This can include less commonly used
#'   control options, such as \code{btol}, \code{cndtol},
#'   \code{sigma}, and \code{scalex}. See
#'   \code{\link[nleqslv]{nleqslv}} for details.
#'
#' @return
#' A flat list containing all solver control settings for pseudo-weight
#' estimation:
#'
#' \describe{
#'   \item{\code{solver}}{The selected numerical solver.}
#'   \item{\code{method}}{The nleqslv numerical method.}
#'   \item{\code{global}}{The nleqslv global strategy.}
#'   \item{\code{xscalm}}{The nleqslv scaling method.}
#'   \item{\code{ftol}}{The function-value convergence tolerance.}
#'   \item{\code{xtol}}{The parameter-step convergence tolerance.}
#'   \item{\code{maxit}}{The maximum number of solver iterations, stored as an integer.
#'     \code{150} if a global strategy is used; \code{20} if \code{global = "none"}.
#'     Since the default global strategy is \code{"dbldog"}, the effective default
#'     is \code{150} unless \code{global = "none"} is explicitly specified.}
#'   \item{\code{trace}}{Logical value indicating whether tracing information is requested.}
#'   \item{\code{nleqslv_control}}{A list of additional options passed to
#'     \code{nleqslv::nleqslv()}.}
#' }
#'
#' @seealso
#' \code{\link{est_pw}}
#'
#' @examples
#' ## Default solver control settings
#' ctrl <- pw_solver_control()
#'
#' ## Custom nleqslv solver settings
#' ctrl <- pw_solver_control(
#'   maxit  = 20,
#'   trace  = FALSE,
#'   method = "Newton",
#'   global = "cline",
#'   xscalm = "auto",
#'   ftol   = 1e-8,
#'   xtol   = 1e-10
#' )
#'
#' ## Additional nleqslv control options
#' ctrl <- pw_solver_control(
#'   method = "Newton",
#'   global = "dbldog",
#'   nleqslv_control = list(
#'     btol = 1e-3
#'   )
#' )
#'
#' @export
pw_solver_control <- function(
    solver = "nleqslv",
    maxit = NULL,
    trace = FALSE,
    method = c("Newton", "Broyden"),
    global = c("dbldog", "cline", "pwldog", "qline", "gline", "hook", "none"),
    xscalm = c("fixed", "auto"),
    ftol = 1e-8,
    xtol = 1e-8,
    nleqslv_control = list()
) {

  if (!is.character(solver) || length(solver) != 1L || is.na(solver)) {
    stop("`solver` must be a single character string.", call. = FALSE)
  }

  solver <- tolower(trimws(solver))

  if (solver != "nleqslv") {
    stop("Only solver = 'nleqslv' is currently supported.", call. = FALSE)
  }

  if (!is.logical(trace) || length(trace) != 1L || is.na(trace)) {
    stop("`trace` must be TRUE or FALSE.", call. = FALSE)
  }

  method <- match.arg(method)
  global <- match.arg(global)
  xscalm <- match.arg(xscalm)

  if (is.null(maxit)) {
    maxit <- if (global == "none") 20L else 150L
  }

  if (!is.numeric(maxit) || length(maxit) != 1L || !is.finite(maxit) || maxit <= 0) {
    stop("`maxit` must be a positive finite number.", call. = FALSE)
  }

  if (!is.numeric(ftol) || length(ftol) != 1L || !is.finite(ftol) || ftol <= 0) {
    stop("`ftol` must be a positive finite number.", call. = FALSE)
  }

  if (!is.numeric(xtol) || length(xtol) != 1L || !is.finite(xtol) || xtol <= 0) {
    stop("`xtol` must be a positive finite number.", call. = FALSE)
  }

  if (!is.list(nleqslv_control)) {
    stop("`nleqslv_control` must be a list.", call. = FALSE)
  }

  duplicated_control_names <- intersect(
    names(nleqslv_control),
    c("ftol", "xtol", "maxit", "trace")
  )

  if (length(duplicated_control_names) > 0L) {
    stop(
      paste0(
        "Do not supply ",
        paste(sprintf("`%s`", duplicated_control_names), collapse = ", "),
        " inside `nleqslv_control`."
      ),
      call. = FALSE
    )
  }

  list(
    solver          = solver,
    method          = method,
    global          = global,
    xscalm          = xscalm,
    ftol            = ftol,
    xtol            = xtol,
    maxit           = as.integer(maxit),
    trace           = trace,
    nleqslv_control = nleqslv_control
  )
}
