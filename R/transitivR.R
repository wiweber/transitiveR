require(tidyverse)

#' transitive.closure
#'
#' @param data
#' @param from
#' @param to
#' @param root
#' @param as.factors
#'
#' @return
#' @export
#'
#' @examples
transitive.closure <- function(po, from, to, root = NULL, as.factors = F){

    po <- dplyr::as.tbl(po)

    from <- dplyr::enquo(from)
    to <- dplyr::enquo(to)

    # check 4 reflexive relations
    reflexiv.relation <- po %>% dplyr::filter(!!from == !!to)
    if(nrow(reflexiv.relation) > 0)
      warning(paste0(nrow(reflexiv.relation), " reflexive relation(s) detected!"))

    # check 4 direct cyclic relations
    cyclic.relation <- po %>% semi_join(po, by = c(quo_name(to), quo_name(from)) %>% setNames(rev(.))) %>% filter(!!from != !!to)
    if(nrow(cyclic.relation) > 0)
      stop(paste0((nrow(cyclic.relation)/2), " cyclic relation detected!"))

    rm(reflexiv.relation)
    rm(cyclic.relation)

    if(is.null(root))
      root <- po %>% dplyr::filter(!(!!to %in% po[[dplyr::quo_name(from)]])) %>% dplyr::pull(!!from) %>% unique()
    else
      if(nrow(po %>% dplyr::filter(!!from == root)) == 0)
        stop(paste0(root, " as root is unknown!"))

    result <- dplyr::tibble(!!from := character(0), !!to := character(0))

    tc <- function(value){
      # reflexive pair
      r <- tibble(!!from := value, !!to := value)
      childs <- po %>% filter(!!to == value, !!to != !!from) %>% pull(!!from)
      for (child in childs) {
        cr <- tc(child)
        r <- bind_rows(r, cr) %>% bind_rows(tibble(!!from := unique(cr %>% pull(!!from)), !!to := value))
      }
      return(r)
    }

    for (r in root) {
      result <- bind_rows(tc(r))
    }

    unique(result) %>% arrange(!!from, !!to)

}

#' transitive.summary
#'
#' @param tr
#' @param from
#' @param to
#' @param extended
#' @param level_suffix
#' @param path_sep
#'
#' @return
#' @export
#'
#' @examples
transitive.summary <- function(tr, from, to, extended = F, level_suffix = 'L_', path_sep = "*"){

  tr <- as.tbl(tr)

  from <- dplyr::enquo(from)
  to <- dplyr::enquo(to)

  tr.sum <- tr %>% dplyr::group_by(!!from) %>% dplyr::summarise(level = n()) %>%
    dplyr::inner_join(
      tr %>% dplyr::group_by(!!from := !!to) %>% dplyr::summarise(childs = n() - 1, is_child = childs == 0)
      ,by = dplyr::quo_name(from)
    )

  if(extended){

    tr.h <- tr %>%
      dplyr::inner_join(tr.sum, by = c(dplyr::quo_name(from)) %>% setNames(dplyr::quo_name(to))) %>%
      dplyr::arrange(!!from, level)

    tr.sum <- tr.sum %>%
      dplyr::inner_join(
        tr.h %>%
          dplyr::group_by(!!from) %>%
          dplyr::summarise(path = paste0(!!to, collapse = path_sep))
        ,by = dplyr::quo_name(from)
      ) %>%
      inner_join(
        tr.h %>%
          dplyr::select(!!from, level, !!to) %>%
          dplyr::mutate(level = paste0(level_suffix, level)) %>%
          tidyr::spread(level, !!to, convert = F)
        ,by = dplyr::quo_name(from)
      ) %>%
      dplyr::arrange(path)

  }

  return(tr.sum)

}
