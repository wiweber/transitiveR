require(tidyverse)

#' transitive.closure
#'
#' @param data
#' @param from
#' @param to
#' @param roots
#'
#' @return
#' @export
#'
#' @examples
transitive.closure <- function(po, from, to, roots = NULL){

  po <- dplyr::as.tbl(unique(po))

  from <- dplyr::enquo(from)
  to <- dplyr::enquo(to)

  # validations -------------------------------------------------------------

  # check 4 reflexive relations
  reflexiv.relation <- po %>% dplyr::filter(!!from == !!to)
  if(nrow(reflexiv.relation) > 0)
    warning(paste0(nrow(reflexiv.relation), " reflexive relation(s) detected!"))

  # check 4 direct cyclic relations
  cyclic.relation <- po %>% semi_join(po, by = c(quo_name(to), quo_name(from)) %>% setNames(rev(.))) %>% filter(!!from != !!to)
  if(nrow(cyclic.relation) > 0)
    stop(paste0((nrow(cyclic.relation)/2), " direct cyclic relation(s) detected!"))

  #check 4 multiple parents
  multiple.parents <- po %>% filter(!!from != !!to & duplicated(!!from)) %>% pull(!!from)
  if(length(multiple.parents) > 0){
    err = po %>%
      filter(!!from %in% multiple.parents) %>%
      group_by(!!from) %>%
      summarise(p = paste0(first(!!from), "->(",paste0(!!to, collapse = ","),")")) %>%
      pull(p) %>% paste0(collapse = ",") %>% paste0("Multiple parents detected: ", . )
    stop(err)
  }

  rm(multiple.parents)
  rm(reflexiv.relation)
  rm(cyclic.relation)

  # logic -------------------------------------------------------------------

  if(is.null(roots))
    roots <- po %>% dplyr::filter(!(!!to %in% po[[dplyr::quo_name(from)]])) %>% dplyr::pull(!!to) %>% unique()

  if(nrow(po %>% dplyr::filter(!!to %in% roots)) == 0)
    stop(paste0(paste0(root, collapse = ","), " as root is invalid!"))

  result <- dplyr::tibble()

  tc <- function(value, parents){

    parents <- c(value, parents)
    childs <- po %>% filter(!!to == value & !!to != !!from) %>% pull(!!from)

    #search for indirect cyclic relation
    if(any(parents %in% childs)){
      stop(paste0("Indirect cyclic relation between: ", paste0(parents[parents %in% childs], " <-> ", value, collapse = ", ") ," detected!"))
    }

    # reflexive pair
    r <- tibble(!!from := value, !!to := value)

    for (child in childs)
      r <- tc(child, parents) %>% bind_rows(tibble(!!from := unique((pull(., !!from))), !!to := value)) %>% bind_rows(r)

    return(r)
  }

  for (r in roots) {
    result <- bind_rows(result, tc(r, c()))
  }

  return(unique(result) %>% arrange(!!from, !!to))
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
