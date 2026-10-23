# Module entry points. Each returns list(controls, nav, stage) for the shell.

home_mod <- function(id) list(controls = NULL, nav = NULL, stage = pl_home())

lca_mod <- function(id, project) lca_ui(project)
lpa_mod <- function(id, project) lpa_ui(project)
lta_mod <- function(id, project) lta_ui(project)
efa_mod <- function(id, project) efa_ui(project)
cfa_mod <- function(id, project) cfa_ui(project)
