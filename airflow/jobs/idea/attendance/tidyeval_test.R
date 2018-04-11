x_save <- function(obj) {
  obj_q <- enexpr(obj)
  obj_name <- quo_name(obj_q)
  
  path <- sprintf("/data/%s.Rda", obj_name)

  save_expr <- expr(save(!!obj_q, file = path, envir = .GlobalEnv))  
  
  eval_tidy(save_expr)
}