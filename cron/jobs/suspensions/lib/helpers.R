# Look up school abbreviations from id numbers 
school_id_to_abbrev <- function(.data){
  
  look_up <- function(x){
    z <- switch(as.character(x), 
                "78102"  = "KAP",
                "7810"   = "KAMS",
                "400146" = "KCCP",
                "400163" = "KBCP"
    )
    z
  }
  out <- mapply(look_up, .data)
  as.character(out)
}