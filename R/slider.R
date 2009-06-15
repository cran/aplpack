slider<-
function (sl.functions, sl.names, sl.mins, sl.maxs, sl.deltas, 
    sl.defaults, but.functions, but.names, no, set.no.value, 
    obj.name, obj.value, reset.function, title, prompt=FALSE) 
{ # pwolf 080310 080603
  slider.env<-"1"; rm("slider.env")
  if (!exists("slider.env")) slider<-slider.env<<-new.env(parent=.GlobalEnv)
  if (!missing(no)) 
          return(as.numeric(tclvalue(get(paste("slider", no, sep = ""), 
              env = slider.env))))
  if (!missing(set.no.value)) {
          try(eval(parse(text = paste("tclvalue(slider", set.no.value[1], 
              ")<-", set.no.value[2], sep = "")), env = slider.env))
          return(set.no.value[2])
  }
  if (!missing(obj.name)) {
          if (!missing(obj.value)) 
              assign(obj.name, obj.value, env = slider.env)
          else obj.value <- get(obj.name, env = slider.env)
          return(obj.value)
  }
  if (missing(title))        title<-"slider control widget"
  if (missing(sl.names))     {sl.defaults <- sl.names <- NULL}
  if (missing(sl.functions)) sl.functions <- function(...) {  }
   

  require(tcltk)
  nt <- tktoplevel()
  tkwm.title(nt, title)
  tkwm.geometry(nt, "+0+15")
  assign("tktop.slider",nt,env=slider.env)

  "relax"  
  for (i in seq(sl.names)) {
     "relax"
     eval(parse(text=paste("assign('slider", i, 
                           "',tclVar(sl.defaults[i]),env=slider.env)",sep="")
     ))
  }
  for (i in seq(sl.names)) {
     tkpack(fr <- tkframe(nt))
     lab <- tklabel(fr, text = sl.names[i], width = "25")
     sc <- tkscale(fr, from = sl.mins[i], to = sl.maxs[i], 
     showvalue = TRUE, resolution = sl.deltas[i], orient = "horiz")
     tkpack(lab, sc, side = "right")
     assign("sc", sc, env = slider.env)
     eval(parse(text=paste("tkconfigure(sc,variable=slider", 
                           i, ")", sep = "")), env = slider.env)
     sl.fun<-if(length(sl.functions)>1)sl.functions[[i]] else sl.functions
     if(!is.function(sl.fun)) 
       sl.fun <- eval(parse(text = paste("function(...){", sl.fun, "}")))
     if(prompt) tkconfigure(sc,command=sl.fun) else tkbind(sc,"<ButtonRelease>",sl.fun)
  }

  assign("slider.values.old", sl.defaults, env = slider.env)
  tkpack(f.but <- tkframe(nt), fill = "x")
  tkpack(tkbutton(f.but, text = "Exit", 
         command = function() tkdestroy(nt)), side = "right")
  if(!missing(reset.function)){
    # reset.function <- function(...) print("relax")
    if(!is.function(reset.function)) 
      reset.function <- eval(parse(text = 
          paste("function(...){",reset.function, "}")))
    tkpack(tkbutton(f.but,text="Reset", command = function(){
         for (i in seq(sl.names)) 
           eval(parse(text = 
             paste("tclvalue(slider", 
                   i, ")<-", sl.defaults[i], 
                   sep = "")), env = slider.env)
         reset.function()
    }), side = "right")
  }

  if (missing(but.names)) but.names <- NULL
  for(i in seq(but.names)) {
    but.fun<-if(length(but.functions)>1)but.functions[[i]] else but.functions
    if(!is.function(but.fun)) 
      but.fun<-eval(parse(text = c("function(...){",but.fun,"}")))
    tkpack(tkbutton(f.but, text = but.names[i], command = but.fun), 
                    side = "left")
  }


  invisible(nt)
}

