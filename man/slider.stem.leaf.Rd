 \name{slider.stem.leaf}      
 \alias{slider.stem.leaf}
 \title{                                                                
   construction of stem and leaf display interactively
 }                                                                       
 \description{ 
   'slider.stem.leaf' computes a stem and leaf display within a 
   graphics device. The parameters are controlled by a control widget.
 }                                                                       
 \usage{                                                                
 slider.stem.leaf(x, main = main)                                        
 }                                                                       
\arguments{                                                            
   \item{x}{                                                            
   data set for plotting
 }                                                                       
   \item{main}{                                                         
   main title of the plot
 }                                                                       
 }                                                                       
 \details{                                                              
   The function 'slider.stem.leaf' allows the user to construct
   a stem and leaf display within a graphics device. The main parameters 
   will be set by a Tcl/Tk control widget. The line rule is selected
   by pressing one of the buttons 'Dixon', 'Sturges', 'Velleman'.
   A slider controls the separation of the stem.
   Additionally the character size device could be set. 
 }                                                                       
\value{                                                                
   a short message is returned
 }                                                                       
\author{                                                               
   Peter Wolf, Nov 2009
 }                                                                       
\note{                                                                 
   The function is a function of the package aplpack
 }  
 \seealso{\code{\link[graphics]{stem}}}
 \examples{     
 \dontrun{    
   slider.stem.leaf(islands)
 }
 }
\keyword{misc}                                                      
