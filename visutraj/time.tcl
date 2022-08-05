proc enabletrace {} { 
  global vmd_frame; 
  trace variable vmd_frame([molinfo top]) w drawcounter 
} 

proc disabletrace {} { 
  global vmd_frame; 
  trace vdelete vmd_frame([molinfo top]) w drawcounter 
} 
proc drawcounter { name element op } { 
  global vmd_frame; 
  draw delete all 
  # puts "callback!" 
  draw color white
  set psperframe 0.04 
  set psoffset 0 
  set time [format "%5.2fsec" [expr ($vmd_frame([molinfo top]) * $psperframe) + $psoffset]] 
  draw text {-3 13.5 1 } "Time="
  draw text {-0.5 13.5 1 } "$time" 
#change values to adjust clock position
} 


