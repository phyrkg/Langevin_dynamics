  #!/bin/bash
  #no of differetn runs
  nruns=200
 
  #inital value of i
  i=1
  rm noofsteps.dat
  #cd ../traject/
  while [ $i -le $nruns ]
  do

  wc -l < ../traject/totaltimesteps$i.dat |tee -a noofsteps.dat
  #printf '\n' > noofsteps.dat
	 
  ((i++))
  done
