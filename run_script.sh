  #!/bin/bash
  #no of samplesi
   #echo "wait for 180min"
   #sleep 180m

  #no of differetn runs
  nruns=200
 
  #inital value of i
  i=1
 
  while [ $i -le $nruns ]
  do
  #cp -r code/ $i/
  #cd init/
  #sh init.sh
  #rm *.in
  #./init.exe >init.out

  # set value of gap here
  # "a\nb\nc\n" if you want to use three inputs a b and c, \n takes to the next line. for space you     may have to use somethingelse
  #first value of i and gap is printed using command printf '%d \n 2 \n %f \n' $i $gap, which is use    d in ./init.exe. %d for integer, %f for real no we can also use
  #printf '1 \n 2 \n 0.11 \n' |./init.exe
  #we can't use following
  # printf '$i \n 2 \n $gap \n' |./init.exe
  #don't print output of files on terminal, it will give error
  #printf '%d \n 2 \n %f \n' $i $gap |./init.exe >init.out

  cd maincode/
  sh compile.sh
  printf '%d \n' $i | ./simulate.exe >code.out
  cd ../
	 


  #if [ $(( $i % 10 )) -eq 0 ]; then
   #    echo "wait for 90min"
    #      sleep 90m
  #fi
 
  ((i++))
  done
