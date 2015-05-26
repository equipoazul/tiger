currentdir=`pwd`
tests=$currentdir/tests/type
cd $tests
elese=`ls`
cd $currentdir


for f in $elese; do 
  echo $tests/$f
  cat $tests/$f
  ./tiger $tests/$f
  echo "-------------------------------------------------\n"
done
