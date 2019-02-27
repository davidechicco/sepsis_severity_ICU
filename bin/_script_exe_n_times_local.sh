#!/bin/bash
#
#$ -cwd
#$ -S /bin/bash
#
set -o nounset -o pipefail -o errexit
set -o xtrace

iteTot=10
random_numberA=$(shuf -i1-100000 -n1)

i=1
outputFile=""
target="sepsis-shock"
subdir="../results/"$target"_rand"$random_numberA"/"
mkdir -p $subdir

for i in $( seq 1 $iteTot )
do

  echo $i
  today=`date +%Y-%m-%d`
  random_numberB=$(shuf -i1-100000 -n1)
  jobName=$target"_"$today"_rand"$random_numberB
  outputFile=$subdir$jobName
  /usr/bin/Rscript random_forest_classifier_sepsis_shock.r > $outputFile 2> $outputFile
done

i=1
outputFile=""
target="survival"
subdir="../results/"$target"_rand"$random_numberA"/"
mkdir -p $subdir

for i in $( seq 1 $iteTot )
do

  echo $i
  today=`date +%Y-%m-%d`
  random_numberB=$(shuf -i1-100000 -n1)
  jobName=$target"_"$today"_rand"$random_numberB
  outputFile=$subdir$jobName
  /usr/bin/Rscript random_forest_classifier_survival.r > $outputFile 2> $outputFile
done

i=1
outputFile=""
target="SOFA-score"
subdir="../results/"$target"_rand"$random_numberA"/"
mkdir -p $subdir

for i in $( seq 1 $iteTot )
do

  echo $i
  today=`date +%Y-%m-%d`
  random_numberB=$(shuf -i1-100000 -n1)
  jobName=$target"_"$today"_rand"$random_numberB
  outputFile=$subdir$jobName
  /usr/bin/Rscript random_forest_classifier_SOFA_score.r > $outputFile 2> $outputFile
done
