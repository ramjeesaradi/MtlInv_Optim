#!/bin/bash
DIR="/usr/rfolder"
DIR1="$DIR/R_Requirement"
DIR2="$DIR/R_Stock"
# init
# look for dir [ "$(ls -A $DIR/R_Stock/)"] &&
while true; do
if [ "$(ls -A $DIR1)" ] && [ "$(ls -A $DIR2)" ]; then
for f_Req in $DIR1/*
do	
f_Stock=${f_Req/_Req_/_Stock_}
f_Stock=${f_Stock/_Requirement/_Stock}
echo $f_Req
echo $f_Stock
echo "Running R script at `date`" >> $DIR/R_App/log
echo `su -c "Rscript $DIR/R_App/optimPart.R $DIR '$f_Req' '$f_Stock'"` >> $DIR/R_App/log
echo `su -c "mv '$f_Req' $DIR/R_Archive"`
echo `su -c "mv '$f_Stock' $DIR/R_Archive"`
done
fi
sleep 2
done
