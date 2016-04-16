#!/bin/bash
FILE=""
DIR="/usr/sap/V01"
DIR1 = "$DIR/R_Requirement"
# init
# look for dir [ "$(ls -A $DIR/R_Stock/)"] &&
while true; do
if [ "$(ls -A $DIR1)" ]; then
echo "Running R script at `date`"$'\r' >> $DIR/R_App/log
echo `Rscript $DIR/R_App/optimPart.R $DIR` >> $DIR/R_App/log
echo `mv $DIR/R_Stock/* $DIR/R_Archive`
echo `mv $DIR/R_Requirement/* $DIR/R_Archive`
fi
sleep 2
done
