#!/bin/bash
FILE=""
DIR="/usr/sap/V01"
# init
# look for empty dir 
while true; do
if [ "$(ls -A $DIR/R_Stock/)" && "$(ls -A $DIR/R_Requirement/)"]; then
echo "Running R script at `date`"$'\r'
echo `Rscript $DIR/R_App/optim.R $DIR`
echo `mv $DIR/R_Stock/* R_Archive/`
echo `mv $DIR/R_Requirement/* R_Archive/`
fi
sleep 2
done
