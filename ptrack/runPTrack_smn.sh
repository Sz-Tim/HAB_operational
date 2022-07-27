#! /usr/bin/env sh

NPARAMS=1
if [ "$#" -ne $NPARAMS ]; then
	echo
	echo "Must supply:"
	echo "(1) .properties file name"
	echo
	exit
fi

pwd

#OUTDIR=output/${3}
# Revised: get the output directory from the properties file
OUTDIR=`sed -n -e '/^destinationDirectory/p' $1 | cut -d "=" -f 2 | xargs`
echo
echo "output directory = "$OUTDIR
#mkdir $OUTDIR 
cd $OUTDIR

# Copy the properties file
cp ../${1} .
# Copy the site file

SITEFILE=`sed -n -e '/^sitefile/p' $1 | cut -d "=" -f 2 | xargs`
echo "sitefile = "$SITEFILE

/usr/local/java/jre1.8.0_211/bin/java -Xmx8192m -Xms4096m -jar /home/sa04ts/biotracker/particle_track.jar ${1} > stdout.txt

mkdir arrivals
mv arrivals_* arrivals/
mkdir connectivity
mv connectivity_* connectivity/
mkdir pstepsI
mv pstepsI* pstepsI/
mkdir pstepsM
mv pstepsM* pstepsM/
mkdir mvmt
mv movement* mvmt/
mkdir locs
mv locations* locs/

