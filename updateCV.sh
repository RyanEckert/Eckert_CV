#! /bin/bash
################################################################################
## This shell script will render .html and .pdf versions of the CV using the  ##
## the 'render_cv.R' script then commit updates and push the origin to GitHub ##                                                                   ##
## Place it in the directory with 'render_cv.R' and your CV .Rmd files        ##
## Make this executable if not already, "chmod +x updateCV.sh"                ##
## Execute with "./updateCV.sh" while in the cv working directory             ##
################################################################################

Rscript render_cv.R
git add *
git commit -m "update cv"
git push origin master
echo "  "
echo "DONE!"
echo "  "
