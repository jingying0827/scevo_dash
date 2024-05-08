#!/bin/bash

export WITH_RENV="true"
export DO_DEPLOY="true"

while [ $# -gt 0 ] ; do
  case $1 in
    --no-renv)
      export WITH_RENV="false"
      ;;
    --dont-deploy)
      export DO_DEPLOY="false"
      ;;
    *)
      ;;
  esac
  shift
done

if [ "$WITH_RENV" != "true" ] ; then
  echo "### building without renv"
# echo "### WARNING : this will currently not work ####"
fi

/bin/rm -r deploy >& /dev/null
cd dashboard

/bin/rm -r .R* renv* .dockerignore deploy Dockerfile >& /dev/null
## echo 'options(repos=c(CRAN="https://packagemanager.posit.co/cran/2023-04-20/"))' > .Rprofile
## echo 'INSTALL_opts=c("--no-lock")' > .Rprofile

echo '################ step 1 #######################'
date

# The echo ZZ is because the following step produces a README file and calls vi to
# allow you edit it; we would rather not do that when running rom a scripts
# The tr strips escape codes (not very well) from the output of vi

if [ "$WITH_RENV" = "true" ] ; then
  echo ZZ | Rscript ../build_dash_renv.rs | tr -d '\^['

  if [ ! -d ../deploy ] ; then
    echo "error building package"
    exit 1
  fi
  if [ ! -f ../deploy/Dockerfile ] ; then
    echo "error creating Dockerfile"
    exit 1
  fi

  sed -i -e 's/COPY scevo/COPY www\/ \/www\/\nCOPY scevo/' ../deploy/Dockerfile
  sed -i -e 's/upgrade="never")/upgrade="never",INSTALL_opts = c("--no-lock"))/' ../deploy/Dockerfile
else
  echo ZZ | Rscript ../build_dash_no_renv.rs | tr -d '\^['

  if [ ! -d ../deploy ] ; then
    mkdir ../deploy
  fi

  if [ ! -f ../scevo_*.tar.gz ] ; then
    echo "error building package"
    exit 1
  fi
  if [ ! -f Dockerfile ] ; then
    echo "error creating Dockerfile"
    exit 1
  fi

  /bin/cp Dockerfile ../deploy/Dockerfile-orig
  /bin/mv ../scevo_*.tar.gz ../deploy
  sed -i -e 's!FROM rocker/r-ver$!FROM rocker/r-ver:4.2.1!' Dockerfile
  cat Dockerfile | sed '/^RUN mkdir \/build_zone$/,$d' > ../deploy/Dockerfile_base
  sed -i -e 's/install -y /install -y  libfontconfig1-dev cmake/' ../deploy/Dockerfile_base

  cat << EOF > ../deploy/Dockerfile
FROM scevo_base

RUN R -e 'remotes::install_github("dbca-wa/rivRmon")'

COPY www/ /www/
COPY scevo_*.tar.gz /app.tar.gz

RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never",INSTALL_opts = c("--no-lock"))'
RUN rm -f /app.tar.gz
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');library(scevo);scevo::run_app()"]
EOF
/bin/rm Dockerfile

fi

/bin/cp -r www ../deploy/

cat << EOF > ../deploy/deploy.sh
echo '################ deploy scevo_base #######################'
date
docker build -f Dockerfile_base --progress=plain -t scevo_base .

echo '################ deploy scevo #######################'
date
docker build -f Dockerfile --progress=plain -t scevo:latest .

echo '################ all done ######################'
date

exit 0
EOF
chmod +x ../deploy/deploy.sh

if [ "$DO_DEPLOY" = "true" ] ; then
  cd ../deploy

  ./deploy.sh

fi

echo '################ all done ######################'
date

exit 0
