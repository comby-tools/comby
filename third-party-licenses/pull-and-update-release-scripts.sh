#!/bin/bash

LIBS="ppx_deriving_yojson core ppxlib ppx_deriving hack_parallel opium pcre-ocaml ocaml-tls camlzip bisect_ppx mparser ocaml-ci-scripts patdiff"

rm ALL.txt 2> /dev/null
for l in $LIBS; do rm -rf $l; done

# MIT
mkdir patdiff && \
wget -P patdiff https://raw.githubusercontent.com/janestreet/patdiff/master/LICENSE.md

# MIT
mkdir ppx_deriving_yojson && \
wget -P ppx_deriving_yojson https://raw.githubusercontent.com/ocaml-ppx/ppx_deriving_yojson/master/LICENSE.txt

# MIT
mkdir core && \
wget -P core https://raw.githubusercontent.com/janestreet/core/master/LICENSE.md

# MIT
mkdir ppxlib && \
wget -P ppxlib https://raw.githubusercontent.com/ocaml-ppx/ppxlib/master/LICENSE.md

# MIT
mkdir ppx_deriving && \
wget -P ppx_deriving https://raw.githubusercontent.com/ocaml-ppx/ppx_deriving/master/LICENSE.txt

# MIT
mkdir hack_parallel && \
wget -P hack_parallel https://raw.githubusercontent.com/rvantonder/hack-parallel/master/LICENSE

# MIT
mkdir opium && \
wget -P opium https://raw.githubusercontent.com/rgrinberg/opium/master/opium.opam

# LGPL
mkdir pcre-ocaml && \
wget -P pcre-ocaml https://raw.githubusercontent.com/mmottl/pcre-ocaml/master/LICENSE.md

# BSD-2
mkdir ocaml-tls && \
wget -P ocaml-tls https://raw.githubusercontent.com/mirleft/ocaml-tls/master/LICENSE.md

# LGPL
mkdir camlzip && \
wget -P camlzip https://raw.githubusercontent.com/xavierleroy/camlzip/master/LICENSE

# MIT
mkdir bisect_ppx && \
wget -P bisect_ppx https://raw.githubusercontent.com/aantron/bisect_ppx/master/LICENSE.md

# LGPL
mkdir mparser && \
wget -P mparser https://raw.githubusercontent.com/comby-tools/mparser/master/LICENSE.txt

# ISC
mkdir ocaml-ci-scripts && \
wget -P ocaml-ci-scripts https://raw.githubusercontent.com/ocaml/ocaml-ci-scripts/master/LICENSE.md


# ALL.txt
for l in $LIBS; do 
    F=$(ls $l | head -n 1)
    echo "LICENSE FOR $l:" >> ALL.txt
    echo "" >> ALL.txt
    cat $l/$F >> ALL.txt
    echo "" >> ALL.txt
done

# update release script
cp ../scripts/install.sh .
printf "\n\n# comby license\n" >> install.sh
echo "#" >>  install.sh
sed 's/^/# /' ../LICENSE >> install.sh
echo "" >> install.sh
printf "# Begin third party licenses\n\n" >> install.sh
sed 's/^/# /' ALL.txt >> install.sh
mv install.sh ../scripts/install-with-licenses.sh
