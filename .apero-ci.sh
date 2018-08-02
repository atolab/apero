sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

OPAM_DEPENDS="lwt dune logs fmt cmdliner"
opam init 
opam update
opam upgrade
opam install ${OPAM_DEPENDS}
eval `opam config env`
make