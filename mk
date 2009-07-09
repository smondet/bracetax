#! /bin/sh

build ()
{
    TAGOPT="-tags pkg_unix"
    I_OPT="-I src/app -I src/lib"
    ocamlbuild $I_OPT $TAGOPT -cflags -dtypes src/app/main$1.byte ocamlbracetax.cma
    rm -f brtx
    ln -s main$1.byte brtx
}
build_no_ocamlbuild ()
{
    CCLIB="ocamlc -for-pack Bracetax -c -I src/lib/"
    $CCLIB    src/lib/signatures.ml
    $CCLIB    src/lib/escape.ml
    $CCLIB    src/lib/commands.ml
    $CCLIB    src/lib/transformer.ml
    $CCLIB    src/lib/latexPrinter.ml
    $CCLIB    src/lib/htmlPrinter.ml
    $CCLIB    src/lib/info.ml
    ocamlc -pack -o bracetax.cmo  \
      src/lib/signatures.cmo \
      src/lib/escape.cmo \
      src/lib/commands.cmo \
      src/lib/transformer.cmo \
      src/lib/latexPrinter.cmo \
      src/lib/htmlPrinter.cmo \
      src/lib/info.cmo
    ocamlc -o brtx -I src/app/ -I . -I src/lib/ unix.cma bracetax.cmo \
        src/app/postProcessor.ml src/app/main.ml
}

echo_help ()
{
    echo "\
$0 <cmd>
b: Build brtx (default action)
bg: Build brtx with debug symbols
bc: Build brtx without ocamlfind and ocamlbuild (e.g. with ocaml 3.09.x)
t: Do the tests
d: Build the documentation without building pdfs
D: Build the whole documentation.
cd: Clean documentation
c: Clean
h: This help"
}

if [ $# -eq 0 ]; then
    build
    exit $?
fi

for todo in $* ; do
    case "$todo" in
        "b" ) build ;;
        "bg" ) build ".d" ;;
        "bc" ) build_no_ocamlbuild ;;
        "t" ) test/do_tests ;;
        "d" ) cd doc/ ; make nopdf ; cd .. ;;
        "D" ) cd doc/ ; make  ; cd .. ;;
        "cd" )rm -rf doc/site/ ;;
        "c" ) ocamlbuild -clean ; rm -rf _test_results/ doc/site/ ;;
        "h" ) echo_help ;;
        * ) echo "see \`mk h\`";;
    esac
done


