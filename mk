#! /bin/sh

build ()
{
    ocamlbuild -I src/app -I src/lib  src/app/main$1.byte ocamlbracetax.cma
    rm -f brtx
    ln -s main$1.byte brtx
}
echo_help ()
{
    echo "\
$0 [trch]
b: Build brtx (default action)
t: Do the tests
d: Build the documentation without building pdfs
D: Build the whole documentation.
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
        "t" ) test/do_tests ;;
        "d" ) cd doc/ ; make nopdf ; cd .. ;;
        "D" ) cd doc/ ; make  ; cd .. ;;
        "c" ) ocamlbuild -clean ; rm -rf _test_results/ gendoc/ doc/site/ ;;
        "h" ) echo_help ;;
        * ) echo "see \`mk h\`";;
    esac
done


