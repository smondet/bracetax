#! /bin/sh

build ()
{
    ocamlbuild -I src/app -I src/lib  src/app/main.byte
    ln -s main.byte bracetax
}
echo_help ()
{
    echo "\
$0 [trch]
b: Build bracetax (default action)
t: Do the tests
d: Build the documentation
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
        "t" ) test/do_tests ;;
        "d" ) tools/make_readme ;;
        "c" ) ocamlbuild -clean ; rm -rf _test_results/ gendoc/ ;;
        "h" ) echo_help ;;
        * ) echo "see \`mk h\`";;
    esac
done


