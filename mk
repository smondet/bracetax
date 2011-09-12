#! /bin/sh

build () {
    ocaml setup.ml -build
}
install_library() {
    ocamlfind install bracetax src/lib/META \
        _build/src/lib/ocamlbracetax.* _build/src/lib/*.cm[iox]
}

echo_help ()
{
    echo "\
$0 <cmd>
build|b: Build brtx (default action)
install_library|il: Install the bracetax library with ocamlfind
uninstall_library|uil: Uninstall the library
tests|t: Run a few tests
doc: Build the documentation (website with PDFs)
docnopdf: Build the documentation without the PDFs
doclib: Build the HTML documentation of the library
docall: Build the whole documentation.
clean|c: Clean
reinstall|ri: re-install everything
-help|--help|help|h: This help"
}

if [ $# -eq 0 ]; then
    build
    exit $?
fi

for todo in $* ; do
    case "$todo" in
        build|b ) build ;;
        install_library|il ) install_library ;;
        uninstall_library|uil ) ocamlfind remove bracetax ;;
        tests|t ) test/do_tests ;;
        doc ) cd doc/ ; make  ; cd ..  ;;
        docnopdf ) cd doc/ ; make nopdf ; cd .. ;;
        doclib ) ocaml setup.ml -doc ;;
        docall|D ) cd doc/ ; make  ; cd .. ; document_library ;;
        clean|c ) ocamlbuild -clean ; rm -rf _test_results/ doc/site/ ;;
        reinstall|ri)
            ocaml setup.ml -uninstall
            ocaml setup.ml -install ;;
        -help|--help|help|h ) echo_help ;;
        * ) echo "Can't understand \`$todo' see \`mk help'";;
    esac
done


