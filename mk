#! /bin/sh

build () {
    ocaml setup.ml -build
}
install_library() {
    ocamlfind install bracetax src/lib/META \
        _build/src/lib/ocamlbracetax.* _build/src/lib/*.cm[iox]
}

document_library(){
    local docinput=" -sort -I _build/src/lib/ src/lib/*.ml"
    local docdir="doc/site/doclib/"
cat <<EOF > /tmp/intro
This is the ocamldoc documentation for the
{{:http://bracetax.berlios.de}{i Bracetax}} library.

Here are some hig-level graphs:
- {{:./bracetax_modules.png}Graph of Modules}
- {{:./bracetax_types.png}Graph of Types}

The modules (don't forget that all modules are "packed" inside the
Bracetax module):
{!modules: 
Transform
Info
Error
Signatures
Parser
LatexPrinter
HtmlPrinter
GenericPrinter
TOCOutput
Escape
Commands
}


Other indexes:

{!indexlist}

EOF
    rm -fr $docdir
    mkdir -p $docdir
    ocamldoc -d $docdir -t "Bracetax Library" -intro /tmp/intro \
        -html -colorize-code $docinput
    ocamldoc -o /tmp/brtxmods.dot -dot -dot-reduce -dot-include-all $docinput
    ocamldoc -o /tmp/brtxtyps.dot -dot -dot-types -dot-reduce $docinput
    grep -v rotate /tmp/brtxmods.dot | dot -Tpng > $docdir/bracetax_modules.png
    grep -v rotate /tmp/brtxtyps.dot | dot -Tpng > $docdir/bracetax_types.png
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
        doclib ) document_library ;;
        docall|D ) cd doc/ ; make  ; cd .. ; document_library ;;
        clean|c ) ocamlbuild -clean ; rm -rf _test_results/ doc/site/ ;;
        reinstall|ri)
            ocaml setup.ml -uninstall
            ocaml setup.ml -install ;;
        -help|--help|help|h ) echo_help ;;
        * ) echo "Can't understand \`$todo' see \`mk help'";;
    esac
done


