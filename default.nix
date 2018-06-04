{ mkDerivation, base, pure-default, pure-json, pure-time, pure-try, pure-txt, stdenv }:
mkDerivation {
  pname = "pure-cond";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-default pure-json pure-time pure-try pure-txt ];
  homepage = "github.com/grumply/pure-cond";
  license = stdenv.lib.licenses.bsd3;
}
