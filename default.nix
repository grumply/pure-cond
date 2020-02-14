{ mkDerivation, base, containers, pure-core, pure-default, pure-json, pure-lifted, pure-try, pure-txt, unordered-containers, vector, stdenv }:
mkDerivation {
  pname = "pure-cond";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers pure-core pure-default pure-json pure-lifted pure-try pure-txt unordered-containers vector ];
  homepage = "github.com/grumply/pure-cond";
  license = stdenv.lib.licenses.bsd3;
}
