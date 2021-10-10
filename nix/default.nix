{ version ? "purple", magicId ? 8, pkgs ? import <nixpkgs> { }}:
let
cardano-node-repo = import ./. { };

in pkgs.mkShell {
buildInputs = with pkgs; [
libsodium
zlib
cabal-install
haskell.compiler.ghc8104
haskellPackages.haskell-language-server
];

CARDANO_NODE_SOCKET_PATH = "${builtins.toString ./.}/state-node-alonzo-${version}/node.socket";
TESTNET_MAGIC = magicId;
}