<p align="center">
  <img align="middle" src=
  "https://github.com/Horrocubes/horrocubes-signature-validator/blob/main/assets/horrologo_black.png"
  height="250" /></br>
  <sup><sup><sup><sup>The Horrocubes logo is licensed under
  <a href="https://creativecommons.org/licenses/by/3.0/">Creative
  Commons 3.0 Attributions license</a></sup></sup></sup></sup>
</p>
 
 ![license](https://img.shields.io/badge/license-APACHE-blue.svg?longCache=true&style=flat) 
 [![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?longCache=true&style=flat)](http://makeapullrequest.com)
 
<p align="center"><b>Cardano NFT Minting Policy Script Factory</b></p>

Horrocubes NFTs leverage the power of Plutus minting policy scripts to create true NFTs; once these NFTs are minted, it is impossible to create a duplicate. The minting policy of this type of NFT takes and validates as a parameter a specific UXTO and makes sure the UTXO is spent during the minting transaction.

You can learn more about it in the following lecture that Lars Brünjes made for the Plutus Pioneer Program:

https://www.youtube.com/watch?v=6VbhY162GQA


If you are an owner of a Horrocube, you can recreate your policy script and policy id, but following these instructions and giving as input the UTXO that was used during the minting of your NFT and the token name (i.e., Horrocubes00001).

### Setting up the environment

If you already have a Haskell development environment set up, feel free to skip this section, otherwise follow along, we will set up a suitable environment for compiling plutus scripts using Nix.

  

We will use Nix to provide both Haskell and Cabal, but if you desire, you could also rely on ghcup to manage these dependencies. However, we won't cover this. You can refer to the official [ghcup](https://gitlab.haskell.org/haskell/ghcup-hs) site for instructions on that.

  

Nix is an amazing tool that, among other things, allows us to create isolated environments in which we can embed all dependencies needed for an application. These dependencies can even be system-level dependencies. Thus, we can create an isolated environment to ensure the application will work since all required dependencies are available.

  
  

Install Nix on any **Linux distribution**, **MacOS** or **Windows** (via WSL) via the recommended [multi-user installation](https://nixos.org/manual/nix/stable/#chap-installation). In short, you need to run this at your terminal:

```
$ sh <(curl -L https://nixos.org/nix/install) --daemon
```

Add IOHK Binary Cache. To improve build speed, it is possible to set up a binary cache maintained by IOHK.

  
```
$ sudo mkdir -p /etc/nix

$ cat <<EOF | sudo tee /etc/nix/nix.conf

substituters = https://cache.nixos.org https://hydra.iohk.io

trusted-public-keys = iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=

EOF
```
  

Before Nix works in your existing shells, you need to close them and open them again. Other than that, you should be ready to go.

Once Nix is installed, log out and then log back in, so it is activated properly in your shell.

Load a shell with Nix using this file with the following command:
```
$ nix-shell ./nix/default.nix
```  

This will take approximately five or ten minutes, then, you should see something similar to this:
```
these paths will be fetched (445.08 MiB download, 5870.53 MiB unpacked):

/nix/store/04jc7s1006vhg3qj4fszg6bcljlyap1a-conduit-parse-0.2.1.0-doc

/nix/store/052kzx9p5fl52pk436i2jcsqkz3ni0r2-reflection-2.1.6-doc
.
.
.
/nix/store/7jq1vjy58nj8rjwa688l5x7dyzr55d9f-monad-memo-0.5.3... (34 KB left)

```

This creates an environment with all dependencies listed in the “buildInputs” section, with GHC 8.10.4 and Cabal among those.


When you have recent versions of GHC and Cabal, make sure to use GHC 8.10.2 or later:

```
[nix-shell:~]$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.4

[nix-shell:~]$ cabal --version
cabal-install version 3.4.0.0
compiled using version 3.4.0.0 of the Cabal library

```

### Plutus tx: Compiling the script

1. **Compile the project**. 
 
 To compile the script simply run:
 
```
$ cabal update

$ cabal build
```

2. **Execute the project**. You must provide the UTXO, the token name and the output name of the file. Those will be passed as arguments to the Plutus script (it is not used by the script right now, but will be required by transactions using the script). 

```
$ cabal run plutus-horrocubes-tokens 0f4533c49ee25821af3c2597876a1e9a9cc63ad5054dc453c4e4dc91a9cd7211#0 MyHorroCubeTokenName ./out.plutus
```
Where MyHorroCubeTokenName is your actual asset name (i.e, Horrocube00001) and the UTXO is the actual UTXO used to create your NFT.

Once you have the minting policy script, run the following command to get the policy id:

<b>Note:</b> You must have a running node to run this step.

```
cardano-cli transaction policyid --script-file out.plutus
```
The policy Id should match that of your NFT.


License
-------

<b>Note:</b> These instructions and the code for the minting policy were taken from IOHK repositories:

Instructions:
https://github.com/input-output-hk/Alonzo-testnet/blob/main/Alonzo-tutorials/Plutus_transactions_tutorial.md

Mintin policy:
https://github.com/input-output-hk/lobster-challenge/blob/main/src/Cardano/PlutusLobster/LobsterScript.hs

I just modified offchain code to take the token name as parameter.

This code is released under the terms of the Apache-2.0 license. See [LICENSE](LICENSE) for more
information or see https://www.apache.org/licenses/LICENSE-2.0.html.
