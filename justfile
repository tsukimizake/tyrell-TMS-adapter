set shell := ["nu", "-c"]
default:
  ~/.cabal/bin/cabal-fmt -i product.cabal
  cabal build; cabal exec product

watch:
  watch . --glob=**/*.hs {|| just default }

