name:                  shakers
version:               VERSION
synopsis:              Shake helpers.
description:           Primitive helpers for Shake files.
homepage:              https://github.com/swift-nav/shakers
license:               MIT
license-file:          LICENSE
author:                Swift Navigation Inc.
maintainer:            Mark Fine <dev@swiftnav.com>
copyright:             Copyright (C) 2016 Swift Navigation, Inc.
category:              Development
build-type:            Simple
cabal-version:         >= 1.22

library
  hs-source-dirs:      src
  exposed-modules:     Development.Shakers
  default-language:    Haskell2010
  ghc-options:         -Wall
  build-depends:       base >= 4.8 && < 5
                     , basic-prelude
                     , directory
                     , shake

executable shake-shakers
  main-is:             Shakefile.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:       base
                     , shakers
  default-language:    Haskell2010

source-repository head
  type:                git
  location:            git@github.com:swift-nav/shakers.git
