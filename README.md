# [shakers][shakers]

[![Build status][travis-img]][travis]

A library of [shake][shake] helpers.


## Development

`shakers` has a shakefile/makefile to provide convience around building and testing:

    # build the project's libraries, executables, and tests
    $ ./Shakefile.hs build-tests-error
    
    # test the project
    $ ./Shakefile.hs tests-error
    
    # start an interpreter with the project's libraries, executables, and tests loaded
    $ ./Shakefile.hs ghci-tests
    
    # clean the project
    $ ./Shakefile.hs clean
    
    # lint the project source code
    $ ./Shakefile.hs lint
    
    # format the project source code
    $ ./Shakefile.hs format


## Dependencies

To build and test `shakers`, the following dependencies may be required:

+ [stack][stack]


[shakers]:       https://github.com/swift-nav/shakers
[hackage]:       https://hackage.haskell.org/package/shakers
[hackage-img]:   https://img.shields.io/hackage/v/shakers.svg?style=flat
[travis]:        https://travis-ci.org/swift-nav/shakers
[travis-img]:    https://img.shields.io/travis/swift-nav/shakers/master.svg?style=flat
[deps]:          http://packdeps.haskellers.com/feed?needle=shakers
[deps-img]:      https://img.shields.io/hackage-deps/v/shakers.svg?style=flat
[basic-prelude]: https://github.com/snoyberg/basic-prelude
[stack]:         https://docs.haskellstack.org/en/stable/README/#how-to-install
[shake]:         http://shakebuild.com/
