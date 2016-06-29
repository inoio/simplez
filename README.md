simplez
=======

A simple typeclass library inspired by scalaz meant as a learning tool by reducing the initial complexity of scalaz.

`Scalaz` is of course the standard typeclass library for Scala, but learning it can be daunting task for someone new to functional programming. Simplez is a kind of educational tool, a scalaz without many of the alias functions (flatMap, bind, >>=, point, pure, &mu;, ...) with a preference for scala idioms and less functionality. It is not meant as a replacement, but given it's smaller footprint, we believe that it can help understand how `scalaz` works.

IDE
===

Works out of the box with eclipse.

For IntelliJ, open the root directory through File->Open. It should prompt for sbt and java sdk on first load. If it ask to import projects, import them all.

On the first compile, you'll need to compile using command line, 'sbt compile' instead of the usual Build->Make Project. Once compilation is done, in the IntelliJ project folder, you must mark 'main/target/scala-2.11/src_managed/main' as 'Sources Root'. Depending on the  version of IntelliJ, you will need right click on main/target, 'Mark Directory As' -> 'Cancel Exclusion', then go to each sub directory and exclude them except for src_managed.

Build->Make Project will now. You may still come across some highlighting errors. If changes are made to SimplezGenerator.scala, you will need to run "sbt compile" again.

[![Build Status](https://travis-ci.org/inoio/simplez.svg?branch=master)](https://travis-ci.org/inoio/simplez)
[![Coverage Status](https://coveralls.io/repos/inoio/simplez/badge.svg)](https://coveralls.io/r/inoio/simplez)
