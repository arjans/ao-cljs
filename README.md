# ao-cljs

Matt Keeter's [ao](https://github.com/mkeeter/ao) translated into Clojurescript. (Work in progress.)

## Overview

Ao is a programmatic CAD tool based on a functional representation of shapes, with Scheme as its scripting language.

### Links:
- [github](https://github.com/mkeeter/ao)
- [project page](https://www.mattkeeter.com/projects/ao/)
- [pdf overview](https://www.mattkeeter.com/research/ao_poster.pdf)

### Background Papers

- [Interval Arithmetic and Recursive Subdivision for Implicit Functions and Constructive Solid Geometry](http://fab.cba.mit.edu/classes/S62.12/docs/Duff_interval_CSG.pdf)
- [Adaptively Sampled Distance Fields](http://www.merl.com/publications/docs/TR2000-15.pdf)

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL.
