-*- mode: markdown; coding: utf-8-unix; -*-

CL-PERCENT-CODING - Read/write percent encoded strings.

Copyright (C) 2013-2015 Olof-Joachim Frahm

Release under a Simplified BSD license.

Protoype state.

# USAGE

This library provides two functions `URL-DECODE` and `URL-ENCODE` for
decoding and encoding of URL encoded data.

To decode/encode a regular percent encoded string the functions would be
invoked like this:

    > (url-decode "%42")
    => "B"

    > (url-encode "foo bar")
    => "foo%20bar"

By default the `*DEFAULT-EXTERNAL-FORMAT*` setting will be used for
conversion, which is, again by default, set to UTF-8 with LF line
endings (at the moment using
`(flexi-streams:make-external-format :utf-8 :eol-style :lf)`).

There's also the option to use form encoding in both directions, e.g.:

    > (url-decode "a+b+c" :x-www-form-urlencoded-p T)
    => "a b c"

    > (url-encode "foo bar" :x-www-form-urlencoded-p T)
    => "foo+bar"

Both functions accept the usual range designators `:START` and `:END`,
as well as `:EXTERNAL-FORMAT` and `:OUTPUT-ELEMENT-TYPE`, by default
outputting to a string, i.e. using `'CHARACTER`.
