# kilo-in-rust
An implementation of the Kilo editor in Rust.
This is a port of [kilo](https://github.com/antirez/kilo), a simple text editor written in < 1000 lines of C that supports search and syntax highlighting.
This version of the editor has exactly 1000 lines of implementation.

This implementation also leaned heavily on [this tutorial](http://viewsourcecode.org/snaptoken/kilo/) which walks through the kilo source.

Usage: kilo `<filename>`

Keys:

    CTRL-S: Save
    CTRL-Q: Quit
    CTRL-F: Find string in file (ESC to exit search, arrows to navigate)
