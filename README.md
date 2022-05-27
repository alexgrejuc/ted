# ted is a modal text editor that runs in a terminal emulator

It is intended to be used for prose rather than code. Like [Vim](https://www.vim.org/about.php) and [Kakoune](https://kakoune.org/), it has one mode for inserting text to the buffer and another for selecting and modifying it.

![A screenshot of this README being viewed with ted](docs/img/ted.png)

A 70 second demo video is available [here](https://media.oregonstate.edu/media/t/1_01l6076t) and a report on the design of the project is available [here](docs/report.pdf).

### How to use it
Clone the repository and navigate into it. The important source code files are in [app/](app/) and [src/](src/).

If you have [stack](https://docs.haskellstack.org/en/stable/README/):

`stack setup`

 Edit your file by running:

`stack run filename`

This will display the contents of the file or a blank screen if the file does not exist. You will be able to insert and delete text and navigate around with arrow keys. You can press escape to toggle the mode.

#### Edit Mode Features
  * **selection:**
    * `w` - curent word
    * `s` - current sentence
    * `p` - current paragraph
  * **others**
    * `x` - cut
    * `c` - copy
    * `C` - paste
    * `l` - to lower case
    * `L` - to upper case
    * `d` - delete
    * `q` - Writes to filename.ted and then quits
    * arrow keys

Note: the reasoning behind some of the less intuitive key mappings is due to a clash with other mappings. Some operations come in related pairs (for example, copy and paste) and they are accessed by the lower/upper case version of the same key.

If you do not have stack, you can load [Zipper.hs](src/Zipper.hs) and [Lib.hs](src/Lib.hs) into ghci and manipulate the internal representation of the text file manually:

`ghci src/Zipper.hs src/Lib.hs`

The tests in [Zipper.hs](src/Zipper.hs) are a good demonstration of how to use the functions. This file also contains `exampleZipper`; you can call the functions on this zipper and see how they manipulate the central data structure of this project.

```
stepRight exampleZipper
Zipper {above = fromList ["This is a paragraph above.","This is another one."], left = "This is text on the left. ", selection = "T", right = "his is text on the right.", below = fromList ["This is a paragraph below."]}
```

## Dependencies
- ncurses
- directory
- filepath
- containers
- text
- mtl
- text-manipulate
- text-icu

