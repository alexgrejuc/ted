# ted is a basic text editor which runs in a terminal emulator

![ted](https://github.com/alexgrejuc/ted/blob/master/ted.png)

### How to use it
Clone the repository and navigate into it. The important source code files are in [app/](app/) and [src/](src/).

If you have [stack](https://docs.haskellstack.org/en/stable/README/):

`stack setup`
    
 Then you can edit your file by running:
    
`stack run filename`

This will display the contents of the file or a blank screen if the file does not exist. You will be able to insert and delete text and navigate around with arrow keys. You can press escape to toggle the mode.

#### Edit Mode Features
Note: the reasoning behind some of the less intuitive key mappings is due to a clash with other mappings. Some operations come in related pairs, for example, copy and paste and they are accessed by the lower/upper case version of the same key.
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
   
If you do not have stack, you can load [Zipper.hs](src/Zipper.hs) and [Lib.hs](src/Lib.hs) into ghci and manipulate the internal representation of the text file manually: 

`ghci src/Zipper.hs src/Lib.hs`

The tests in [Zipper.hs](src/Zipper.hs) are a good demonstration of how to use the functions. This file also contains `exampleZipper`; you can call the functions on this zipper and see how they manipulate the central data structure of this project.

```
stepRight exampleZipper 
Zipper {above = fromList ["This is a paragraph above.","This is another one."], left = "This is text on the left. ", selection = "T", right = "his is text on the right.", below = fromList ["This is a paragraph below."]}
```

This may not work if your ghci is too old.

## Dependencies
- ncurses
- directory
- filepath
- containers
- text
- mtl
- text-manipulate
- text-icu

## Milestone 2
## Goals
I intend to make a text editor with an insert and edit mode for the purpose of editing and writing prose rather than code. It will borrow some features from [Vim](https://en.wikipedia.org/wiki/Vim_(text_editor)) and [Kakoune](https://kakoune.org/), although it will be simpler and hopefully easier to use than both. Insert mode will support text navigation as well as character insertion and deletion. My goal for edit mode is to offer basic features. Some possibilities include: selecting words, sentences, or paragraphs and cutting, copying, or deleting them; jumping around text; and searching and replacing. I would also like to create a set of atomic functions which could be used to easily develop more complex features for the mode. These would be created in the code itself, and, if time allows, could also be exposed to the user with a tiny DSL that is parsed into these functions. Also, if time allows, I would like to include visual feedback for the edit mode operations (e.g. highlighting or underlining selected text).

## Progress
  * Fixed arrow key navigation and backspace bugs
  * Added delete key functionality
  * Changed arrow keys to navigate screen lines rather than paragraphs
  * Added scrolling and related position logic
  * Added a monad transformer stack to track editor state and do IO
  * Started edit mode
  
## Design Questions
1. What are your ideas for an edit mode? How might you structure a text editing language (for editing prose text rather than code)? Do you have suggestions for core objects (e.g. words, sentences, paragraphs), operations (e.g. copying, pasting, cutting, shifting case), or modifiers (e.g. repeat n times, do to every nth sub-object).
2. What is a good way to to display a screen's worth of text at the proper line offset? It is currently handled in three lines of just-get-it-to-work code (`displayLines` and `toRows` of Zipper.hs and `let rows = ...` in `draw` of Editor.hs) which splits the entire zipper into line-length rows and is _very_ inefficient. A better way to do it is to count up/down the remaining lines of text from the current line, possibly across paragraph boundaries. Is there a better way to do it?
3. How can I reduce ncurses flickering? One idea is add an intermediary function which determines how much needs to be redrawn (the entire screen, only cursor moves, a few lines, etc) based on zipper actions and then pass that information to `tedRender` rather than simply clearing and redrawing the screen with every action. Does anyone have suggestions about this? Possibly a system of data types which determine what to redraw? 
4. How can I visually indicate edit mode selections? I don't think anyone else has experience with the [ncurses](https://hackage.haskell.org/package/ncurses-0.2.16/docs/UI-NCurses.html) library, but it's worth asking. I thought it would be trivial to highlight or underline text with a library function, but alas, it doesn't exist! One approach is to overlay a window on top of the current window with modified text.


--- 

## Milestone 1
If you are unable to run ted at all, you can watch [this 30 second demonstration video](https://media.oregonstate.edu/media/t/0_kenf40lk). Note that this video was made on May 11th and there are more features now.

### Goals & progress
I intend to make a text editor with an insert and edit mode for the purpose of editing and writing prose rather than code. It will borrow some features from [Vim](https://en.wikipedia.org/wiki/Vim_(text_editor)) and [Kakoune](https://kakoune.org/), although it will be simpler and hopefully easier to use than both. Currently it supports opening or creating a file and editing it in insert mode by writing text, navigating with arrow keys, removing characters with backspace, and making new lines with the enter key (only on unix-like systems for now due to what is probably my own misunderstanding of the Haskell ncurses library I am using). After ironing out some insert mode bugs, I will develop the edit mode with features as I see fit. My goal for edit mode is to offer basic features and also create a set of atomic functions which could be used to easily develop more complex features for the mode. These would be created in the code base itself, and, if time allows, could also be exposed to the user with a tiny DSL that is parsed into these functions.

### Design questions
1. How should I handle the logic for two separate modes? This will introduce more state and complexity to the code, and many key presses will have valid (and occasionally equivalent) semantics in both modes. I suspect the state monad may be useful, but I have not read up on it yet.
2. What is an elegant but efficient way to keep the display and the zipper in sync? My current approach is to execute operations on the zipper and pass it to `tedRender` which translates the zipper to a display. While this is concise, easy to reason about, and does away with synchronization, it is inefficient. `tedRender` is currently pretty dumb; it clears and redraws all text every time, even if all that happend was an arrow key press. I have noticed some stuttering/gltiching when editing text and I wonder if this is the cause.
