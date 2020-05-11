# ted is a basic text editor which runs in a terminal emulator

### Goals & progress
I intend to make a text editor with an insert and edit mode for the purpose of editing and writing prose rather than code. It will borrow some features from [Vim](https://en.wikipedia.org/wiki/Vim_(text_editor)) and [Kakoune](https://kakoune.org/), although it will be simpler and hopefully easier to use than both. Currently it supports opening or creating a file and editing it in insert mode by writing text, navigating with arrow keys, removing characters with backspace, and making new lines with the enter key (only on unix-like systems for now due to what is probably my own misunderstanding of the Haskell ncurses library I am using). After ironing out some insert mode bugs, I will develop the edit mode with features as I see fit. My goal for edit mode is to offer basic features and also create a set of atomic functions which could be used to easily develop more complex features for the mode. These would be created in the code base itself, and, if time allows, could also be exposed to the user with a tiny DSL that is parsed into these functions.

### How to run it
Clone the repository and navigate into it.

If you have [stack](https://docs.haskellstack.org/en/stable/README/):

`stack setup`

`stack install`
    
 Then you can edit your file by running:
    
`stack exec ted-exe filename`
   
If you do not have stack, you can load [Zipper.hs](src/Zipper.hs) and [Lib.hs](src/Lib.hs) into ghci and manipulate the internal representation of the text file manually: 

`ghci src/Zipper.hs src/Lib.hs`

The tests in [Zipper.hs](src/Zipper.hs) are a good demonstration of how to use the functions.

This may not work if your ghci is too old.

If you are unable to run ted at all, you can watch [this 30 second demonstration video](https://media.oregonstate.edu/media/t/0_kenf40lk).

### Design questions
1. How should I handle the logic for two separate modes? This will introduce more state and complexity to the code, and many key presses will have valid (and occasionally equivalent) semantics in both modes. I suspect the state monad may be useful, but I have not read up on it yet.
2. What is an elegant but efficient way to keep the display and the zipper in sync? My current approach is to execute operations on the zipper and pass it to `tedRender` which translates the zipper to a display. While this is concise, easy to reason about, and does away with synchronization, it is inefficient. `tedRender` is currently pretty dumb; it clears and redraws all text every time, even if all that happend was an arrow key press. I have noticed some stuttering/gltiching when editing text and I wonder if this is the cause.
