# chesstrainer 0.9-8 (2025-01-16)

- can play en passant
- can promote pawns
- help opens up in the plot, not in the console
- changed `<r>` key to `<Ctrl-r>` and `<d>` key to `<Ctrl-d>`
- added `<r>` key to toggle between different sequence selection modes
- allow adjusting the line width during play with the `<(>` and `<)>` keys
- settings are saved across sessions
- colors can be adjusted and are stored across sessions
- text sizes for text at the top and bottom can be adjusted and are stored across sessions
- moves in long algebraic notation are added to the sequence files
- added `<?>` key to find all sequences that start with the same moves entered so far
- the `<t>` key is used for taking back moves (in add mode) or a score adjustment (in play mode)
- added a board editor (which can be opened in add mode with the `<b>` key)
- sequences contain a position evaluation variable, which (if not missing) is shown as an evaluation bar (can be toggled on/off with `<v>`)
- can use Stockfish in the background to automatically obtain position evaluations when adding sequences
- the `<u>` key is used to recalculate (update) the position evaluations during play mode (`<escape>` is used to redraw the board)
- keep track of when a sequence was last played and allow selection based on how long ago sequences were played
- the `<A>` key can be used like the `<a>` key but only plays the moves to the currently shown position
- simplified the arguments of the `play()` function
- `<F9>` prints the FEN of the current position, copies it to the clipboard (`clipr` package added to `Imports`), and opens up the position on lichess.org

# chesstrainer 0.1-0 (2024-12-17)

- first public release
