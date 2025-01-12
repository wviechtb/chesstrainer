# chesstrainer 0.9-2 (2025-01-12)

- can now play en passant
- can now promote pawns
- help opens up in the plot, not in the console
- changed `<r>` key to `<Ctrl-r>` and `<d>` key to `<Ctrl-d>`
- added `<r>` key (and `random` argument) to toggle between random/sequential mode
- allow adjusting the line width during play with the `<(>` and `<)>` keys
- settings are now saved across sessions
- colors can be adjusted and are stored across sessions
- text sizes for text at the top and bottom can be adjusted and are stored across sessions
- moves in long algebraic notation are added to the sequence files
- added `<?>` key to find all sequences that start with the same moves entered so far
- the `<t>` key is now used for taking back moves (in add mode) or a score adjustment (in play mode)
- added a board editor (which can be opened in add mode with the `<b>` key)
- sequences now contain a position evaluation variable, which (if not missing) is shown as an evaluation bar (can be toggled on/off with `<v>`)
- can now use Stockfish in the background to automatically obtain position evaluations when adding sequences
- the `<u>` key is now used to recalculate (update) the position evaluations during play mode (`<escape>` is used to redraw the board)
- keep track of when a sequence was last played and allow selection based on how long ago sequences were played

# chesstrainer 0.1-0 (2024-12-17)

- first public release
