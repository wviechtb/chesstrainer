# chesstrainer 0.9-45 (2025-04-14)

- can play en passant
- can promote pawns
- help opens up in the plot, not in the console
- changed `<r>` key to `<Ctrl-r>` and `<d>` key to `<Ctrl-d>`
- added `<r>` key to toggle between different sequence selection modes
- allow adjusting the line width during play with the `<(>` and `<)>` keys
- various settings (including the colors and text sizes) are saved across sessions and can be adjusted when the trainer is running
- moves in long algebraic notation are added to the sequence files
- added `<?>` key to find all sequences that start with the same moves entered so far
- the `<t>` key is used for taking back moves (in add mode) or a score adjustment (in play mode)
- added a board editor (which can be opened in add mode with the `<b>` key)
- sequences contain a position evaluation variable, which (if not missing) is shown as an evaluation bar (can be toggled on/off with `<v>`)
- can use Stockfish in the background to automatically obtain position evaluations when adding sequences
- the `<u>` key is used to recalculate (update) the position evaluations during play mode
- trainer keeps track of when a sequence was last played and allows selection based on how long ago sequences were played
- progress over time in stored for each player in the sequence files and can be displayed with the `<g>` key
- the `<A>` key can be used like the `<a>` key but only plays the moves up to the currently shown position
- `<F9>` prints the FEN of the current position, copies it to the clipboard (`clipr` package added to `Imports`), and opens up the position on lichess.org
- `<F10>` shows histograms / scatterplot of the sequence statistics
- simplified the arguments of the `play()` function
- sequences now also save drawn circles and arrows during add mode and replay them during play mode
- sequences now also save the FEN for each move; can use this to select sequences
- sequences can have a start and/or end comment now
- can now add, remove, and select sequence directories with `<F8>`

# chesstrainer 0.1-0 (2024-12-17)

- first public release
