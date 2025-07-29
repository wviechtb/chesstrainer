# chesstrainer 0.9-71 (2025-07-29)

- can play en passant
- can promote pawns
- help opens up in the plot, not in the console
- changed `<r>` key to `<Ctrl-r>` and `<d>` key to `<Ctrl-d>`
- added `<m>` key to toggle between different sequence selection modes
- allow adjusting the line width during play with the `<(>` and `<)>` keys
- various settings (including the colors and text sizes) are saved across sessions and can be adjusted when the trainer is running
- moves in long algebraic notation are added to the sequence files
- added `<?>` key to find all sequences that start with the same moves entered so far
- the `<t>` key is used for taking back moves (in add mode) or a score adjustment (in test mode)
- added a board editor (which can be opened in add mode with the `<b>` key)
- sequences can contain position evaluations, which (if not missing) are shown as an evaluation bar (can be toggled on/off with `<v>`)
- can use Stockfish in the background to automatically obtain position evaluations when adding sequences
- the `<u>` key is used to recalculate (update) the position evaluations during test mode
- trainer keeps track of when a sequence was last played and allows selection based on how long ago sequences were played
- progress over time in stored for each player in the sequence files and can be displayed with the `<g>` key
- the `<A>` key can be used like the `<a>` key but continues from the currently shown position
- `<F9>` prints the FEN of the current position, copies it to the clipboard (`clipr` package added to `Imports`), and opens up the position on lichess.org
- `<F10>` shows histograms / scatterplot of the sequence statistics
- simplified the arguments of the `play()` function
- sequences now also save drawn circles and arrows during add mode and replay them during test mode
- sequences now also save the FEN for each move; can use this to select sequences
- sequences can have a start and/or end comment now
- can now add, remove, and select sequence directories with `<F8>`
- can now evaluate multiple principal variations with Stockfish
- added a 'timed' mode
- can add, select, and manage bookmarks
- checks now whether a move is legal
- show visually when a king is in check
- the `<g>` key shows an evaluation plot in play mode
- allow selection based on RMSSD
- keep track of session history

# chesstrainer 0.1-0 (2024-12-17)

- first public release
