\name{chesstrainer-package}
\alias{chesstrainer-package}
\alias{chesstrainer}
\docType{package}
\title{chesstrainer: Create and Test Yourself on Chess Lines}
\description{

   The main purpose of the \pkg{chesstrainer} package is to create sequences of moves (lines), save them, and then test yourself on these sequences. This is especially useful for training openings, but can also be used for creating chess puzzles or training tactics.

}
\section{Starting the Trainer}{

   To start the trainer, run \code{\link{play}()} from the console. When starting the trainer for the first time, a directory is created for storing settings and a directory where sequences will be stored. A few example sequences also come with the package and you will receive a prompt whether to copy these sequences to the sequence directory. To familiarize yourself with the trainer, it would be good to do so.

   You will then receive a prompt to enter a player name. Choose a name and press enter.

   The trainer then starts in \emph{add mode} (see the \sQuote{Note} section at the bottom if you receive an error that the graphics device does not support event handling). For now, click on the window showing the chess board and press \verb{<space>} (i.e., the space bar), which will switch to the \emph{test mode} (note: in this manual, key presses are indicated as: \verb{<key>}). One of the existing sequences will then be chosen at random. The trainer may immediately prompt you to play a certain opening (via a comment at the top of the window), it may make some moves for you, or it may start at a later position during a game. Either way, you will then have to make the correct move based on the position. You do this by either left-clicking and dragging the piece to be played from its current position to the desired target square or by left-clicking on the piece to be moved and then left-clicking on the target square.

   If the line width of the green rectangles (that show up when moving a piece) is too narrow or too thick, you can use the \verb{<(>} and \verb{<)>} keys to decrease/increase the line width. Adjust this value such that the rectangles are clearly visible, but do not overlap into adjacent squares. If the text on the bottom or other visual elements are getting cut off, you can use the \verb{<\{>} and \verb{<\}>} keys to decrease/increase the margin width.

   The trainer will then make the next move, after which you will then again have to make the correct move. This will continue until the end of the sequence. If you do not know what the correct move is, the \verb{<h>} key will provide a hint showing which piece to move. Pressing \verb{<h>} for a second time will show the target square. Once the sequence has ended, left-clicking starts a new sequence.

   In \emph{test mode}, information is shown at the bottom about the current mode, the name of the current sequence, the player name, the current move number and the total number of moves of the sequence, the number of rounds (how many times the sequence has been played so far), the age of the sequence (how many days ago the sequence was last played), the estimated difficulty of the sequence (see below), and the sequence score (see below). With \verb{<z>}, you can toggle on \sQuote{zen mode} to hide this information.

   With \verb{<F1>}, you can obtain an overview of all keyboard shortcuts. The \verb{<left>} and \verb{<right>} arrow keys toggle between the two help pages. The \verb{<t>} key prints this manual in text format to the console. The \verb{<m>} key opens up the manual online. The \verb{<F1>} or \verb{<escape>} keys close the help overlay.

   You can quit the trainer with the \verb{<q>} key.

}
\section{The Sequence Score}{

   The trainer keeps track of a score for each sequence. For a new sequence, the score starts out at 100 points. The score is multiplied by 0.8 after completing a sequence without any mistakes. When making a wrong move when playing a sequence, the score is increased by 40 points (up to a maximum of 100). You can use the \verb{<t>} key to take back the score adjustment (but you should only do this when making the wrong move accidentally). Receiving a hint with the \verb{<h>} key increases the score by 20 points (40 points when asking for both the starting and the target square, so this is equivalent to making a mistake).

   The trainer uses the score to choose the next sequence. Sequences with higher scores have a higher chance to be presented. The probability of playing a particular sequence is computed with \eqn{p_i = s_i^e / \sum s_i^e}, where \eqn{s_i} is the score of the \eqn{i}th sequence and \eqn{e} is the \sQuote{exponent value}. This is set to 2 by default and enhances the probability that sequences with higher scores are more likely to be played. The exponent value can be adjusted with the \verb{<^>} (or \verb{<6>}) key. Setting the exponent value to 0 makes all sequences equally likely to be played. Setting it to a large value (e.g., 10) makes it very likely that the sequence with the highest score is played next. The score for the current sequence can also be set manually with the \verb{<o>} key. Setting the score to 0 means that the sequence has a zero probability of being presented.

   After completing a sequence, the current score for the sequence is saved (together with the number of times that the sequence has been played and when it was last played) to the sequence file. This way, your progress in learning the sequence is saved permanently across sessions. Using the \verb{<g>} key, a progress graph (showing the sequence scores over consecutive rounds) for the current sequence can be shown (if the sequence has been played before). This is useful to check whether progress is being made in learning the sequence. You can zoom into the graph by left-clicking twice (once for the start and once for the end of the interval). This can be repeated to zoom in further. Clicking the right mouse button zooms all the way out. Clicking outside of the graph, right-clicking anywhere when zoomed out, or the \verb{<escape>} key closes the graph.

   Using the \verb{<G>} key, you can toggle whether the progress graph will be automatically shown at the end of each completed sequence.

   With \verb{<F11>} (or \verb{<Ctrl-1>}), you can obtain a plot of the average score of all sequences (in the current sequence directory; see below) as a function of each completed sequence during the current session. The horizontal dotted line corresponds to the average score at the beginning of the session. This is useful to check whether progress is being made in learning all of the sequences.

}
\section{Managing and Selecting Sequences}{

   Using the \verb{<l>} key, you can obtain a list of all sequences, together with their current score, the number of rounds (how times they have been played so far), their age (in days; \code{NA} if a sequence has not been played before), the estimated difficulty of the sequence (\code{NA} if this cannot be computed yet), and the current play probability (with 0 to 5 asterisks next to it based on the probability). Note that this is shown in the console, so ideally you should place the window with the chess board and the console window next to each other.

   In \emph{test mode}, you can use \verb{<Ctrl-d>} to delete the current sequence. Note that this is permanent and all progress information about the sequence will be lost. You will receive a prompt to confirm the deletion.

   Using the \verb{</>} (or \verb{<,>}) key, you can select a subset of the sequences in various ways by entering:

   \itemize{
   \item a text string (which is applied as a regular expression to the filenames of the sequences),
   \item a sequence number (e.g., \verb{2} to select the 2nd sequence),
   \item a sequence number range (e.g., \verb{2-5} to select sequences 2 to 5),
   \item something like \verb{score > 80} to select sequences with a score above a certain threshold,
   \item something like \verb{rounds < 1} to select sequences that have been played less than a certain number of times,
   \item something like \verb{age > 2} to select sequences that have been played more than a certain number of days ago,
   \item something like \verb{difficulty > 20} to select sequences with a difficulty above a certain threshold,
   \item a \href{https://en.wikipedia.org/wiki/Forsyth-Edwards_Notation}{FEN} to select all sequences that contain a position matching the FEN,
   \item \verb{c:} followed by a (case insensitive) text string to select all sequences where a comment contains the text string.
   }

   The keywords \verb{score}, \verb{rounds}, \verb{age}, and \verb{difficulty} can be abbreviated to \verb{s}, \verb{r}, \verb{a}, and \verb{d} (so \verb{s > 80}, \verb{r < 1}, \verb{a > 2}, and \verb{d > 20} would also work). Other comparators such as \verb{>=}, \verb{<}, and \verb{>=} can also be used (e.g., \verb{s <= 10}). When entering a text string, you can refine your search by prefixing the string with either \sQuote{\verb{& }} or \sQuote{\verb{| }}, that is, \sQuote{\verb{& <text string>}} searches for the intersection of the currently selected sequences and those matching the new text string while \sQuote{\verb{| <text string>}} searches for their union.

   Just pressing \verb{<enter>} at the prompt (i.e., not entering any search string) keeps the current selection. Entering \verb{*} at the prompt selects all sequences. The \verb{<*>} (or \verb{<8>}) key also selects all sequences without using \verb{</>} first. If you only want to search for sequences (without selecting them), you can use the \verb{<|>} key (instead of \verb{</>}).

   In \emph{test mode}, sequences are played by default at random based on the score as described above. However, you can toggle into a different sequence selection mode with the \verb{<m>} key. Selection can be based on:

   \enumerate{
   \item the score, where sequences with higher scores have a higher play probability (the default),
   \item the score, where the sequence with the highest score is played next,
   \item the number of rounds, where sequences with a lower number of rounds have a higher play probability,
   \item the number of rounds, where the sequence with the lowest number of rounds is played next,
   \item the age, where sequences that were last played longer ago have a higher play probability,
   \item the age, where the sequence that was last played the longest ago is played next,
   \item the difficulty, where more difficult sequences have a higher play probability,
   \item the difficulty, where the sequence with the highest difficulty is played next.
   }

   The exponent value affects selection modes 1, 3, 5, and 7. Setting the exponent value to a large value makes mode 1 essentially the same as mode 2 (and similarly, mode 3 like mode 4, mode 5 like mode 6, and mode 7 like mode 8). For modes 2, 4, 6, and 8, if the highest score, lowest number of rounds, highest age, or highest difficulty is the same for multiple sequences, the alphabetically first one is played next. For modes 5 and 6, sequences that have never been played before are given the highest priority. For all modes, sequences whose score was set to 0 are never played.

   One can also choose to play sequences sequentially, either ordered alphabetically, ordered by their length (which is given by the number of moves that have to be made by the player), or ordered by their moves. The latter is especially useful when practicing a particular opening and one wants to go through a set of corresponding sequences in the order of the variations.

   For the sequential selection modes and for selection mode 6 (i.e., when playing the oldest sequence next), a message is shown after completing one round of all (selected) sequences (as long as more than one sequence is selected). For the other selection modes, one can also set a \sQuote{target score} with the \verb{<T>} key (the default value is 0, which has no effect). Once all (selected) sequences have a score below this target score, a message will be shown.

   In \emph{test mode}, the \verb{<r>} key repeats the last played sequence. At the end of a sequence, you can also use the middle mouse button to repeat the sequence (before a new sequence is started). With the \verb{<R>} key, you can toggle whether a sequence will be automatically repeated if a mistake was made while playing the sequence.

}
\section{Timed Mode}{

   With the \verb{<x>} key, you can switch into a \emph{timed mode}. In this mode, all moves played during a sequence must be completed in a certain amount of time, which is given by the total number of moves that must be played in the sequence times the current setting for \sQuote{time per move} (which is 5 seconds by default). For example, if 8 moves must be played, then you have \eqn{8 \times 5 = 40} seconds in total to complete all moves (the time taken by the trainer for playing moves is not counted). Not completing the moves within this amount of time is treated like a mistake (i.e., the score is increased by 40 points).

   When playing a sequence in this mode, the bar to the right of the board indicates whether the current playing time is above (in green) or below (in red) the amount of time you should have spent on the moves so far (i.e., if every move took exactly the amount of time allowed by the \sQuote{time per move} setting).

   The \sQuote{time per move} value can be adjusted via the \verb{<F6>} key.

}
\section{Sequence Difficulty}{

   After playing a sequence multiple times, the difficulty of the sequence can be estimated based on past performance. By default, this is computed as the average score increase (ASI) over the plays of the sequence. Using the \verb{<d>} key, one can bring up a menu to choose from a variety of different methods for estimating the difficulty, including:

   \enumerate{
   \item the average score increase over the plays of the sequence (ASI),
   \item the percentage of plays where the score did not improve (PNI),
   \item the number of plays where the score did not improve (NNI),
   \item the percentage of plays with a change in the score trend (PCT),
   \item the average of the scores of the sequence (AVG),
   \item the standard deviation of the scores of the sequence (SD),
   \item the root mean square successive difference (RMSSD),
   \item the decay parameter from an exponential regression model (ERM).
   }

   These different calculations methods reflect the difficulty of a sequence in various ways. Sequences with high values for the ASI, PNI, NNI, PCT, SD, and RMSSD tend to show larger fluctuations in their scores over time and are apparently difficult to remember. Sequences with high AVG values are also difficult. Sequences with high ERM values are deviating strongly from the optimal decrease in the score that should happen when never making mistakes while playing a sequence (i.e., \eqn{100}, \eqn{100 \times 0.8 = 80}, \eqn{80 \times 0.8 = 64}, and so on). By default, only the 15 most recent scores are used in the calculation of these statistics. Also, by default the difficulty is only computed when a sequence has been played at least 5 times. These values can also be adjusted via the menu that comes up when pressing the \verb{<d>} key.

}
\section{Adding New Sequences}{

   As described above, the trainer comes with a few example sequences to teach you how it can be used, but serious use of the trainer requires that you add your own sequences. With \verb{<space>}, you can switch into the \emph{add mode}, where you can enter new sequences. The first thing to decide is whether the player should play with white or black. With the \verb{<f>} key, the board can be flipped accordingly (the player always plays with the pieces shown at the bottom). Next, you need to decide whether some moves will be automatically played at the beginning of a sequence. If some moves should be played automatically, then simply make these moves on the board. Once you arrive at the position where a player should start making moves, press the \verb{<z>} key (note that \sQuote{Show} at the bottom left then turns from \sQuote{Yes} to \sQuote{No}). Now continue playing the line as it should be played. The sequence must end with a move by the player. If you make a mistake when playing a move, use the \verb{<t>} key to take the move back. Once the sequence is complete, use the \verb{<s>} key to save the sequence. You will be prompted for a filename. Once the sequence has been saved, it will automatically become part of the sequences that may be presented in \emph{test mode}.

   A sequence may also start immediately in the starting position. Here, you will press the \verb{<z>} key right at the beginning. Since it is then essentially impossible for a player to know what they should play (since there are many sensible moves one could make), you will have to instruct the player what they should do. Using the \verb{<c>} key, you can add a comment to the current move which will be shown to the player (before they have to make the move). For example, you could tell the player to play the Spanish (Ruy Lopez) opening. You should then make the appropriate moves on the board.

   Comments can be added to any move using the \verb{<c>} key. These will be shown before the move is played. This may be useful if one could venture into multiple sensible lines in a given position. You would then have to instruct the player to make a particular move (either by telling the player directly which move to make or giving some appropriate hint, such as the name of a particular line/variation). Comments can also be used to mention variation names or other useful information.

   If a sequence should commence not at the starting position of a chess game, but later on, you can move the pieces into the desired position and then use the \verb{<0>} key to make the current position the starting position for a particular sequence. Alternatively, you can use the \verb{<b>} key to start a board editor (see below for details).

   Occasionally, one may want to construct sequences where the player also has to make the moves of the opposing side (i.e., for the computer). The \verb{<Z>} key can be used to toggle whether the computer moves will be automatically played or not (when this is toggled off, then this will also be indicated at the bottom left). For example, one could memorize entire games by pressing the \verb{<z>} and \verb{<Z>} keys at the very beginning and then making all moves of the game. A comment should then be added before making the first move to provide a reminder which game should be played.

   The \verb{<e>} key can be used to edit the comments for a particular sequence. This way, you can also add a \sQuote{start comment} and/or an \sQuote{end comment} to a particular sequence (the former will be shown in an overlay box before the sequence starts, the latter at the top after the last move). The \verb{<E>} key opens up the \code{\link[=edit]{data editor}} on the data frame that contains the moves for the current sequence. This can also be used to add, edit, and remove comments in the \verb{comment} column or make adjustments to the other columns (but this should only be done if their meaning is clear).

   Once a number of moves have been made for the sequence, the \verb{<left>} and \verb{<right>} arrow keys can be used to go back or forward one move. With the \verb{<1>} or \verb{<up>} arrow keys, one can jump to the first move that needs to be made by the player (i.e., the first move where \sQuote{Show} is set to \sQuote{No}) or to the very beginning of the sequence if one is already at or before this position in the sequence. The \verb{<2>} or \verb{<down>} arrow keys jump to the very end of the sequence. The \verb{<3>}, \verb{<4>}, and \verb{<5>} keys jump to the first quarter, middle, and third quarter of the sequence (when the sequence consists of very few moves, these different positions may not be differentiable). Note that when making a move when not looking at the very end of the sequence, then this deletes all subsequent moves that were already part of the sequence.

   With the \verb{<?>} key, you can search for sequences that start with the same moves that have been entered/played so far (this also works in \emph{test mode}). If any matching sequences are found, a prompt will ask if they should be selected. This is useful for checking what sequences are already available based on some initial moves (and then practicing these sequences) or finding other sequences similar to the one currently being played. Alternatively, with the \verb{<'>} key, you can search for sequences that include the current position anywhere in the sequence (irrespective of the particular moves that led to the position). With the \verb{<">} key, you can search for sequences that end on the current position. Note that all of these searches account for whether the board is flipped or not (i.e., only sequences are shown where the player is either moving the white or black pieces).

   With the \verb{<.>} key, you can select the last saved sequence (if one was saved before in the current session). This is useful for checking the sequence in \emph{test mode} and/or creating multiple branching lines from the ending position of this sequence. Similarly, in \emph{test mode}, the \verb{<a>} key will create a copy of the current sequence, which can then be extended with additional moves and saved as a new sequence. The \verb{<A>} key does the same thing, but immediately switches to \emph{add mode} starting at the current position. At the end of a sequence, the right mouse button also switches to \emph{add mode}.

   In \emph{add} or \emph{test mode}, you can bookmark a sequence with the \verb{<<>} key. In \emph{add mode}, the last saved sequence is bookmarked. In \emph{test mode}, the currently played sequence is bookmarked. With the \verb{<>>} key, you can select and manage bookmarks (use \verb{<F1>} while managing the bookmarks to obtain help). This is useful for creating multiple branching lines from the ending position of certain sequences or to remember sequences that you want to come back to at a later point.

   Sequences are saved to and selected from the current sequence directory. You can add, remove, and select sequence directories with the \verb{<F8>} key. Removing a sequence directory simply removes it from being shown in the list of sequence directories (i.e., it does not delete the directory or any sequences therein). Bookmarks are specific to each sequence directory. Also, selecting a sequence directory switches to the sequence selection mode that was active when it was last selected.

   Note that sequences are quite personalized, since they involve going into a particular line that one deems most appropriate (and hence wants to remember). Others may disagree and prefer a different line. Therefore, you should create sequences that you want to train yourself on, so you will be able to play them quickly from memory in actual games.

}
\section{The Board Editor}{

   When in \emph{add mode}, the \verb{<b>} key opens up a board editor. Here, you can quickly set up a more advanced position (e.g., for a chess puzzle). With left-click and drag, pieces can be moved around. With right-click and drag, you can copy a piece that is already on the board. A right-click without a drag (or moving a piece to the left or right of the board) deletes a piece. The \verb{<f>} key flips the board. The \verb{<n>} key resets the board into the starting position. The \verb{<c>} key clears the board. The \verb{<s>} key can be used to select which side plays the first move. The \verb{<r>} key can be used to enter castling availability in FEN notation (K = white can castle kingside, Q = white can castle queenside, k = black can castle kingside, q = black can castle queenside, - = neither side can castle). With the \verb{<o>} key, you can enter a FEN to set up the corresponding position. With \verb{<F1>}, you can receive a help overlay. You can quit the board editor with \verb{<q>} or \verb{<escape>}.

}
\section{Managing and Selecting Players}{

   The trainer can be used by multiple players (i.e., sequence scores and other progress information is player specific). A player can be selected with the \verb{<p>} key. You can either enter a player number, the name of an existing player, or enter a new player name. Note that player names must be syntactically valid (i.e., they should start with a letter and not contain any spaces).

   The current player can be deleted with \verb{<Ctrl-r>}. Since this deletes all progress information for this player from the sequence files (in the current sequence directory), you will receive a prompt to confirm this.

   With \verb{<F2>}, a leaderboard (based on the lowest average score) and player statistics are shown (i.e., the average score, the standard deviation of the scores, the minimum and maximum score, and the total number of rounds). The statistics are based on the currently selected sequences (or all sequences in the current sequence directory if no selection has been applied). Also, sequences whose score was manually set to 0 are not included in these statistics.

   For each player, the trainer also keeps track of each session including the play time. With \verb{<F12>} (or \verb{<Ctrl-2>}), you can obtain a plot of your session history, showing the play time (in minutes) over days (multiple sessions on the same day are collapsed into a single session; sessions shorter than two minutes are not counted). The total play time across all sessions is also shown. With the \verb{<down>/<up>} arrow keys (or the middle mouse button), you can toggle to a view of the number of sequences played over days. You can zoom into a particular time frame by left-clicking twice (once for the start and once for the end of the interval). This can be repeated to zoom in further. Clicking the right mouse button zooms all the way out. With the \verb{<right>/<left>} arrow keys, you can toggle to a view of the history over weeks and months (these views are only available if the session history spans multiple weeks/months). Again, one can zoom into particular time frames in these views by clicking the left mouse button twice. Clicking outside of the graph, right-clicking anywhere when zoomed out, or \verb{<escape>} closes the graph.

}
\section{Position Evaluations via Stockfish}{

   If Stockfish (\url{https://stockfishchess.org}) is installed, it can be used in the background to automatically obtain position evaluations when adding sequences. For this, specify the path (including the executable) to Stockfish via the \code{sfpath} argument of the \code{\link{play}} function. If this is set correctly, then position evaluations will be automatically computed and added to a sequence in \emph{add mode}. Evaluations are given in centipawns divided by 100, except for forced mates, which are given as \eqn{\pm99.9}. When playing a sequence, these evaluations are then shown as an evaluation bar on the left of the board.

   By default, evaluations are based on a search depth of 14 half-moves. Obtaining such an evaluation for a given position should be relatively quick and not add much lag when creating a sequence. In \emph{add mode}, the \verb{<h>} key shows the best move according to Stockfish in a given position. The \verb{<H>} key will run a deeper analysis (by default based on search depth of 24) and show the best move according to Stockfish. Since this may take some time, a progress bar is shown at the top while the analysis is running. With \verb{<F7>}, you can adjust the Stockfish settings, including the calculation depths (for fast and deep evaluations and for the \emph{play mode}; see below), the number of principal evaluations that will be evaluated (for fast and deep evaluations), the number of threads that Stockfish can use during its calculations (the default is 1), the hash size in MB (the default is 256), and the maximum depth of the line(s) shown at the top of the board when asking for the best move. If the pause after moving pieces in \emph{add mode} feels too long, lower the search depth for fast evaluations (e.g., to something like 10) for increased responsiveness.

   The \verb{<u>} key updates the evaluations for all moves in a sequence by recalculating the evaluations with the current setting for a deeper analysis. Note that this can take a bit of time, depending on the number of moves in the sequence and the search depth for deep evaluations. After updating the evaluations, they are automatically saved to the sequence file when in \emph{test mode}.

   Note that asking Stockfish to evaluate impossible positions (e.g., black is in check but it is white's turn to move) crashes Stockfish. This should be handled gracefully by the trainer.

}
\section{Play and Analysis Mode}{

   If Stockfish is running, you can switch into \emph{play mode} at any time with the \verb{<\>} or \verb{<#>} key. This is especially useful at the end of a sequence, to continue playing further moves against the computer from the end position of a sequence (e.g., to practice holding an advantage gained during an opening sequence). In this mode, the computer chooses by default its move according to a search depth of 6 half-moves, but this can be adjusted via \verb{<F7>}. Here, one can also set a limit on the play strength of Stockfish (either as a skill level between 0 and 20 or as an approximate Elo value between 1320 and 3190).

   Using the \verb{<g>} key in \emph{play mode} shows a graph of the position evaluations for the moves played so far (this also works in \emph{add mode}). By default, the y-axis shows the evaluation based on the centipawns (\eqn{cp}) divided by 100, but pressing the middle mouse shows the win percentage (computed with \eqn{50 + 50 \times (2 / (1 + \exp(-0.00368208 * cp)) - 1)}; see here: \verb{https://lichess.org/page/accuracy}).

   Using the \verb{<t>} key, you can take back the last move (i.e., the move played by the computer and your own move), so that it is again your move to play.

   The \verb{<left>}, \verb{<right>}, \verb{<up>}, and \verb{<down>} arrow keys and the \verb{<1>}, \verb{<2>}, \verb{<3>}, \verb{<4>}, and \verb{<5>} keys work the same as described earlier (except that \verb{<1>} always jumps to the start of the game). This automatically takes you into the \emph{analysis mode}, where you can try out variations and/or force the computer to make a different move from a given position. Before doing so, using the \verb{<0>} key, you can store the current game (i.e., sequence of moves) as the main variation. Using the \verb{<9>} key, you can jump back to the end of the main variation.

   Once in \emph{play mode}, the \verb{<\>} or \verb{<#>} key toggles between the analysis and the play mode. On the other hand, \verb{<space>} takes you back to the mode that was active before the \emph{play mode} was initially chosen.

}
\section{Other Settings, Options, and Shortcuts}{

   When playing a sequence, the graphical elements shown on the playing board are created by repeatedly drawing on top of the board. Therefore, when resizing the board, all elements are redrawn, which takes a few moments. With the \verb{<escape>} key, the board can be redrawn in its current state, after which it can be resized without having to wait for all previous graphical elements to be redrawn.

   Clicking the right mouse button on a square adds a circle to the square. Dragging the mouse from one square to another while pressing the right mouse button draws an arrow. Those familiar with the \url{https://lichess.org} website will recognize these visualizations. Clicking with the left mouse button on a square, the \verb{<escape>} key, or making a move removes these visualizations. When adding a sequence, circles and arrows that are drawn are also saved and will be shown when replaying the sequence in \emph{test mode}. When such annotations are shown as part of a move that is made by the trainer, the sequence will only proceed after a click.

   Various other settings can be adjusted while the trainer is running:

   \itemize{
   \item By default, there is a 0.5 second sleep time between moves played by the trainer. This can be decreased/increased with the \verb{<->} and \verb{<+>} keys.
   \item By default, the trainer waits for a mouse click at the end of a sequence before starting a new sequence. You can toggle this on/off with the \verb{<w>} key.
   \item With the \verb{<i>} key, the language of the trainer can be toggled between English and German (author note: I wrote this trainer in part for my chess playing kids, which is the reason why a German translation is available).
   \item With the \verb{<v>} key, the evaluation bar can be toggled on/off. This is mode specific (i.e., one can switch off the evaluation bar when in \emph{test mode}, but still keep it on when in \emph{add mode}). If evaluations are not available, the bar is not shown irrespective of whether it is toggled on or off. If the evaluation bar is turned off in \emph{test mode}, it is still shown at the end of each sequence.
   }

   The settings are stored across sessions and can be shown with \verb{<F3>}. With \verb{<F4>}, you can adjust the colors. With \verb{<F5>}, you can adjust the size of the text at the top, the text at the bottom, and the size of the evaluation value in the evaluation bar (if it is shown).

   With \verb{<F6>}, you can adjust some miscellaneous settings, namely the multiplier for the score when completing a sequence without any mistakes (0.8 by default), the score penalty when making a wrong move (40 by default), the score penalty per hint (20 by default), the number of animation steps for the evaluation bar (5 by default), the time per move for the \emph{timed mode} (5 seconds by default), and the idle time (120 seconds by default; time without any inputs longer than this value is not counted towards the play time for the session history).

   With \verb{<F9>}, the FEN of the current position is printed to the console and the position is opened on the \url{https://lichess.org} website for analysis. With \verb{<Ctrl-f>}, the FEN is printed and copied to the clipboard (this makes use of the \code{\link[clipr]{write_clip}} function from the \pkg{clipr} package). With \verb{<Ctrl-c>}, the current sequence name is printed and copied to the clipboard in \emph{test mode}.

   With \verb{<F10>}, you can obtain histograms of the scores of the (selected) sequences, how many times they have been played so far (the number of rounds), how long ago they were played, their difficulty, and a scatterplot of the number of times they have been played versus their scores. In the scatterplot, the downward sloping dashed line indicates the best possible performance (i.e., never making a mistake when playing the sequence over and over) and the solid line the actual performance (as estimated based on a regression model; only shown when there are 5 or more points). If a target score value above 0 has been set (see above), the horizontal dotted line indicates this value.

}
\section{Playing Sounds}{

   The trainer plays various sounds (e.g., when playing moves, when capturing pieces, when making a wrong move, when completing a sequence). This may or may not work depending on your operating system and configuration. See the \code{\link{playsound}} function for further details.

   The sound volume can be decreased/increased with the \verb{<[>} and \verb{<]>} keys. Setting the volume to 0 disables the sound. This can also be useful if playing sounds adds too much lag.

}
\section{Resetting and Cleaning Up}{

   If you would like to remove the config files (for the settings and colors), you can use the following two commands:

\preformatted{file.remove(file.path(tools::R_user_dir(package="chesstrainer", which="config"), "settings.rds"))
file.remove(file.path(tools::R_user_dir(package="chesstrainer", which="config"), "colors.rds"))}

   This may be useful in case these files were corrupted with nonsensical settings. The files will be recreated (with default values) when restarting the trainer. If you would like to remove both of these files and the directory where these files are stored, you can use:

\preformatted{unlink(tools::R_user_dir(package="chesstrainer", which="config"), recursive=TRUE)}

   If you would like to remove the default directory where sequences are stored (and all files therein), you can use:

\preformatted{unlink(file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sequences"), recursive=TRUE)}

   If you would like to remove the directory where the session history of all players is stored (and all files therein), you can use:

\preformatted{unlink(file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sessions"), recursive=TRUE)}

}
\section{Backing Up Sequences}{

   It is advisable to regularly make a backup of the sequences you have created. The default directory where sequences are stored can be found with the command: \code{file.path(tools::R_user_dir(package="chesstrainer", which="data"), "sequences").}

}
\section{License Info}{

   The chess pieces used in the package were obtained from \verb{https://commons.wikimedia.org/wiki/Category:SVG_chess_pieces} where they are available under the Creative Commons Attribution-Share Alike 3.0 Unported License.

   The sound files used in the package were obtained from \verb{https://github.com/lichess-org/lila/tree/master} where they are available under the GNU Affero General Public License.

}
\note{
   The trainer makes extensive use of \code{\link{getGraphicsEvent}} to capture mouse movements and keyboard inputs. Only some graphics devices support this. If you receive the error message \sQuote{The graphics device does not support event handling} when starting the trainer, then the current graphics device does not support this kind of interactivity. For example, this will be the case for the \code{RStudioGD} graphics device that is used by RStudio. You can then try running \code{x11()} before starting the trainer with \code{play()}.
}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com})
}
\keyword{package}
