\name{chesstrainer-package}
\alias{chesstrainer-package}
\alias{chesstrainer}
\docType{package}
\title{chesstrainer: Create and Test Yourself on Chess Lines}
\description{

   The purpose of the \pkg{chesstrainer} package is to create sequences of moves (lines), save them, and then test yourself on these sequences. This is especially useful for training openings, but can also be used for creating chess puzzles or training tactics.

}
\section{Starting the Trainer}{

   To start the trainer, run \code{\link{play}()} from the console. When starting the trainer for the first time, a directory is created for storing settings and a directory where sequences will be stored. A few example sequences also come with the package and you will receive a prompt whether to copy these sequences to the sequence directory. To familiarize yourself with the trainer, it would be good to do so.

   You will then receive a prompt to enter a player name. Choose a name and hit enter.

   The trainer then starts in \sQuote{add} mode (see the \sQuote{Note} section below if you receive an error that the graphics device does not support event handling). For now, click on the window showing the chess board and hit \code{<space>} (i.e., the space bar), which will switch to the \sQuote{test} mode. One of the existing sequences will then be chosen at random. The trainer may immediately prompt you to play a certain opening, it may make some moves for you, or it may start at a later position during a game. Either way, you will then have to make the correct move based on the position. You do this by left-clicking and dragging the piece to be played from its current position to the desired target square. The trainer will then make the next move, after which you will then again have to make the correct move. This will continue until the end of the sequence. If you do not know what the correct move is, the \code{<h>} key will provide a hint showing which piece to move. Hitting \code{<h>} for a second time will show the target square.

   In test mode, information is shown at the bottom about the current mode, the player name, the name of the current sequence, the current move number and the total number of moves of the sequence, the number of times that the sequence has been played so far, and the sequence score (see below).

   With \code{<F1>}, you can receive an overview of all keyboard shortcuts. These will be explained in further detail below. You can quit the trainer with the \code{<q>} key.

}
\section{The Sequence Score}{

   The trainer keeps track of a score for each sequence. For a new sequence, the score starts out at 100. The score is multiplied by 0.8 after completing a sequence without any mistakes. When making a wrong move when playing a sequence, the score is increased by 40 points (up to a maximum of 100). Receiving a hint increases the score by 20 points (40 points when asking for both the starting and the target square).

   The trainer uses the score to choose the next sequence. Sequences with higher scores have a higher chance to be presented. The probability of playing a particular sequence is computed with \eqn{p_i = s_i^e / \sum s_i^e}, where \eqn{s_i} is the score of the \eqn{i}th sequence and \eqn{e} is the \sQuote{exponent value}. This is set to 2 by default and enhances the probability that sequences with higher scores are more likely to be played. The exponent value can be adjusted with the \code{<^>} or \code{<6>} key. Setting the exponent value to 0 makes all sequences equally likely to be played. Setting it to a large value (e.g., 100) almost guarantees that the sequence with the highest score is played next. The score for the current sequence can also be set manually with the \code{<o>} key. Setting the score to 0 means that the sequence has a zero probability of being presented.

   Note that the trainer does not check for illegal or impossible moves. Therefore, if you make such a move by accident when playing a sequence (and hence receive the 40 point score penalty described above), you can use the \code{<t>} key to take the score adjustment back.

   After completing a sequence, the current score for the sequence is saved (together with the number of times that the sequence has been played and when it was last played) to the sequence file. This way, your progress in learning the sequence is saved permanently across sessions. Note that this information is player specific. Using the \code{<g>} key, a progress graph for the current sequence can be shown (if the sequence has been played before). Using the \code{<G>} key, you can toggle whether the progress graph will be automatically shown at the end of each completed sequence.

}
\section{Managing and Selecting Sequences}{

   Using the \code{<l>} key, you can list all sequences, together with their current score, the number of times they have been played (the play frequency), the last time they have been played (in days; \code{NA} if a sequence has not been played before), and the current play probability (with 0 to 5 asterisks next to it based on the probability). In \sQuote{test} mode, you can use the \code{Ctrl-<d>} key to delete the current sequence. Note that this is permanent and all progress information about the sequence will be lost. You will receive a prompt to confirm the deletion.

   Using the \code{</>} key (or \code{<,>} which is useful for a German keyboard layout), you can select a subset of the sequences in various ways:

   \itemize{
   \item by entering a search term (which is applied as a regular expression to the filenames of the sequences),
   \item by entering a sequence number (e.g., \code{2} to select the 2nd sequence),
   \item by entering a sequence number range (e.g., \code{2-5} to select sequences 2 to 5),
   \item by entering something like \code{score > 80} to select sequences with a score above a certain threshold,
   \item by entering something like \code{played < 1} to select sequences that have been played less than a certain number of times,
   \item by entering something like \code{days > 2} to select sequences that have been played more than a certain number of days ago,
   \item by entering a \href{https://en.wikipedia.org/wiki/Forsyth-Edwards_Notation}{FEN} to select all sequences that contain a position matching the FEN,
   \item by entering \code{c: } followed by a (case insensitive) text string to select all sequences where a comment contains the text string.
   }

   The keywords \code{score}, \code{played}, and \code{days} can be abbreviated to \code{s}, \code{p} , and \code{d} (so \code{s > 80}, \code{p < 1}, and \code{d > 2} would also work). Other comparators such as \code{>=}, \code{<}, and \code{>=} can also be used (e.g., \code{s <= 10}).

   Hitting \code{<enter>} at the prompt (i.e., not entering any search string) keeps the current selection. Entering \code{*} at the prompt selects all sequences. The \code{<*>} or \code{<8>} keys also select all sequences without using the \code{</>} key first.

   In \sQuote{test} mode, sequences are played by default at random based on the score as described above. However, you can toggle into a different selection mode with the \code{<m>} key. The options are:
   \enumerate{
   \item selection based on the score at random where sequences with higher scores have a higher play probability (the default),
   \item selection based on the score where the sequence with the highest score is played next,
   \item selection based on the play frequency at random where sequences with lower play frequencies have a higher play probability,
   \item selection based on the play frequency where the sequence with the lowest frequency is played next,
   \item selection based on the date at random where sequences that have last been played longer ago have a higher play probability,
   \item selection based on the date where the oldest sequence is played next,
   \item sequential, where the sequences are played in order.
   }

   The exponent value affects selection modes 1, 3, and 5. Setting the exponent value to a very large value makes mode 1 essentially the same as mode 2 (and similarly, mode 3 like mode 4 and mode 5 like mode 6). For modes 2, 4, and 6, if the highest score, lowest play frequency, or oldest date is the same for multiple sequences, the alphabetically first one is played next. For modes 5 and 6, sequences that have never been played before are given the highest priority. For all modes, sequences whose score was set to 0 are never played.

   In \sQuote{test} mode, the \code{<r>} key repeats the last played sequence. With the \code{<R>} key, you can toggle on/off whether a sequence will be automatically repeated if a mistake was made while playing the sequence.

}
\section{Timed Mode}{

   With the \code{<x>} key, you can switch into a timed mode. In this mode, all moves played during a sequence must be completed in a certain amount of time, which is given by the total number of moves that must be played in the sequence times the current setting for \sQuote{time per move} (which is 5 seconds by default). For example, if 8 moves must be played, then you have \eqn{8 \times 5 = 40} seconds to complete all moves (the time taken by the trainer for playing moves is not counted). Not completing the moves within this amount of time is treated like a mistake (i.e., the score is increased by 40 points).

   During timed mode, the bar on the right indicates whether the current playing time is above (in green) or below (in red) the amount of time you should have spent on the moves so far (i.e., if every move took exactly the amount of time allowed by the \sQuote{time per move} setting).

   The \sQuote{time per move} setting can be adjusted via the \code{<F6>} key.

}
\section{Adding New Sequences}{

   As described above, the trainer comes with a few example sequences to teach you how it can be used, but serious use of the trainer requires that you add your own sequences. With \code{<space>}, you can switch into the \sQuote{add} mode, where you can enter new sequences. The first thing to decide is whether the player should play with white or black. With the \code{<f>} key, the board can be flipped accordingly (the player always plays with the pieces at the bottom). Next, you need to decide whether some moves will be automatically played at the beginning of a sequence. If some moves should be played automatically, then simply make these moves on the board. Note that there is no check if the moves are legal and/or are played in the correct order, so care must be taken to do so appropriately. Once you arrive at the position where a player should start making moves, hit the \code{<z>} key (note that \sQuote{Show} at the bottom left then turns from \sQuote{Yes} to \sQuote{No}). Now continue playing the line as it should be played. The sequence should end with a move by the player. If you make a mistake when playing a move, use the \code{<t>} key to take the move back. Once the sequence is complete, use the \code{<s>} key to save the sequence. You will be prompted for a filename. Once the sequence has been saved, it will automatically become part of the sequences that may be presented in \sQuote{test} mode.

   A sequence may also start immediately in the starting position. Here, you will hit the \code{<z>} key right at the beginning. Since it is then essentially impossible for a player to know what they should play (since there are many sensible moves one could make), you will have to instruct the player what they should do. Using the \code{<c>} key, you can add a comment to the current move which will be shown to the player. For example, you could tell the player to play the Spanish (Ruy Lopez) opening. You should then make the appropriate moves on the board.

   Comments can be added to any move using the \code{<c>} key. This may be useful if one could venture into multiple sensible lines in a given position. You would then have to instruct the player to make a particular move (either by telling the player directly which move to make or giving some appropriate hint, such as the name of a particular line/variation). Comments are also useful for mentioning variation names or other useful information.

   If a sequence should commence not at the starting position of a chess game, but later on, you can move the pieces into the desired position and then use the \code{<0>} key to make the current position the starting position for a particular sequence. Alternatively, you can use the \code{<b>} key to start a board editor (see below for details).

   The \code{<e>} key can be used to edit the comments for a particular sequence. This way, you can also add a \sQuote{start comment} and/or an \sQuote{end comment} to a particular sequence (the former will be shown in an overlay box before the sequence starts, the latter at the top after the last move). The \code{<E>} key opens up the \code{\link[=edit]{data editor}} on the data frame that contains the moves for the current sequence. This can also be used to add, edit, and remove comments in the \code{comment} column or make adjustments to the other columns (but this should only be done if their meaning is clear).

   With the \code{<?>} key, you can search for sequences that start with the same moves that have been entered/played so far (this also works in \sQuote{test} mode). If any matching sequences are found, a prompt will ask if they should be selected. This is useful for checking what sequences are already available based on some initial moves (and then practicing these sequences) or finding other sequences similar to the one currently being played. Alternatively, with the \code{<'>} key, you can search for sequences that include the same position (irrespective of the particular sequence of moves that led to the position).

   With the \code{<.>} key, you can select the last saved sequence (if one was saved before in the current session). This is useful for checking the sequence in \sQuote{test} mode and/or creating multiple branching lines from the ending position of this sequence. Similarly, in \sQuote{test} mode, the \code{<a>} key will create a copy of the current sequence, which can then be extended with additional moves (and saved as a new sequence). The \code{<A>} key does the same thing, but only plays the moves to the currently shown position.

   Sequences are saved to and selected from the current sequence directory. You can add, remove, and select sequence directories with the \code{<F8>} key. Removing a sequence directory simply removes it from being shown in the list of sequence directories (i.e., it does not delete the directory).

   Note that sequences are quite personalized, since they involve going into a particular line that one deems most appropriate (and hence wants to remember). Others may disagree and prefer a different line. Therefore, you should create sequences that you want to train yourself on, so you will be able to play them quickly from memory in actual games.

}
\section{The Board Editor}{

   When in \sQuote{add} mode, the \code{<b>} key opens up a board editor. Here, you can quickly set up a more advanced position (e.g., for a chess puzzle). With left-click and drag, pieces can be moved around. With right-click and drag, you can copy a piece. A right-click without a drag deletes a piece. The \code{<f>} key flips the board. The \code{<n>} key resets the board into the starting position. The \code{<c>} key clears the board. The \code{<s>} key can be used to select which side plays the first move. With \code{<F1>}, you can receive a help overlay. You can quit the board editor with the \code{<q>} or \code{<escape>} key.

}
\section{Managing and Selecting Players}{

   The trainer can be used by multiple players. A player can be selected with the \code{<p>} key. You can either enter a player number, the name of an existing player, or enter a new player name. Note that player names must be syntactically valid (i.e., should start with a letter and not contain any spaces).

   The current player can be deleted with the \code{Ctrl-<r>} key. Since this deletes all progress information for this player from the sequence files, you will receive a prompt to confirm this.

   With \code{<F2>}, a leaderboard (based on the lowest average score) and player statistics are shown (i.e., the average score, the standard deviation of the scores, the minimum and maximum score, and the total number of sequences played). The statistics are based on the currently selected sequences (or all sequences if no selection has been applied). Also, sequences whose score was manually set to 0 are not included in these statistics.

}
\section{Playing Sounds}{

   The trainer plays various sounds (when playing moves, when capturing pieces, when making a wrong move, or when completing a sequence). This may or may not work depending on your operating system and configuration. See the \code{\link{playsound}} function for further details.

   The sound volume can be decreased/increased with the \code{<[>} and \code{<]>} keys. If the sound is not working, setting the volume to 0 disables the sound. This can also be useful if playing sounds adds too much lag.

}
\section{Position Evaluations via Stockfish}{

   If Stockfish (\url{https://stockfishchess.org}) is installed, it can be used in the background to automatically obtain position evaluations when adding sequences. For this, specify the path (including the executable) to Stockfish via the \code{sfpath} argument of the \code{\link{play}} function. If this is set correctly, then position evaluations will be automatically computed and added to a sequence in \sQuote{add} mode. Evaluations are given in centipawns divided by 100, except for forced mates, which are given as \eqn{\pm99.9}. When playing a sequence, these evaluations are then shown as an evaluation bar on the left of the board.

   By default, evaluations are based on a search depth of 20 half-moves. Obtaining such an evaluation for a given position should be relatively quick and not add much lag when creating a sequence. In \sQuote{add} mode, the \code{<h>} key shows the best move according to Stockfish in a given position. The \code{<H>} key will run a deeper analysis (by default based on search depth of 30) and show the best move according to Stockfish. Since this may take some time, a progress bar is shown at the top while the analysis is running. With \code{<F7>}, you can adjust the Stockfish settings, including the calculation depths (for fast and deep evaluations), the number of principal evaluations that will be evaluated (for fast and deep evaluations), the number of threads that Stockfish can use during its calculations (the default is 1), and the hash size in MB (the default is 256).

   In \sQuote{test} mode, the \code{<u>} key updates the evaluations for all moves in a sequence by recalculating the evaluations with the current setting for a deeper analysis. Note that this can take a bit of time, depending on the number of moves in the sequence. After updating the evaluations, they are automatically saved to the sequence file.

   Note that asking Stockfish to evaluate impossible positions (e.g., black is in check but it is white's turn to move) crashes Stockfish. This should be handled gracefully by the trainer. With \code{<F7>}, you can restart Stockfish.

}
\section{Play Mode}{

   If Stockfish is running, you can switch into \sQuote{play} mode with the \code{<\>} or \code{<#>} key. This is especially useful at the end of a sequence, to continue playing further moves against the computer from the end position of a sequence (e.g., to practice holding an advantage gained during an opening sequence). As noted above, illegal positions crash Stockfish. Use \code{<F7>} to restart it. Also, there is no check if the moves are legal and/or are played for the correct side/color.

}
\section{Other Options and Shortcuts}{

   When playing a sequence, the graphical elements shown on the playing board are created by repeatedly drawing on top of the board. Therefore, when resizing the board, all elements are redrawn, which takes a few moments. With the \code{<escape>} key, the board can be redrawn in its current state, after which it can be resized without having to wait for all previous graphical elements to be redrawn.

   Clicking the right mouse button on a square adds a circle to the square. Dragging the mouse from one square to another while pressing the right mouse button draws an arrow. Those familiar with the \url{https://lichess.org} website will recognize these visualizations. Clicking with the left mouse button on a square, the \code{<escape>} key, or making a move removes these visualizations. When adding a sequence, circles and arrows that are drawn are also saved and will be shown when replaying the sequence in \sQuote{test} mode. When such annotations are shown as part of a move that is made by the trainer, the sequence will only proceed after a click.

   Various other settings can be adjusted while the trainer is running:

   \itemize{
   \item With the \code{<(>} and \code{<)>} keys, the line width for drawing rectangles can be decreased/increased. With some trial and error, adjust this value such that the rectangles that are drawn around squares are clearly visible, but do not overlap into adjacent squares.
   \item By default, there is a 0.5 second sleep time between moves played by the trainer. This can be decreased/increased with the \code{<->} and \code{<+>} keys.
   \item By default, the trainer waits for a mouse click at the end of a sequence before starting a new sequence. You can toggle this on/off with the \code{<w>} key.
   \item With the \code{<i>} key, the language of the trainer can be toggled between English and German (author note: I wrote this trainer in part for my chess playing kids, which is the reason why a German translation is available).
   \item With the \code{<v>} key, the evaluation bar can be toggled on/off (if no evaluations are available for a sequence, the bar is never shown).
   }

   The settings are stored across sessions and can be shown with \code{<F3>}. With \code{<F4>}, you can adjust the colors. With \code{<F5>}, you can adjust the size of the text at the top, the text at the bottom, and the size of the evaluation value in the evaluation bar (if it is shown).

   With \code{<F6>}, you can adjust some miscellaneous settings, namely the multiplier for the score when completing a sequence without any mistakes (0.8 by default), the score penalty when making a wrong move (40 by default), the score penalty per hint (20 by default), the number of animation steps for the evaluation bar (10 by default), and the time per move for the timed mode (5 seconds by default).

   With \code{<F9>}, the FEN of the current position is printed and the position is opened on the \url{https://lichess.org} website for analysis. With \code{Ctrl-<c>}, the FEN is copied to the clipboard (this makes use of the \code{\link[clipr]{write_clip}} function from the \code{clipr} package).

   With \code{<F10>}, you can obtain histograms of the scores of the (selected) sequences, how many times they have been played, how long ago they were played, and a scatterplot of the number of times they have been played versus their scores. In the scatterplot, the dotted line indicates the best possible performance (i.e., never making a mistake when playing the sequence over and over) and the solid line the actual performance (as estimated based on a regression model; only shown when there are 5 or more points).

}
\section{Resetting and Cleaning Up}{

   If you would like to remove the config files (for the settings and colors), you can use the following two commands:

\preformatted{file.remove(file.path(tools::R_user_dir(package="chesstrainer", which="config"), "settings.rds"))
file.remove(file.path(tools::R_user_dir(package="chesstrainer", which="config"), "colors.rds"))}

   This may be useful in case these files were corrupted with nonsensical settings. The files will be recreated (with default values) when restarting the trainer. If you would like to remove both of these files (if they exist) and the directory where these files are stored, you can use:

\preformatted{unlink(tools::R_user_dir(package="chesstrainer", which="config"), recursive=TRUE)}

   If you would like to remove all files stored in the default directory where sequences are stored, you can use:

\preformatted{file.remove(list.files(tools::R_user_dir(package="chesstrainer", which="data"), full.names=TRUE))}

   To remove the corresponding folder (including all files therein), you can use:

\preformatted{unlink(tools::R_user_dir(package="chesstrainer", which="data"), recursive=TRUE)}

}
\section{Backing Up Sequences}{

   It is advisable to regularly make a backup of the sequences you have created. The directory where sequences are stored can be found with the command: \code{tools::R_user_dir(package="chesstrainer", which="data").}

}
\section{License Info}{

   The chess pieces used in the package were obtained from \verb{https://commons.wikimedia.org/wiki/Category:SVG_chess_pieces} where they are available under the Creative Commons Attribution-Share Alike 3.0 Unported License.

   The sound files used in the package were obtained from \verb{https://github.com/lichess-org/lila/tree/master} where they are available under the GNU Affero General Public License.

}
\note{
   The trainer makes extensive use of \code{\link{getGraphicsEvent}} to capture mouse movements and keyboard inputs. Only some graphics devices support this. If you receive the error message \sQuote{The graphics device does not support event handling}, then the default graphics device that was opened does not support event handling. For example, this will be the case for the \code{RStudioGD} graphics device that is used by RStudio. You can then try running \code{x11()} before starting the trainer with \code{play()}.
}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com})
}
\keyword{package}
