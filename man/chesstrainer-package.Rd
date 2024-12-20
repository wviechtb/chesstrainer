\name{chesstrainer-package}
\alias{chesstrainer-package}
\alias{chesstrainer}
\docType{package}
\title{chesstrainer: Create and Quiz Yourself on Chess Lines}
\description{

   The purpose of the \pkg{chesstrainer} package is to create sequences of moves (lines), save them, and then quiz yourself on these sequences. This is especially useful for training openings, but can also be used for creating chess puzzles.

}
\section{Starting the Trainer for the First Time}{

   To start the trainer, run \code{play()} from the console (see the \code{\link{play}} function for some startup arguments that can be adjusted). When starting the trainer for the first time, a directory is created for storing settings and a directory where sequences will be stored. A few example sequences also come with the package and you will receive a prompt whether to copy these sequences to the sequence directory. To familiarize yourself with the trainer, it would be good to do so.

   You will then receive a prompt to enter a player name. Choose a name and hit enter.

   The trainer then starts in \sQuote{add} mode (see the \sQuote{Note} section below if you receive an error that the graphics device does not support event handling). For now, you should hit \code{<space>} (i.e., the spacebar), which will switch to \sQuote{play} mode. One of the existing sequences will then be chosen at random. The trainer may immediately prompt you to play a certain opening, it may make some moves for you, or it may start at a later position during a game. Either way, you will then have to make the correct move based on the position. You do this by dragging the piece to be played from its current position to the desired target square. The trainer will then make the next move, after which you will then again have to make the correct move. This will continue until the end of the sequence. If you do not know what the correct move is, the \code{<h>} key will provide a hint by showing which piece to move. Hitting \code{<h>} for a second time will show the target square.

   In play mode, information is shown at the bottom about the current mode, the player name, the name of the current sequence, the current move number and the total number of moves of the sequence, the score (see below), and the number of times that the sequence has been played.

   With \code{<F1>}, you can receive an overview of all keyboard shortcuts (use \code{<Escape>} to close it). You can quit the trainer with the \code{<q>} key.

}
\section{The Score}{

   The trainer keeps track of a score for each sequence. For a new sequence, the score starts out at 100. The score is multiplied by 0.80 after completing a sequence (rounded up to the nearest integer). When making the wrong move when playing a sequence, the score is increased by 50 points (up to a maximum of 100). Receiving a hint increases the score by 25 points (50 points when asking for both the starting and target square).

   The trainer uses the score to select the next sequence to play. Sequences with higher scores have a higher chance to be presented. The probability of selecting a particular sequence is computed with \eqn{p_i = s_i^e / \sum(s_i^e)}, where \eqn{s_i} is the score of the \eqn{i}th sequence and \eqn{e} is the \sQuote{exponent value}. This is set by default to 2 and enhances the probability that sequences with a large score are more likely to be selected. The exponent value can be adjusted with the \code{<^>} key. Setting the exponent value to 0 makes all sequences equally likely to be selected. Setting it to a large value (e.g., 100) almost guarantees that the sequence with the highest score is selected next. The score for the current sequence can also be set manually with the \code{<o>} key. Setting the score to 0 means that the sequence has a zero probability of being selected.

   If you make a misclick during a move and want to take the score adjustment back, you can do so with the \code{<t>} key.

   After completing a sequence, the current score for the sequence is saved (together with the number of times that the sequence has been played) to the sequence file. This way, your progress in learning the sequences is saved permanently across sessions. Note that the scores are player specific.

}
\section{Managing/Selecting Sequences}{

   Using the \code{<l>} key, you can list all sequences, together with their current score, the number of times they have been played, and the current probability of selection. During \sQuote{play} mode, you can use the \code{Ctrl-<d>} key to delete the current sequence. Note that this is permanent and all progress information about the sequence will be lost. You will receive a prompt to confirm the deletion.

   Using the \code{</>} (or \code{<.>}) key, you can select a subset of the sequences in various ways:

   \itemize{
   \item by entering a search term (which is applied as a regular expression to the filenames of the sequences),
   \item by entering a sequence number (e.g., \code{2} to select the 2nd sequence),
   \item by entering a sequence number range (e.g., \code{2-5} to select sequences 2 to 5),
   \item by entering something like \code{score >= 80} to select sequences with a score above (or below) a certain threshold,
   \item by entering something like \code{played <= 1} to select sequences that have been played a minimum/maximum number of times.
   }

   Hitting \code{<enter>} at the prompt selects all sequences.

   By default, sequences are selected at random during \sQuote{play} mode (as described above). However, one can toggle into a sequential mode with the \code{<r>} key, in which case sequences are played in order.

}
\section{Adding New Sequences}{

   With \code{<space>}, you can switch into the \sQuote{add} mode. This is where you can enter new sequences. The first thing to decide is whether the player should play with white or black. With the \code{<f>} key, the board can be flipped accordingly (the player always plays with the pieces at the bottom). Next, you need to decide whether some moves will be automatically played at the beginning of a sequence. If some moves should be played automatically, then simply make these moves on the board. Note that there is no check if the moves are legal and/or are played in the correct sequence, so care must be taken to do so appropriately. Once you arrive at the position where a player should start making moves, hit the \code{<z>} key (note that \sQuote{Show} at the bottom left then turns from \sQuote{Yes} to \sQuote{No}). Now continue playing the line as it should be played. The sequence should end with a move of the player. If you make a mistake when playing a move, use the \code{<b>} key to take the move back. Once the sequence is complete, use the \code{<s>} key to save the sequence. You will be prompted for a filename. Once the sequence has been saved, it will automatically become part of the sequences that may be selected in \sQuote{play} mode.

   A sequence may also start immediately in the starting position. Here, you will hit the \code{<z>} key right at the beginning. Since it is then essentially impossible for a player to know what they should play (since there are many sensible moves one could make), you will have to instruct the player what they should do. Using the \code{<c>} key, you can add a comment to the current move which will be shown to the player. For example, you could tell the player to play the Spanish (Ruy Lopez) opening. You should then make the appropriate moves on the board.

   Comments can be added to any move using the \code{<c>} key. At times this may be useful if one could venture into multiple sensible lines in a given position. You would then have to instruct the player to make a particular move.

   If a sequence should start not at the starting position of a chess game, but later on, you can move the pieces into the desired position and then use the \code{<0>} key to make the current position the starting position for a particular sequence. This way, one can create chess puzzles, such as asking the player to find a mate in a certain number of moves. This is currently tedious (since many pieces may need to be moved around and removing pieces is only possible by capturing them). In the future, the package will include a proper board editor.

   When saving a sequence, the color of the pieces to be played is automatically prepended to the filename (i.e., as \code{w_} or \code{b_}). This can be useful when selecting sequences (e.g., using the search term \code{^w_} will select all sequences that start with \code{w_} and are hence sequences where the player plays with white).

   The \code{<e>} key opens up the \code{\link[=edit]{data editor}} on the data frame that contains the moves for the current sequence. This is most useful for adding or removing comments in the \code{comment} column. Making adjustments to the other columns should only be done if their meaning is clear.

   In \sQuote{play} mode, the \code{<a>} key will create a copy of the current sequence, which can then be extended with additional moves and saved as a new sequence. This is useful for creating multiple branching lines from a given sequence.

   Note that sequences are quite personalized, since they involve going into a particular line that one deems most appropriate. Others may disagree and prefer a different line. Therefore, you should create sequences that you want to train yourself on, so you will be able to play them quickly from memory.

}
\section{Managing/Selecting Players}{

   The trainer can be used by multiple players. A player can be selected with the \code{<p>} key. One can either enter a player number, the name of an existing player, or enter a new player name. The current player can be deleted with the \code{Ctrl-<r>} key. Since this deletes all progress information for this player from the sequence files, you will receive a prompt to confirm this.

   With \code{<F2>}, a leaderboard (based on the lowest average score) and player statistics are shown (i.e., the average score, the standard deviation of the scores, the minimum and maximum score, and the total number of sequences played). Sequences whose score was manually set to 0 are not included in these statistics.

}
\section{Sound}{

   The trainer plays various sounds (when playing moves, when capturing pieces, when making a wrong move, or when completing a sequence). This may or may not work depending on your operating system and configuration. See the \code{\link{playsound}} function for further details.

   If the sound is working, it can be toggled on/off with the \code{<m>} key. This can be useful if playing sounds adds too much lag. The sound volume can be decreased/increased with the \code{<[>} and \code{<]>} keys.

}
\section{Other Options / Keyboard Shortcuts}{

   When playing a sequence, the graphical elements shown on the playing board are created by repeatedly drawing on top of the board. Therefore, when resizing the board, all elements are redrawn, which takes a few moments. With the \code{<u>} key, the board can be redrawn in its current state, after which it can be resized without having to wait for all previous graphical elements to be redrawn.

   Clicking the right mouse button on a square adds a circle to the square. Dragging the mouse from one square to another while pressing the right mouse button draws an arrow. Those familiar with the \verb{https://lichess.org} website will recognize these visualizations. Clicking with the left mouse button on a square, the \code{<u>} key, or making a move removes these visualizations.

   Various other settings can be adjusted while the trainer is running:

   \itemize{
   \item With the \code{<(>} and \code{<)>} keys, the line width for drawing rectangles can be decreased/increased. With some trial and error, one can adjust this value such that the rectangles that are drawn around squares are clearly visible, but do not overlap into adjacent squares.
   \item By default, there is a 0.5 second sleep time between moves played by the trainer. This can be decreased/increased with the \code{<->} and \code{<+>} keys.
   \item By default, the trainer waits after completing a sequence before starting a new sequence. One can toggle this pause on/off with the \code{<w>} key.
   \item With the \code{<i>} key, the language of the trainer can be toggled between English and German (author note: I wrote this trainer in part for my chess playing kids, which is the reason why a German translation is available).
   }

   The settings are automatically stored in a file called \code{settings.rds} at \code{tools::R_user_dir(package="chesstrainer", which="config")} and are read in when starting the trainer. The current settings can be shown with \code{<F3>}.

   With \code{<F4>}, one can adjust the colors. The colors are automatically stored in a file called \code{colors.rds} at \code{tools::R_user_dir(package="chesstrainer", which="config")} and are read in when starting the trainer.

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
