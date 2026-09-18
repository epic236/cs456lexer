
THIS CODE WAS WRITTEN IN ACCORDANCE WITH THE COLLABORATION AND AI POLICIES FOR THIS COURSE. Ethan Hsiung, Mingyang Li

Comments are handled by a global variable integer commentDepth, which serves as a counter. It is normally set to 0, meaning the file is currently in the initial state and not in a comment state. When /* is first reached, the state is changed to comment state and commentDepth goes up by 1. Within the comment state, every time a /* is reached, commentDepth goes up by 1. Whenever */ is reached, commentDepth goes down by 1, and when commentDepth reaches 0, the lexer is no longer in the comment state and returned back to the initial state.

Strings are a simpler case, there is a global variable boolean inString that is initially set to false, and whenever \" is reached, it flips the state of inString. Special tokens within strings have and additional backslash to separate them from being recognized as those from the initial state.

Errors are handled by ErrorMsg.error, where the byte position yypos and line number yytext is returned at the position of the error. The specific error cases are handled by catching the disallowed character or escape sequence, or ascii out of range, and continue() keeps the lexer running instead of crashing at the point of the error.

End of file is checked by eof(), where it checks whether the state is in comment or string. If so, it returns the appropriate error message.

For testing, we used the given test cases and created our own, with nested comments, strings with spaces, tabs, newlines, and gaps in them, and unclosed comments and strings. We ran Parse.parse on all of the test cases and ensured it returned the correct results.

Other implementation features we figured are worth noting are the inclusion of gaps within strings, as whitespaces within strings should contribute to the character location and line count.

We used AI extensively for debugging, as small discrepancies in the characters ended up breaking the lexer quite a bit. The major design decisions were made by us, and we implemented all of the token matching ourselves, but to catch the cases such as gaps within strings, it was Claude that suggested it. We also used Claude to help implement the string buffer.