import  AnsiEscapeCodes

main :: IO ()
main = do
  putStr $
    "Hi"
    ++ (show (CursorForwardSequence 1))
    ++ (show (CursorForwardSequence 1))
    ++ "B"
    ++ (show (CursorForwardSequence 1))
    ++ (show (CursorForwardSequence 1))
    ++ "O"
    ++ (show (CursorForwardSequence 1))
    ++ (show (CursorForwardSequence 1))
    ++ "0"
