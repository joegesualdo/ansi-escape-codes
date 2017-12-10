## ansi-escape-codes
> Haskell package to generate ANSI escape codes for styling strings in the terminal Edit

## Install
```
Coming soon...
```

## Usage
```haskell
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
```

## Test
```
Coming soon...
```

## API
Coming soon

## License
MIT © [Joe Gesualdo]()
