# haskell lecture code

## Simple I/O operations
###Output functions
http://downloads.haskell.org/~ghc/7.6.3/docs/html/libraries/base/System-IO.html#t:IO
####putChar :: Char -> IO ()

Write a character to the standard output device (same as hPutChar stdout).

####putStr :: String -> IO ()

Write a string to the standard output device (same as hPutStr stdout).

####putStrLn :: String -> IO ()

The same as putStr, but adds a newline character.

####print :: Show a => a -> IO ()

The print function outputs a value of any printable type to the standard output device. Printable types are those that are instances of class Show; print converts values to strings for output using the show operation and adds a newline.


###Input functions

####getChar :: IO Char

Read a character from the standard input device (same as hGetChar stdin).

####getLine :: IO String

Read a line from the standard input device (same as hGetLine stdin).

####getContents :: IO String

The getContents operation returns all user input as a single string, which is read lazily as it is needed (same as hGetContents stdin).

####interact :: (String -> String) -> IO ()

The interact function takes a function of type String->String as its argument. The entire input from the standard input device is passed to this function as its argument, and the resulting string is output on the standard output device.

###Files

####type FilePath = String

File and directory names are values of type String, whose precise meaning is operating system dependent. Files can be opened, yielding a handle which can then be used to operate on the contents of that file.

####readFile :: FilePath -> IO String

The readFile function reads a file and returns the contents of the file as a string. The file is read lazily, on demand, as with getContents.

####writeFile :: FilePath -> String -> IO ()

The computation writeFile file str function writes the string str, to the file file.

####appendFile :: FilePath -> String -> IO ()

The computation appendFile file str function appends the string str, to the file file.

Note that writeFile and appendFile write a literal string to a file. To write a value of any printable type, as with print, use the show function to convert the value to a string first.

main = appendFile "squares" (show [(x,x*x) | x <- [0,0.1..2]])

####readIO :: Read a => String -> IO a

The readIO function is similar to read except that it signals parse failure to the IO monad instead of terminating the program.

####readLn :: Read a => IO a

The readLn function combines getLine and readIO.
