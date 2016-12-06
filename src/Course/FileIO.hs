{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
-- run it with `run "path to the files.txt from ./share dir" in ghci
main ::
  IO ()
main =
  getArgs >>= \l ->
  case l of
    h :. _ -> run h
    otherwise -> error "Provide the path to the index file"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
-- run = 
--   error "todo: Course.FileIO#run"
-- run idx =
--   case getFile idx of
--     (_, fs) -> lines fs

run p =
  readFile p >>= \q ->
  getFiles (lines q) >>= \r ->
  printFiles r

runn p = do
  q <- readFile p
  r <- getFiles (lines q)
  printFiles r
    


getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles ps =
  -- sequence will turn the List (IO (FilePath, Chars)) into IO (List (FilePath, Chars))
  sequence (getFile <$> ps)

  -- or refactored
getFiless =
  sequence . (<$>) getFile

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile p =
  -- do q <- readFile p
  --    pure (q, p)
  (\q -> (p, q)) <$> readFile p

-- same as above
getFilee =
  lift2 (<$>) (,) readFile

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles ps = void (sequence ((\(p, c) -> printFile p c) <$> ps))
--         ps = void (sequence ((\(p, c) -> uncurry printFile (p, c)) <$> ps))
--         ps = void (sequence (uncurry printFile <$> ps))
-- move <$> into prefix position
--         ps = void (sequence ((<$>) (uncurry printFile) ps))
-- replace right-associativity with composition (.)
--         ps = void . sequence . (<$>) (uncurry printFile) ps
-- remove ps at all
--            = void . sequence . (<$>) (uncurry printFile)


  -- error "todo: Course.FileIO#printFiles"

-- this needs to print the 2 params on a 2 separate lines. The two params are both Chars
printFile ::
  FilePath
  -> Chars
  -> IO ()

-- Solution
-- printFile p c = putStrLn p *> putStrLn "\n" *> putStrLn c 
printFile p c = putStrLn("=======" ++ p ++ "\n" ++ c) -- this one is more readable because it let us know that expression inside () doesn't do any IO ops where the previous one remove this knowledge
  

-- printFile = readFile >>= (\(IO cs) _ -> putStrLn)
--          IO Chars -> Filename -> Chars -> IO ()

