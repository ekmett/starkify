module Main where

import Commands
import Options

main :: IO ()
main = getCommand >>= runCommand
