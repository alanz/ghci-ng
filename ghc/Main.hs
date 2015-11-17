{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Main (main) where

import MainRunner


import System.Console.Haskeline as Haskeline

-----------------------------------------------------------------------------
-- GHC's command-line interface

main :: IO ()
main = runMain runInputT
