{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Var (Var(..)) where

newtype Var = Var String deriving (Eq, Show)