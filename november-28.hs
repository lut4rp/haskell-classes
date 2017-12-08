module Ulta where

ulta xs = if xs == []
  then []
  else let h = head xs
           t = tail xs
       in ulta t ++ [h]
