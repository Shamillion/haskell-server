{-# LANGUAGE OverloadedStrings #-}

module Lib where




fromMaybe (Just e) = e
fromMaybe Nothing  = "???"


head' [] = "???"
head' ls = head ls

last' [] = "???"
last' ls = last ls
