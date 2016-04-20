{-# LANGUAGE GADTs, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Applicative
import Data.List (sort)

type Set = [Int]

sa <\> sb = sort $ filter (`notElem` sb) sa

sa <+> sb = sort $ sa ++ (sb <\> sa)

sa <*> sb = sort $ filter (`elem` sb) sa

data Block = RD
  { name :: String
  , gen :: Set
  , kill :: Set
  , preds :: Set
  , inset :: Set
  , outset :: Set
  }
  deriving (Eq, Show)

runRD :: [Block] -> [[Block]]
runRD bs =
  let entry = (head bs){outset = []}
      bs' = entry : map (\i ->
          let b = bs !! i
              gen' = gen b
              kill' = kill b
              preds' = preds b
              outset' = gen' <+> (inset b <\> kill')
              inset' = foldr (<+>) [] $ map (outset . (bs' !!)) preds'
          in (bs !! i){inset = inset', outset = outset'}
        ) [1 .. length bs - 1]
      scanbs = bs' : runRD bs'
  in if bs' == bs
        then []
        else scanbs

testcase1 :: [Block]
testcase1 =
  [ RD "Entry" [] [] [] [] []
  , RD "B1" [1,2,3] [4,5,6,7] [0] [] []
  , RD "B2" [4,5] [1,2,7] [1,4] [] []
  , RD "B3" [6] [3] [2] [] []
  , RD "B4" [7] [1,4] [2,3] [] []
  , RD "Exit" [] [] [4] [] []
  ]

shortPrint :: Block -> String
shortPrint b = name b ++ " inset = " ++ show (inset b) ++ ", outset = " ++ show (outset b)

main :: IO ()
main = do
  let p x = sequence_ $ map (putStrLn . shortPrint) x ++ [putStrLn ""]
  mapM_ p $ runRD testcase1

