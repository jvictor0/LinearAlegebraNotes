module MakeRefs where

import Data.List
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe

splitAtChar ch l = splitAtH ch l [] 
  where splitAtH ch [] l = [reverse l]
        splitAtH ch (a:as) l = if a == ch then (reverse l):(splitAtH ch as []) else splitAtH ch as (a:l)
        
lookupErr x mp = case Map.lookup x mp of
  Nothing -> error $ "lookupErr: lookuping up " ++ (show x) ++ " in " ++ (show mp)
  (Just res) -> res
        
readCSV = transpose.(map (splitAtChar ',')).lines

readPageNums = do
  dat <- fmap readCSV $ readFile "pagenums - Sheet1.csv"
  return $ Map.fromList $ concatMap (\col -> zipWith (\r i -> ((head col) ++ " " ++ (show i), (show $ (read r :: Int) - 1))) (tail col) [1..]) dat

makeLinks filename htmlfun wordfun = do
  dat <- fmap readCSV $ readFile filename
  pagenums <- readPageNums
  return $ flip map dat $ \(_:rst) 
                          -> flip concatMap rst 
                             $  \s -> if not $ null s 
                                      then "\t$\\Box$~~\\href{"++(htmlfun pagenums s)++"}{"++(wordfun s)
                                           ++"}\n\n\n" 
                                      else ""
    
theWordFun lets r = let (a:as) = words r in unwords $ a:lets:as
theHtmlFun lets pagenums r = let (a:as) = words r 
                    in "http://www.stanford.edu/class/math51/tests/" 
                       ++ (nm a) ++ "-" ++ lets ++".pdf"
                       ++ "\\#page=" ++ (lookupErr ((head $ words r) ++ " " ++ lets ++ " " ++ (init $ head $ tail $ words r)) pagenums) 
  where nm ('A':rst) = rst ++ "aut"
        nm ('S':rst) = rst ++ "spr"
        nm ('W':rst) = rst ++ "win"
        
makeLinksTex lets1 lets2 filename = do
  links <- makeLinks filename (theHtmlFun lets1) (theWordFun lets2)
  return $ concat $ flip map (zip [1..] links) $ \(i,ls) -> "\\newcommand{\\exersises" ++ [['a'..'z']!!(i-1)] ++ (letsNoNum lets1) ++ "}[0]\n{\n" ++ ls ++ "}\n\n"
  
makeAllTexLinks = do
  dats <- fmap concat $ mapM (\i -> makeLinksTex (lets1s!!i) (lets2s!!i) (filenames!!i)) [0,1,2]
  let fstr = dats ++ (flip concatMap [1..26] $ \i -> "\\newcommand{\\exersises" ++ [['a'..'z']!!(i-1)] ++ "}[0]\n{\n" 
                                                     ++ "\t\\subsection{Exam Exercises}\nTry the following exercises from past exams\n\n"
                                                     ++ (concatMap (\l -> "\t\\exersises" ++ [['a'..'z']!!(i-1)] ++ l ++ "\n") $ map letsNoNum lets1s) 
                                                     ++ "}\n\n")
  writeFile "exercises.tex"  fstr
  

lets1s = ["m1","m2","f"]
letsNoNum "m1" = "m"
letsNoNum "m2" = "n"
letsNoNum "f" = "f"
lets2s = ["Midterm 1", "Midterm 2", "Final"]
filenames = ["midterm 1 questions - Sheet1.csv","midterm 2 questions - Sheet1.csv","final questions - Sheet1.csv"]