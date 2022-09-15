module Main where

import System.Process

split :: String -> String -> [String]
split st sep
  | length st == 0 = [st]
  | otherwise = splitloop st sep "" [] where
    splitloop st sep word arr
      |  length st == 0 = append arr word
      |  (take (length sep) st) == sep = splitloop (rest (length sep) st) sep "" (append arr word)
      |  otherwise = splitloop (rest (length sep) st) sep (word++(take (length sep) st)) arr
    rest = (\num -> \li -> if num == 0; then li; else let (x:xs) = li in (rest (num-1) xs))
    append li el =
      case li of
        [] -> [el]
        (x : xs) -> x : (append xs el)

join :: [String] -> String -> String
join strlist sep =
  case strlist of
    [last] -> last
    (first : rest) -> first ++ sep ++ (join rest sep)

replace :: String -> String -> String -> String
replace srcstr old new = join (split srcstr old) new

maketable :: String -> String -> String -> String
maketable fname fcontents caption = headers ++format++"}\n"++firstline++" \\\\\n"++restlines++"\\end{tabular}\n\\vspace{3mm}\\caption{"++caption++"}\n\\label{table:"++fname++"}\n\\end{table}"
  where headers = "\\renewcommand{\\arraystretch}{1.5}\n\\rowcolors{2}{gray!20}{gray!40}\n\\begin{table}[h!]\n\\centering\n\\begin{tabular}{"
        format = head sfcontents
        sfcontents = split fcontents "\n"
        firstline = join splitfirstline " &\n"
        splitfirstline = map (\text -> "\\cellcolor{violet!60}\\color{white}"++text) (split (splitcontents!!0) "|")
        restlines = join (map (\text -> replace text "|" " & ") (tail splitcontents)) " \\\\\n"
        splitcontents = tail sfcontents


main :: IO ()
main = do
  putStrLn "Name of the source file?"
  fname <- getLine
  putStrLn "Caption of generated table?"
  caption <- getLine
  fcontents <- readFile fname
  let textable = maketable fname fcontents caption in
    do
      putStrLn ""
      putStrLn textable
      putStrLn ""
      appendFile "out.table" textable
      callCommand "xclip -sel clip out.table"
      putStrLn "Copied!"
      callCommand "rm out.table"
