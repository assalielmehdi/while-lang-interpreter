-- data S = A String A'

-- data A' 
--   = A' Int A'
--   | Stop

-- instance Read S where
--   readsPrec _ s = 
--     let 
--       head' = head . words $ s
--       tail' = unwords . tail . words $ s 
--     in
--       [(A head' (read tail' :: A'), "")]

-- instance Read A' where
--   readsPrec _ ('b':'r':cs) = [(Stop, "")]
--   readsPrec _ s = 
--     let 
--       head' = head . words $ s
--       tail' = unwords . tail . words $ s 
--     in
--       [(A' (read head' :: Int) (read tail' :: A'), "")]

-- instance Show S where
--   show (A str rem) = str ++ " " ++ show rem

-- instance Show A' where
--   show Stop = ""
--   show (A' i rem) = show i ++ " " ++ show rem

-- extract :: Maybe String -> String
-- extract Nothing = ""
-- extract (Just s) = s

-- tokens = S.fromList ["a", "bc", "def", "g", "13434", "0"]

add = (+)

f x = do
  let x' = add x 1
  let x = add x' 2
  let x' = add x 3
  x'

main :: IO ()
main = print . f $ 0