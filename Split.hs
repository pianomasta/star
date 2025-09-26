module Split
where

chunksOf :: Integral n => n -> [x] -> [[x]]
chunksOf n = chunksOf' n n []
chunksOf' _ _ store [] = [reverse store]
chunksOf' n0 0 store x = reverse store : chunksOf' n0 n0 [] x
chunksOf' n0 n store (a : rest) = chunksOf' n0 (n - 1) (a : store) rest
